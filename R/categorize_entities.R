#' Provide a brief description and some examples to categorize entities
#' 
#' This function sends your vector of entity names alongside your categories
#' and brief descriptions to the ChatGPT API to obtain a categorized 
#' dataset.
#' @param api_key A character string representing your OpenAI API key.
#' @param entities A vector of entity names.
#' @param description A description of the categories and the abbreviations to 
#'   for ChatGPT to use.
#' @param examples A dataframe with with two columns for ChatGPT to learn from
#'   (optional). The first column is assumed to contain entities and the 
#'   second column is assumed to contain labels.
#' @param model A character string specifying the model to be used.
#'   Defaults to "gpt-4o".
#' @param seed A string that is used for used to set a seed value for the random 
#'    number generator used by the model.
#'
#' @return A dataframe with your submitted entities in the first column and 
#'    categorizations in the second row.
#'
#' @examples
#' \dontrun{
#'    entities <- c("blue", "triangle", "purple", "cube")
#'    description <- "Categorize the entities as color or shape."
#'    examples <- data.frame(
#'     "entity" = c("blue", "triangle"),
#'     "label" = c("color", "shape")
#'    )
#'    categorize_entities("your_api_key_here", entities = entities, 
#'                        description = description, examples = examples)
#'    
#' }
#' 
#' @export
categorize_entities <- function(api_key, 
                                entities, 
                                description, 
                                examples = NULL, 
                                model = "gpt-4o", 
                                seed = NA){
  categorization_role <- "You are a Research Assistant in a research project.
  Your task is to code entities for us. We have gathered a dataset of various
  entities and need to get an overview over what our dataset contains before
  we identify each entity in detail. For instance, we may provide you with a
  list of \"entity a,\" \"entity b,\" and \"entity c,\" and ask you to apply
  labels \"x\" and \"y\" to them. You should respond to every request with
  only a json object with all the original strings and your proposed labels.
  Where you are unable to make a determination, simply respond \"uncertain.\"
  For instance, to the example above you may respond with the following json
  object:
  
  {
    \"entities\": [
      [\"entity a\", \"x\"],
      [\"entity b\", \"y\"],
      [\"entity c\", \"z\"]
    ]
  }
  
  We may send you our coding requests in chunks. That is, we might send you a
  request that includes three parts: (1) For reference, a list of entities that 
  have already been coded and their codes; (2) for reference, a list of 
  that we may send to you or another coder in a subsequent request; (3) a list 
  of the entities that you should code right away in your immediate response.
  When our request is chunked, respond only with a list of the entities for you 
  to code right away. Parts (1) and (2) are only included for reference."
  
  coded_entities_messages <- "First, here is list of the entities that have 
  already been coded. Please make sure your proposed labels are in line with
  how we have coded these. Do not repeat these! Respond only with your 
  suggestions for coding the entities at the very bottom. Here are the entities
  that we have already coded:"
  code_later_message <- "Next, here is a list of entities that we will have 
  you or another research assistant code later. Keep in mind that you should
  not include these in your response. These are included only for reference:"
  current_chunk_message <- "And finally, here are the entities that you should
  code in your immediate response. That is, in your next response you must only
  include all the entities from this list below and your proposed labels. 
  That is, your response may take into consideration entities from the 
  following and previous chunks, but you cannot include them in the json
  object that you respond with. Here are the entities for you to code right
  away, separated by newlines:"
  
  reminder <- "Remember to respond with a json object and follow this format:
  
  {
    \"entities\": [
      [\"entity 1\", \"label 1\"],
      [\"entity 2\", \"label 2\"],
      [\"entity 3\", \"label 3\"]
    ]
  }"
  
  role <- paste0(categorization_role, "\n\n", description)
  
  # Number of entities in number of characters to include in one chunk. 
  # Based on sending a long request and seeing when it cuts for length and then
  # reducing by 30%. So no exact science.
  # I will occasionally increase this number until I run into trouble.
  chunk_size <- 6100
  
  categorization_call <- function(call, chunk, chunk_max){
    message <- chatgpt(api_key, content = call, role = role, 
                       model = model, seed = seed) |>
      httr::content()
    if (message$choices[[1]]$finish_reason == "stop") {
      message(paste0("Query successful (", chunk, "/", chunk_max, ")."))
      # Could add a tryCatch here if this conversion proves tricky.
      coded_now <- jsonlite::fromJSON(message$choices[[1]]$message$content) |>
        as.data.frame() |>
        `colnames<-`(c("entity", "label"))
      message("Returning dataframe.")
      return(coded_now)
    } else {
      print(message)
      stop("Query not successful. See last API response above.")
    }
  }
  
  counts <- data.frame(entities = entities)
  counts$n <- nchar(counts$entities)
  counts$cum <- cumsum(counts$n)
  counts$chunk <- floor(counts$cum / chunk_size) + 1
  
  if (!is.null(examples)) {
    coded <- examples
    colnames(coded) <- c("entity", "label")
  } else {
    coded <- data.frame(entity = NULL, label = NULL)
  }
  
  prepare_chunk <- function(chunked_df, chunk_){
    categorization_task <- ""
    if (chunk_ != 1 | !is.null(examples)){
      categorization_task <- paste0(
        coded_entities_messages,
        "\n\n", 
        paste(
          utils::capture.output(
            utils::write.table(coded, col.names= F, row.names = F)
          ), 
          collapse = "\n"),
        "\n\n"
      )
    }
    if (chunk_ != max(chunked_df$chunk)){
      upcoming <- counts |>
        subset(chunk >= chunk_) |>
        with(entities)
      categorization_task <- paste0(
        categorization_task, 
        code_later_message,
        "\n\n",
        paste(upcoming, collapse = "\n"),
        "\n\n"
      )
    }
    
    # Finally, add current chunk
    current <- counts |>
      subset(chunk == chunk_) |>
      with(entities)
    categorization_task <- paste0(
      categorization_task,
      current_chunk_message,
      "\n\n",
      paste(current, collapse = "\n"),
      "\n\n", 
      reminder
    )
    categorization_task
  }
  
  for (chunk in 1:max(counts$chunk)) {
    # for (chunk in 2) { # For testing
    message(paste("Starting on chunk", chunk, "of", max(counts$chunk)))
    coded <- rbind(coded,
                   categorization_call(prepare_chunk(counts, chunk),
                                       chunk,
                                       max(counts$chunk)))
  }
  return(coded)
}





