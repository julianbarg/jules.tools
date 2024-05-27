#' Consolidate variations in spelling for entity names using ChatGPT
#' 
#' This function sends a vector of organization names along with instructions to 
#' the ChatGPT API to consolidate variations in spelling.
#' @param api_key A character string representing your OpenAI API key.
#' @param entities A vector of organization names.
#' @param model A character string specifying the model to be used.
#'   Defaults to "gpt-4o".
#' @param seed A string that is used for used to set a seed value for the random 
#'    number generator used by the model.
#' 
#' @return A dataframe with the submitted strings in the first column and 
#'    corrections in the second column.
#' 
#' @examples
#' \dontrun{
#'    entities <- c("ExxonMobil", "Exxon Mobil", "ExxonMobil Corporation")
#'    consolidate_entities("mock_api_key", 
#'                         entities, 
#'                         model = "gpt-4o-2024-05-13", 
#'                         seed = 13
#'    )
#' }
#' 
#' @export
consolidate_entities <- function(api_key,
                                 entities, 
                                 model = "gpt-4o", 
                                 seed = NA){
  # For the OpenAI LLMs (or any other LLM), there is a max token length both for
  # messages you send and receive. For the recent OpenAI models, the input
  # limits are quite generous, and we take advantage of that. We always send the
  # full context but then make sure that the response does not exceed the limit.
  # Based on a conservative estimate, coding a maximum of 5,000 characters at a
  # time should mitigate this issue.
  consolidation_role <- "You are a Research Assistant in a research project. 
  Your task is to consolidate or correct a list of entities. Some will have 
  spelling mistakes or variations even though they are fundamentally the same 
  entity. There might be typos, letters switched around, alternative 
  spellings, or extraneous letters. For instance, you may see \"ExxonMobil\", 
  \"Exxon Mobil\", and  \"ExxonMobil Corporation,\" and you should consolidate 
  all to the same spelling, ideally the smallest common denominator, i.e., 
  \"ExxonMobil\". You may also see entries where a subunit is listed. In that 
  instance, consolidate entries with different subunits to the same parent unit.
  For instance, \"BP Europe\" and \"BP America\" should both become \"BP.\"
  The same goes for different business units. That is, \"Chevron Pipe Line
  Company\" and \"Chevron Refining\" should both become \"Chevron\".
  If multiple entities are mentioned in one entry, but they are all part of the
  same overarching company, you should consolidate the entry to the level
  of the overarching entity.
  You should respond to every request with only a json object 
  with all the original string and proposed consolidation. Where the entity 
  needs no consolitation, simply print the original string twice. Your response 
  should be an array of arrays named \"entities\" just to save some space. Make 
  sure the changes are unequivocal, that is, don't repeat the same entry 
  multiple times with different normalizations. Make as few changes as possible 
  to consolidate the number of unique entries. For example, a request may look
  like this:
  
  Hitachi
  Hitachi, Ltd.
  Hitatchi
  
  Then, you should respond with the following json object:
  
  {
    \"entities\": [
      [\"ExxonMobil\", \"ExxonMobil\"],
      [\"Exxon Mobil\", \"ExxonMobil\"],
      [\"ExxonMobil Corporation\", \"ExxonMobil\"]
    ]
  }
  
  We may send you the coding request in chunk. That is, we may tell you about 
  entities we have already coded in previous chunks; about entities from future
  chunks -- not to be coded right away -- that you need to be aware of for 
  coding for the shortest common denominator; and about the entities for you to 
  code right away in your immediater response."
  
  coded_entities_message <- "First, here is an overview of the entities that we 
  have already coded. Please ensure that your suggestions are in line with these 
  completed consolidations. Do not repeat these. Respond only with your 
  suggestions for consolidations of the entities at the very bottom. Here the 
  coded entities:"
  code_later_message <- "Next, here is a list of entities that we will have 
  you code later. Keep in mind that you should not keep these in your response
  you should keep these entities in mind when you code the entities in this 
  chunk for the lowest common denomonator consolidation across all chunks:"
  current_chunk_message <- "And finally, here are the entities that you should 
  code in your immediate response. That is, in your next response you must only
  include all the original chunks from this list below and your proposed 
  consolidations. Your proposed consolidations should take into consideration
  the previous and following chunks, but you cannot include them in the list
  that you respond with. Here the entities to code right now, separated by
  newlines:"
  
  # Function to call in for consolidation
  consolidate_call <- function(call, chunk, chunk_max){
    message <- chatgpt(api_key, content = call, role = consolidation_role, 
                       model = model, seed = seed) |>
      httr::content()
    
    if (message$choices[[1]]$finish_reason == "stop") {
      message(paste0("Query successful (", chunk, "/", chunk_max, ")."))
      # Could add a tryCatch here if this conversion proves tricky.
      consolidated <- jsonlite::fromJSON(message$choices[[1]]$message$content) |>
        as.data.frame() |>
        `colnames<-`(c("entity", "consolidated"))
      message("Returning dataframe.")
      return(consolidated)
    } else {
      print(message)
      stop("Query not successful. See last API response above.")
    }
  }
  
  counts <- data.frame(entities = entities)
  counts$n <- nchar(counts$entities)
  counts$cum <- cumsum(counts$n)
  counts$chunk <- floor(counts$cum / 5000) + 1
  
  # Skip over the complexity of chunking for short queries.  
  if (max(counts$chunk) == 1) {
    consolidate_task <- "Here is a set of entities for you to consolidate. They 
  are separated by newlines:\n\n"
    consolidate_content <- paste(consolidate_task, "\n\n", 
                                 paste(entities, collapse = "\n"))
    single_call <- consolidate_call(consolidate_content, 1, 1)
    return(single_call)
  } 
  
  consolidated <- data.frame(
    entity = NULL,
    consolidated = NULL
  )
  
  prepare_chunk <- function(chunked_df, chunk_){
    consolidate_task <- ""
    if (chunk_ != 1){
      consolidate_task <- paste0(
        coded_entities_message,
        "\n\n",
        paste(
          utils::capture.output(
            utils::write.table(consolidated, col.names= F, row.names = F)
            ), 
          collapse = "\n"),
        "\n\n"
        )
    }
    if (chunk_ != max(chunked_df$chunk)){
      upcoming <- counts |>
        subset(chunk >= chunk_) |>
        with(entities)
      consolidate_task <- paste0(
        consolidate_task, 
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
    consolidate_task <- paste0(
      consolidate_task,
      current_chunk_message,
      "\n\n",
      paste(current, collapse = "\n")
    )
  }
  
  for (chunk in 1:max(counts$chunk)) {
  # for (chunk in 2) { # For testing
    message(paste("Starting on chunk", chunk, "of", max(counts$chunk)))
    consolidated <- rbind(consolidated,
                          consolidate_call(prepare_chunk(counts, chunk),
                                           chunk,
                                           max(counts$chunk)))
  }
  return(consolidated)
}










