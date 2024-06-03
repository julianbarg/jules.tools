#' Use ChatGPT to match a entities against a list of known relevant matches
#'
#' Provide two lists and receive back a dataframe of fuzzy matches. This 
#' function sends both lists to ChatGPT wrapped in a prompt and with a role
#' to receive back a fuzzy-matched dataframe with all entities contained in 
#' the first list and matches from the second list.
#'
#' This function provides a way to directly interact with the OpenAI models 
#' using OpenAI's API. It sets up an HTTP POST request to the OpenAI service 
#' to get a response from the specified model.
#'
#' @param api_key A character string representing your OpenAI API key.
#' @param list_1 description
#' @param list_2 descr
#' @param model A character string specifying the model to be used.
#'   Defaults to "gpt-4o".
#' @param json_out A logical value to indicate whether the response should be 
#'    formatted as json rather than as a text string. Defaults to TRUE. 
#' @param seed A string that is used for used to set a seed value for the 
#'    random number generator used by the model.
#'
#' @return A dataframe with all entities from the first list and, where 
#'    applicable, matches from the second list.
#'
#' @examples
#' \dontrun{
#' dataset <- c("entity a", "entity b, inc", "entity c")
#' of_interest <- c("entity b", "entity d")
#' 
#' matches <- llmatch("mock_api_key", 
#'                    dataset,
#'                    of_interest)
#' }
#' @export
llmatch <- function(api_key, 
                    list_1, 
                    list_2, 
                    model = "gpt-4o",
                    json_out = T,
                    seed = NA
) {
  # Prompt part about coding request in chunks not required in this task.
  # It doesn't really matter how other entities are matched when working
  # on subsequent entities. Probably assuming that consolidation is already 
  # done.
  role <- "You are a Research Assistant in a research project. We want to 
  identify certain entities that could be present in our dataset. For that 
  purpose, we will provide you with a list of entities of interest, and a list
  of entities from our research project. Your task will be to check our database
  against the list of entities one by one. The key challenge is that the 
  spelling may diverge between our dataset and the reference lists.
  
  Next, we will provide you with the input data. We will send you the dataset
  and the list, both of which contain entities separated by newlines. For 
  instance, you would receive a request in the following format:
  
  dataset:
  
  entity a
  entity b, inc
  entity c
  
  entities of interest:
  entity b
  entity d corporation
  
  Then, you should respond with the following json object:
  
  {
    \"matches\": [
      [\"entity a\", \"\"],
      [\"entity b, inc\", \"entity b\"]
      [\"entity c\", \"\"]
    ]
  }
  
  Importantly, your response should only include this json object. As shown in
  the example, your response should include every entity from the dataset we 
  send to you, and list matches only where applicable."
  
  dataset_message <- "First, we will send you our dataset. All entries in our 
  dataset are separated by newlines. Keep in mind that you need to include all
  of these entities in your response."
  entities_of_interest_message <- "Next, we will send you our list of entities
  of interest. These are also separated by newlines. You should not include
  these in your response unless they match an entity in our dataset, in which
  case you should list it in the json object alongside the matching entity."
  reminder_message <- "This is all the information you need to complete the 
  task. Remember to respond with a json object. This json object should 
  include all entities from our dataset with a potential match where one 
  exists."
  # Guess based on other functions, probably more conservative than necessary.
  chunk_size <- 7100 
        
  counts <- data.frame(entities = list_1)
  counts$n <- nchar(counts$entities)
  counts$cum <- cumsum(counts$n)
  counts$chunk <- floor(counts$cum / chunk_size) + 1
  
  
  prep_call <- function(chunk, chunk_max){
    chunk_entities <- counts[counts$chunk == chunk,] |>
      with(entities) |>
      paste0(collapse = "\n")
    of_interest <- paste0(list_2, collapse = "\n")
    paste0(dataset_message, "\n\n", chunk_entities, "\n\n",
           entities_of_interest_message, "\n\n", 
           of_interest, "\n\n", reminder_message)
    
  }
  
  match_call <- function(call, chunk, chunk_max){
    message <- chatgpt(api_key, content = call, role = role, 
                       model = model, seed = seed) |>
      httr::content()
    
    if (message$choices[[1]]$finish_reason == "stop") {
      message(paste0("Query successful (", chunk, "/", chunk_max, ")."))
      # Could add a tryCatch here if this conversion proves tricky.
      matched_now <- jsonlite::fromJSON(message$choices[[1]]$message$content) |>
        as.data.frame() |>
        `colnames<-`(c("entity", "match"))
      message("Returning dataframe.")
      return(matched_now)
    } else {
      print(message)
      stop("Query not successful. See last API response above.")
    }
  }
  
  matched <- data.frame(entity = NULL, match = NULL)
  
  for (chunk in 1:max(counts$chunk)){
    message(paste("Starting on chunk", chunk, "of", max(counts$chunk)))
    matched <- rbind(matched, 
                     match_call(prep_call(chunk, max(counts$chunk)), 
                                chunk, 
                                max(counts$chunk)) 
                     )
  }
  matched$match[matched$match == ""] <= NA
  return(matched)
}
