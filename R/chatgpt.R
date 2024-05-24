#' Access OpenAI Models Directly via OpenAI API
#'
#' This function provides a way to directly interact with the OpenAI models using OpenAI's API.
#' It sets up an HTTP POST request to the OpenAI service to get a response from the specified model.
#'
#' @param api_key A character string representing your OpenAI API key.
#' @param content A character string or list specifying the content for the interaction.
#' @param role A character string specifying the role for the interaction (system). 
#'   Defaults to "You are a helpful assistant." 
#'   When json_out is TRUE, defaults to "You are a helpful assistant designed to output JSON."
#' @param model A character string specifying the model to be used.
#'   Defaults to "gpt-4o".
#' @param json_out A logical value to indicate whether the response should be formatted as json rather than as a text string. 
#'   Defaults to TRUE. 
#' @param seed A string that is used for used to set a seed value for the random number generator used by the model.
#'
#' @return The HTTP response object containing the model's response.
#'
#' @examples
#' \dontrun{
#' response <- chatgpt(
#'   api_key = "your_api_key_here",
#'   content = "Who will win the next Darwin award?"
#' )
#' print(httr::content(response))
#' }
#'
#' @export
chatgpt <- function(api_key,
                    content,
                    role = NA,
                    model = "gpt-4o",
                    json_out = T,
                    seed = NA) {
  # Get correct role
  if (!is.na(role)) {
    role_ = role
  } else if (json_out) {
    role_ = "You are a helpful assistant designed to output JSON."
  } else {
    role = "You are a helpful assistant."
  }
  
  # Set up headers for the POST request.
  headers <- httr::add_headers(c(
    `Content-Type` = "application/json",
    Authorization = paste("Bearer", api_key)
  ))
  
  # Prepare data payload
  body_payload <- list(model = model,
                       messages = list(
                         list(role = "system",
                              content = role_),
                         list(role = "user",
                              content = content)
                       ))
  if (json_out) {
    body_payload$response_format = list(type = "json_object")
  }
  if (!is.na(seed)) {
    body_payload$seed = seed
  }
  
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    headers,
    body = jsonlite::toJSON(body_payload, auto_unbox = TRUE),
    encode = "json"
  )
  
  return(response)
}
