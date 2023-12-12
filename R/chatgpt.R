#' Access OpenAI Models Directly via OpenAI API
#'
#' This function provides a way to directly interact with the OpenAI models using OpenAI's API.
#' It sets up an HTTP POST request to the OpenAI service to get a response from the specified model.
#'
#' @param api_key A character string representing your OpenAI API key.
#' @param role A character string specifying the role for the interaction (user/system).
#' @param content A character string or list specifying the content for the interaction.
#' @param model A character string specifying the model to be used.
#'   Defaults to "gpt-3.5-turbo".
#' @param referer A character string that is sent as the referer header in the request.
#'   Set to your personal website or "http://localhost:3000" for testing purposes.
#'
#' @return The HTTP response object containing the model's response.
#'
#' @examples
#' \dontrun{
#' response <- chatgpt_xl(
#'   api_key = "your_api_key_here",
#'   role = "user",
#'   content = "Who will win the next Darwin award?",
#'   referer = "http://localhost:3000"
#' )
#' print(content(response))
#' }
#'
#' @export
chatgpt <- function(
    api_key,
    role,
    content,
    model = "gpt-3.5-turbo", # Default model
    referer = "http://localhost:3000" # Default referer for testing
) {
  
  # URL for the OpenAI API
  openai_url <- "https://api.openai.com/v1/chat/completions"
  
  # Set up headers for the HTTP request
  headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key),
    "Referer" = referer
  )
  
  # Prepare the data payload for the POST request
  data <- list(
    model = model,
    messages = list(
      list(role = role, content = content)
    )
  )
  
  # Make the POST request to the OpenAI API
  response <- httr::POST(
    url = openai_url,
    httr::add_headers(.headers = headers),
    body = jsonlite::toJSON(data, auto_unbox = TRUE),
    encode = "json",
    httr::timeout(1500) # Set timeout for the request
  )
  
  # Return the response object from the API
  return(response)
}
