#' Access OpenAI Models via OpenRouter API
#'
#' This function provides a way to interact with the OpenAI models using the OpenRouter API.
#' It sets up an HTTP POST request to the OpenRouter service to get a response from the specified model.
#'
#' @param api_key A character string representing your OpenAI API key.
#' @param referer A character string that is sent as the referer header in the request.
#'   Set to your personal website or "http://localhost:3000" for testing purposes.
#'   See more at https://openrouter.ai/docs.
#' @param role A character string specifying the role for the interaction (user/system).
#' @param content A character string or list specifying the content for the interaction.
#' @param model A character string specifying the model to be used.
#'   Defaults to "gpt-3.5-turbo".
#'
#' @return The HTTP response object containing the model's response.
#'
#' @examples
#' \dontrun{
#' response <- chatgpt_xl(
#'   api_key = "your_api_key_here",
#'   referer = "http://localhost:3000",
#'   role = "user",
#'   content = "Who will win the next Darwin award?"
#' )
#' print(content(response))
#' }
#'
#' @export
chatgpt <- function(
    api_key,
    referer = "http://localhost:3000", # Default referer for testing
    role,
    content,
    model = "gpt-3.5-turbo" # Default model
) {

  # URL for the OpenRouter API
  openrouter_url <- "https://api.openai.com/v1/chat/completions"

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

  # Make the POST request to the OpenRouter API
  response <- httr::POST(
    url = openrouter_url,
    httr::add_headers(.headers = headers),
    body = jsonlite::toJSON(data, auto_unbox = TRUE),
    encode = "json",
    httr::timeout(1500) # Set timeout for the request
  )

  # Return the response object from the API
  return(response)
}
