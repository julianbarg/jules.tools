#' Access Extended Array of OpenAI Models via OpenRouter
#'
#' @param api_key A character string. Your OpenAI API key.
#' @param role A character string specifying the role for the interaction.
#' @param content A character string or list specifying the content for the interaction.
#' @param model A character string specifying the model to be used. Default is "gpt-3.5-turbo".
#' @param api_endpoint A character string. The API endpoint for OpenAI. Default is OpenAI's endpoint.
#'
#' @return A list containing the model's response.
#'
#' @examples
#' \dontrun{
#' response <- chatgpt_xl(api_key = "your_api_key_here", role = "system", content = "Hello, ChatGPT!")
#' }
#'
#' @export
chatgpt_xl <- function(
  api_key,
  role,
  content,
  model = "gpt-3.5-turbo",
  api_endpoint = "https://api.openai.com/v1/chat/completions") {

  openrouter_url <- "https://openrouter.ai/api/v1/chat/completions"

  headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", api_key),
    "HTTP-Referer" = api_endpoint,
    "X-Title" = "OpenAI"
  )

  data <- list(
    model = model,
    messages = list(
      list(role = role, content = content)
    )
  )

  response <- httr::POST(
    url = openrouter_url,
    httr::add_headers(.headers = headers),
    body = jsonlite::toJSON(data, auto_unbox = TRUE),
    encode = "json"
  )

  return(response)
}
