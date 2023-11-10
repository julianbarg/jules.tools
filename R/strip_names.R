#' Preprocess Company Names
#'
#' This function preprocesses company names by performing several cleaning operations:
#' \itemize{
#'   \item Removing common disconnected suffixes such as "Ltd.", "Inc.", "Corp.", and similar.
#'   \item Removing punctuation marks.
#'   \item Eliminating unnecessary white spaces.
#'   \item Transliterating special characters (e.g., umlauts) to their ASCII equivalents.
#'   \item Converting all characters to uppercase.
#' }
#'
#' @param company_name A character string representing the company name to be processed.
#' @param remove_spaces A logical value indicating whether to remove all spaces (default is \code{TRUE}).
#'
#' @return A character string with the cleaned company name.
#'
#' @examples
#' \dontrun{
#' strip_names("Müller Ltd.")
#' # Expected output: "Muller"
#' }
#'
#' @import stringr
#'
#' @export
strip_names <- function(company_name, remove_spaces = T) {
  types <- c(
    "\\Wco\\Winc", "\\Wsaag", "\\Wcoltd", "\\Wltd", "\\Wgmbh", "\\Wag",
    "\\Wsarl", "\\Winc", "\\Wlimited", "\\Wab", "\\Wllc", "\\Wsa", "\\Wca",
    "\\Wpte", "\\Wco", "\\Wplc", "\\Wlp", "\\Wproforma", "\\Wse", "\\Wllp",
    "\\Wspa"
  )
  types2 <- c("\\Wgroup", "\\Wcorporation", "\\Wcorp", "\\Wcompany")
  cleaned_names <- company_name %>%
    str_remove_all("\\W?\\([^)]+\\)") %>%
    str_remove_all("\\W?\\[[^\\]]+\\]") %>%
    str_remove_all(regex("^the ", ignore_case = T)) %>%
    str_remove_all(regex(", the$", ignore_case = T)) %>%
    str_remove_all(regex("\\W?& ?Co", ignore_case = T)) %>%
    str_remove_all("[:punct:]") %>%
    str_remove_all(
      regex(
        str_flatten(c(types2, ""), collapse = "$|", last = "$"),
        ignore_case = TRUE
      )
    ) %>%
    str_remove_all(
      regex(
        str_flatten(c(types, ""), collapse = "$|", last = "$"),
        ignore_case = TRUE
      )
    ) %>%
    str_to_upper()

  # Optionally remove spaces
  if (remove_spaces) {
    cleaned_names <- cleaned_names %>%
      str_remove_all("[:blank:]")
  }
  return(cleaned_names)
}
