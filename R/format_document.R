#' Exclude a metric from formatting
#'
#' This function marks a metric to ensure that it is printed as digits and
#' not printed as text for numbers one through twelve.
#' 
#' 
#' @param x A numeric value.
#' @return A character string token to be processed by the knitr inline hook.
#' @export
as_digits <- function(x) {
  paste0("##DIGITS##", x)
}

#' Format document
#'
#' This function triggers a knitr inline hook which ensures that numbers are 
#' formatted correctly for a manuscript. Numbers from one through twelve are
#' spelled out unless explicitly excepted from this with the `as_digits` 
#' function. For large numbers, we ensure that the thousands etc. digits are 
#' comma separated.
#' @export

format_document <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("The 'knitr' package is required but not available.")
  }
  
  # Define helper functions within the activation function
  is_numeric_convertible <- function(x) {
    !any(is.na(suppressWarnings(as.numeric(x))))
  }
  
  
  
  # Set the knitr inline hook using the helper functions
  knitr::knit_hooks$set(inline = function(x) {
    if (is.character(x) && grepl("##DIGITS##", x)) {
      x <- gsub("##DIGITS##", "", x)
      return(format(as.numeric(x), big.mark = ","))
    }
    
    if (is_numeric_convertible(x)) {
      x <- as.numeric(x)
      if (x <= 12 && x > 0) {
        numbers <- c("one", "two", "three", "four", "five", "six", "seven",
                     "eight", "nine", "ten", "eleven", "twelve")
        return(numbers[x])
      } else {
        return(format(x, big.mark = ","))
      }
    } else {
      return(as.character(x))
    }
  })
}