#' Convert a dataframe into a pandoc table
#' 
#' This function uses write_csv and pandoc to convert a dataframe into a 
#' table that can is usable in pandoc markdown.
#' 
#' @param df Dataframe to be converted.
#' @param file Target to write resulting markdown to.
#' @param max_width Target line width.
#' 
#' @examples
#' \dontrun{
#'    data(iris)
#'    make_pandoc_table(iris, "example.md")
#' } 
#' 
#' @importFrom readr format_csv
#' @importFrom pandoc pandoc_convert
#' @importFrom stringr str_split
#' @importFrom stringr str_locate_all
#' 
#' @export

make_pandoc_table <- function(df, file, max_width = 80){
  df <- data.frame(lapply(df, as.character))
  
  # Compute the effective width of a cell (its longest line)
  effective_width <- function(cell) {
    max(nchar(unlist(str_split(cell, "\n"))))
  }
  
  # Convert the dataframe to a markdown table string using pandoc
  make_string <- function(df_){
    csv_string <- readr::format_csv(df_)
    pandoc::pandoc_convert(
      text = csv_string, 
      from = "csv", 
      to = "markdown",
      version = "system"
    )
  }
  
  # Check the maximum line width in the markdown string
  check_linewidth <- function(md_string){
    max(sapply(str_split(md_string, "\n"), nchar))
  }
  
  md_string <- make_string(df)
  
  # Candidate columns that we can try to modify
  candidate_cols <- names(df)
  
  while (check_linewidth(md_string) > max_width) {
    if(length(candidate_cols) == 0){
      message("No modifiable columns left, writing as-is.")
      break
    }
    
    # Identify the candidate column with the highest standard deviation of cell widths
    col_sd <- apply(df[, candidate_cols, drop=FALSE], 2, function(col) 
      stats::sd(sapply(col, effective_width))
    )
    shorten_col <- names(col_sd)[which.max(col_sd)]
    
    # Within that column, select the row with the maximum effective width
    shorten_row <- which.max(sapply(df[[shorten_col]], effective_width))
    offending_text <- df[shorten_row, shorten_col]
    
    # Check if the longest word in the cell dictates its effective width.
    # If so, exclude this column from further processing.
    words <- unlist(str_split(offending_text, "\\s+"))
    longest_word_len <- if(length(words) > 0) max(nchar(words)) else 0
    
    # If the effective width is dictated by the longest word or if the column title is longer,
    # then no further wrapping will help; exclude the column from further processing.
    if(length(words) == 0 || effective_width(offending_text) == longest_word_len || 
       nchar(shorten_col) >= longest_word_len) {
      message("Column '", shorten_col, "' cannot be shortened further, excluding it from processing.")
      candidate_cols <- setdiff(candidate_cols, shorten_col)
      md_string <- make_string(df)
      next
    }
    
    # Determine current line count and reduce the width iteratively to force one extra line break
    current_lines <- length(str_split(offending_text, "\n")[[1]])
    new_width <- effective_width(offending_text) - 1
    
    repeat {
      wrapped_text <- str_wrap(offending_text, width = new_width)
      new_lines <- length(str_split(wrapped_text, "\n")[[1]])
      if (new_lines > current_lines || new_width <= 1) break
      new_width <- new_width - 1
    }
    
    df[shorten_row, shorten_col] <- wrapped_text
    md_string <- make_string(df)
  }
  
  writeLines(md_string, file)
}
