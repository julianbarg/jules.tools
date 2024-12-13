#' Convert a dataframe into a pandoc table
#' 
#' This function uses write_csv and pandoc to convert a dataframe into a 
#' table that can is usable in pandoc markdown.
#' 
#' @param df Dataframe to be converted.
#' @param file Target to write resulting markdown to.
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

  make_string <- function(df_){
    csv_string <- readr::format_csv(df_)
    md_string <- pandoc::pandoc_convert(
      text = csv_string, 
      from = "csv", 
      to = "markdown",
      version = "system"
      )
    md_string
  }
  
  check_linewidth <- function(md_string){
    stringr::str_split(md_string, "\n") |> # strsplit cannot handle empty string
      sapply(nchar) |>
      max()
  }
  
  replace_space <- function(long_str){
    # Not entirely precise but precise enough
    lines <- stringr::str_locate_all(long_str, "[\n]|^|$")[[1]][,1]
    # lines <- unlist(gregexpr("[\n]|^|$", long_str)) # built-in does not work 
    spaces <- unlist(gregexpr(" ", long_str))
    # That operation returns -1 if there are no spaces, otherwise it is a vector
    if (all(spaces %in% -1)){
      message("No spaces found to insert linebreak, writing as-is.")
      return(NULL)
    }
    
    break_rank <- abs(outer(lines, spaces, FUN = "-")) |>
      apply(2, min) |>
      which.max()
    break_loc <- spaces[break_rank]
    substr(long_str, break_loc, break_loc) <- "\n"
    
    long_str
  }
  
  md_string <- make_string(df)
  
  while (check_linewidth(md_string) > max_width){
    # We should really figure out first whether there are rows that don't 
    # include spaces, but I will not handle that edge case for now.
    col_sd <- apply(df, 2, function(row) sd(sapply(row, check_linewidth)))
    # To lazy to handle ties right now but they could occur.
    shorten_col <- names(col_sd[which.max(col_sd)])
    # This conveniently returns only the first match
    shorten_row <- which.max(sapply(df[[shorten_col]], check_linewidth))
    
    shortened_entry <- replace_space(df[shorten_row, shorten_col])
    if (is.null(shortened_entry)) {
      break
    }
    
    df[shorten_row, shorten_col] <- shortened_entry
    md_string <- make_string(df)
  }
  
  writeLines(md_string, file)
}






















