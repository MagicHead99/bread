#' Internal helper function generating the grep Command String
#'
#' Writes a string containing a grep call from the function parameters
#'
#' @param file String. Full path to a file
#' @param patterns Vector of strings. One or several patterns used to filter the data from the input file. Each element of the vector should correspond to the column to be filtered. Can use regular expressions.
#' @param filtered_columns Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the pattern with which it will be filtered.
#' @param meta_output List. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#' @keywords filter grep
#' @return A string
#' @examples
#' bfilterStr(file = "./data/test.csv", patterns = c("2002", "red"), filtered_columns = c("YEAR", "COLOR"))


bfilterStr <- function(file = NULL,
                       patterns = NULL,
                       filtered_columns = NULL,
                       meta_output = NULL){

  if((length(patterns) != length(filtered_columns)) & !is.null(filtered_columns)){
    stop('\n *** patterns must correspond to the filtered_columns (vectors of \n
    same length). If several values must be matched in some of the columns\n
    please use regexp "or" = "|" ***')
  }

  if(is.null(meta_output)){
    meta_output = bmeta(file, ...)
  }

  filterStr <- paste(patterns, collapse = "|")
  unixCmdStr <- paste0('grep -aEu "', filterStr, '" ')
  return(unixCmdStr)
}
