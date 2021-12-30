#' Internal help function generating nrow, ncol and colnames for the target file
#'
#' Generates a list that will be passed to all other functions of the bread package
#' in order to indicate the number of rows and the name and number of columns in the file.
#' The output can be saved in a variable and provided in the other functions in the
#' meta_output argument to save time. Counting rows in very large files can take some time.
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param ... Arguments that must be passed to data.table::fread() like "sep" and "dec".
#'
#' @return A list of 3
#'
#' @examples
#' ## Filtering on 2 columns, using regex.
#' meta_output <- bmeta(file = "./data/test.csv")
#' @export

bmeta <- function(file = NULL, ...){
  args <- list(...)
  output = NULL

  output$nrows <- shell(paste0("wc -l ", file), intern = TRUE) %>%
    str_remove(pattern = " .*$") %>% as.numeric() %>% -1 ## -1 because headers

  unixCmdStr <- paste('head -n 2', file)
  args <- c(cmd = unixCmdStr, args)
  output$colnames <- do.call(fread, args) %>%
    colnames()

  output$ncol <- length(output$colnames)
  return(output)
}
