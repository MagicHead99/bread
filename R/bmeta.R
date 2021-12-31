#' Internal help function generating nrow and colnames for the target file
#'
#' Generates a list that can be passed to the bfile_split() function
#' in order to indicate the number of rows and the name of columns in the file.
#' The output can be saved in a variable and provided in the meta_output
#' argument to save time. Counting rows in very large files can take some time.
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param ... Arguments that must be passed to data.table::fread() like "sep".
#'
#' @return A list of 2
#'
#' @examples
#' ## Filtering on 2 columns, using regex.
#' meta_output <- bmeta(file = "./data/test.csv")
#' @export

bmeta <- function(file = NULL, ...){
  args <- list(...)
  output = NULL

  output$nrows <- bnrow(file)

  output$colnames <- bcolnames(file, ...)

  return(output)
}
