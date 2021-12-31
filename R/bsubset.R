#' Pre-subsets rows of a data file by index number before loading it in memory
#'
#' Simple wrapper for data.table::fread() allowing to subset rows of data from a file
#' with the Unix sed command. This method is useful if you want to load a file
#' too large for your available memory (and encounter the "cannot allocate vector of size" error).
#' You need to input the index number of the first and last rows you want to load in memory with fread(),
#' or alternatively use either the head or tail arguments to subset the first or last rows of the file.
#'
#' @param file String. Full path to a file
#' @param first_row Numeric. First row of the portion of the file to subset.
#' @param last_row Numeric. Last row of the portion of the file to subset.
#' @param head Numeric. How many rows starting from the first in the file.
#' @param tail Numeric. How many rows starting from the last in the file.
#' @param ... Arguments that must be passed to data.table::fread() like "sep".
#' @keywords big file subset sed allocate vector size
#' @return A dataframe containing the subsetted rows
#' @examples
#' ## Head or Tail... for the first n or last n rows
#' bsubset(file = "./data/test.csv", head = 5)
#' ## Subset from the middle of a file
#' bsubset(file = "./data/test.csv", first_row = 5, last_row = 10)
#' ## first_row defaults as 1 and last_row as the last row of the file
#' bsubset(file = "./data/test.csv", first_row = 5)
#' bsubset(file = "./data/test.csv", last_row = 10)
#' @import data.table
#' @import dplyr
#' @export

bsubset <- function(file = NULL,
                    head = NULL, tail = NULL,
                    first_row = NULL, last_row = NULL,
                    ...){

  args = list(...)

  meta_output <- list()
  meta_output$colnames <- bcolnames(file, ...)

  unixCmdStr <- bsubsetStr(file = file, head = head, tail = tail,
                           first_row = first_row, last_row = last_row,
                           ...) %>%
    paste(file)
  args <- c(cmd = unixCmdStr, args)
  df <- do.call(fread, args)
  ## sed loses the colnames, we put them back on
  colnames(df) <- meta_output$colnames

  return(df)
}
