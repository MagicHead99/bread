#' Retrieve the column names directly from a big file
#'
#' Simply reads the first line of a file with fread and the head Unix command.
#' This allows analyzing big files that would not fit in memory (and cause an error
#' such as "cannot allocate vector of size").
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param ... Arguments that must be passed to data.table::fread() like "sep".
#'
#' @return A character vector
#'
#' @examples
#' ## Retrieving the column names
#' bcolnames(file = "./data/test.csv")
#' @import data.table
#' @import dplyr
#' @export

bcolnames <- function(file = NULL, ...){
  args <- list(...)

  ## We get the 2 first rows - which is not much slower than one row - because
  ## in some cases, the first row alone will not be parsed cleanly by colnames()
  unixCmdStr <- paste('head -n 2', file)
  args <- c(cmd = unixCmdStr, args)
  colnames <- do.call(fread, args) %>%
    colnames()

  return(colnames)
}
