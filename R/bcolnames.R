#' Retrieve the column names directly from a big file without loading it in memory
#'
#' Simply reads the first line of a file with data.table::fread and the head Unix command.
#' This allows analyzing big files that would not fit in memory (and cause an error
#' such as 'cannot allocate vector of size').
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param ... Arguments that must be passed to data.table::fread() like 'sep'.
#'
#' @return A character vector
#'
#' @examples
#' file <- system.file('extdata', 'test.csv', package = 'bread')
#' ## Retrieving the column names
#' bcolnames(file = file)
#' @import dplyr
#' @export

bcolnames <- function(file = NULL, ...){
  args <- list(...)

  ## Quoting the file to prevent errors due to special characters like ')'
  ## according to environment
  if(.Platform$OS.type == 'windows'){
    qfile <- shQuote(file, type = 'cmd2')
  } else if(.Platform$OS.type == 'unix'){
    qfile <- shQuote(file)
  }

  ## We get the 2 first rows - which is not much slower than one row - because
  ## in some cases, the first row alone will not be parsed cleanly by colnames()
  unixCmdStr <- paste("head -n 2", qfile)
  args <- c(cmd = unixCmdStr, args)
  colnames <- do.call(data.table::fread, args) %>%
    colnames()

  return(colnames)
}
