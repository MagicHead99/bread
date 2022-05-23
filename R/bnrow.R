#' Count the number of rows of a big file without loading it in memory
#'
#' Counts the number of rows using data.table::fread() and the "wc" Unix command.
#' This allows analyzing big files that would not fit in memory (and cause an error
#' such as "cannot allocate vector of size").
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#'
#' @return A numeric
#'
#' @examples
#' file <- system.file("extdata", "test.csv", package = "bread")
#' ## Counting rows (almost like the band)
#' bnrow(file = file)
#' @import dplyr
#' @export


bnrow <- function(file = NULL){

  ## Quoting the file to prevent errors due to special characters like ")"
  ## according to environment
  if(.Platform$OS.type == "windows"){
    qfile <- shQuote(file, type = "cmd2")
  } else if(.Platform$OS.type == "unix"){
    qfile <- shQuote(file)
  }

  nrows <- system(paste0("wc -l ", qfile), intern = TRUE) %>%
    stringr::str_remove(pattern = " .*$") %>% as.numeric() %>% -1 ## -1 because headers
  return(nrows)
  }
