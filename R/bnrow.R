#' Count the number of rows of a big file
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
#' ## Counting rows (almost like the band)
#' bnrow(file = "./data/test.csv")
#' @import stringr
#' @import dplyr
#' @export


bnrow <- function(file = NULL){
  nrows <- shell(paste0("wc -l ", file), intern = TRUE) %>%
    str_remove(pattern = " .*$") %>% as.numeric() %>% -1 ## -1 because headers
  return(nrows)
  }
