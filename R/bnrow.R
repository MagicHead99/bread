#' Count the number of rows of a big file without loading it in memory
#'
#' Counts the number of rows using data.table::fread() and the 'wc' Unix command.
#' This allows analyzing big files that would not fit in memory (and cause an error
#' such as 'cannot allocate vector of size').
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#'
#' @return A numeric
#'
#' @examples
#' file <- system.file('extdata', 'test.csv', package = 'bread')
#' ## Counting rows (almost like the band)
#' bnrow(file = file)
#' @export


bnrow <- function(file = NULL){

  if(grepl(pattern = "\\'", file) == T){
    stop("### Can't parse because filename contains the character \"'\".")
  }

  ## Getting full path, in case the file is in the wd
  file <- normalizePath(path = file)
  if(startsWith(file, "\\")){
    file <- gsub(pattern = "\\\\", replacement = "/", x = file)
  }

  nrows <- system(paste0('wc -l \'', file, '\''), intern = TRUE)
  nrows <- gsub(pattern = ' .*$', replacement = '', x = nrows)
  nrows <- as.numeric(nrows) - 1L ## -1 because headers

  return(nrows)
  }
