#' Tries to identify the separator / delimiter used in a table format file
#'
#' The function reads the first row and tests the following common separators by default:
#'       \code{";" "\\t" " " "|" ":" ","}
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param ntries Numeric. Number of rows to check for
#' @param separators Vector of strings. Additional uncommon delimiter to check for
#'
#' @return A string
#'
#' @examples
#' file <- system.file("extdata", "test.csv", package = "bread")
#' ## Checking the delimiter on the first 12 rows, including headers
#' bsep(file = file, ntries = 12)
#' @import dplyr
#' @export

bsep <- function(file, ntries = 10, separators = c(";", "\t", " ", "|", ":", ",")){

  # meta_output <- list()
  # meta_output$colnames <- bcolnames(file)

  ## init loop
  ii <- 1

  ## Quoting the file to prevent errors due to special characters like ")"
  ## according to environment
  if(.Platform$OS.type == "windows"){
    qfile <- shQuote(file, type = "cmd2")
  } else if(.Platform$OS.type == "unix"){
    qfile <- shQuote(file)
  }

  rowz <- system(command = paste0("head -n ", ntries, " ", qfile), intern = T)

  while(!exists("sep")){
    ## if the number of separators is equal in all the rows and not zero
    ## we have found the separator (probably)
    count_per_row <- stringr::str_count(string = rowz, pattern = stringr::fixed(separators[ii]))
    if(length(unique(count_per_row))==1 & unique(count_per_row)[1]!=0){
      sep <- separators[ii]
      break
    } else {
      ii <-  ii + 1
    }
    if(ii > length(separators)){
      stop("*** ERROR: We are having trouble determining the separator,
             please add a sep = '...' argument ***")
    }
  }
return(sep)
}
