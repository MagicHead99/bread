#' Internal helper function generating the sed Command String
#'
#' Writes a string containing a sed call from the function parameters
#'
#' @param file String. Full path to a file
#' @param first_row Numeric. First row of the portion of the file to subset.
#' @param last_row Numeric. Last row of the portion of the file to subset.
#' @param head Numeric. How many rows starting from the first in the file.
#' @param tail Numeric. How many rows starting from the last in the file.
#' @param meta_output List. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#' @param ... Arguments that must be passed to data.table::fread() like "sep". Only used for the bmeta() call here.
#' @keywords subset sed
#' @return A string
#' @examples
#' bsubsetStr(file = "./data/test.csv", head = 5)
#' bsubsetStr(file = "./data/test.csv", first_row = 5, last_row = 10)


bsubsetStr <- function(file = NULL,
                       first_row = NULL, last_row = NULL,
                       head = NULL, tail = NULL,
                       meta_output = NULL,
                       ...){

  if(is.null(meta_output)){
    meta_output = bmeta(file, ...)
  }

  ### input checks:
  if(((!is.null(head) | !is.null(tail)) & (!is.null(first_row) | !is.null(last_row))) |
     (!is.null(head) & !is.null(tail))){
    stop("*** You can use only one of 'head' OR 'tail' OR 'first_row & last_row' ***")
  }
  ### autofill missing parameters
  if(is.null(first_row)){ first_row <- 1 }
  if(is.null(last_row)){ last_row <- meta_output$nrows }
  if(first_row < 1 | last_row < 1 | !(first_row == round(first_row)) |
     !(last_row == round(last_row))){
    stop("*** first_row and last_row must be positive integer numbers ***")
  }




  ### String building
  if(is.null(head) & is.null(tail)){
    unixCmdStr <- paste0('sed -e 1,', (first_row), 'd;', (last_row + 1),'q ')
  } else if(!is.null(head)){
    unixCmdStr <- paste0('sed ', (head + 1), 'q ')
  } else {
    ###
    unixCmdStr <- paste0('sed -e 1,', last_row - tail, 'd;', (last_row + 1), 'q ')
  }
  return(unixCmdStr)
}
