#' Internal helper function generating the cut Command String
#'
#' Writes a string containing a cut call from the function parameters
#'
#' @param file String. Full path to a file
#' @param colnames Vector of strings. Exact names of columns to select. If both colnames and colnums are provided, colnums will be prefered.
#' @param colnums Vector of numeric. Columns index numbers.
#' @param meta_output List. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#' @param ... Arguments that must be passed to data.table::fread() like "sep". Only used for the bmeta() call here.
#' @keywords select cut
#' @return A string
#' @examples
#' bselectStr(file = "./data/test.csv", colnums = c(1,3))
#' bselectStr(file = "./data/test.csv", colnums = c("PRICE", "COLOR"))


bselectStr <- function(file = NULL,
                       colnames = NULL, colnums = NULL,
                       meta_output = NULL,
                       ...){
  #args <- list(...)

  if(is.null(meta_output)){
    meta_output = bmeta(file, ...)
  }

  ## Case1: colnums provided but not colnames
  if(is.null(colnames)){
    if(!is.null(colnums)){
      colnumStr <- paste(colnums, collapse = ",")
    }
    ## Case2: neither colnums nor colnames provided
    else {
      stop("*** One of colnames OR colnums needed ! ***")
    }
    ## Case3: both colnums and colnames provided
  } else {
    if(!is.null(colnums)){
      warning("*** if both colnums and colnames are provided, colnums takes over (arbitrarily) ***")
      colnumStr <- paste(colnums, collapse = ",")
      ## Case4: colnames provided but not colnums
    } else {
      colnums <- match(colnames, meta_output$colnames)
      colnumStr <- paste(colnums, collapse = ",")
    }
  }
  ## unix cmd to cut the selected columns
  unixCmdStr <- paste0('cut -d"', sep,'" -f', colnumStr, " ")
  return(unixCmdStr)
}
