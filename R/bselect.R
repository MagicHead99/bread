#' Pre-selects columns of a data file before loading it in memory
#'
#' Simple wrapper for data.table::fread() allowing to select columns of data from a file
#' with the Unix cut command. This method is useful if you want to load a file
#' too large for your available memory (and encounter the "cannot allocate vector of size" error).
#'
#' @param file String. Full path to a file
#' @param colnames Vector of strings. Exact names of columns to select. If both colnames and colnums are provided, colnums will be prefered.
#' @param colnums Vector of numeric. Columns index numbers.
#' @param ... Arguments that must be passed to data.table::fread() like "sep" or "dec".
#' @keywords big file select cut allocate vector size
#' @return A dataframe with the selected columns
#' @examples
#' file <- system.file("extdata", "test.csv", package = "bread")
#' ## Select the columns numbered 1 and 3
#' bselect(file = file, colnums = c(1,3))
#' ## Select the columns named "PRICE" and "COLOR"
#' bselect(file = file, colnames = c("PRICE", "COLOR"))
#' @import dplyr
#' @export

bselect <- function(file = NULL,
                    colnames = NULL, colnums = NULL,
                    ...){
  args <- list(...)

  ## Quoting the file to prevent errors due to special characters like ")"
  ## according to environment
  if(.Platform$OS.type == "windows"){
    qfile <- shQuote(file, type = "cmd2")
  } else if(.Platform$OS.type == "unix"){
    qfile <- shQuote(file)
  }

  unixCmdStr <- bselectStr(file = file,
                           colnames = colnames, colnums = colnums,
                           ...) %>%
    paste(qfile)
  args <- c(cmd = unixCmdStr, args)
  df <- do.call(data.table::fread, args)
  return(df)
}
