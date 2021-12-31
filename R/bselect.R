#' Pre-selects columns of a data file without loading it in memory
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
#' bselect(file = "./data/test.csv", colnums = c(1,3))
#' bselect(file = "./data/test.csv", colnames = c("PRICE", "COLOR"))
#' @export

bselect <- function(file = NULL,
                    colnames = NULL, colnums = NULL,
                    ...){
  args <- list(...)

  unixCmdStr <- bselectStr(file = file,
                           colnames = colnames, colnums = colnums,
                           ...) %>%
    paste(file)
  args <- c(cmd = unixCmdStr, args)
  df <- do.call(fread, args)
  return(df)
}
