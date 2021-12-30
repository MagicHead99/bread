#' Pre-subsets rows of a data file by index number before loading it in memory
#'
#' Simple wrapper for data.table::fread() allowing to subset rows of data from a file
#' with the Unix sed command. This method is useful if you want to load a file
#' too large for your available memory (and encounter the "cannot allocate vector of size" error).
#' You need to input the index number of the first and last rows you want to load in memory with fread(),
#' or alternatively use either the head or tail arguments to subset the first or last rows of the file.
#'
#' @param file String. Full path to a file
#' @param first_row Numeric. First row of the portion of the file to subset.
#' @param last_row Numeric. Last row of the portion of the file to subset.
#' @param head Numeric. How many rows starting from the first in the file.
#' @param tail Numeric. How many rows starting from the last in the file.
#' @param meta_output List. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#' @param ... Arguments that must be passed to data.table::fread() like "sep" or "dec".
#' @keywords big file subset sed allocate vector size
#' @return A dataframe containing the subsetted rows
#' @examples
#' bsubset(file = "./data/test.csv", head = 5)
#' bsubset(file = "./data/test.csv", first_row = 5, last_row = 10)
#' @export

bsubset <- function(file = NULL,
                    head = NULL, tail = NULL,
                    first_row = NULL, last_row = NULL,
                    sep = ";", dec = ",",
                    meta_output = NULL){
  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }
  unixCmdStr <- bsubsetStr(file = file, head = head, tail = tail,
                           first_row = first_row, last_row = last_row, meta_output = meta_output) %>%
    paste(file)
  df <- fread(cmd = unixCmdStr, sep = sep, dec = dec)
  colnames(df) <- meta_output$colnames
  return(df)
}
