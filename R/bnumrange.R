#' Pre-filters a data file using column numerical range before loading it in memory
#'
#' Simple wrapper for data.table::fread() allowing to filter data by numerical value
#' from a file #' with the Unix awk command. This method is useful if you want to
#' load a file #' too large for your available memory (and encounter
#' the 'cannot allocate vector of size' error #' for example).
#'
#' @section Warning: The value comparisons are inclusive, meaning inferior/superior OR EQUAL
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param range_min Vector of numeric. One or several minimal values used to filter (inclusively, as in superior OR EQUAL to that value) the data from the input file. Each element of the vector should correspond to the numrange_column to be filtered.
#' @param range_max Vector of numeric. One or several maximal values used to filter (inclusively, as in inferior OR EQUAL to that value) the data from the input file. Each element of the vector should correspond to the numrange_column to be filtered.
#' @param numrange_columns Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the range_min and range_man values with which it will be filtered.
#' @param ... Arguments that must be passed to data.table::fread() like 'sep' and 'dec'.
#' @keywords big file filter awk allocate vector size
#'
#' @return A dataframe
#'
#' @examples
#' file <- system.file('extdata', 'test.csv', package = 'bread')
#'
#' ## Filtering with only min value
#'
#'
#' ## Filtering on 2 columns
#' bnumrange(file = file, range_min = c(2006, 1500), range_max = c(2010, 1990),
#'       numrange_columns = c(1,3))
#' bnumrange(file = file, range_min = c(2000, 1500), range_max = c(2005, 1990),
#'       numrange_columns = c('YEAR', 'PRICE'), sep = ';')
#'
#' @import dplyr
#' @export


bnumrange <- function(file = NULL,
                    range_min = NULL, range_max = NULL,
                    numrange_columns = NULL,
                    ...){

  ###
  # fread(cmd='awk -F; "{ if (NR == 1 || ($1 >= 2006 && $3 > 2000)) print }" C:/Users/Vz/Documents/R/win-library/4.0/bread/extdata/test.csv')

  args <- list(...)

  meta_output <- list()
  meta_output$colnames <- bcolnames(file)

  ## Quoting the file to prevent errors due to special characters like ')'
  ## according to environment
  if(.Platform$OS.type == 'windows'){
    qfile <- shQuote(file, type = 'cmd2')
  } else if(.Platform$OS.type == 'unix'){
    qfile <- shQuote(file)
  }

  unixCmdStr <- bnumrangeStr(file = file,
                             range_min = range_min, range_max = range_max,
                             numrange_columns = numrange_columns) %>%
    paste(qfile)
  args <-  c(cmd = unixCmdStr, args)
  df <- do.call(data.table::fread, args)
  colnames(df) <- meta_output$colnames

  return(df)
}
