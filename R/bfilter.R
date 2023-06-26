#' Pre-filters a data file using column values before loading it in memory
#'
#' Simple wrapper for data.table::fread() allowing to filter data from a file
#' with the Unix 'grep' command. This method is useful if you want to load a file
#' too large for your available memory (and encounter the 'cannot allocate vector of size' error
#' for example).
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param patterns Vector of strings. One or several patterns used to filter the data from the input file. Each element of the vector should correspond to the column to be filtered. Can use regular expressions.
#' @param filtered_columns Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the pattern with which it will be filtered.
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param ... Arguments that must be passed to data.table::fread() like 'sep' and 'dec'.
#' @keywords big file filter grep allocate vector size
#'
#' @return A dataframe
#'
#' @examples
#' file <- system.file('extdata', 'test.csv', package = 'bread')
#' ## Filtering on 2 columns, using regex.
#' bfilter(file = file, patterns = c('200[4-6]', "red"),
#'       filtered_columns = c('YEAR', 'COLOR'), sep = ';')
#' bfilter(file = file, patterns = c('2004|2005', 'red'),
#'       filtered_columns = c('YEAR', 'COLOR'), sep = ';')
#' ## You need to use fixed = T if some patterns contain special characters
#' ## that mess with regex like '(' and ')'
#' bfilter(file = file, patterns = 'orange (purple)',
#'       filtered_columns = 'COLOR', fixed = TRUE, sep = ';')
#' ## If you do not provide the filtered_columns, you risk encountering
#' ## false positives because the grep command filters on the whole file,
#' ## not column by column. Here, the value 2002 will be found in the 'PRICE'
#' ## column as well. The filtered_column argument will just make the script
#' ## do a second pass with dplyr::filter() to remove false positives.
#' bfilter(file = file, patterns = '2002', sep = ';')
#' @export


bfilter <- function(file = NULL,
                    patterns = NULL,
                    filtered_columns = NULL,
                    fixed = FALSE,
                    ...){

  args <- list(...)

  meta_output <- list()
  meta_output$colnames <- bcolnames(file)

  ## Getting full path, in case the file is in the wd
  file <- normalizePath(path = file)
  if(startsWith(file, "\\")){
    file <- gsub(pattern = "\\\\", replacement = "/", x = file)
  }
  ## Quoting the file to prevent errors due to special characters like ')'
  ## according to environment
  if(.Platform$OS.type == 'windows'){
    qfile <- shQuote(file, type = 'cmd2')
    ## More quoting to manage filepaths with spaces
    qfile <- paste0('\'', qfile, '\'')
  } else if(.Platform$OS.type == 'unix'){
    qfile <- shQuote(file)
  }


  if(fixed == T){
    patterns <- escape_special_characters(patterns)
  }

  unixCmdStr <- bfilterStr(file = file, patterns = patterns,
                           filtered_columns = filtered_columns)
  unixCmdStr <- paste(unixCmdStr, qfile)
  args <-  c(cmd = unixCmdStr, args)
  df <- do.call(data.table::fread, args)
  colnames(df) <- meta_output$colnames
  ## filtered_column can be a vector of string colnames or a vector of col indexes
  ## We prefer names in order to filter
  if(is.numeric(filtered_columns)){
    filtered_columns <- meta_output$colnames[filtered_columns]
  }
  if(is.null(filtered_columns)){
    warning("*** No filtered_columns entered. Data has been filtered\n
            but there might be some false positives. ***")
  } else {
    for(ii in 1:length(filtered_columns)){
      ## for historical purposes, tidyversion included
      #df <- df %>% filter(stringr::str_detect(!!sym(filtered_columns[ii]), patterns[ii]))
      df <- df[grepl(pattern = patterns[ii], x = df[[filtered_columns[ii]]]), ]
    }
  }
  return(df)
}
