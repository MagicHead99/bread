#' Reads a file in table format, selecting columns, subsetting rows by number and filtering them by column values
#'
#' Wrapper for data.table::fread() simplifying the use of Unix commands like grep, cut, awk and sed
#' on a data file *before* loading it in memory. The Unix commands are automatically generated
#' from the arguments.
#' This is useful if you want to load a big file too large for your available memory
#' (and encounter the "cannot allocate vector of size" error) and know you can work on a
#' subsample. "b" stands for "big file".
#' This function allows to subset rows by their index number, select columns and filter
#' with a pattern.
#'
#' You can mix and match the row subsetting, the filtering by value and the selecting of columns.
#' In order, the function:
#' 1. subsets the rows by their numbers (with sed & awk). You need to input the index
#' number of the first and last rows you want to load in memory with fread(),
#' or alternatively use either the head or tail arguments to subset the first or
#' last rows of the file.
#' 2. selects columns by index number or name (with cut). If both colnames and
#' colnums are provided, colnums will be prefered.
#' 3. filters the data selected so far with a pattern by column (with grep). The
#' columns to be filtered should be indicated through their names or their index
#' number. Each element of the vector should correspond to the pattern with which
#' it will be filtered.#'
#'
#' @section Warning:
#' Best practice would probably be to load the big file in a SQL database or something.
#' Or not working on huge CSV files in the first place.
#' But if you have to, you hopefully won't have to delve into the fascinating grammar of
#' Unix commands.
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param first_row Numeric. First row of the portion of the file to subset.
#' @param last_row Numeric. Last row of the portion of the file to subset.
#' @param head Numeric. How many rows starting from the first in the file.
#' @param tail Numeric. How many rows starting from the last in the file.
#' @param colnames Vector of strings. Exact names of columns to select. If both colnames and colnums are provided, colnums will be prefered.
#' @param colnums Vector of numeric. Columns index numbers.
#' @param patterns Vector of strings. One or several patterns used to filter the data from the input file. Each element of the vector should correspond to the column to be filtered. Can use regular expressions.
#' @param filtered_columns Vector of strings or numeric. Optional. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the pattern with which it will be filtered.
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param ... Arguments that must be passed to data.table::fread() like "sep" or "dec".
#' @keywords big file select cut allocate vector size
#' @return A data frame with the selected columns and the subsetted and filtered data
#' @examples
#' file <- system.file("extdata", "test.csv", package = "bread")
#' ## Select the columns numbered 1 and 3
#' bread(file = file, colnums = c(1,3))
#' ## Select the columns named "YEAR" and "PRICE", then filter to keep only the
#' ## value "2022" in column "YEAR"
#' bread(file = file, colnames = c("YEAR", "PRICE"),
#'       patterns = 2002, filtered_columns = "YEAR")
#' ## Subset to keep only the rows 10 to 18, select the columns named "YEAR"
#' ## and "COLOR" then filter to keep only the value "red" in column "COLOR"
#' bread(file = file, colnames = c("YEAR", "COLOR"),
#'       patterns = "red", filtered_columns = "COLOR",
#'       first_row = 10, last_row = 18)
#' @import dplyr
#' @export

bread <- function(file = NULL,
                  first_row = NULL, last_row = NULL,
                  head = NULL, tail = NULL,
                  colnames = NULL, colnums = NULL,
                  patterns = NULL, filtered_columns = NULL, fixed = FALSE,
                  ...) {
  ## 0. write "unixCmdStr" depending on what's provided
  ## 1. first, select row numbers with head & sed/awk
  ## 2. second, select columns with cut
  ## 3. third, filter the rows with the patterns provided by column

  args = list(...)

  meta_output <- list()
  meta_output$colnames <- bcolnames(file, ...)

  unixCmdVec <- NULL

  ### 1. subset / sed
  if(!is.null(first_row) | !is.null(last_row) | !is.null(head) | !is.null(tail) ){
    unixCmdVec <- append(unixCmdVec,
                         bsubsetStr(file = file,
                                    head = head, tail = tail,
                                    first_row = first_row, last_row = last_row,
                                    ...))
  }

  ### 2. select / cut
  if(!is.null(colnames) | !is.null(colnums)){
    unixCmdVec <- append(unixCmdVec,
                         bselectStr(file = file,
                                    colnames = colnames, colnums = colnums,
                                    ...))

    ## as we selected some columns, we must find the new set of colnames
    if(is.null(colnames)){
      if(!is.null(colnums)){
        meta_output$colnames <- meta_output$colnames[colnums]
      }
      ## Case2: neither colnums nor colnames provided
      else {
        stop("*** One of colnames OR colnums needed ! ***")
      }
      ## Case3: both colnums and colnames provided
    } else {
      if(!is.null(colnums)){
        warning("*** if both colnums and colnames are provided, colnums takes over (arbitrarily) ***")
        meta_output$colnames <- meta_output$colnames[colnums]
        ## Case4: colnames provided but not colnums
      } else {
        colnums <- match(colnames, meta_output$colnames)
        meta_output$colnames <- meta_output$colnames[colnums]
      }
    }
  }
  ### 3. filter / grep
  if(!is.null(patterns) | !is.null(filtered_columns)){
    patterns <- as.character(patterns)
    if(fixed == T){
      patterns <- escape_special_characters(patterns)
    }
    unixCmdVec <- append(unixCmdVec,
                         bfilterStr(file = file,
                                    patterns = patterns,
                                    filtered_columns = filtered_columns))
  }

  ### Here we have a vector of 1-3 unix command(s) as strings that we must seperate with "|"
  ### adding the file after the first
  unixCmdStr <- paste(unixCmdVec[1], shQuote(file))
  if(length(unixCmdVec) > 1){
    unixCmdVec <- paste(unixCmdVec[-1], collapse = "| ")
    unixCmdStr <- paste(unixCmdStr, unixCmdVec, sep = "| ")
  }
  ### Using the Unix Cmd now
  args <- c(cmd = unixCmdStr, args)
  df <- do.call(data.table::fread, args)
  ### Adding back ColNames (sed & awk & grep lose them)
  colnames(df) <- meta_output$colnames
  ## filtered_column can be a vector of string colnames or a vector of col indexes
  ## We prefer names for dplyr::filter()
  if(is.numeric(filtered_columns)){
    filtered_columns <- meta_output$colnames[filtered_columns]
  }
  if(is.null(filtered_columns) & !is.null(patterns)){
    warning('*** Filtering according to patterns but no filtered_columns entered.\nData has been filtered\n
            but there might be some false positives. ***')
  } else if(!is.null(filtered_columns) & !is.null(patterns)){
    for(ii in 1:length(filtered_columns)){
      df <- df %>% filter(stringr::str_detect(!!sym(filtered_columns[ii]), patterns[ii]))
    }
  }
  return(df)
}
