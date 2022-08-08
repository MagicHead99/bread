#' Reads a file in table format, selecting columns, subsetting rows by number and filtering them by column values
#'
#' Wrapper for data.table::fread() simplifying the use of Unix commands like 'grep', 'cut', 'awk' and 'sed'
#' on a data file *before* loading it in memory. The Unix commands are automatically generated
#' from the arguments.
#' This is useful if you want to load a big file too large for your available memory
#' (and encounter the 'cannot allocate vector of size' error) and know you can work on a
#' subsample. 'b' stands for 'big file'.
#' This function allows to subset rows by their index number, select columns and filter
#' with a pattern.
#'
#' You can mix and match the row subsetting, the filtering by value and the selecting of columns.
#' In order, the function:
#' 1. subsets the rows by their numbers (with 'sed' & 'awk'). You need to input the index
#' number of the first and last rows you want to load in memory with fread(),
#' or alternatively use either the head or tail arguments to subset the first or
#' last rows of the file.
#' 2. selects columns by index number or name (with 'cut'). If both colnames and
#' colnums are provided, colnums will be prefered.
#' 3. filters the data selected so far with a pattern by column (with 'grep'). The
#' columns to be filtered should be indicated through their names or their index
#' number. Each element of the vector should correspond to the pattern with which
#' it will be filtered.
#' 4. filters (inclusively, as in inferior/superior OR EQUAL) the data selected
#' so far by numerical value on a different set of provided columns with the 'sed' command.
#'
#' @section Warning:
#' Best practice would probably be to load the big file in a SQL database or something.
#' Or not working on huge CSV files in the first place.
#' But if you have to, you hopefully won"t have to delve into the fascinating grammar of
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
#' @param range_min Vector of numeric. One or several minimal values used to filter (inclusively, as in superior OR EQUAL to that value) the data from the input file. Each element of the vector should correspond to the numrange_column to be filtered.
#' @param range_max Vector of numeric. One or several maximal values used to filter (inclusively, as in inferior OR EQUAL to that value) the data from the input file. Each element of the vector should correspond to the numrange_column to be filtered.
#' @param numrange_columns Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the range_min and range_man values with which it will be filtered.
#' @param ... Arguments that must be passed to data.table::fread() like 'sep' or 'dec'.
#' @keywords big file select cut allocate vector size
#' @return A data frame with the selected columns and the subsetted and filtered data
#' @examples
#' file <- system.file('extdata', 'test.csv', package = 'bread')
#' ## Select the columns numbered 1 and 3
#' bread(file = file, colnums = c(1,3))
#' ## Select the columns named 'YEAR' and 'PRICE', then filter to keep only the
#' ## value '2022' in column 'YEAR'
#' bread(file = file, colnames = c('YEAR', 'PRICE'),
#'       patterns = 2002, filtered_columns = 'YEAR')
#' ## Select the columns names 'YEAR' and 'PRICE', then filter to keep only values
#' ## superior or equal to 2004 in YEAR and to 2000 in PRICE
#' bread(file = file, colnames = c("YEAR", "PRICE"),
#'       range_min = c(2004,2000), numrange_columns = c(1,3))
#' ## Subset to keep only the rows 10 to 18, select the columns named 'YEAR'
#' ## and 'COLOR' then filter to keep only the value 'red' in column 'COLOR'
#' bread(file = file, colnames = c('YEAR', 'COLOR'),
#'       patterns = 'red', filtered_columns = 'COLOR',
#'       first_row = 10, last_row = 18)
#' @import dplyr
#' @export

bread <- function(file = NULL,
                  first_row = NULL, last_row = NULL, head = NULL, tail = NULL,
                  colnames = NULL, colnums = NULL,
                  patterns = NULL, filtered_columns = NULL, fixed = FALSE,
                  range_min = NULL, range_max = NULL, numrange_columns = NULL,
                  ...) {
  ## 0. write 'unixCmdStr' depending on what"s provided
  ## 1. first, select row numbers with head & sed/awk
  ## 2. second, select columns with cut
  ## 3. third, filter the rows with the patterns provided by column
  ## 4. fourth, filter the rows by numerical values by column

  args = list(...)

  meta_output <- list()
  meta_output$colnames <- bcolnames(file, ...)

  ## Quoting the file to prevent errors due to special characters like ')'
  ## according to environment
  if(.Platform$OS.type == 'windows'){
    qfile <- shQuote(file, type = 'cmd2')
  } else if(.Platform$OS.type == 'unix'){
    qfile <- shQuote(file)
  }

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
    ## we have to use 'sort()' as the 'cut' command generates the columns in the
    ## order it reads them, not in the order requested
    ## also have to take into account the numrange option

    numrange_names <- NULL
    numrange_index <- NULL

    if(!is.null(numrange_columns)){
      if(is.null(colnames)){
        colnames <- meta_output$colnames[colnums]
      }

      else {
        colnums <- match(colnames, meta_output$colnames)
      }
      if(is.numeric(numrange_columns)){
        numrange_names <- meta_output$colnames[numrange_columns]
        numrange_index <- numrange_columns
      } else {
        numrange_names <- numrange_columns
        numrange_index <- match(numrange_columns, meta_output$colnames)
      }


    }

    if(is.null(colnames)){
      if(!is.null(colnums)){
        meta_output$colnames <- meta_output$colnames[sort(colnums)]
      }
      ## Case2: neither colnums nor colnames provided
      else {
        stop('*** One of colnames OR colnums needed ! ***')
      }
      ## Case3: both colnums and colnames provided
    } else {
      if(!is.null(colnums)){
        #warning('*** if both colnums and colnames are provided, colnums takes over (arbitrarily) ***')
        meta_output$colnames <- meta_output$colnames[sort(colnums)]
        ## Case4: colnames provided but not colnums
      } else {
        colnums <- match(colnames, meta_output$colnames)
        meta_output$colnames <- meta_output$colnames[sort(colnums)]
      }
    }
    if(!is.null(numrange_columns) & !is.null(numrange_index)){
      if(!all(numrange_index %in% colnums)){
        stop('Cannot filter by numrange on missing column')
      } else {
        numrange_columns <- match(numrange_names, meta_output$colnames)
      }}
  }

  ### if using sed to filter by row number then awk to filter by value, it removes the first row and then
  ### awk mistakenly prints the first row without filtering, so we change the NR to 0
  if((!is.null(first_row) | !is.null(last_row) | !is.null(head) | !is.null(tail)) & (!is.null(range_min) | !is.null(range_max) | !is.null(numrange_columns))){
    sed_first_row_RN <- 0
  } else {
    sed_first_row_RN <- 1
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

  ### 4. filter by value / awk
  if(!is.null(range_min) | !is.null(range_max) | !is.null(numrange_columns)){

    unixCmdVec <- append(unixCmdVec,
                         bnumrangeStr(file = file,
                                      range_min = range_min, range_max = range_max,
                                      numrange_columns = numrange_columns,
                                      sed_first_row_RN = sed_first_row_RN,
                                      ...))
  }

  ### Here we have a vector of 1-4 unix command(s) as strings that we must separate with '|'
  ### adding the file after the first
  unixCmdStr <- paste(unixCmdVec[1], qfile)
  if(length(unixCmdVec) > 1){
    unixCmdVec <- paste(unixCmdVec[-1], collapse = '| ')
    unixCmdStr <- paste(unixCmdStr, unixCmdVec, sep = '| ')
  }

  ### Using the Unix Cmd now
  args <- c(cmd = unixCmdStr, args)
  df <- do.call(data.table::fread, args)
  #print(unixCmdStr)
  ### Adding back ColNames (sed & awk & grep lose them)
  if(ncol(df) > 0){
    colnames(df) <- meta_output$colnames

    ## filtered_column can be a vector of string colnames or a vector of col indexes
    ## We prefer names for dplyr::filter()
    if(is.numeric(filtered_columns)){
      filtered_columns <- meta_output$colnames[filtered_columns]
    }
    if(is.null(filtered_columns) & !is.null(patterns)){
      warning("*** Filtering according to patterns but no filtered_columns entered.\nData has been filtered\n
            but there might be some false positives. ***")
    } else if(!is.null(filtered_columns) & !is.null(patterns)){
      for(ii in 1:length(filtered_columns)){
        df <- df %>% filter(stringr::str_detect(!!sym(filtered_columns[ii]), patterns[ii]))
      }
    }
    return(df)
  } else {
    print("*** Range selection returned an empty data.frame ***")
    return(df)
  }
}
