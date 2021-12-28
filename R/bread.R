bread <- function(file = NULL,
                  first_row = NULL, last_row = NULL,
                  head = NULL, tail = NULL,
                  colnames = NULL, colnums = NULL,
                  patterns = NULL, filtered_columns = NULL,
                  sep = ";", dec = ",",
                  meta_output = NULL) {
  ## 0. write "unixCmdStr" depending on what's provided
  ## 1. first, select row numbers with head & sed
  ## 2. second, select columns with cut
  ## 3. third, filter the rows with the patterns provided by column
  
  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }
  
  unixCmdVec <- NULL
  
  ### 1. subset / sed
  if(!is.null(first_row) | !is.null(last_row) | !is.null(head) | !is.null(tail) ){
    unixCmdVec <- append(unixCmdVec, 
                         bsubsetStr(file = file, head = head, tail = tail,
                                    first_row = first_row, last_row = last_row, meta_output = meta_output))
  }
  
  ### 2. select / cut
  if(!is.null(colnames) | !is.null(colnums)){
    unixCmdVec <- append(unixCmdVec, 
                         bselectStr(file = file, 
                                    colnames = colnames, colnums = colnums, 
                                    meta_output = meta_output, 
                                    sep = sep))
    
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
    unixCmdVec <- append(unixCmdVec, 
                         bfilterStr(file = file, 
                                    patterns = patterns, filtered_columns = filtered_columns, 
                                    meta_output = meta_output, 
                                    sep = sep))
  }
  
  ### Here we have a vector of 1-3 unix command(s) as strings that we must seperate with "|"
  ### adding the file after the first 
  unixCmdStr <- paste(unixCmdVec[1], file)
  if(length(unixCmdVec) > 1){
    unixCmdVec <- paste(unixCmdVec[-1], collapse = "| ")
    unixCmdStr <- paste(unixCmdStr, unixCmdVec, sep = "| ")
  }
  ### Using the Unix Cmd now
  df <- fread(cmd = unixCmdStr, sep = sep, dec = dec)
  ### Adding back ColNames (sed & grep lose them)
  colnames(df) <- meta_output$colnames
  ## filtered_column can be a vector of string colnames or a vector of col indexes
  ## We prefer names for dplyr::filter()
  if(is.numeric(filtered_columns)){
    filtered_columns <- meta_output$colnames[filtered_columns]
  }
  if(is.null(filtered_columns) & !is.null(patterns)){
    warning('*** Filtering according to patterns but no filtered_columns entered.\nData has been filtered\n
            but there might be some false positives. ***')
  } else {
    for(ii in 1:length(filtered_columns)){
      df <- df %>% filter(str_detect(!!sym(filtered_columns[ii]), patterns[ii]))
    }
  }
  return(df)
}