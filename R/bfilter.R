bfilter <- function(file = NULL, 
                    patterns = NULL, 
                    filtered_columns = NULL, 
                    meta_output = NULL, 
                    sep = ";", dec = ","){
  unixCmdStr <- bfilterStr(file = file, patterns = patterns, filtered_columns = filtered_columns, meta_output = meta_output) %>% 
    paste(file)
  df <- fread(cmd = unixCmdStr, sep = sep, dec = dec)
  colnames(df) <- meta_output$colnames
  ## filtered_column can be a vector of string colnames or a vector of col indexes
  ## We prefer names for dplyr::filter()
  if(is.numeric(filtered_columns)){
    filtered_columns <- meta_output$colnames[filtered_columns]
  }
  if(is.null(filtered_columns)){
    warning('*** No filtered_columns entered. Data has been filtered\n
            but there might be some false positives. ***')
  } else {
    for(ii in 1:length(filtered_columns)){
      df <- df %>% filter(str_detect(!!sym(filtered_columns[ii]), patterns[ii]))
    }
  }
  return(df)
}