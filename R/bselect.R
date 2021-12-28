bselect <- function(file = NULL, 
                    colnames = NULL, colnums = NULL, 
                    meta_output = NULL, 
                    sep = ";", dec = ","){
  unixCmdStr <- bselectStr(file = file, 
                           colnames = colnames, colnums = colnums, 
                           meta_output = meta_output, 
                           sep = sep) %>% 
    paste(file)
  df <- fread(cmd = unixCmdStr, sep = sep, dec = dec)
  return(df)
}