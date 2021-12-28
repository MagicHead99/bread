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