##### TO DO:
##### function(,...)
##### args = list(...)
##### remove sep and dec
##### args = c(cmd = unixCmdStr, args) # à tester
##### do.call(fread, args)

bmeta <- function(file = NULL, ...){
  #print("*** DEBUG: CURRENTLY IDENTIFYING COLNAMES, NCOL & NROWS ***")
  args <- list(...)
  output = NULL
  ## -1 because headers
  output$nrows <- shell(paste0("wc -l ", file), intern = TRUE) %>%
    str_remove(pattern = " .*$") %>% as.numeric() %>% -1
  unixCmdStr <- paste('head -n 2', file)
  args = c(cmd = unixCmdStr, args)
  output$colnames <- do.call(fread, args) %>%
    colnames()
  output$ncol <- length(output$colnames)
  # output$colnames <-  output$colnames %>% t() %>% as.data.frame() %>%
  #    rename_all(1:output$ncol)
  return(output)
}
