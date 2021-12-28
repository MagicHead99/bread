bmeta <- function(file = NULL, sep = ";"){
  #print("*** DEBUG: CURRENTLY IDENTIFYING COLNAMES, NCOL & NROWS ***")
  output = NULL
  output$nrows <- shell(paste0("wc -l ", file), intern = TRUE) %>%
    str_remove(pattern = " .*$") %>% as.numeric()
  output$colnames <- fread(cmd = paste('head -n 2', file), sep = sep) %>%
    colnames()
  output$ncol <- length(output$colnames)
  # output$colnames <-  output$colnames %>% t() %>% as.data.frame() %>%
  #    rename_all(1:output$ncol)
  return(output)
}
