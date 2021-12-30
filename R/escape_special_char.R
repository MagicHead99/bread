## internal function to escape special characters

escape_special_characters <- function(string){
  str_replace_all(as.character(string), "(\\W)", "\\\\\\1")
}
