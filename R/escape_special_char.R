escape_special_characters <- function(string){
  str_replace_all(string, "(\\W)", "\\\\\\1")
}
