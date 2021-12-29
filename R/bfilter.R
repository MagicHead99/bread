#' Filter data without loading it in memory with fread() and the grep command
#'
#' Writes a string containing a grep call from the function parameters
#'
#' @param file String. Required. Full path to a file compatible with data.table::fread()
#' @param patterns String or vector of strings. Required. One or several patterns used to filter the data from the input file. Each element of the vector should correspond to the column to be filtered. Can use regular expressions.
#' @param filtered_columns String, numeric or vector of strings or numeric. Optional. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the pattern with which it will be filtered.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param meta_output List. Optional. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#' @keywords filter grep
#' @examples
#' bfilter(file = "./data/test.csv", patterns = c("200[0-9]", "red"), filtered_columns = c("YEAR", "COLOR"))
#' bfilter(file = "./data/test.csv", patterns = "orange (purple)", filtered_columns = "COLOR", fixed = FALSE) # if T, bug
#' bfilter(file = "./data/test.csv", patterns = "2002", fixed = F) # False positive because no column provided


bfilter <- function(file = NULL,
                    patterns = NULL,
                    filtered_columns = NULL,
                    fixed = FALSE,
                    meta_output = NULL,
                    sep = ";", dec = ","){

  # filter avant de charger en memoire
  # on definit un pattern de filtre qui passera sur tout le fichier
  # on definit (de preference mais optionnel) les colonnes qui doivent etre filtrees
  # filtered_columns accepte les vecteurs de noms ou d'index (ex: c(1,3) ou c("SIREN", "DATE"))
  # s'il y en a plusieurs, on doit faire correspondre les vecteurs patterns et filtered_columns
  # dans ce cas il y a une 2nde passe avec dplyr::filter pour supprimer les faux positifs
  # si on veut match plusieurs patterns pour une colonne on utilise "|" par exemple:
  # patterns = c("51602913", "2019|2020"), filtered_columns = c("SIREN", "DATE"))
  # on aura 1 seul SIREN et les dates 2019 et 2020 en mémoire
  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }

  if(fixed == T){
    patterns <- escape_special_characters(patterns)
  }

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
