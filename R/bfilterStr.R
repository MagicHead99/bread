bfilterStr <- function(file = NULL, 
                       patterns = NULL, 
                       filtered_columns = NULL, 
                       meta_output = NULL){
  # filter avant de charger en memoire
  # on definit un pattern de filtre qui passera sur tout le fichier
  # on definit (de preference mais optionnel) les colonnes qui doivent etre filtrees
  # filtered_columns accepte les vecteurs de noms ou d'index (ex: c(1,3) ou c("SIREN", "DATE"))
  # s'il y en a plusieurs, on doit faire correspondre les vecteurs patterns et filtered_columns
  # dans ce cas il y a une 2nde passe avec dplyr::filter pour supprimer les faux positifs
  # si on veut match plusieurs patterns pour une colonne on utilise "|" par exemple:
  # patterns = c("51602913", "2019|2020"), filtered_columns = c("SIREN", "DATE"))
  # on aura 1 seul SIREN et les dates 2019 et 2020 en mémoire
  if(length(patterns) != length(filtered_columns)){
    stop('\n *** patterns must correspond to the filtered_columns (vectors of \n 
    same length). If several values must be matched in some of the columns\n
    please use regexp "or" = "|" ***')
  }
  
  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }
  
  
  filterStr <- paste(patterns, collapse = "|")
  unixCmdStr <- paste0('grep -aEu "', filterStr, '" ')
  return(unixCmdStr)
}