bselectStr <- function(file = NULL, 
                       colnames = NULL, colnums = NULL, 
                       meta_output = NULL, 
                       sep = ";"){
  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }
  
  ## Case1: colnums provided but not colnames
  if(is.null(colnames)){
    if(!is.null(colnums)){
      colnumStr <- paste(colnums, collapse = ",")
    }
    ## Case2: neither colnums nor colnames provided
    else { 
      stop("*** One of colnames OR colnums needed ! ***")
    }
    ## Case3: both colnums and colnames provided
  } else {
    if(!is.null(colnums)){
      warning("*** if both colnums and colnames are provided, colnums takes over (arbitrarily) ***")
      colnumStr <- paste(colnums, collapse = ",")
      ## Case4: colnames provided but not colnums
    } else {
      colnums <- match(colnames, meta_output$colnames)
      colnumStr <- paste(colnums, collapse = ",")
    }
  }
  ## cmd unix pour cut les colonnes selectionnees
  unixCmdStr <- paste0('cut -d"', sep,'" -f', colnumStr, " ")
  return(unixCmdStr)
}