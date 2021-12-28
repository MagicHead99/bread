bsubsetStr <- function(file = NULL,
                       first_row = NULL, last_row = NULL,
                       head = NULL, tail = NULL,
                       #sep = ";", dec = ",",
                       meta_output = NULL){
  
  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }
  
  ### input checks:
  if((!is.null(head) | !is.null(tail)) & (!is.null(first_row) | !is.null(last_row))){
    stop("*** Can't use head/tail and first_row/last_row ***")
  }
  ### autofill missing parameters
  if(is.null(first_row)){ first_row <- 1 }
  if(is.null(last_row)){ last_row <- meta_output$nrows }
  if(first_row < 1 | last_row < 1 | !(first_row == round(first_row)) | !(last_row == round(last_row))){
    stop("*** first_row and last_row must be positive integers ***")
  }
  
  
  if(!is.null(head) & !is.null(tail)){
    stop("*** Can't use head and tail simultaneously, if you need both, do it twice ***")
  }
  
  
  
  ### String building
  if(is.null(head) & is.null(tail)){
    unixCmdStr <- paste0('sed -e 1,', (first_row), 'd;', (last_row + 1),'q ')
  } else if(!is.null(head)){
    unixCmdStr <- paste0('sed ', (head + 1), 'q ')
  } else {
    ### A VERIFIER NIVEAU +1
    unixCmdStr <- paste0('sed -e 1,', last_row - tail, 'd;', (last_row + 1), 'q ')
  }
  return(unixCmdStr)
}