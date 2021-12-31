## internal function to escape special characters

escape_special_characters <- function(string){
  str_replace_all(as.character(string), "(\\W)", "\\\\\\1")
}

#* Internal helper function generating the grep Command String
#*
#* Writes a string containing a grep call from the function parameters
#*
#* @param file String. Full path to a file
#* @param patterns Vector of strings. One or several patterns used to filter the data from the input file. Each element of the vector should correspond to the column to be filtered. Can use regular expressions.
#* @param filtered_columns Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the pattern with which it will be filtered
#* @keywords filter grep
#* @return A string
#* @examples
#* bfilterStr(file = "./data/test.csv", patterns = c("2002", "red"), filtered_columns = c("YEAR", "COLOR"))


bfilterStr <- function(file = NULL,
                       patterns = NULL,
                       filtered_columns = NULL){

  if((length(patterns) != length(filtered_columns)) & !is.null(filtered_columns)){
    stop('\n *** patterns must correspond to the filtered_columns (vectors of \n
    same length). If several values must be matched in some of the columns\n
    please use regexp "or" = "|" ***')
  }

  filterStr <- paste(patterns, collapse = "|")
  unixCmdStr <- paste0('grep -aEu "', filterStr, '" ')
  return(unixCmdStr)
}


#* Internal helper function generating the cut Command String
#*
#* Writes a string containing a cut call from the function parameters
#*
#* @param file String. Full path to a file
#* @param colnames Vector of strings. Exact names of columns to select. If both colnames and colnums are provided, colnums will be prefered.
#* @param colnums Vector of numeric. Columns index numbers.
#* @param meta_output List. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#* @param ... Arguments that must be passed to data.table::fread() like "sep". Only used for the bmeta() call here.
#* @keywords select cut
#* @return A string
#* @examples
#* bselectStr(file = "./data/test.csv", colnums = c(1,3))
#* bselectStr(file = "./data/test.csv", colnums = c("PRICE", "COLOR"))


bselectStr <- function(file = NULL,
                       colnames = NULL, colnums = NULL,
                       meta_output = NULL,
                       ...){
  #args <- list(...)

  if(is.null(meta_output)){
    meta_output <- list()
    meta_output$colnames <- bcolnames(file, ...)
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
  ## unix cmd to cut the selected columns
  unixCmdStr <- paste0('cut -d"', sep,'" -f', colnumStr, " ")
  return(unixCmdStr)
}


#* Internal helper function generating the sed Command String
#*
#* Writes a string containing a sed call from the function parameters
#*
#* @param file String. Full path to a file
#* @param first_row Numeric. First row of the portion of the file to subset.
#* @param last_row Numeric. Last row of the portion of the file to subset.
#* @param head Numeric. How many rows starting from the first in the file.
#* @param tail Numeric. How many rows starting from the last in the file.
#* @param ... Arguments that must be passed to data.table::fread() like "sep". Only used for the bmeta() call here.
#* @keywords subset sed
#* @return A string
#* @examples
#* bsubsetStr(file = "./data/test.csv", head = 5)
#* bsubsetStr(file = "./data/test.csv", first_row = 5, last_row = 10)


bsubsetStr <- function(file = NULL,
                       first_row = NULL, last_row = NULL,
                       head = NULL, tail = NULL,
                       ...){

  meta_output <- list()
  meta_output$colnames <- bcolnames(file, ...)

  ### input checks:
  if(((!is.null(head) | !is.null(tail)) & (!is.null(first_row) | !is.null(last_row))) |
     (!is.null(head) & !is.null(tail))){
    stop("*** You can use only one of 'head' OR 'tail' OR 'first_row & last_row' ***")
  }

  ### autofill missing parameters
  if(is.null(first_row)){ first_row <- 1 }

  ### consistency check...
  if(!is.null(last_row)){
    if(first_row < 1 | last_row < 1 | !(first_row == round(first_row)) |
       !(last_row == round(last_row))){
      stop("*** first_row and last_row must be positive integer numbers ***")
    }
  } else {
    if(first_row < 1 | !(first_row == round(first_row))){
      stop("*** first_row must be a positive integer number ***")
    }
  }

  ### String building
  ### 1st case: first_row and/or last_row are provided
  if(is.null(head) & is.null(tail)){
    if(!is.null(last_row)){
      unixCmdStr <- paste0('sed -e 1,', (first_row), 'd;', (last_row + 1),'q ')
    } else {
      unixCmdStr <- paste0('sed -e 1,', (first_row), 'd;')
    }
    ### 2nd case, head is provided
  } else if(!is.null(head)){
    unixCmdStr <- paste0('sed ', (head + 1), 'q ')
  } else {
    ### 3rd case: tail
    ### tail.exe is hard to find on Windows (not in RTools)
    ### maybe in git / mingw / cygwin...
    ### Exceptionnally we'll use powershell if it's installed
    if(.Platform$OS.type == "windows"){
      if(suppressWarnings(str_detect(string = system("where tail.exe",
                                                     intern = T),
                                     pattern = "tail.exe"))){
        ### if tail.exe is found, simplest solution
        unixCmdStr <- paste0("tail -n ", tail)
        ### if note Check env for powershell trace
      } else if("PSModulePath" %in% names(Sys.getenv())){
        ### OK, now the variable name doesn't make sense anymore but let's be pragmatic
        ### just this once
        unixCmdStr <- paste0("powershell -command Get-Content -Tail ", tail, " ")
      } else { ## else, we'll use a sed workaround
        ### thx dcaswell: https://stackoverflow.com/a/18453366
        ### very smart but not very fast for big files
        unixCmdStr <- paste0("sed -e :a -e '$q;N;", tail + 1,",$D;ba' ")
      }


    } else {
      ### if unix, tail should be installed hopefully
      ### to be confirmed, i don't have enough experience there
      unixCmdStr <- paste0("tail -n ", tail)
    }
  }

  return(unixCmdStr)
}
