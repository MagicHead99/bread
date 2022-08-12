
## internal function to escape special characters

escape_special_characters <- function(string){
  gsub('(\\W)', '\\\\\\1', x = string)
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
#* bfilterStr(file = './data/test.csv', patterns = c('2002', 'red'), filtered_columns = c('YEAR', 'COLOR'))


bfilterStr <- function(file = NULL,
                       patterns = NULL,
                       filtered_columns = NULL){

  if((length(patterns) != length(filtered_columns)) & !is.null(filtered_columns)){
    stop("\n *** patterns must correspond to the filtered_columns (vectors of \n
    same length). If several values must be matched in some of the columns\n
    please use regexp 'or' = '|' ***")
  }

  filterStr <- paste(patterns, collapse = '|')
  unixCmdStr <- paste0('grep -aE "', filterStr, '" ')
  return(unixCmdStr)
}

#* Internal helper function generating the awk Command String
#*
#* Writes the string with the awk call to filter based on numeric range
#*
#* @param file String. Name or full path to a file compatible with data.table::fread()
#* @param range_min Vector of numeric One or several minimal values used to filter the data from the input file. Each element of the vector should correspond to the numrange_column to be filtered.
#* @param range_max Vector of numeric. One or several maximal values used to filter the data from the input file. Each element of the vector should correspond to the numrange_column to be filtered.
#* @param numrange_columns Vector of strings or numeric. The columns to be filtered should be indicated through their names or their index number. Each element of the vector should correspond to the range_min and range_man values with which it will be filtered.
#* @keywords big file filter awk allocate vector size


bnumrangeStr <- function(file = NULL,
                         range_min = NULL, range_max = NULL,
                         numrange_columns = NULL,
                         sed_first_row_RN = 1,
                         ...){

  args = list(...)

  ### input checks:
  if(is.null(numrange_columns)){
    stop('*** numrange_columns is mandatory ***')
  }

  if(is.null(range_max) & is.null(range_min)){
    stop('*** Please provide at least one of range_min or range_max ***')
  }

  if((!is.null(range_max) & (length(numrange_columns) != length(range_max))) | (!is.null(range_min) & (length(numrange_columns) != length(range_min)))){
    stop('*** range_min and range_max must correspond to the numrange_columns \n
    (vectors of same length). ***')
  }

  if(!all(range_min <= range_max)){
    stop('*** all range_min must be inferior or equal to their corresponding range_max (for a given column index) ***')
  }

  ### numrange_columns can be a vector of string colnames or a vector of col indexes
  ### We need indexes for awk

  ### check first if current colnames are provided, in case we already selected/cut
  meta_output <- list()
  if('current_colnames' %in% names(args)){
    meta_output$colnames = args[['current_colnames']]
  } else {
    meta_output$colnames <- bcolnames(file)
  }
  if(!is.numeric(numrange_columns)){
    numrange_columns <- match(numrange_columns, meta_output$colnames)
  }

  ### String building
  #### create vector of range checks, empty at first
  #### output must look like: ($1 >= 2006 && $1 <= 2009 && $3 >= 10)
  numrangeVec = NULL
  for(ii in 1:length(numrange_columns)){
    if(!is.null(range_min[ii])){
      numrangeVec <- append(numrangeVec,
                            paste0('$', numrange_columns[ii], ' >= ', range_min[ii]))
    }
    if(!is.null(range_max[ii])){
      numrangeVec <- append(numrangeVec,
                            paste0('$', numrange_columns[ii], ' <= ', range_max[ii]))
    }
  }
  numrangeStr <- paste(numrangeVec, collapse = " && ")

  ## unix cmd to cut the selected columns
  if('sep' %in% names(args)){
    sepz = args[['sep']]
  } else {
    ii <- 1
    ## classic separators : if it"s not one of those, the user will have to set it
    separatorz <- c(',',';','\t', ' ', '|', ':')
    ## Quoting the file to prevent errors due to special characters like ')'
    ## according to environment
    if(.Platform$OS.type == 'windows'){
      qfile <- shQuote(file, type = 'cmd2')
    } else if(.Platform$OS.type == 'unix'){
      qfile <- shQuote(file)
    }
    header <- system(command = paste('head -n 1 ', qfile), intern = T)
    while(!exists('sepz')){
      ## if the number of separators in the header is equal to the number of columns (minus one)
      ## we have found the separator
      if(nchar(gsub(paste0('[^', separatorz[ii],']'),'', x = header)) == length(meta_output$colnames) - 1){
        sepz <- separatorz[ii]
        break
      } else {
        ii <-  ii + 1
      }
      if(ii > length(separatorz)){
        stop('*** ERROR: We are having trouble determining the separator,
             please add a sep = "..." argument ***')
      }
    }
  }

  ### win
  ### fread(cmd='awk -F; "{ if (NR == 1 || ($1 >= 2006 && $3 > 2000)) print }" .../extdata/test.csv')
  ### ubuntu
  ### awk -F';' '{if (NR == 1 || $1 == 2006 && $3 > 2000) print}' /home/zzz/R/x86_64-pc-linux-gnu-library/4.0/bread/extdata/test.csv

  if(.Platform$OS.type == "windows"){
  unixCmdStr <- paste0('awk -F', sepz, ' "{ if (NR == ', sed_first_row_RN, ' || (', numrangeStr, ')) print }" ')
  } else {
    unixCmdStr <- paste0("awk -F'", sepz, "' '{ if (NR == ", sed_first_row_RN, " || (", numrangeStr, ")) print }' ")
  }
  #print(unixCmdStr)
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
#* @param ... Arguments that must be passed to data.table::fread() like 'sep'. Only used for the bmeta() call here.
#* @keywords select cut
#* @return A string
#* @examples
#* bselectStr(file = './data/test.csv', colnums = c(1,3))
#* bselectStr(file = './data/test.csv', colnums = c('PRICE', 'COLOR'))


bselectStr <- function(file = NULL,
                       colnames = NULL, colnums = NULL,
                       ...){

  args = list(...)
  meta_output <- list()
  meta_output$colnames <- bcolnames(file, ...)

  ## Case1: colnums provided but not colnames
  if(is.null(colnames)){
    if(!is.null(colnums)){
      colnumStr <- paste(colnums, collapse = ',')
    }
    ## Case2: neither colnums nor colnames provided
    else {
      stop('*** One of colnames OR colnums needed ! ***')
    }
    ## Case3: both colnums and colnames provided
  } else {
    if(!is.null(colnums)){
      warning('*** if both colnums and colnames are provided, colnums takes over (arbitrarily) ***')
      colnumStr <- paste(colnums, collapse = ',')
      ## Case4: colnames provided but not colnums
    } else {
      colnums <- match(colnames, meta_output$colnames)
      colnumStr <- paste(colnums, collapse = ',')
    }
  }
  ## unix cmd to cut the selected columns
  if('sep' %in% names(args)){
    sepz = args[['sep']]
  } else {
    ii <- 1
    ## classic separators : if it"s not one of those, the user will have to set it
    separatorz <- c(',',';','\t', ' ', '|', ':')
    ## Quoting the file to prevent errors due to special characters like ')'
    ## according to environment
    if(.Platform$OS.type == 'windows'){
      qfile <- shQuote(file, type = 'cmd2')
    } else if(.Platform$OS.type == 'unix'){
      qfile <- shQuote(file)
    }
    header <- system(command = paste('head -n 1 ', qfile), intern = T)
    while(!exists('sepz')){
      ## if the number of separators in the header is equal to the number of columns (minus one)
      ## we have found the separator
      if(nchar(gsub(paste0('[^', separatorz[ii],']'),'', x = header)) == length(meta_output$colnames) - 1){
        sepz <- separatorz[ii]
        break
      } else {
        ii <-  ii + 1
      }
      if(ii > length(separatorz)){
        stop('*** ERROR: We are having trouble determining the separator,
             please add a sep = "..." argument ***')
      }
    }
  }

  unixCmdStr <- paste0('cut -d"', sepz,'" -f', colnumStr, ' ')
  return(unixCmdStr)
}


#* Internal helper function generating the sed/awk Command String
#*
#* Writes a string containing a sed or awk call from the function parameters
#*
#* @param file String. Full path to a file
#* @param first_row Numeric. First row of the portion of the file to subset.
#* @param last_row Numeric. Last row of the portion of the file to subset.
#* @param head Numeric. How many rows starting from the first in the file.
#* @param tail Numeric. How many rows starting from the last in the file.
#* @param ... Arguments that must be passed to data.table::fread() like 'sep'. Only used for the bmeta() call here.
#* @keywords subset sed awk
#* @return A string
#* @examples
#* bsubsetStr(file = './data/test.csv', head = 5)
#* bsubsetStr(file = './data/test.csv', first_row = 5, last_row = 10)


bsubsetStr <- function(file = NULL,
                       first_row = NULL, last_row = NULL,
                       head = NULL, tail = NULL,
                       ...){

  meta_output <- list()
  meta_output$colnames <- bcolnames(file, ...)

  ### input checks:
  if(((!is.null(head) | !is.null(tail)) & (!is.null(first_row) | !is.null(last_row))) |
     (!is.null(head) & !is.null(tail))){
    stop('*** You can use only one of "head" OR "tail" OR "first_row & last_row" ***')
  }

  ### autofill missing parameters
  if(is.null(first_row)){ first_row <- 1 }

  ### consistency check...
  if(!is.null(last_row)){
    if(first_row < 1 | last_row < 1 | !(first_row == round(first_row)) |
       !(last_row == round(last_row))){
      stop('*** first_row and last_row must be positive integer numbers ***')
    }
  } else {
    if(first_row < 1 | !(first_row == round(first_row))){
      stop('*** first_row must be a positive integer number ***')
    }
  }

  ### String building
  ####### added as.integer in v0.1.4 to prevent scientific notation...
  ### 1st case: first_row and/or last_row are provided
  if(is.null(head) & is.null(tail)){

    if(.Platform$OS.type == 'windows'){
      if(!is.null(last_row)){
        unixCmdStr <- paste0('sed -e "1,', as.integer(first_row), 'd;', as.integer(last_row + 1),'q" ')
      } else {
        unixCmdStr <- paste0('sed -e "1,', as.integer(first_row), 'd;"')
      }
    } else {
      if(!is.null(last_row)){
        unixCmdStr <- paste0('awk "NR >= ', as.integer(first_row +1), ' && NR <= ', as.integer(last_row + 1), '" ')
      } else {
        unixCmdStr <- paste0('awk "NR >= ', as.integer(first_row +1), '" ')
      }
    }



    ### 2nd case, head is provided
  } else if(!is.null(head)){
    unixCmdStr <- paste0('head -n ', as.integer(head + 1), ' ')
  } else {
    ### 3rd case: tail
    ### tail.exe is hard to find on Windows (not in older versions of RTools)
    ### maybe in git / cygwin...
    ### Exceptionnally we"ll use powershell if it"s installed
    if(.Platform$OS.type == 'windows'){
      if(suppressWarnings(grepl(pattern = 'tail.exe', x = system('where tail.exe', intern = T)))){
        ### if tail.exe is found, simplest solution
        unixCmdStr <- paste0('tail -n ', as.integer(tail))
        ### if not Check env for powershell trace
      } else if('PSModulePath' %in% names(Sys.getenv())){
        ### OK, now the variable name doesn"t make sense anymore but let"s be pragmatic
        ### just this once
        unixCmdStr <- paste0('powershell -command Get-Content -Tail ', as.integer(tail), ' ')
      } else { ## else, we"ll use a sed workaround
        ### thx dcaswell: https://stackoverflow.com/a/18453366
        ### very smart but not very fast for big files
        unixCmdStr <- paste0('sed -e :a -e "$q;N;', as.integer(tail + 1),',$D;ba" ')
      }


    } else {
      ### if unix, tail should be installed hopefully
      unixCmdStr <- paste0('tail -n ', as.integer(tail))
    }
  }

  return(unixCmdStr)
}

addCmdsToPath <- function(){

  ### If BDF Environment...
  if(Sys.getenv('BDF_OSVER') != '' & .Platform$OS.type == 'windows'){
    oldPath <- Sys.getenv('PATH')
    Sys.setenv(PATH = paste(oldPath, 'C:\\Program Files\\Git\\usr\\bin;C:\\Produits\\R\\Rtools\\usr\\bin', sep = ';'))



  } else if(.Platform$OS.type == 'windows'){
    oldPath <- Sys.getenv('PATH')
    # add Rtools / Git / Cygwin to path
    CMD = c('reg query "HKLM\\Software\\R-core\\Rtools" /v InstallPath',
            'reg query "HKLM\\Software\\Cygwin\\setup" /v rootdir',
            'reg query "HKLM\\Software\\GitForWindows" /v InstallPath')
    # subdirectories with cmds for those 3 apps
    DIR = c('\\usr\\bin',
            '\\bin',
            '\\usr\\bin')
    output <- c()

    for(ii in 1:length(CMD)){
      output[ii] <- readReg(CMD = CMD[ii], DIR = DIR[ii], output = NA)
    }

    output <- stats::na.omit(output)
    output <- paste(output, collapse = ';')

    if(output == ''){
      message('### Neither RTools, Git nor Cygwin have been detected.
### Please make sure you have another source for the necessary Unix cmds
### in your PATH.')
    }
    Sys.setenv(PATH = paste(oldPath, output, sep = ';'))
  }
}

readReg <- function(CMD, DIR, output = NA){
  tryCatch(
    {
      # check registry for installPaths, extract it and add subdirectories
      output <- system(command = CMD, intern = T, ignore.stderr = T)
      output <- output[grepl(pattern = 'REG_SZ', x = output)]
      output <- strsplit(output, split = '  ')
      output <- unlist(output)
      output <- output[length(output)]
      output <- paste0(output, DIR)
    },
    error=function(cond) {
      output <- NA
    },
    warning=function(cond) {
      output <- NA
    },
    finally = {
      return(output)
    }
  )
}


##
.onLoad <- function(libname, pkgname) {
  addCmdsToPath()
}
