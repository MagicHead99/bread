#' Splits files in several smaller files
#'
#' This function helps splitting a big csv file in smaller csv files using one of those 3 methods:
#' 1. by_nrows: Each new file will contain a number of rows defined by the user
#' 2. by_nfiles: The user decide the number of files created with the rows equally distributed
#' 3. by_columns: The file will be split by the combinations of unique values in the columns chosen by the user
#' Like all other functions in the bread package, this is achieved using Unix commands
#' that allow opening, reading and splitting big files that wouldn't fit in memory
#' (The goal being to help with the "cannot allocate vector of size" error).
#'
#' @param file String. Name or full path to a file compatible with data.table::fread()
#' @param by_nfiles Numeric. Number of files with an equal number of rows to be created. Only the last one will be slightly larger, containing the remainder.
#' @param by_nrows Numeric. Number of rows composing the new split files. The last one may be smaller, containing only the remainder.
#' @param by_columns Vector of strings or numeric. Indicates either the names or index number of the columns whose combinations of unique values will be used to split the files.
#' @param drop_empty_files Logical. Defaults to TRUE. Used only with the "by_column" argument. If changed to FALSE, empty files may be created.
#' @param meta_output List. Optional. Output of the bmeta() function on the same file. It indicates the names and numbers of columns and rows. If not provided, it will be calculated. It can take a while on file with several million rows.
#' @param ... Arguments that must be passed to data.table::fread() like "sep" and "dec".
#' @keywords big file split allocate vector size
#'
#' @return Creates a number of csv files from the original larger file
#'
#' @examples
#' ## Filtering on 2 columns, using regex.
#' bfilter(file = "./data/test.csv", patterns = c("200[0-9]", "red"), filtered_columns = c("YEAR", "COLOR"), sep = ";")
#' @export

bfile_split <- function(file = NULL,
                        by_nfiles, by_nrows,
                        by_columns, drop_empty_files = T,
                        meta_output = NULL,
                        ...){
  args = list(...)
  ## ADD CHECKS THAT ONLY 1 PARAM IS PROVIDED, STOP IF MORE
  if (missing(by_nfiles) + missing(by_nrows) + missing(by_columns) < 2L){
    stop("Used more than one of the arguments by_nfiles=, by_nrows=, by_columns=.")
  }
  if (missing(by_nfiles) + missing(by_nrows) + missing(by_columns) == 3L){
    stop("Need to provide one of the arguments by_nfiles=, by_nrows= or by_columns=.")
  }
  ## Will be used as the basis of the name the future splitted files
  base_file <- tools::file_path_sans_ext(file)

  ## nrow, ncol and colnames are necessary
  if(is.null(meta_output)){
    meta_output = bmeta(file, ...)
  }

  ### 1. Splitting by number of files
  if(!missing(by_nfiles)){
    #### Count the number of rows of all file except the last
    rows_by_chunk_except_last <- meta_output$nrows %/% (by_nfiles)
    #### Last file will contain the leftover rows and will be slightly bigger
    last_chunk <- (meta_output$nrows %% (by_nfiles)) + rows_by_chunk_except_last
    #### In order to name files, we'll pad the iterating number so that
    #### if we have 150 files, they will be named "001", "002",..., "150"
    n_char_num <- nchar(as.character(by_nfiles))

    #### 1st chunk
    ##### +1 because header
    unixCmdStr <- paste('head -n', (rows_by_chunk_except_last + 1), file)
    args_fread <- c(cmd = unixCmdStr, args)
    df_temp <- do.call(fread, args_fread)

    #print(paste0("file 1 : ", nrow(df_temp)))
    args_fwrite <- c(x = df_temp,
                     file = paste0(base_file, "_", str_pad(1, n_char_num, pad = "0"), ".csv"),
                     args)
    do.call(fwrite, args_fwrite)

    #### chunks 2 to n-1
    for(ii in 2:(by_nfiles - 1)){
      unixCmdStr <- paste0('sed -e 1,', ((rows_by_chunk_except_last * (ii-1)) +1),
                           'd;', (rows_by_chunk_except_last * ii) + 1,'q ', file)
      ##### sed loses the colnames, we must add them back
      args_fread <- c(cmd = unixCmdStr, args)
      df_temp <- do.call(fread, args_fread) %>%
        `colnames<-`(meta_output$colnames)

      #print(paste0("file ", ii, " : " , nrow(df_temp)))
      args_fwrite <- c(x = df_temp,
                       file = paste0(base_file, "_", str_pad(ii, n_char_num, pad = "0"), ".csv"),
                       args)
      do.call(fwrite, args_fwrite)
    }


    #### last chunk n
    unixCmdStr <- paste0('sed -e 1,',
                         ((rows_by_chunk_except_last * (by_nfiles - 1))) + 1,
                         'd;', meta_output$nrows + 1,'q ', file)
    args_fread <- c(cmd = unixCmdStr, args)
    df_temp <- do.call(fread, args_fread) %>%
      `colnames<-`(meta_output$colnames)
    #print(paste0("file ", by_nfiles, " : " , nrow(df_temp)))

    args_fwrite <- c(x = df_temp,
                     file = paste0(base_file, "_",
                                   str_pad(by_nfiles, n_char_num, pad = "0"),
                                   ".csv"),
                     args)
    do.call(fwrite, args_fwrite)
  }

  ### 2. Splitting by number of rows
  if(!missing(by_nrows)){
    rows_by_chunk_except_last <- by_nrows
    #### Count number of files needed depending on by_nrows
    nfiles <- meta_output$nrows %/% rows_by_chunk_except_last
    #### remainder of rows go in last file
    last_chunk <- meta_output$nrows %% rows_by_chunk_except_last
    n_char_num <- nchar(as.character(nfiles + 1))

    #### a warning in case the user choices cause an excessive number of files
    #### to be created
    if(nfiles > 50){
      user_input <- readline(paste0("**********\n  Are you sure you want to generate ",
                                    nfiles + 1," files ? (y/n)  \n**********"))
      if(user_input != 'y'){
        stop('Exiting...')
      }
    }

    #### 1st chunk
    ##### +1 because header

    ##### head doesn't lose colnames
    unixCmdStr <- paste('head -n', (rows_by_chunk_except_last + 1),
                        file)
    args_fread <- c(cmd = unixCmdStr, args)
    df_temp <- do.call(fread, args_fread)
    print(paste0(1, " - - ", nrow(df_temp)))

    args_fwrite <- c(x = df_temp,
                     file = paste0(base_file, "_",
                                   str_pad(1, n_char_num, pad = "0"), ".csv"),
                     args)
    do.call(fwrite, args_fwrite)


    #### chunk 2 to n, here the last one is n+1
    for(ii in 2:(nfiles)){
      unixCmdStr <- paste0('sed -e 1,',
                           ((rows_by_chunk_except_last * (ii-1)) +1),
                           'd;', (rows_by_chunk_except_last * ii) + 1,
                           'q ', file)
      args_fread <- c(cmd = unixCmdStr, args)
      df_temp <- do.call(fread, args_fread) %>%
        `colnames<-`(meta_output$colnames)
      print(paste0(ii, " - - ", nrow(df_temp)))

      args_fwrite <- c(x = df_temp,
                       file = paste0(base_file, "_", str_pad(ii, n_char_num, pad = "0"), ".csv"),
                       args)
      do.call(fwrite, args_fwrite)
    }
    #### The remainder can be zero, in that case there is not a n+1th file
    if(last_chunk != 0){
      unixCmdStr <- paste0('sed -e 1,',
                           ((rows_by_chunk_except_last * (nfiles)) + 1), 'd;',
                           meta_output$nrows + 1,'q ', file)
      args_fread <- c(cmd = unixCmdStr, args)
      df_temp <- do.call(fread, args_fread) %>%
        `colnames<-`(meta_output$colnames)
      print(paste0(nfiles + 1, " - - ", nrow(df_temp)))

      args_fwrite <- c(x = df_temp,
                       file = paste0(base_file, "_",
                                     str_pad((nfiles + 1), n_char_num, pad = "0"), ".csv"),
                       args)
      do.call(fwrite, args_fwrite)
    }
  }

  ### 3. Splitting by value per column
  if(!missing(by_columns)){
    #### Argument accepts vectors of characters or numeric
    #### We make sure we have both (although not always necessary)
    if(is.character(by_columns)){
      colnums <- match(by_columns, meta_output$colnames)
      colnames <- by_columns
    }
    if(is.numeric(by_columns)){
      colnums <- by_columns
      colnames <- meta_output$colnames[by_columns]
    }

    #### building the unix str
    unixCmdStr <- bselectStr(file = file,
                             colnums = colnums,
                             meta_output = meta_output,
                             sep = sep) %>%
      paste(file)

    #### we load only the columns we need to identify the unique values then
    #### find all combinations to later apply filters
    args_fread <- c(cmd = unixCmdStr, args)
    unique_values_for_split <- do.call(fread, args_fread) %>%
      lapply(FUN = unique) %>%
      expand.grid(stringsAsFactors = F)
    #### number of combos = number of files
    nfiles <- nrow(unique_values_for_split)

    #### warning to not create a million files for nothing
    if(nfiles > 50){
      user_input <- readline(paste0("**********\n\n  Are you sure you want to generate ",
                                    nfiles," files ? (y/n)  \n\n**********"))
      if(user_input != 'y'){
        stop('Exiting...')
      }
    }

    #### GENERAL CASE: SPLITTING WITH n>=1 COLUMN

    for(ii in 1:nfiles){
      ###### In case some combos result in empty df
      skipped_files = 0
      ##### one row is one pattern to filter for
      patt = unique_values_for_split[ii,]
      ##### escaping special characters in the vectors
      patt_escaped <- escape_special_characters(patt)
      ##### collapsing vectors in order to name the output files
      file_ext = paste(patt, collapse = "-")
      print(paste0("Generating file ", ii, " of ",
                   nfiles, " : ", paste0(base_file, "_", file_ext, ".csv")))
      ##### filtering by unique combos then writing the outputs
      df_temp <- bfilter(file = file,
                         patterns = patt_escaped,
                         filtered_columns = colnums,
                         meta_output = meta_output,
                         ...)
      ###### dropping empty df, except if argument drop_empty_files is T
      if(nrow(df_temp) > 0 | drop_empty_files == F){
        args_fwrite <- c(x = df_temp,
                         file = paste0(base_file, "_", file_ext, ".csv"),
                         args)
        do.call(fwrite, args_fwrite)
        print(paste0("File ", ii, " of ",
                     nfiles, " :OK!"))
      } else {
        print(paste0("File ", ii, " of ",
                     nfiles, " is empty, skipping."))
        skipped_files <- skipped_files + 1
      }
    }

  }
  print(paste0("Generated ", nfiles - skipped_files, " files ! (", skipped_files," files skipped"))
}


