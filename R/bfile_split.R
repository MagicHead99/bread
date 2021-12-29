### TO DO, add ... args that can be passed to fwrite

bfile_split <- function(file = NULL,
                        nfiles, nrows,
                        by_col,
                        meta_output = NULL,
                        sep = ";", dec = ","){
  ### ADD CHECKS THAT ONLY 1 PARAM IS PROVIDED
  ### ERROR IF MORE
  if (missing(nfiles) + missing(nrows) + missing(by_col) < 2L){
    stop("Used more than one of the arguments nfiles=, nrows=, by_col=.")
  }
  if (missing(nfiles) + missing(nrows) + missing(by_col) == 3L){
    stop("Need to provide one of the arguments nfiles=, nrows= or by_col=.")
  }
  base_file <- tools::file_path_sans_ext(file)

  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }

  if(!missing(nfiles)){
    rows_by_chunk_except_last <- meta_output$nrows %/% (nfiles)
    last_chunk <- (meta_output$nrows %% (nfiles)) + rows_by_chunk_except_last
    n_char_num <- nchar(as.character(nfiles))

    #### 1st chunk
    ##### +1 because header
    df_temp <- fread(cmd = paste('head -n', (rows_by_chunk_except_last + 1), file),
                     sep = sep, dec = dec, stringsAsFactors = FALSE)
    print(paste0("file 1 : ", nrow(df_temp)))
    fwrite(df_temp, paste0(base_file, "_", str_pad(1, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)

    #### chunk 2 to n-1
    for(ii in 2:(nfiles - 1)){
      df_temp <- fread(cmd = paste0('sed -e 1,', ((rows_by_chunk_except_last * (ii-1)) +1), 'd;', (rows_by_chunk_except_last * ii) + 1,'q ', file), sep = sep, dec = dec) %>%
        `colnames<-`(meta_output$colnames)
      print(paste0("file ", ii, " : " , nrow(df_temp)))
      fwrite(df_temp, paste0(base_file, "_", str_pad(ii, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)
    }


    #### last chunk n
    df_temp <- fread(cmd = paste0('sed -e 1,', ((rows_by_chunk_except_last * (nfiles - 1))) + 1, 'd;', meta_output$nrows + 1,'q ', file), sep = sep, dec = dec) %>%
      `colnames<-`(meta_output$colnames)
    print(paste0("file ", nfiles, " : " , nrow(df_temp)))
    fwrite(df_temp, paste0(base_file, "_", str_pad(nfiles, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)
  }


  if(!missing(nrows)){
    rows_by_chunk_except_last <- nrows
    nfiles <- meta_output$nrows %/% rows_by_chunk_except_last
    last_chunk <- meta_output$nrows %% rows_by_chunk_except_last
    n_char_num <- nchar(as.character(nfiles + 1))

    if(nfiles > 10){
      user_input <- readline(paste0("**********\n\n  Are you sure you want to generate ",
                                    nfiles," files ? (y/n)  \n\n**********"))
      if(user_input != 'y'){
        stop('Exiting...')
      }
    }

    #### 1st chunk
    ##### +1 because header
    df_temp <- fread(cmd = paste('head -n', (rows_by_chunk_except_last + 1), file),
                     sep = sep, dec = dec, stringsAsFactors = FALSE)
    #print(nrow(df_temp))
    fwrite(df_temp, paste0(base_file, "_", str_pad(1, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)

    #### chunk 2 to n, here the last one is n+1
    for(ii in 2:(nfiles)){
      print(paste0(ii, " - - ", nrow(df_temp)))
      df_temp <- fread(cmd = paste0('sed -e 1,', ((rows_by_chunk_except_last * (ii-1)) +1), 'd;', (rows_by_chunk_except_last * ii) + 1,'q ', file),
                       sep = sep, dec = dec) %>%
        `colnames<-`(meta_output$colnames)

      fwrite(df_temp, paste0(base_file, "_", str_pad(ii, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)
    }

    if(last_chunk != 0){
      #### last chunk n+1 sauf si le nombre est rond et qu'il n'y a pas de reste
      df_temp <- fread(cmd = paste0('sed -e 1,', ((rows_by_chunk_except_last * (nfiles)) + 1), 'd;', meta_output$nrows + 1,'q ', file),
                       sep = sep, dec = dec) %>%
        `colnames<-`(meta_output$colnames)
      print(nrow(df_temp))
      fwrite(df_temp, paste0(base_file, "_", str_pad((nfiles + 1), n_char_num, pad = "0"), ".csv"),
             sep = sep, dec = dec)

    }
  }
  if(!missing(by_col)){
    if(is.character(by_col)){
      colnums <- match(by_col, meta_output$colnames)
      colnames <- by_col
    }
    if(is.numeric(by_col)){
      colnums <- by_col
      colnames <- meta_output$colnames[by_col]
    }

    unixCmdStr <- bselectStr(file = file,
                             colnums = colnums,
                             meta_output = meta_output,
                             sep = sep) %>%
      paste(file)
    columns_to_split <- fread(cmd = unixCmdStr, sep = sep, dec = dec)
    nfiles <- dim(unique(columns_to_split[,1]))[1]
    if(length(columns_to_split) > 1){
      warning("*** Only the first column provided will be used to split the file. ***")
    }

    if(nfiles > 10){
      user_input <- readline(paste0("**********\n\n  Are you sure you want to generate ",
                                    nfiles," files ? (y/n)  \n\n**********"))
      if(user_input != 'y'){
        stop('Exiting...')
      }
    }

    for(ii in 1:nfiles){
      patt = as.character(unique(columns_to_split[,1])[ii])
      patt_escaped <- escape_special_characters(patt)
      print(paste0("Generating file ", ii, " of ",
                   nfiles, " : ", paste0(base_file, "_", patt, ".csv")))
      df_temp <- bfilter(file = file,
                         patterns = patt_escaped,
                         filtered_columns = colnums[1],
                         meta_output = meta_output,
                         sep = sep, dec = dec)
      fwrite(df_temp, paste0(base_file, "_", patt, ".csv"),
             sep = sep, dec = dec)
    }
  }
  warning(paste0("Generated ", nfiles, " files !"))
}
### 3 things:
### 1. split by nrow
### 2. split by number of files
### 3. split by individual values on one (or more?) columns

