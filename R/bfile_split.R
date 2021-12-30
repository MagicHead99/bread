### 3 things:
### 1. split by nrow
### 2. split by number of files
### 3. split by individual values on one (or more?) columns

### TO DO, add ... args that can be passed to fwrite

bfile_split <- function(file = NULL,
                        by_nfiles, by_nrows,
                        by_columns, drop_empty_files = T,
                        meta_output = NULL,
                        sep = ";", dec = ","){
  ### ADD CHECKS THAT ONLY 1 PARAM IS PROVIDED
  ### ERROR IF MORE
  if (missing(by_nfiles) + missing(by_nrows) + missing(by_columns) < 2L){
    stop("Used more than one of the arguments by_nfiles=, by_nrows=, by_columns=.")
  }
  if (missing(by_nfiles) + missing(by_nrows) + missing(by_columns) == 3L){
    stop("Need to provide one of the arguments by_nfiles=, by_nrows= or by_columns=.")
  }
  base_file <- tools::file_path_sans_ext(file)

  if(is.null(meta_output)){
    meta_output = bmeta(file)
  }

  if(!missing(by_nfiles)){
    rows_by_chunk_except_last <- meta_output$nrows %/% (by_nfiles)
    last_chunk <- (meta_output$nrows %% (by_nfiles)) + rows_by_chunk_except_last
    n_char_num <- nchar(as.character(by_nfiles))

    #### 1st chunk
    ##### +1 because header
    df_temp <- fread(cmd = paste('head -n', (rows_by_chunk_except_last + 1), file),
                     sep = sep, dec = dec, stringsAsFactors = FALSE)
    print(paste0("file 1 : ", nrow(df_temp)))
    fwrite(df_temp, paste0(base_file, "_", str_pad(1, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)

    #### chunk 2 to n-1
    for(ii in 2:(by_nfiles - 1)){
      df_temp <- fread(cmd = paste0('sed -e 1,', ((rows_by_chunk_except_last * (ii-1)) +1), 'd;', (rows_by_chunk_except_last * ii) + 1,'q ', file), sep = sep, dec = dec) %>%
        `colnames<-`(meta_output$colnames)
      print(paste0("file ", ii, " : " , nrow(df_temp)))
      fwrite(df_temp, paste0(base_file, "_", str_pad(ii, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)
    }


    #### last chunk n
    df_temp <- fread(cmd = paste0('sed -e 1,', ((rows_by_chunk_except_last * (by_nfiles - 1))) + 1, 'd;', meta_output$nrows + 1,'q ', file), sep = sep, dec = dec) %>%
      `colnames<-`(meta_output$colnames)
    print(paste0("file ", by_nfiles, " : " , nrow(df_temp)))
    fwrite(df_temp, paste0(base_file, "_", str_pad(by_nfiles, n_char_num, pad = "0"), ".csv"), sep = sep, dec = dec)
  }


  if(!missing(by_nrows)){
    rows_by_chunk_except_last <- by_nrows
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


  if(!missing(by_columns)){
    if(is.character(by_columns)){
      colnums <- match(by_columns, meta_output$colnames)
      colnames <- by_columns
    }
    if(is.numeric(by_columns)){
      colnums <- by_columns
      colnames <- meta_output$colnames[by_columns]
    }

    unixCmdStr <- bselectStr(file = file,
                             colnums = colnums,
                             meta_output = meta_output,
                             sep = sep) %>%
      paste(file)
    ## find all combinations of filters to apply
    columns_to_split <- fread(cmd = unixCmdStr, sep = sep, dec = dec) %>%
      lapply(FUN = unique) %>%
      expand.grid(stringsAsFactors = F)
    ##
    nfiles <- nrow(columns_to_split)


    if(nfiles > 10){
      user_input <- readline(paste0("**********\n\n  Are you sure you want to generate ",
                                    nfiles," files ? (y/n)  \n\n**********"))
      if(user_input != 'y'){
        stop('Exiting...')
      }
    }
    # ### SPLITTING WITH 1 COLUMN
    # if(length(columns_to_split) == 1){
    # for(ii in 1:nfiles){
    #   patt = as.character(unique(columns_to_split[,1])[ii])
    #   patt_escaped <- escape_special_characters(patt)
    #   print(paste0("Generating file ", ii, " of ",
    #                nfiles, " : ", paste0(base_file, "_", patt, ".csv")))
    #   df_temp <- bfilter(file = file,
    #                      patterns = patt_escaped,
    #                      filtered_columns = colnums[1],
    #                      meta_output = meta_output,
    #                      sep = sep, dec = dec)
    #   fwrite(df_temp, paste0(base_file, "_", patt, ".csv"),
    #          sep = sep, dec = dec)
    # }
    # }
    ### SPLITTING WITH n>=1 COLUMN

      for(ii in 1:nfiles){
        skipped_files = 0
        patt = columns_to_split[ii,]
        patt_escaped <- escape_special_characters(patt)
        file_ext = paste(patt, collapse = "-")
        print(paste0("Generating file ", ii, " of ",
                     nfiles, " : ", paste0(base_file, "_", file_ext, ".csv")))
        df_temp <- bfilter(file = file,
                           patterns = patt_escaped,
                           filtered_columns = colnums,
                           meta_output = meta_output,
                           sep = sep, dec = dec)
        if(nrow(df_temp) > 0 | drop_empty_files == F){
        fwrite(df_temp, paste0(base_file, "_", file_ext, ".csv"),
               sep = sep, dec = dec)
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


