# data-cleaning functions here
download_raw_p2_data <- function(endpoints = p2_api_endpoints(),
                                 output_dir = h("raw-eidith-data"),
                                 verbose = TRUE) {
  if (!dir.exists(output_dir)) dir.create(output_dir)

  files <- purrr::map(endpoints, function(x) {
    file_out <- file.path(output_dir, paste0(x, ".tsv.gz"))
    dat <- eidith::ed2_get(x, postprocess = FALSE, verbose = verbose)
    readr::write_tsv(dat, file_out)
    return(file_out)
  })
  invisible(files)
}

# indentify NA cells
get_na <- function(dat){
  which(is.na(dat), arr.ind=TRUE) %>%
    as_tibble() %>%
    mutate(flag = "NA",
           fill = "red")
}

# identify solo unique values
get_solo_char <- function(dat){
  map_df(seq_along(dat), function(i){
    col_dat <- dat %>% pull(i)
    if(class(col_dat) == "character" & !all(is.na(col_dat))){
      col_tbl <- table(col_dat) %>% as_tibble()
      solo_vals <- col_tbl %>%
        filter(n==1) %>%
        mutate(col_dat = paste0("^", col_dat, "$")) %>%
        pull(col_dat) %>%
        paste(collapse = "|")
      if(!solo_vals == ""){
        solo_vals_match <-  grepl(solo_vals,  col_dat)
        which_solo <- tibble(row = which(solo_vals_match==TRUE), col = i,
                             flag = "unique value", fill = "green")
        return(which_solo)
      }
    }
  })
}

# numeric identify outliers
get_outlier <- function(dat){
  map_df(seq_along(dat), function(i){
    col_dat <- dat %>% pull(i)
    if(class(col_dat) == "numeric" & !all(is.na(col_dat)) & !grepl("Latitude|Longitude", colnames(dat[,i])) ){
      outlier <- boxplot.stats(col_dat, coef = 2.5)$out # x < (25th perc - coef * iqr) | x > (75th perc + coef * iqr)
      if(length(outlier)>0){
        outlier_match <-  col_dat  %in% outlier
        which_outlier <- tibble(row = which(outlier_match==TRUE), col = i,
                                flag = "numeric outlier", fill = "yellow")
        return(which_outlier)
      }
    }
  })
}


