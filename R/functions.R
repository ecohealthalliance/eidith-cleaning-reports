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
get_na <- function(dat, ignore_notes = TRUE){
  out = which(is.na(dat), arr.ind=TRUE) %>%
    as_tibble() %>%
    mutate(flag = "NA",
           fill = "red")
  if(ignore_notes){
    col_names <- names(dat)[unique(out$col)]
    col_names <- col_names[!grepl("notes", col_names, ignore.case = TRUE)]
    out <- filter(out, col %in%  which(names(dat) %in% col_names))
  }
  return(out)
}

# identify solo unique values
get_solo_char <- function(dat){
  map_df(seq_along(dat), function(i){
    col_dat <- dat %>% pull(i)
    if(class(col_dat) == "character" &
       !all(is.na(col_dat)) &
       !grepl("Notes|EventName", colnames(dat[,i]), ignore.case = TRUE)){
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
    if(class(col_dat) == "numeric" &
       !all(is.na(col_dat)) &
       !grepl("Latitude|Longitude", colnames(dat[,i]), ignore.case = TRUE) ){
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

# check that EventName matches SiteName and EventDate
library(stringi)
library(lubridate)

get_event_mismatch <- function(dat){
  dat <- dat %>%
    mutate(year_check = as.character(year(EventDate)) == str_sub(EventName,-9,-6),
           month_check = as.character(month(EventDate)) == match(str_sub(EventName,-5,-3), month.abb),
           day_check = str_pad(day(EventDate), width = 2, side = "left", pad = "0") == str_sub(EventName,-2,-1),
           site_name_check = tolower(SiteName) == tolower(str_sub(EventName,4,-11))) %>%
    rowwise() %>%
    mutate(event_name_check = all(year_check, month_check, day_check, site_name_check))

  tibble(row = which(dat$event_name_check==FALSE), col = grep("EventName", names(dat)),
         flag = "EventName does not match EventDate or SiteName", fill = "orange")
}
