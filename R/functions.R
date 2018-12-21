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

# create unique character values table
create_unique_table <- function(dat, cols_to_ignore = c(), date_col = ""){

  # select character data only
  dat_char <- dat %>% select_if(is.character)

  # ignore specified columns (regex matching)
  cols_to_ignore_regex <- paste(cols_to_ignore, collapse = "|")

  if(!is.null(cols_to_ignore)){dat_char <- dat_char %>% select(-matches(!!cols_to_ignore_regex))}

  # summarize unique values
  dat_char_uniq <- apply(dat_char, 2, unique) %>%
    map_df(~paste(.x, collapse = "; "))

  # add special case for date range
  if(date_col != ""){
    dat_char_uniq <- dat_char_uniq %>%
      mutate(!!date_col := paste0(min(dat %>% pull(!!date_col)), " to ", max(dat %>% pull(!!date_col)))) %>%
      select(Country, !!date_col, everything())
  }

  # final formatting
  dat_char_uniq <- dat_char_uniq %>%
    t() %>%
    kable() %>%
    kable_styling()

  return(dat_char_uniq)
}

# identify NA cells
get_na <- function(dat, cols_to_ignore = c()){

  # ID all NAs
  which_na = which(is.na(dat), arr.ind=TRUE) %>%
    as_tibble() %>%
    mutate(flag = "NA",
           fill = "red")

  # ignore columns that are 100% NA
  all_na <- which(apply(dat, 2, function(x){all(is.na(x))}))
  which_na <- which_na %>% filter(!col %in% all_na)

  # ignore other specified columns (regex matching)
  cols_to_ignore_regex <- paste(cols_to_ignore, collapse = "|")

  if(!is.null(cols_to_ignore)){
    col_names <- names(dat)[unique(which_na$col)]
    ignore_names <- col_names[grepl(cols_to_ignore_regex, col_names, ignore.case = TRUE)]
    which_na <- which_na %>% filter(!col %in% which(names(dat) %in% ignore_names))
  }

  return(which_na)
}

# identify solo unique values
get_solo_char <- function(dat) {

  # for every column...
  map_df(seq_along(dat), function(i) {

    col_dat <- dat %>% pull(i)

    # if these criteria are met...
    if(class(col_dat) == "character" &
       !all(is.na(col_dat)) &
       !grepl("Notes|EventName|AnimalID", colnames(dat[,i]), ignore.case = TRUE)) {

      # summarize count by unique character string
      col_tbl <- table(col_dat) %>% as_tibble()

      # select character strings that occur only once
      solo_vals <- col_tbl %>%
        filter(n==1) %>%
        pull(col_dat)

      if(!(length(solo_vals) == 1 & solo_vals[1] == "")) {

        solo_vals_match <- col_dat %in% solo_vals

        which_solo <- tibble(row = which(solo_vals_match == TRUE), col = i,
                             flag = "unique value", fill = "green")

        return(which_solo)
      }
    }
  })
}

# numeric identify outliers
get_outlier <- function(dat){

  # for every column...
  map_df(seq_along(dat), function(i){

    col_dat <- dat %>% pull(i)

    # if these criteria are met...
    if(class(col_dat) == "numeric" &
       !all(is.na(col_dat)) &
       !grepl("Latitude|Longitude", colnames(dat[,i]), ignore.case = TRUE) ){

      # identify outliers
      outlier <- boxplot.stats(col_dat, coef = 2.5)$out # x < (25th perc - coef * iqr) | x > (75th perc + coef * iqr)

      if(length(outlier)>0){
        outlier_match <-  col_dat %in% outlier
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

  # compare year, month, day, sitename
  which_mismatch <- dat %>%
    mutate(year_check = as.character(year(EventDate)) == str_sub(EventName,-9,-6),
           month_check = as.character(month(EventDate)) == match(str_sub(EventName,-5,-3), month.abb),
           day_check = str_pad(day(EventDate), width = 2, side = "left", pad = "0") == str_sub(EventName,-2,-1),
           site_name_check = tolower(SiteName) == tolower(str_sub(EventName,4,-11))) %>%
    rowwise() %>%
    mutate(event_name_check = all(year_check, month_check, day_check, site_name_check))

  # output mismatches
  tibble(row = which(which_mismatch$event_name_check==FALSE), col = grep("EventName", names(dat)),
         flag = "EventName does not match EventDate or SiteName", fill = "orange")
}

# identify duplicates
get_dups <- function(dat, col_name){

  which_dup <- dat %>%
    select(!!col_name) %>%
    mutate(is_dup = duplicated(!!col_name)|duplicated(!!col_name, fromLast = TRUE))

  tibble(row = which(which_dup$is_dup==TRUE), col = grep(col_name, names(dat)),
         flag = "duplicate identifier", fill = "purple")

}

# Generate a highlighted (formatted) XLSX workbook object
# dfs = a list of dataframes to write to distinct worksheets of the xlsx workbook file
# tab.names = a vector of desired names for the worksheet tabs
# markup.dfs = a list of mark up dataframes that contain styling information for each dataframe;
# should indicate the row and column positions of cells as well as the appropriate "fill" styling

get_highlighted_wb <- function(dfs, tab.names, markup.dfs) {

  # Check length of arguments

  assertthat::assert_that(length(dfs) == length(tab.names))
  assertthat::assert_that(length(tab.names) == length(markup.dfs))

  # Create workbook, generate the worksheets, save the dataframes to the worksheets

  wb <- createWorkbook()

  sapply(seq_along(dfs), function(x) addWorksheet(wb, sheetName = tab.names[x]))

  sapply(seq_along(dfs), function(x) writeDataTable(wb, sheet = tab.names[x], x = dfs[[x]]))

  # Generate a style list containing style information for each worksheet

  style.list <- vector("list", length(dfs))

  names(style.list) <- tab.names

  for(df in seq_along(dfs)) {

    style.list[[df]] <- list(rep(NULL), length(markup.dfs[[df]]$fill))

    style.list[[df]] <-

      sapply(1:length(markup.dfs[[df]]$fill), function(x)

        style.list[[df]][[x]] <- createStyle(fgFill = as.character(markup.dfs[[df]]$fill[x]))
      )
  }

  # Generate a collated style list for the whole workbook

  wb.list <- vector("list", 3)

  names(wb.list) <- c("row", "col", "style")

  for(df in seq_along(dfs)) {

    wb.list[["row"]][[df]] <- markup.dfs[[df]]$row

    wb.list[["col"]][[df]] <- markup.dfs[[df]]$col

    wb.list[["style"]][[df]] <- style.list[[df]]
  }

  # Style the worksheets in the workbook

  for(df in seq_along(dfs)) {

    sapply(1:length(wb.list[["style"]][[df]]), function(x)

      addStyle(wb,
               sheet = tab.names[df],
               style = wb.list[["style"]][[df]][[x]],
               rows = wb.list[["row"]][[df]][[x]] + 1,
               cols = wb.list[["col"]][[df]][[x]]
      )
    )
  }

  # Return workbook

  return(wb)
}
