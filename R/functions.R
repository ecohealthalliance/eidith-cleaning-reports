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

  sapply(1:length(dfs), function(x) addWorksheet(wb, sheetName = tab.names[x]))

  sapply(1:length(dfs), function(x) writeDataTable(wb, sheet = tab.names[x], x = dfs[[x]]))

  # Generate a style list containing style information for each worksheet

  style.list <- vector("list", length(dfs))

  names(style.list) <- tab.names

  for(df in 1:length(dfs)) {

    style.list[[df]] <- list(rep(NULL), length(markup.dfs[[df]]$fill))

    style.list[[df]] <-

      sapply(1:length(markup.dfs[[df]]$fill), function(x)

        style.list[[df]][[x]] <- createStyle(fgFill = as.character(markup.dfs[[df]]$fill[x]))
      )
  }

  # Generate a collated style list for the whole workbook

  wb.list <- vector("list", 3)

  names(wb.list) <- c("row", "col", "style")

  for(df in 1:length(dfs)) {

    wb.list[["row"]][[df]] <- markup.dfs[[df]]$row

    wb.list[["col"]][[df]] <- markup.dfs[[df]]$col

    wb.list[["style"]][[df]] <- style.list[[df]]
  }

  # Style the worksheets in the workbook

  for(df in 1:length(dfs)) {

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
