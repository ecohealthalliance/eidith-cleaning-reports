# data-cleaning functions here
download_raw_p2_data <- function(endpoints = p2_api_endpoints(),
                                 output_dir = h("raw-eidith-data"),
                                 verbose = TRUE) {
  if (!dir.exists(output_dir)) dir.create(output_dir)

  files <- purrr::walk(endpoints, function(x) {

    dat <- eidith::ed2_get(x, postprocess = FALSE, verbose = verbose)
    if (!is.null(dat$Country)) {
      datlist <- split(dat, dat$Country)
      purrr::walk(datlist, function(z) {
        file_out <- file.path(output_dir, stri_join(x, "-", z$Country[1], ".tsv.gz"))
        readr::write_tsv(z, file_out)
      })
    } else {
      file_out <- file.path(output_dir, stri_join(x, ".tsv.gz"))
      readr::write_tsv(dat, file_out)
    }
  })
  return(TRUE)
}

# create unique values table
create_unique_table <- function(dat, cols_to_ignore = c()){

  # ignore specified columns (regex matching)
  cols_to_ignore_regex <- stri_join(c(cols_to_ignore, "Latitude", "Longitude"), collapse = "|")

  dat <- dat %>% select(-matches(!!cols_to_ignore_regex))

  collapse_mult <- function(x){

    clean_x <- str_split(x, pattern = ";") %>%
      unlist() %>%
      str_trim()

    unique_values <- unique(clean_x) %>% sort()

    sum_tbl <- table(clean_x)

    stri_join(unique_values, " (", sum_tbl, ")" ) %>%
      stri_join(., collapse = "; ")
  }

  # character data
  dat_char <- dat %>%
    select_if(is.character) %>%
    gather() %>%
    na.omit() %>%
    group_by(key) %>%
    summarize(values = collapse_mult(value),
              count_missing = stri_join(nrow(dat) - n(), nrow(dat), sep = "/"))

  # numeric data
  dat_num <- dat %>%
    select_if(is.numeric) %>%
    gather() %>%
    na.omit() %>%
    group_by(key) %>%
    summarize(values = stri_join(min(value), max(value), sep = "-"), count_missing = stri_join(nrow(dat) - n(), nrow(dat), sep = "/"))

  # date data
  dat_date <- dat %>%
    select_if(function(col) is.Date(col) | is.difftime(col))

  dat_date <- map_df(seq_along(dat_date), function(i){
    test <- dat_date %>%
      select(i) %>%
      gather() %>%
      na.omit() %>%
      group_by(key) %>%
      summarize(values = stri_join(min(value), max(value), sep = " to "), count_missing = stri_join(nrow(dat) - n(), nrow(dat), sep = "/"))
  })

  # all na
  dat_na_names <- dat %>%
    select_if(function(x) all(is.na(x))) %>%
    colnames()
  dat_na <- tibble(key = dat_na_names, values = "all missing", count_missing = stri_join(nrow(dat), nrow(dat), sep = "/"))

  # together

  mm.measures <- c("BatEarHeight", "BatTailLength", "BatHindFoodLength",
                   "BatForearmLength", "BatHeadBodyLength", "NHPBodyLength",
                   "NHPHeadWidth", "NHPHeadLength", "NHPRightEarWidth",
                   "NHPLeftEarWidth", "NHPRightEarHeight", "NHPLeftEarHeight",
                   "RodentOtherRightEarHeight", "RodentOtherTailLength",
                   "RodentOtherRHindFoot", "RodentOtherBodyLength")

  g.measures <- c("BatWeight", "NHPWeight", "RodentOtherWeight")

  bind_rows(dat_char, dat_num, dat_date, dat_na) %>%
    arrange(factor(key, levels = colnames(dat))) %>%
    rename(field = key) %>%
    mutate(count_missing =
             cell_spec(count_missing, format = "html",
                       background = ifelse(grepl("^0/", count_missing), "#FFFFFF", "#EA8E9A")
             ),
           field = case_when(
             field %in% mm.measures ~ stri_join(field, " (mm)"),
             field %in% g.measures ~ stri_join(field, " (g)"),
             TRUE ~ field
           )
    ) %>%
    kable(format = "html", escape = FALSE) %>%
    kable_styling()
}

# identify NA cells
get_na <- function(dat, cols_to_ignore = c()){

  # ID all NAs
  which_na = which(is.na(dat), arr.ind=TRUE) %>%
    as_tibble() %>%
    mutate(flag = "missing value",
           fill = "red")

  # ignore columns that are 100% NA
  all_na <- which(apply(dat, 2, function(x){all(is.na(x))}))
  which_na <- which_na %>% filter(!col %in% all_na)

  # ignore other specified columns (regex matching)
  cols_to_ignore_regex <- stri_join(cols_to_ignore, collapse = "|")

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
    if(class(col_dat)[1] == "character" &
       !all(is.na(col_dat)) &
       !grepl("Notes|EventName|ID", colnames(dat[,i]), ignore.case = FALSE)) {

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
    if(class(col_dat)[1] == "numeric" &
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

# check that SpecimenID matches Animal ID and SpecimentType/Medium
get_specimen_mismatch <- function(dat){

  lookup <- read_csv(here::here("specimen_lookup.csv"))

  specimen_lookup <- lookup %>% filter(type != "storage medium") %>% select(-type) %>% rename(full_specimen = full)
  medium_lookup <- lookup %>% filter(type == "storage medium") %>% select(-type) %>% rename(full_medium = full)

  which_mismatch <- dat %>%
    extract(SpecimenID, into = c("id", "specimen_abbr", "medium_abbr"),
            regex = "^([^\\.]+)\\.(\\w{2})(\\w)", remove = FALSE) %>%
    left_join(specimen_lookup, by = c("specimen_abbr" = "code")) %>%
    left_join(medium_lookup, by = c("medium_abbr" = "code")) %>%
    mutate(ID_check = id == `Animal/Human ID`,
           specimen_check = tolower(full_specimen) == tolower(SpecimenType),
           medium_check = tolower(full_medium) == tolower(Medium)) %>%
    replace_na(list(specimen_check = FALSE, medium_check = FALSE)) %>%
    rowwise() %>%
    mutate(specimen_name_check = all(ID_check, specimen_check, medium_check))

  # output mismatches
  tibble(row = which(which_mismatch$specimen_name_check==FALSE), col = grep("SpecimenID", names(dat)),
         flag = "Mismatch between SpecimenID and Animal/Human ID, SpecimenType, and/or Medium. Abbreviated code in SpecimenID may not be recognized.", fill = "orange")
}

# identify duplicates
get_dups <- function(dat, col_name){

  which_dup <- dat %>%
    select(!!col_name) %>%
    mutate(is_dup = duplicated(.)|duplicated(., fromLast = TRUE))

  tibble(row = which(which_dup$is_dup==TRUE), col = grep(col_name, names(dat)),
         flag = "duplicate identifier", fill = "purple")

}

# Get cells for which the row has notes in the "Notes" column

get_cells_w_notes <- function(dat) {

  which_rows <- which(!is.na(dat$Notes))

  n.cols <- dim(dat)[2]

  tibble(row = rep(which_rows, each = n.cols),
         col = rep(1:n.cols, times = length(which_rows)),
         flag = "notes for row", fill = "darkseagreen1")
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

  # Add a "cleaning_flags" field to all dataframes for ease of interpreation

  for(df in seq_along(dfs)) {

    # Modifcation of the markup.df to account for the "Notes" field

    markup.dfs[[df]] <- bind_rows(markup.dfs[[df]], get_cells_w_notes(dfs[[df]]))

    # Increment the "col" field in the markup dataframe by 1 since the new "cleaning_flag"
    # column will be first in the final exported data, throwing everything off by 1

    markup.dfs[[df]]$col <- markup.dfs[[df]]$col + 1

    # Save original column names

    original.cols <- colnames(dfs[[df]])

    # Generate flag values for each relevant row of the dataframe and add these to the
    # data as a "cleaning_flags" column

    # Generate initial flag values, ignoring "notes for row" since we want these to be the
    # final flag listed in relevant rows

    flag.table <- markup.dfs[[df]] %>%
      arrange(row, col) %>%
      mutate(col.name = original.cols[col - 1],
             col = cellcol_lookup[col],
             flag_mod = stri_join(flag, " (", col, ", ", col.name, ")")
      ) %>%
      group_by(row) %>%
      filter(flag != "notes for row") %>%
      summarize(row_flag = stri_join(flag_mod, collapse = "; "))

    # Which rows need the addition of "notes for row" into the "cleaning_flag" column?

    rows.w.notes <- unique(get_cells_w_notes(dfs[[df]])$row)

    # Generate "cleaning_flags" column in the dataframe, pasting in "notes for row" to create
    # final flag values for the relevant rows

    dfs[[df]]$cleaning_flags <- ""
    dfs[[df]]$cleaning_flags[flag.table$row] <- flag.table$row_flag
    dfs[[df]]$cleaning_flags[rows.w.notes] <-
      stri_join(dfs[[df]]$cleaning_flags[rows.w.notes], "notes for row", sep = "; ") %>%
      stri_replace(., "", regex = "^; ")

    # Reorder columns in the dataframe such that "cleaning_flags" appears first in the
    # output workbook

    dfs[[df]] <- select(dfs[[df]], cleaning_flags, original.cols)

    # Modify markup.df to result in special styling for any "cleaning_flags" cell that
    # contains a flag

    new.markup.rows <- expand.grid(row = which(dfs[[df]]$cleaning_flags != ""), col = 1,
                            flag = "flag in row", fill = "lightpink")
    new.markup.rows$flag <- as.character(new.markup.rows$flag)
    new.markup.rows$fill <- as.character(new.markup.rows$fill)

    markup.dfs[[df]] <- bind_rows(markup.dfs[[df]], new.markup.rows)
  }

  # Create workbook, generate the worksheets, save the dataframes to the worksheets

  wb <- createWorkbook()

  sapply(seq_along(dfs), function(x) addWorksheet(wb, sheetName = tab.names[x]))

  sapply(seq_along(dfs), function(x) writeDataTable(wb, sheet = tab.names[x], x = dfs[[x]]))

  # Generate a style list containing style information for each worksheet

  style.list <- lapply(markup.dfs, function(x) {

    x %>%
      group_by(fill) %>%
      summarize(rows = list(row + 1),
                cols = list(col),
                style = list(createStyle(fgFill = fill[1]))
      ) %>%
      ungroup()
  })

  for(df in seq_along(dfs)) {

    for(x in seq_along(style.list[[df]]$style)) {

      addStyle(wb,
               sheet = tab.names[df],
               style = style.list[[df]]$style[[x]],
               rows = style.list[[df]]$rows[[x]],
               cols = style.list[[df]]$cols[[x]],
      )
    }
  }

  # Return workbook

  return(wb)
}

# Get leaflet labels for events

get_event_labels <- function() {

  # Generate text labels

  labs <- lapply(seq(nrow(event)), function(x) {

    stri_join("Site Name: ",
           pull(event[x, "SiteName"]),
           "<br>Concurrent Sampling Site: ",
           pull(event[x, "ConcurrentSamplingSite"])
    )
  })

  # Convert to html for display by leaflet

  labs <- lapply(labs, htmltools::HTML)

  return(labs)
}

# Get leaflet icon styling for events

get_event_icons <- function() {

  # Generate a table of all unique concurrent site values, along
  # with a column indicating the level of those unique values,
  # ensuring that "Independent Site" is always level 1

  sites <- event %>%
    pull(ConcurrentSamplingSite) %>%
    sort() %>%
    factor()

  if("Independent Site" %in% sites) sites <- relevel(sites, "Independent Site")

  level.table <- sites %>%
    levels() %>%
    data.frame() %>%
    mutate(site_factor_level = 1:n())

  colnames(level.table)[1] <- "ConcurrentSamplingSite"

  level.table$ConcurrentSamplingSite <- as.character(level.table$ConcurrentSamplingSite)

  # Join this information to the event table

  event_mod <- left_join(event, level.table, by = c("ConcurrentSamplingSite"))

  # Colors allowed by awesomeIcons()

  possible.colors <- c("lightgray", "green", "red", "blue",
                       "orange", "beige", "purple", "pink",
                       "white", "gray", "darkpurple", "cadetblue",
                       "darkgreen", "darkred", "darkblue",
                       "lightgreen", "lightred", "lightblue"
  )

  # Generate a vector of colors based on the site factor level

  colors <- possible.colors[event_mod$site_factor_level]

  # Generate icons for plotting with leaflet

  return(
    awesomeIcons(
      icon = "fa-circle-o",
      library = "fa",
      iconColor = "black",
      markerColor = colors
    )
  )
}

# Create a lookup table for Excel column lettering scheme

cellcol_lookup <- cellranger::num_to_letter(1:2000)
