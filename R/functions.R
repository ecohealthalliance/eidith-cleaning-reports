# data-cleaning functions here
download_raw_p2_data <- function(endpoints = p2_api_endpoints(),
                                 output_dir = h("raw-eidith-data"),
                                 verbose = TRUE,
                                 country = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir)

  files <- purrr::walk(endpoints, function(x) {

    dat <- eidith::ed2_get(x, postprocess = FALSE, verbose = verbose, country = country)
    if (!is.null(dat$Country)) {
      datlist <- split(dat, dat$Country)
      purrr::walk(datlist, function(z) {
        file_out <- file.path(output_dir, stringi::stri_join(x, "-", z$Country[1], ".tsv.gz"))
        readr::write_tsv(z, file_out)
      })
    } else {
      file_out <- file.path(output_dir, stringi::stri_join(x, ".tsv.gz"))
      readr::write_tsv(dat, file_out)
    }
  })
  return(TRUE)
}

# support function for summarizing values in create_unique_table
collapse_mult <- function(x){

  if(all(is.na(x))){return("all empty")}

  if(is.character(x)){
    clean_x <- str_split(x, pattern = ";") %>%
      unlist() %>%
      str_trim()

    unique_values <- unique(clean_x) %>% sort()

    sum_tbl <- table(clean_x)

    return(stri_join(unique_values, " (", sum_tbl, ")" ) %>%
             stri_join(., collapse = "; "))
  }

  if(is.numeric(x)){
    return(stri_join(min(x, na.rm = TRUE), max(x, na.rm = TRUE), sep = "-"))
  }

  if(is.Date(x)){
    return(stri_join(min(x, na.rm = TRUE), max(x, na.rm = TRUE), sep = " to "))
  }

  if(is.difftime(x)){
    x <- kimisc::seconds.to.hms(as.numeric(x))
    return(stri_join(min(x, na.rm = TRUE), max(x, na.rm = TRUE), sep = " to "))
  }
}

# support function to check special conditions in missing value checks
check_conditions <- function(dat, col_name, condition_check){

  # evaluate whether special condition applies
  if(col_name %in% condition_check$col_na){

    # if condition applies, get index of condition in condition_check df
    which_condition <- which(col_name == condition_check$col_na)

    # for each condition
    ct <- map_dfc(which_condition, function(j){

      # get data from condition column
      col_condition <- dat %>% pull(condition_check$col_condition[j]) %>% stri_split_regex(., ";")

      # determine where condition column meets condition
      map_lgl(col_condition, function(x){
        cond <- any(tolower(x) %in% tolower(unlist(condition_check$condition[j])))

        # special eval of condition if condition is NA
        if(all(is.na(unlist(condition_check$condition[j])))){
          cond <- is.na(x)
        }

        # take inverse of condition if specified
        if(condition_check$inverse[j]){
          cond <- !cond
        }
        cond
      })
    })

    # get single vector of condition
    condition <- apply(ct, 1, all)
  }else{
    condition <- NULL
  }
  condition
}


# create unique values table
create_unique_table <- function(dat, metanames, condition_check, cols_to_ignore = c(), do_not_count_empty =c()){

  # check if null
  if(is.null(dat)){
    return(kable("No data in EIDITH", col.names = ""))
  }

  # specified columns to ignore (regex matching)
  cols_to_ignore_regex <- stri_join(cols_to_ignore, collapse = "|")

  # specified columns not to summarize count empty
  cols_not_empty_regex <- stri_join(do_not_count_empty, collapse = "|")

  # for each column in dataframe
  out <-map_df(seq_along(dat), function(i){

    # only include col names selected from metadata
    if(!i %in% which(names(dat) %in% metanames)){return()}

    # ignore other specified columns
    if(i %in% which(str_detect(names(dat), cols_to_ignore_regex))){return()}

    # get column data and name
    col_dat <- dat %>% pull(i)
    col_name <- names(dat)[i]

    # check special conditions
    condition <-  check_conditions(dat, col_name, condition_check)

    # subset column data for where condition is TRUE
    if(!is.null(condition)){
      col_dat <- col_dat[condition]
    }

    missing_numerator <- sum(is.na(col_dat))
    missing_denomenator <- length(col_dat)

    tibble(field = col_name,
           values = collapse_mult(col_dat),
           count_empty = ifelse(str_detect(col_name, cols_not_empty_regex),
                                "--",
                                stri_join(missing_numerator, missing_denomenator, sep = "/")))

  })

  mm.measures <- c("BatEarHeight", "BatTailLength", "BatHindFoodLength",
                   "BatForearmLength", "BatHeadBodyLength", "NHPBodyLength",
                   "NHPHeadWidth", "NHPHeadLength", "NHPRightEarWidth",
                   "NHPLeftEarWidth", "NHPRightEarHeight", "NHPLeftEarHeight",
                   "RodentOtherRightEarHeight", "RodentOtherTailLength",
                   "RodentOtherRHindFoot", "RodentOtherBodyLength")

  g.measures <- c("BatWeight", "NHPWeight", "RodentOtherWeight")

  out %>%
    mutate(values = stri_split(values, fixed = "; ")) %>%
    unnest() %>%
    mutate(
      values = ifelse(
        str_detect(values, fixed("(1)")),
        paste0("<span style=\"border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: #CEFCC7;\">", values, "</span>"),
        values
      ),
      count_empty =
        cell_spec(count_empty, format = "html",
                  background = ifelse(grepl("^0/|--", count_empty), "#FFFFFF", "#EA8E9A")
        ),
      field = case_when(
        field %in% mm.measures ~ stri_join(field, " (mm)"),
        field %in% g.measures ~ stri_join(field, " (g)"),
        TRUE ~ field
      )
    ) %>%
    group_by(field, count_empty) %>%
    mutate(values = paste0(values, collapse = "<br>")) %>%
    distinct() %>%
    select(field, values, count_empty) %>%
    kable(format = "html", escape = FALSE) %>%
    kable_styling()
}

# identify empty cells
get_empty <- function(dat, metanames, condition_check, cols_to_ignore = c()){

  # specified columns to ignore (regex matching)
  cols_to_ignore_regex <- stri_join(cols_to_ignore, collapse = "|")

  # load special rules for counting empties in col_na only if col_condition meets condition
  # condition_check <- read_csv(h("condition_check_v2.csv")) %>%
  #   filter(module_with_na == module) %>%
  #   mutate(condition = stri_split_regex(condition, ", "))

  which_na <- map_df(seq_along(dat), function(i){

    # only include col names selected from metadata
    if(!i %in% which(names(dat) %in% metanames)){return()}

    # ignore other specified columns
    if(i %in% which(str_detect(names(dat), cols_to_ignore_regex))){return()}

    # get column data and name and NAs
    col_dat <- dat %>% pull(i)
    col_name <- names(dat)[i]
    row_id <-  which(is.na(col_dat))

    # check special conditions
    condition <-  check_conditions(dat, col_name, condition_check)

    # get row ids of NA and condition
    if(!is.null(condition)){
      row_id <- which(is.na(col_dat) & condition)
    }

    # get the ids
    tibble(row = row_id,
           col = i,
           flag = "empty value",
           fill = "gray")

  })
  return(which_na)
}

# identify solo unique values
get_solo_char <- function(dat, by_SiteName = TRUE) {

  map_df(names(dat)[!names(dat) %in% "SiteName"], function(col_name) {

    sub_dat <- dat %>% select(SiteName, col_name)
    col_dat <- sub_dat %>% pull(2)

    # if these criteria are met...
    if(class(col_dat)[1] == "character" &
       !all(is.na(col_dat)) &
       !grepl("Notes|TestName|Comment|SiteName|ID|EventName|^Class$|^Order$|^Family$|^Genus$|Species|Q37 TravelledCity|Q37 Latitude|Q37 Longitude", col_name, ignore.case = FALSE)) {

      # summarize count by unique character string
      cols <- c("SiteName", col_name)
      if(!by_SiteName){cols <- cols[-1]}

      # unlist and sort multi response answers

      col_tbl <- sub_dat %>%
        mutate(!!col_name := str_split(get(col_name), pattern = "; "),
               !!col_name := map(get(col_name), ~.x[order(sapply(.x, sort))]),
               !!col_name :=  map_chr(get(col_name), ~stri_join(.x, collapse = '; '))) %>%
        group_by_at(cols) %>%
        count()

      solo_vals_match <- left_join(sub_dat, col_tbl)

      which_solo <- tibble(row = which(solo_vals_match$n == 1),
                           col = which(names(dat) == col_name),
                           flag = ifelse(by_SiteName, "unique value within given SiteName", "unique value"),
                           fill = "green")

      return(which_solo)
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
  tibble(row = which(which_mismatch$specimen_name_check==FALSE), col = which(names(dat)=="SpecimenID"),
         flag = "Mismatch between SpecimenID and Animal/Human ID, SpecimenType, and/or Medium. Abbreviated code in SpecimenID may not be recognized.", fill = "orange")
}

# identify duplicates
get_dups <- function(dat, col_name){

  which_dup <- dat %>%
    select(!!col_name) %>%
    mutate(is_dup = duplicated(.)|duplicated(., fromLast = TRUE))

  tibble(row = which(which_dup$is_dup==TRUE), col = which(names(dat)==col_name),
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

    if(nrow(x)==0){
      return(
        tibble(fill = "white", rows = list(2), cols = list(1), style = list(createStyle(fgFill = fill[1])))
      )
    }

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

# Colors allowed by awesomeIcons()

possible.aI.colors <- c("lightgray", "green", "red", "blue",
                        "orange", "beige", "purple", "pink",
                        "white", "gray", "darkpurple", "cadetblue",
                        "darkgreen", "darkred", "darkblue",
                        "lightgreen", "lightred", "lightblue"
)

# Scratch code to look at all awesomeIcons() marker colors
# RGB values can be extracted from screen with Digital Color Meter in Mac

# dat <- data.frame(
#   lat = seq(length.out = 18, from = 0, to = 80),
#   lng = seq(length.out = 18, from = 100, to = 300)
# )
#
# leaflet(data = dat) %>%
#   addTiles() %>%
#   addAwesomeMarkers(lng = ~lng, lat = ~lat,
#                     icon = awesomeIcons(markerColor = possible.aI.colors)
#   )

# Generate an awesomeIcons() to hex color lookup table

color.lookup.table <- data.frame(
  aI_color = possible.aI.colors,
  hex_color = c(
    "#A3A3A3", "#6FAB25", "#D03C29", "#37A8DA",
    "#F3952F", "#FFC98F", "#CD50B5", "#FF8BE8",
    "#FBFBFB", "#565656", "#593869", "#416574",
    "#72822E", "#9E3235", "#00669F",
    "#BBF770", "#FF8A7C", "#88DAFF"
  ),
  stringsAsFactors = FALSE
)

# Get leaflet labels for events

get_event_labels <- function() {

  # Generate text labels

  labs <-
    stri_join("Site Name: ",
              event$SiteName,
              "<br>Concurrent Sampling Site: ",
              event$ConcurrentSamplingSite
    )

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

  # Generate a vector of colors based on the site factor level

  colors <- possible.aI.colors[event_mod$site_factor_level]

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

# Get leaflet legend data for events

get_event_legend <- function() {

  # Generate legend labels

  label <- event$ConcurrentSamplingSite

  # Generate corresponding awesomeIcons() colors

  aI_color <- get_event_icons()$markerColor

  # These should be the same length

  assertthat::assert_that(length(label) == length(aI_color))

  legend_data <- data.frame(label, aI_color, stringsAsFactors = FALSE) %>%
    distinct() %>%
    left_join(., color.lookup.table, by = c("aI_color")) %>%
    arrange(label)

  return(legend_data)
}

# Create a lookup table for Excel column lettering scheme

cellcol_lookup <- cellranger::num_to_letter(1:2000)
