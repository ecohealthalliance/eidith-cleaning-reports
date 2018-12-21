#!/usr/bin/env Rscript
h <- here::here

countries <- sort(
  unique(
    readr::read_tsv(h("raw-eidith-data", "Event.tsv.gz"),
                    col_types = readr::cols_only(Country = readr::col_character()))$Country
  )
)

for (country in countries) {
  rmarkdown::render("report-template.Rmd",
                    output_file = paste0(country, "-eidith-cleaning-report.html"),
                    output_dir = h("outputs"),
                    params = list(country = country))
}
