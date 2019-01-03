#!/usr/bin/env Rscript
h <- here::here

countries <- c(
  "Bangladesh",
  "China",
  "Egypt",
  "India",
  "Indonesia",
  "Ivory Coast",
  "Jordan",
  "Liberia",
  "Republic of Congo",
  "South Sudan",
  "Thailand"
)

for (country in countries) {
  rmarkdown::render("report-template.Rmd",
                    output_file = paste0(country, "-eidith-cleaning-report.html"),
                    output_dir = h("outputs"),
                    params = list(country = country))
}
