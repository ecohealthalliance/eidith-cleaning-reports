#!/usr/bin/env Rscript
h <- here::here
source("R/functions.R")


# First, clone this repository and open the RStudio project:
# https://github.com/ecohealthalliance/eidith-cleaning-reports
#
# If you don't have them the following will install all packages need for this
# devtools::install_deps(dependencies=TRUE)

# Set the countries of interest
countries <- c(
  "Malaysia, Peninsular",
  "Malaysia, Sabah"
)

# Run the following with your Malaysia-enabled EIDITH credentials in your
# .Renviron file. (See ?eidith::ed_auth) It will download data from all
# countries youhave access to.
download_raw_p2_data(endpoints = eidith::p2_api_endpoints(),
                     output_dir = "raw-eidith-data",
                     verbose = TRUE)

# Now run this to generate the reports and excel files, which will be in the
# 'outputs' directory
for (country in countries) {
  rmarkdown::render("report-template.Rmd",
                    output_file = paste0(country, "-eidith-cleaning-report.html"),
                    output_dir = h("outputs"),
                    params = list(country = country))
}
