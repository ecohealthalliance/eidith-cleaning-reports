#!/usr/bin/env Rscript

source("R/functions.R")
# To limit the countries you are downloading data for, set the `country`
# variable, e.g., `country = "Thailand"`, or `country = c("Thailand", China")`
# `country = NULL`` will download data for all countries you have access to.
download_raw_p2_data(endpoints = eidith::p2_api_endpoints()[!eidith::p2_api_endpoints() %in% "Training"],
                     output_dir = "raw-eidith-data",
                     verbose = TRUE,
                     country = NULL)


