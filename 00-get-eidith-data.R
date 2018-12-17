#!/usr/bin/env Rscript

source("R/functions.R")
require(future)
plan(multiprocess(workers = 8))
download_raw_p2_data(endpoints = eidith::p2_api_endpoints(),
                     output_dir = "raw-eidith-data",
                     verbose = FALSE)
