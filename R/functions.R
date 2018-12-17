# data-cleaning functions here
download_raw_p2_data <- function(endpoints = p2_api_endpoints(),
                                 output_dir = h("raw-eidith-data"),
                                 verbose = FALSE) {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  for (endpoint in endpoints) {
    future({
      dat <- eidith::ed2_get(endpoint, postprocess = FALSE, verbose = verbose)
      readr::write_csv(dat, file.path(output_dir, paste0(endpoint, ".tsv.gz")))
    })
  }
}

