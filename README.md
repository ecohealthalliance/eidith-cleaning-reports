[![CircleCI Build Status](https://circleci.com/gh/ecohealthalliance/eidith-cleaning-reports.svg?style=svg&circle-token=33f2118a7d105d8eddfe688e7cd626268dc6e4f1)](https://circleci.com/gh/ecohealthalliance/eidith-cleaning-reports)

# PREDICT data cleaning reports

This repository contains R code to generate reports to support cleaning EIDITH data.

Currently, reports for sites, animals, and specimens are implemented.

Run `devtools::install_deps()` to get all packages required to run this code.
These are defined in the DESCRIPTION file.

Run `00-get-eidith-data.R` to download EIDITH data into the (currently empty)
`raw-eidith-data/`
folder.  Data for all countries that you have access to will downloaded.
This requires that you set `EIDITH_USERNAME` and `EIDITH_PASSWORD`
environment variables.  See [`?eidith::ed_auth`](https://ecohealthalliance.github.io/eidith/reference/ed_auth.html) in the **eidith** R package for details.

Modify `make.R` to specify for which countries to generate reports. Then run
script to generate the reports.

For each country, there will be
an HTML report that summarizes unique values, empty values, and ranges for variables.  For each country there will also be
a Microsoft Excel workbook with cells flagged being unique, empty, or otherwise
requiring inspection to see if they are correct.  More details on these outputs
are in the first section of the HTML reports themselves, or you can find them
in the `report-template.Rmd` file.

`*.encrypted`, `dropbox_upload.R`, and files under the `.circleci/` directory are specific
to EcoHealth Alliance's automated pipeline infrastructure and require EHA
encryption keys to use.





