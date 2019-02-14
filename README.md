# PREDICT data cleaning reports

This repository contains R code to generate reports to support cleaning EIDITH data.

Currently, reports for sites, animals, and specimens are implemented.

To use, clone this repository and set it as your working directory. (You may open
the `eidith-cleaning-reports.Rproj` file if you use RStudio.)

Run `devtools::install_deps()` in R to get all packages required to run this code.
These are defined in the DESCRIPTION file.

Run the `00-get-eidith-data.R` script to download EIDITH data into the (currently empty)
`raw-eidith-data/`
folder.  Data for all countries that you have access to will downloaded.
This requires that you set `EIDITH_USERNAME` and `EIDITH_PASSWORD`
environment variables.  See [`?eidith::ed_auth`](https://ecohealthalliance.github.io/eidith/reference/ed_auth.html) in the **eidith** R package for details.

Modify the `make.R` script to specify for which countries to generate reports. Then run
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





