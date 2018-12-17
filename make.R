countries <- c("Thailand", "Indonesia")

for (country in countries) {
  rmarkdown::render("report-template.Rmd",
                    output_file = paste0("eidith-cleaning-report-",
                                         country, "-", Sys.Date),
                    output_dir = h("reports"),
                    params = list(country = country))
}
