sample_data <- read_csv("inst/shinyapp/data/sample_data.csv")

devtools::use_data(sample_data, overwrite = TRUE)