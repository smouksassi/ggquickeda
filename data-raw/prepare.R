sample_data <- read_csv("inst/shinyapp/data/sample_data.csv")

use_data(sample_data, overwrite = TRUE)


logistic_data <- read_csv("data-raw/logisticdata.csv")

usethis::use_data(logistic_data, overwrite = TRUE)

