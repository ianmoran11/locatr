library(tidyverse)
library(locatr)

  locate_data_test <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    locate_data(data_type == "numeric") %>%
    attr("data_cells")

usethis::use_data(locate_data_test, overwrite = TRUE)



