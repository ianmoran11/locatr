testthat::test_that("fmt_* functions work", {

  library(stringr)
  library(rlang)
  library(dplyr)
  library(purrr)

function_list <- lsf.str("package:locatr")

fmt_functions <- function_list %>% .[str_detect(.,"^fmt_") & !str_detect(.,"single")] %>% sort()

tidyxl_df <-
  locatr_example("worked-examples.xlsx") %>% xlsx_cells_fmt(sheets = "pivot-annotations")


fmt_functions_test <-
  map(fmt_functions[-c(14,15,19,20)],
      ~invoke(.x,format_id_vec =  tidyxl_df$local_format_id,
              sheet_formats =  attr(tidyxl_df, "formats")) %>%
        as.character)


testthat::expect_identical(fmt_functions_test, locatr::fmt_functions_test)

})

