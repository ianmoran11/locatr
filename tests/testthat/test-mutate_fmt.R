test_that("mutate_fmt() works", {

  locate_data_test_temp <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    mutate_fmt(test_var = 2) %>% attributes()

  testthat::expect_true("formats" %in% names(locate_data_test_temp) )
})
