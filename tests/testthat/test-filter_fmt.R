test_that("filter_fmt() works", {

  locate_data_test_temp <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    filter_fmt(row > 2) %>% attributes()

  testthat::expect_true("formats" %in% names(locate_data_test_temp) )
})
