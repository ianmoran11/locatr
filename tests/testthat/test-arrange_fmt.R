test_that("arrange_fmt() works", {

  locate_data_test_temp <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    arrange_fmt(row) %>% attributes()

  testthat::expect_true("formats" %in% names(locate_data_test_temp) )
})
