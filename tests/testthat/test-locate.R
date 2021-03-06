test_that("locate() works", {
  locate_test_temp <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    locate_data(data_type == "numeric") %>%
    locate(direction = "WNW", name = subject_type) %>%
    locate(direction = "W", name = subject) %>%
    locate(direction = "NNW", name = gender) %>%
    locate(direction = "N", name = name)

  testthat::expect_identical(locatr::locate_test,   locate_test_temp)
})



