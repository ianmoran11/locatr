test_that("locate_data() works", {
  locate_data_test_temp <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-annotations") %>%
    locate_data(data_type == "numeric") %>%
    attr("data_cells")

  expect_identical(
    dplyr::select(locate_data_test_temp, -character_formatted),
    dplyr::select(locatr::locate_data_test, -character_formatted)
  )
})
