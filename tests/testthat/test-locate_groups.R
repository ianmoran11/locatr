test_that("locate_groups() works", {
  locate_groups_test_temp <-
    locatr_example("worked-examples.xlsx") %>%
    xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
    append_fmt(fmt_alignment_indent) %>%
    locate_data(data_type == "numeric") %>%
    locate_groups(
      direction = "W",
      .groupings = groupings(fmt_alignment_indent),
      .hook_if = hook_if(any(fmt_alignment_indent == 0))
    ) %>%
    locate(direction = "N", name = student) %>%
    dplyr::select(-character_formatted)

  expect_identical(
    locate_groups_test_temp,
    locatr::locate_groups_test
  )
})
