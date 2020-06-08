library(tidyverse)
library(locatr)

locate_groups_test <-
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

usethis::use_data(locate_groups_test, overwrite = TRUE)


