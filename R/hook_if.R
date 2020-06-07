#' Supply an expression to identify which header groups are hooked.
#'
#' @description
#' This function is used with the `.hook_if` or `.hook_if_rev`
#'  arguments in the `locate_groups` function.
#' This expression must evaluate to a single value for each header group.
#' For example, see below that any(fmt_alignment_indent == 0) is used rather that
#' fmt_alignment_indent == 0.
#'
#' It passes an expression to `dplyr::summarise` that identifies which header groups are hooked
#'  - for example swiched from N to NNW.
#' See the the `locate_groups` documentation for more information and an example.
#'
#'
#' @param ...  expression applied to a  identifies which header groups are hooked
#'
#' @export
#' @examples
#' \dontrun{
#'
#'
#' library(tidyverse)
#'
#' # Read in tidyxl data frame
#' xl_df <-
#'   locatr_example("worked-examples.xlsx") %>%
#'   xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
#'   append_fmt(fmt_alignment_indent)
#'
#' # Add location annotations
#' xl_df <-
#'   xl_df %>%
#'   locate_data(data_type == "numeric") %>%
#'   locate_groups(
#'     direction = "W",
#'     .groupings = groupings(fmt_alignment_indent),
#'     .hook_if = hook_if(any(fmt_alignment_indent == 0))
#'   ) %>%
#'   locate(direction = "N", name = student)
#'
#' # Use `migrate` to reshape the data frame such that each data cells has its own row and each
#'  header variable has its own column.
#' xl_df %>% migrate()
#' }

hook_if <- function(...) {
  rlang::quos(...)
}
