
#' Plots tidyxl data frame in a grid layout
#' @description
#' This function plots the excel sheet, highlighting the relationship between headers and table
#'  values.
#' @param sheet tidyxl data frame
#' @param text text variable to be represented in values plot.
#' @param interactive TRUE produces an interactive plotly visualisation.
#' @export
#' @examples
#' \dontrun{
#' library(tidyverse)
#'
#' # Create annotated tidyxl data frame
#' xl_df <-
#'   locatr_example("worked-examples.xlsx") %>%
#'   xlsx_cells_fmt(sheets = "pivot-hierarchy") %>%
#'   append_fmt(fmt_alignment_indent) %>%
#'   locate_data(data_type == "numeric") %>%
#'   locate_groups(
#'     direction = "W",
#'     .groupings = groupings(fmt_alignment_indent),
#'     .hook_if = hook_if(any(fmt_alignment_indent == 0))
#'   ) %>%
#'   locate(direction = "N", name = student)
#'
#' # Plot direction annotations
#' xl_df %>% plot_cells()
#' }
plot_cells <- function(sheet, text = values, interactive = FALSE) {
  if (!is.null(attr(sheet, "data_cells"))) {
    data_cells <-
      sheet %>%
      attr("data_cells") %>%
      dplyr::mutate(.direction = "\U2610", .header_label = "data")

    sheet <- dplyr::bind_rows(sheet, data_cells)
  } else {

    # Add annotation variables if missing
    added_var_list <- list(sheet, ".header_label", ".direction", ".value")
    sheet <- added_var_list %>% purrr::reduce(add_variable_if_missing)

    sheet <-
      sheet %>%
      dplyr::mutate(.header_label = "None")
  }


  # show_col(hue_pal()(4))
  # hue_pal()(5) %>% rep(10) %>% dput()
  # color_pal <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3", "#F8766D",
  #                "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500",
  #                "#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500", "#00BF7D",
  #                "#00B0F6", "#E76BF3", "#F8766D", "#A3A500", "#00BF7D", "#00B0F6",
  #                "#E76BF3", "#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3",
  #                "#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3", "#F8766D",
  #                "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500",
  #                "#00BF7D", "#00B0F6", "#E76BF3", "#F8766D", "#A3A500", "#00BF7D",
  #                "#00B0F6", "#E76BF3")

  # rgb(188/255,188/255,188/255,1)
  # color_na <- "#BCBCBCFF"


  if (interactive == FALSE) {

    sheet_01 <-
        sheet %>%
      dplyr::left_join(direction_plot_noninteractive, by = ".direction") %>%
      dplyr::mutate(values = dplyr::coalesce(
        as.character(numeric), as.character(character),
        as.character(logical), as.character(date)))

    dplyr::bind_rows(
      dplyr::mutate(sheet_01, .arrow = NA, set = "Cell values"),
      dplyr::mutate(sheet_01, {{ text }} := NA, set = "Directions")) %>%
      ggplot2::ggplot(ggplot2::aes(x = col, y = row, fill = .header_label)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(
        ggplot2::aes(label = stringr::str_sub({{ text }}, 1, 6))) +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(.rotate == 0, .arrow , ""))) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(.rotate %in% 90, .arrow, "")), angle = 90) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(.rotate %in% -90, .arrow, "")), angle = -90) +
      ggplot2::facet_wrap(~set, scales = "free") +
      ggplot2::labs(y = "Row", x = "Column") +
      ggplot2::scale_y_reverse() +
      # ggplot2::scale_fill_manual(values = color_pal,na.value=color_na) +
      NULL
  } else {
    sheet_01 <-
      sheet %>%
      dplyr::left_join(direction_plot_interactive, by = ".direction") %>%
      dplyr::mutate(values = dplyr::coalesce(
        as.character(numeric), as.character(character),
        as.character(logical), as.character(date)
      ))

    plot_object <-
      dplyr::bind_rows(
        dplyr::mutate(sheet_01, .arrow = NA, set = "Cell values"),
        dplyr::mutate(sheet_01, {{ text }} := NA, set = "Directions")
      ) %>%
      ggplot2::ggplot(ggplot2::aes(x = col, y = row, fill = .header_label)) + ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = stringr::str_sub({{ text }}, 1, 5)) %>% ifelse(is.na(.),"")) +
      ggplot2::geom_text(ggplot2::aes(label = .arrow %>% ifelse(is.na(.),""))) +
      ggplot2::facet_wrap(~set, scales = "free") +
      ggplot2::labs(y = "Row", x = "Column") +
      ggplot2::scale_y_reverse() +
      # ggplot2::scale_fill_manual(values = color_pal,na.value=color_na) +
      NULL

    plotly::ggplotly(plot_object)
  }
}

