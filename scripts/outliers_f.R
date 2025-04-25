find_outliers_group <- function(data, group_col, value_col) {
  top <- data %>%
    group_by(.data[[group_col]]) %>%
    filter(
      .data[[value_col]] > quantile(.data[[value_col]], 0.75, na.rm = TRUE) + 1.5 * IQR(.data[[value_col]], na.rm = TRUE)
    ) %>%
    ungroup()
  bottom <- data %>%
    group_by(.data[[group_col]]) %>%
    filter(
      .data[[value_col]] < quantile(.data[[value_col]], 0.25, na.rm = TRUE) - 1.5 * IQR(.data[[value_col]], na.rm = TRUE)
    ) %>%
    ungroup()
  return(list(top=top, bottom=bottom))
}

find_outliers <- function(data, group_col, value_col) {
  outliers <- data %>% filter(.data[[value_col]] < quantile(.data[[value_col]], 0.25, na.rm = TRUE) - 1.5 * IQR(.data[[value_col]], na.rm = TRUE) | .data[[value_col]] > quantile(.data[[value_col]], 0.75, na.rm = TRUE) + 1.5 * IQR(.data[[value_col]], na.rm = TRUE))
  
  return(outliers)
}