agg_y_over_x_and_avg_over_ref_id <- function(df, x, y, filename) {
  df <- df %>%
    tidyr::pivot_wider(
      id_cols = ref_id,
      names_from = dplyr::all_of(x),
      values_from = dplyr::all_of(y),
      values_fill = 0,
      values_fn = sum
    ) %>%
    dplyr::summarise(dplyr::across(!c(ref_id), ~ mean(.x, na.rm = TRUE))) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = all_of(x),
      values_to = all_of(y)
    )
  df[[x]] <- as.integer(df[[x]])
  ggfig <- df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]]
    )) +
    ggplot2::geom_col(fill = "skyblue", color = "black") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = x,
      y = y,
    )

  ggplot2::ggsave(filename, ggfig)
}
