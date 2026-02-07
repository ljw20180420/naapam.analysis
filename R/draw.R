col_plot <- function(df, filename) {
  x_name <- colnames(df)[1]
  y_name <- colnames(df)[2]
  ggfig <- df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[x_name]],
        y = .data[[y_name]]
      )
    ) +
    ggplot2::geom_col(fill = "skyblue", color = "black") +
    ggplot2::theme_classic()

  invisible(ggplot2::ggsave(filename, ggfig))
}
