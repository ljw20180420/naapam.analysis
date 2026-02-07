col_plot <- function(df, filename) {
  ggfig <- df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[1]],
        y = .data[[2]]
      )
    ) +
    ggplot2::geom_col(fill = "skyblue", color = "black") +
    ggplot2::theme_classic()

  invisible(ggplot2::ggsave(filename, ggfig))
}
