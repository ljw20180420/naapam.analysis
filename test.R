{
  df_treat <- read_treat(
    filename = "test/treat.csv"
  )
  df_control <- read_control(
    filename = "test/control.csv"
  )

  df_treat <- annote_all(df_treat)
  df_control <- annote_all(df_control)

  df %>%
    dplyr::filter(tem_indicator) %>%
    sgRNA_summarise(
      name = templated_ins_size,
      value = freq_kim
    ) %>%
    col_plot("figures/templated_ins_size_filter.png")

  df %>%
    sgRNA_summarise(
      name = templated_ins_size,
      value = freq_kim
    ) %>%
    col_plot("figures/templated_ins_size.png")

  df %>%
    sgRNA_summarise(
      name = del_size,
      value = freq_kim
    ) %>%
    col_plot("figures/del_size.png")

  df %>%
    sgRNA_summarise(
      name = random_ins_size,
      value = freq_kim
    ) %>%
    col_plot("figures/random_ins_size.png")

  # for (name in c("templated_ins_size", "del_size", "random_ins_size")) {
  #   df %>%
  #     sgRNA_summarise(
  #       name = rlang::as_name(name),
  #       value = freq_kim
  #     ) %>%
  #     col_plot(sprintf("figures/%s.png", name))
  # }
}
