{
  df <- read_treat(
    filename = "/home/ljw/sdb1/naapam/analyze/treat/annote/treat.csv"
  )
  # df <- df %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     mh = get_micro_homology(ref_end1, ref_start2, cut1, cut2, ref1, ref2)
  #   )
  # df <- annote_treat(df)

  df <- df %>%
    annote_indicator() %>%
    annote_indel_type() %>%
    annote_indel_size() %>%
    filter_treat()

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
