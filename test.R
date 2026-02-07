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
    filter(tem_indicator) %>%
    sgRNA_summarise(
      name = templated_ins_size,
      value = freq_kim
    ) %>%
    col_plot("figures/templated_ins_size_filter.png")

  for (name in c("templated_ins_size", "del_size", "random_ins_size")) {
    df %>%
      sgRNA_summarise(
        name = rlang::as.name(name),
        value = freq_kim
      ) %>%
      col_plot(sprintf("figures/%s.png", name))
  }
}
