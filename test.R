{
  df <- read_treat(
    filename = "/home/ljw/sdb1/naapam/analyze/treat/annote/treat.csv"
  )
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      mh = get_micro_homology(ref_end1, ref_start2, cut1, cut2, ref1, ref2)
    )
  df <- annote_treat(df)
  df <- filter_treat(df)

  df %>%
    agg_y_over_x_and_avg_over_ref_id(
      x = templated_ins_size,
      y = freq_kim,
      filename = "figures/all.png"
    )
  for (stem_name in unique(indel_csv_final["stem"])$stem) {
    agg_y_over_x_and_avg_over_ref_id(
      df = df %>% dplyr::filter(stem == stem_name),
      x = "templated_ins_size",
      y = "freq_kim",
      filename = sprintf("figures/%s.png", stem_name)
    )
  }
}
