{
  df <- read_treat(
    filename = "/home/ljw/sdb1/naapam/analyze/treat/annote/treat.csv"
  )
  df <- annote_treat(df)
  df <- filter_treat(df)

  agg_y_over_x_and_avg_over_ref_id(
    df = df,
    x = "templated_ins_size",
    y = "freq_kim",
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
