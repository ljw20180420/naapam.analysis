read_treat <- function(filename) {
  # coltypes = collumn types 数（i），name列为字符串（c），age列也为整数（i），date列为日期（d）。
  df <- readr::read_csv(
    "/home/ljw/sdb1/naapam/analyze/treat/annote/treat.csv",
    col_types = "iiiicidclciciiiddddicc",
    na = c("NA")
  ) %>%
    dplyr::select(
      ref_id,
      cut1,
      cut2,
      ref_end1,
      random_insertion,
      ref_start2,
      count,
      stem,
      legal,
      freq_mut_kim,
      freq_kim,
      barcode_id,
      barcode,
      sgRNA
    ) %>%
    dplyr::filter(legal) %>%
    dplyr::select(-legal)
}

annote_indicator <- function(df) {
  df <- df %>%
    dplyr::mutate(
      up_tem = ref_end1 > cut1,
      ran_ins = random_insertion != "",
      down_tem = ref_start2 < cut2,
      up_del = ref_end1 < cut1,
      down_del = ref_start2 > cut2,
      del = up_del | down_del,
      tem_ins = up_tem | down_tem,
      ins = tem_ins | ran_ins,
    )
}

annote_indel_type <- function(df) {
  df <- df %>%
    dplyr::mutate(
      indel_type = factor(
        dplyr::case_when(
          ins & !del ~ "insertion",
          del & !ins ~ "deletion",
          ins & del ~ "indel",
          .default = "wt"
        ),
        levels = c("insertion", "deletion", "indel", "wt")
      ),
      indel_type_ex = factor(
        dplyr::case_when(
          tem_ins & del & ran_ins ~ "full",
          tem_ins & del & !ran_ins ~ "temdel",
          tem_ins & !del & ran_ins ~ "temran",
          !tem_ins & del & ran_ins ~ "randel",
          tem_ins & !del & !ran_ins ~ "templated",
          !tem_ins & del & !ran_ins ~ "deletion",
          !tem_ins & !del & ran_ins ~ "random",
          .default = "wt"
        ),
        levels = c(
          "wt",
          "deletion",
          "templated",
          "random",
          "temran",
          "temdel",
          "randel",
          "full"
        )
      )
    )
}


annote_indel_size <- function(df) {
  df <- df %>%
    dplyr::mutate(
      up_size = ref_end1 - cut1,
      down_size = cut2 - ref_start2,
      del_size = dplyr::case_when(
        up_del & down_del ~ up_size + down_size,
        up_del & !down_del ~ up_size,
        !up_del & down_del ~ down_size,
        .default = 0
      ),
      templated_ins_size = dplyr::case_when(
        down_tem ~ down_size,
        .default = 0
      ),
      random_ins_size = stringr::str_length(random_insertion),
      ins_size = templated_ins_size + random_ins_size
    )
}

annote_treat <- function(df) {
  df <- annote_indicator(df)
  df <- annote_indel_type(df)
  df <- annote_indel_size(df)
}

filter_treat <- function(df) {
  df <- df %>%
    dplyr::filter(
      !(indel_type == "indel" & del_size == -1 & random_ins_size == 1)
    ) %>%
    select(
      ref_id,
      cut1,
      cut2,
      ref_end1,
      random_insertion,
      ref_start2,
      count,
      stem,
      freq_mut_kim,
      freq_kim,
      barcode_id,
      barcode,
      sgRNA,
      indel_type,
      indel_type_ex,
      up_size,
      down_size,
      del_size,
      templated_ins_size,
      random_ins_size,
      ins_size
    )
}
