#' Annote binary indicator of indels
#'
#' Annote whether up/down-stream indels actually happen.
#'
#' @param df the tibble containing mutant information
#' @return the tibble with annoted indicators
#' @export
#'
#' @examples
#' df <- annote_indicator(df)
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

#' Annote indel type
#'
#' Annote indel type (insertion, deletion, indel, wt).
#' Annote extended indel type (wt, deletion, templated, random, temran, temdel, randel, full).
#'
#' @param df the tibble with binary indel inidicator information
#' @return the tibble with annoted indel type information
#' @export
#'
#' @examples
#' df <- annote_indel_type(df)
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
          "full",
          "temdel",
          "temran",
          "randel",
          "templated",
          "deletion",
          "random",
          "wt"
        )
      )
    )
}

#' Annote indel size
#'
#' Annote indel size to the input tibble (including up/down-stream indel size, deletion size, templated insertion size, random insertion size, insertion size).
#'
#' @param df the tibble with binary indel indicator information
#' @return the tibble with annoted indel size
#' @export
#'
#' @examples
#' df <- annote_indel_size(df)
annote_indel_size <- function(df) {
  df <- df %>%
    dplyr::mutate(
      up_size = ref_end1 - cut1,
      down_size = cut2 - ref_start2,
      up_del_size = ifelse(up_del, up_size, 0),
      down_del_size = ifelse(down_del, down_size, 0),
      del_size = up_del_size + down_del_size,
      templated_ins_size = dplyr::case_when(
        down_tem ~ down_size,
        .default = 0
      ),
      random_ins_size = stringr::str_length(random_insertion),
      ins_size = templated_ins_size + random_ins_size
    )
}

#' Annote deletion type
#'
#' Annote deletion type (MMEJ, unilateral_top, unilateral_bottom, medial, no_deletion).
#' Annote extended deletion type (MMEJ_noran, unilateral_top_noran, unilateral_bottom_noran, medial_noran, MMEJ_ran, unilateral_top_ran, unilateral_bottom_ran, medial_ran, no_deletion).
#'
#' @param df the tibble with binary indel inidicator information and micro-homology sequence
#' @param mh_thres the maximal mh length considered as non-micro-homology
#' @return the tibble with annoted deletion type information
#' @export
#'
#' @examples
#' df <- annote_indel_type(df)
annote_deletion_type <- function(df, mh_thres) {
  df <- df %>%
    dplyr::mutate(
      mh_len = stringr::str_length(mh),
      "deletion_type_{mh_thres}" := factor(
        dplyr::case_when(
          del & mh_len > mh_thres ~ "MMEJ",
          del &
            mh_len <= mh_thres &
            !up_del &
            down_del ~ "unilateral_top",
          del &
            mh_len <= mh_thres &
            up_del &
            !down_del ~ "unilateral_bottom",
          del & mh_len <= mh_thres & up_del & down_del ~ "medial",
          !del & ins ~ "insertion",
          .default = "wt"
        ),
        levels = c(
          "MMEJ",
          "unilateral_top",
          "unilateral_bottom",
          "medial",
          "insertion",
          "wt"
        )
      ),
      "deletion_type_ex_{mh_thres}" := factor(
        dplyr::case_when(
          del & !ran_ins & mh_len > mh_thres ~ "MMEJ_noran",
          del &
            !ran_ins &
            mh_len <= mh_thres &
            !up_del &
            down_del ~ "unilateral_top_noran",
          del &
            !ran_ins &
            mh_len <= mh_thres &
            up_del &
            !down_del ~ "unilateral_bottom_noran",
          del &
            !ran_ins &
            mh_len <= mh_thres &
            up_del &
            down_del ~ "medial_noran",
          del & ran_ins & mh_len > mh_thres ~ "MMEJ_ran",
          del &
            ran_ins &
            mh_len <= mh_thres &
            !up_del &
            down_del ~ "unilateral_top_ran",
          del &
            ran_ins &
            mh_len <= mh_thres &
            up_del &
            !down_del ~ "unilateral_bottom_ran",
          del & ran_ins & mh_len <= mh_thres & up_del & down_del ~ "medial_ran",
          !del & ins ~ "insertion",
          .default = "wt"
        ),
        levels = c(
          "MMEJ_noran",
          "unilateral_top_noran",
          "unilateral_bottom_noran",
          "medial_noran",
          "MMEJ_ran",
          "unilateral_top_ran",
          "unilateral_bottom_ran",
          "medial_ran",
          "insertion",
          "wt"
        )
      )
    )
}


#' Annote indel information
#'
#' Call all annotation methods on the input tibble with mutation information.
#'
#' @param df the tibble with mutation information
#' @return the tibble with annoted with detailed indel information
#' @export
#'
#' @examples
#' df <- annote_all(df)
annote_all <- function(df) {
  df <- annote_indicator(df)
  df <- annote_indel_type(df)
  df <- annote_indel_size(df)
  df <- annote_deletion_type(df, mh_thres = 0)
  df <- annote_deletion_type(df, mh_thres = 1)
}
