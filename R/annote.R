#' Read the finally output of the naapam package
#'
#' The pip package naapam output a csv file with mutation information parsed by the bioconda package rearr. This function read this file and select inportant columns.
#'
#' @param filename the full path to the csv file
#' @return a tibble with selected columns
#' @export
#'
#' @examples
#' df <- read_treat(filename)
read_treat <- function(filename) {
  # coltypes = collumn types 数（i），name列为字符串（c），age列也为整数（i），date列为日期（d）。
  df <- readr::read_csv(
    file = filename,
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

#' Infer indel type
#'
#' Infer indel type (insertion, deletion, indel, wt) and extended indel type (wt, deletion, templated, random, temran, temdel, randel, full) from binary indel indicators.
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

#' Annote indel information
#'
#' Call all annotation methods on the input tibble with mutation information.
#'
#' @param df the tibble with mutation information
#' @return the tibble with annoted with detailed indel information
#' @export
#'
#' @examples
#' df <- annote_treat(df)
annote_treat <- function(df) {
  df <- annote_indicator(df)
  df <- annote_indel_type(df)
  df <- annote_indel_size(df)
}


#' Filter bad rows and select useful columns
#'
#' Filter the row with 1bp substitution. Select useful columns from annoted mutation tibble.
#'
#' @param df the tibble with annoted information
#' @return the filtered tibble with selected columns
#' @export
#'
#' @examples
#' df <- filter_treat(df)
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
