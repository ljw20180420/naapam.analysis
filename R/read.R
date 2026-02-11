#' Read the finally treat output of the naapam package
#'
#' The pip package naapam output a csv file of treat with mutation information parsed by the bioconda package rearr. The columns are ref_id (i), cut1 (i), cut2 (i), ref_end1 (i), random_insertion (c), ref_start2 (i), count(d), stem (c), tem_indicator (l), legal (l), chip (c), time (i), wt (c), count_ctl (i), count_wt_ctl (i), count_tot_ctl (i), count_kim (d), count_tot_kim (d), freq_mut_kim (d), barcode_id (i), barcode (c), sgRNA (c), ref1 (c), ref2 (c), mh (c). This function read this file and select inportant columns.
#'
#' @param filename the full path to the csv file of treat
#' @return a tibble with selected columns
#' @export
#'
#' @examples
#' df_treat <- read_treat(filename)
read_treat <- function(filename) {
  df_treat <- readr::read_csv(
    file = filename,
    col_types = "iiiicidcllciciiidddiccccc",
    na = c("NA")
  ) %>%
    dplyr::select(
      cut1,
      cut2,
      ref_end1,
      random_insertion,
      ref_start2,
      count,
      stem,
      tem_indicator,
      legal,
      chip,
      time,
      wt,
      barcode_id,
      barcode,
      sgRNA,
      ref1,
      ref2,
      mh
    )
}

#' Read the finally control output of the naapam package
#'
#' The pip package naapam output a csv file of control with mutation information parsed by the bioconda package rearr. The columns are ref_id (i), cut1 (i), cut2 (i), ref_end1 (i), random_insertion (c), ref_start2 (i), count (d), stem (c), chip (c), time (i), wt (c), barcode_id (i), barcode (c), sgRNA (c), ref1 (c), ref2 (c), mh (c). This function read this file and select inportant columns.
#'
#' @param filename the full path to the csv file of control
#' @return a tibble with selected columns
#' @export
#'
#' @examples
#' df_control <- read_control(filename)
read_control <- function(filename) {
  df_control <- readr::read_csv(
    file = filename,
    col_types = "iiiicidcciciccccc",
    na = c("NA")
  ) %>%
    dplyr::select(
      cut1,
      cut2,
      ref_end1,
      random_insertion,
      ref_start2,
      count,
      stem,
      chip,
      time,
      wt,
      barcode_id,
      barcode,
      sgRNA,
    )
}
