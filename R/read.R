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
    col_types = "iiiicidcllciciiiddddicccc",
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
      tem_indicator,
      legal,
      freq_mut_kim,
      freq_kim,
      barcode_id,
      barcode,
      sgRNA,
      ref1,
      ref2,
    ) %>%
    dplyr::filter(legal) %>%
    dplyr::select(-legal)
}
