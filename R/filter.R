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
    )
}
