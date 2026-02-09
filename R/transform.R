#' Pivot with sgRNA as row
#'
#' Pivot with sgRNA as row, name as column and value as value. Fill missing value as 0. Repeated values are summed.
#'
#' @param df the tibble with indel information
#' @param name the tidyselect column as pivot column
#' @param value the tidyselect column as pivot value
#' @return the pivot wide table
#' @export
#'
#' @examples
#' df <- sgRNA_pivot(df, indel_type, count)
sgRNA_pivot <- function(df, name, value) {
  df <- df %>%
    tidyr::pivot_wider(
      id_cols = sgRNA,
      names_from = {{ name }},
      values_from = {{ value }},
      values_fill = 0,
      values_fn = sum
    )
}

#' Summarise the mean along sgRNA
#'
#' Summarise the mean of value along sgRNA with name as group. NA is removed. sgRNA_Pivot is applied before summarise to so that missing values are fill with 0 before taking the mean along sgRNA.
#'
#' @param df the tibble with indel information
#' @param name the column as group
#' @param value the value to calculate mean
#' @return the tibble with two columns: name and value
#' @export
#'
#' @examples
#' df <- sgRNA_summarise(df, indel_type, count)
sgRNA_summarise <- function(df, name, value) {
  name_type <- df %>%
    dplyr::select({{ name }}) %>%
    purrr::map_chr(class) %>%
    .[1]
  df <- df %>%
    sgRNA_pivot(
      name = {{ name }},
      value = {{ value }}
    ) %>%
    dplyr::summarise(dplyr::across(!c(sgRNA), ~ mean(.x, na.rm = TRUE))) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = rlang::as_name(rlang::ensym(name)),
      values_to = rlang::as_name(rlang::ensym(value))
    )

  if (name_type == "numeric" || name_type == "integer") {
    df <- df %>%
      dplyr::mutate(
        "{{name}}" := as.numeric({{ name }})
      )
  } else if (name_type == "factor") {
    df <- df %>%
      dplyr::mutate(
        "{{name}}" := as.factor({{ name }})
      )
  }

  return(df)
}
