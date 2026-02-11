correct_kim <- function(df_treat, df_control, names) {
  treat_name_types <- df_treat %>%
    dplyr::select({{ names }}) %>%
    purrr::map_chr(class)
  if (!(treat_name_types == "factor" %>% all())) {
    stop("name type must be factor")
  }
  control_name_types <- df_control %>%
    dplyr::select({{ names }}) %>%
    purrr::map_chr(class)
  if (!(control_name_types == "factor" %>% all())) {
    stop("name type must be factor")
  }

  dplyr::bind_rows(
    df_treat,
    df_control,
    .id = "tc"
  ) %>%
    tidyR::pivot_wider(
      id_cols = c(tc, sgRNA),
      names_from = {{ names }},
      values_from = count,
      values_fill = 0,
      values_fn = sum
    ) %>%
    dplyr::mutate(
      count_tot = sum(dplyr::c_across(!c(tc, sgRNA)), na.rm = TRUE)
    )
}
