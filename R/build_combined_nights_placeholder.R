build_combined_nights_placeholder <- function(manifest) {
  csv_files <- manifest |>
    dplyr::filter(.data$file_type == "csv")

  if (nrow(csv_files) == 0) {
    return(tibble::tibble(
      subject_id = character(),
      night_date = as.Date(character()),
      spt_duration_hours = numeric(),
      sleep_duration_hours = numeric(),
      waso_minutes = numeric(),
      sleep_efficiency = numeric(),
      nonwear_fraction_spt = numeric(),
      source_file = character()
    ))
  }

  # Placeholder until real GGIR column mapping is added.
  # This keeps the workflow runnable while we implement parsers for specific GGIR outputs.
  csv_files |>
    dplyr::transmute(
      subject_id = dplyr::coalesce(.data$subject_id_guess, .data$file_name),
      night_date = as.Date(NA),
      spt_duration_hours = NA_real_,
      sleep_duration_hours = NA_real_,
      waso_minutes = NA_real_,
      sleep_efficiency = NA_real_,
      nonwear_fraction_spt = NA_real_,
      source_file = .data$file_path
    )
}
