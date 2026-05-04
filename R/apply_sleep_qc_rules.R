#' Apply sleep QC rules
#'
#' Applies rule-based quality-control flags to a night-level sleep table.
#'
#' @param nights A night-level tibble. Expected columns include `subject_id`,
#'   `night_date`, `spt_duration_hours`, `sleep_duration_hours`,
#'   `waso_minutes`, `sleep_efficiency`, and `nonwear_fraction_spt`.
#' @param config Review configuration list.
#'
#' @return A tibble of night-level flags.
#' @export
apply_sleep_qc_rules <- function(nights, config = load_review_config(NULL)) {
  thresholds <- config$qc_thresholds

  required <- c("subject_id", "night_date")
  missing_required <- setdiff(required, names(nights))
  if (length(missing_required) > 0) {
    rlang::abort(glue::glue(
      "Night table is missing required columns: {paste(missing_required, collapse = ', ')}"
    ))
  }

  nights |>
    dplyr::mutate(
      flag_missing_main_sleep = is.na(.data$spt_duration_hours) | is.na(.data$sleep_duration_hours),
      flag_very_short_spt = !is.na(.data$spt_duration_hours) & .data$spt_duration_hours < thresholds$very_short_spt_hours,
      flag_very_long_spt = !is.na(.data$spt_duration_hours) & .data$spt_duration_hours > thresholds$very_long_spt_hours,
      flag_very_short_sleep = !is.na(.data$sleep_duration_hours) & .data$sleep_duration_hours < thresholds$very_short_sleep_hours,
      flag_very_long_sleep = !is.na(.data$sleep_duration_hours) & .data$sleep_duration_hours > thresholds$very_long_sleep_hours,
      flag_low_sleep_efficiency = !is.na(.data$sleep_efficiency) & .data$sleep_efficiency < thresholds$low_sleep_efficiency_pct,
      flag_high_waso = !is.na(.data$waso_minutes) & .data$waso_minutes > thresholds$high_waso_minutes,
      flag_high_nonwear_in_spt = !is.na(.data$nonwear_fraction_spt) & .data$nonwear_fraction_spt > thresholds$high_nonwear_fraction_spt
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      flag_reasons = paste(
        names(c(
          missing_main_sleep = .data$flag_missing_main_sleep,
          very_short_spt = .data$flag_very_short_spt,
          very_long_spt = .data$flag_very_long_spt,
          very_short_sleep = .data$flag_very_short_sleep,
          very_long_sleep = .data$flag_very_long_sleep,
          low_sleep_efficiency = .data$flag_low_sleep_efficiency,
          high_waso = .data$flag_high_waso,
          high_nonwear_in_spt = .data$flag_high_nonwear_in_spt
        ))[
          c(
            .data$flag_missing_main_sleep,
            .data$flag_very_short_spt,
            .data$flag_very_long_spt,
            .data$flag_very_short_sleep,
            .data$flag_very_long_sleep,
            .data$flag_low_sleep_efficiency,
            .data$flag_high_waso,
            .data$flag_high_nonwear_in_spt
          )
        ],
        collapse = ";"
      ),
      flagged = .data$flag_reasons != "",
      severity_rank = dplyr::case_when(
        .data$flag_missing_main_sleep | .data$flag_very_short_spt | .data$flag_very_long_spt ~ 3L,
        .data$flag_very_short_sleep | .data$flag_very_long_sleep | .data$flag_high_nonwear_in_spt ~ 2L,
        .data$flag_low_sleep_efficiency | .data$flag_high_waso ~ 1L,
        TRUE ~ 0L
      ),
      severity = dplyr::case_when(
        .data$severity_rank == 3L ~ "high",
        .data$severity_rank == 2L ~ "medium",
        .data$severity_rank == 1L ~ "low",
        TRUE ~ "none"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      .data$subject_id,
      .data$night_date,
      .data$flagged,
      .data$severity,
      .data$severity_rank,
      .data$flag_reasons,
      dplyr::starts_with("flag_")
    )
}
