#' Apply sleep QC rules
#'
#' Applies rule-based quality-control flags to a night-level sleep table.
#'
#' @param nights A night-level tibble created by [combine_ggir_nights()].
#' @param sleep_candidates Optional candidate sleep-period table created by
#'   [detect_sleep_candidates_from_sib()].
#' @param config Review configuration list.
#'
#' @return A tibble of night-level flags.
#' @export
apply_sleep_qc_rules <- function(nights,
                                 sleep_candidates = NULL,
                                 config = load_review_config(NULL)) {
  thresholds <- config$qc_thresholds

  required <- c("subject_id", "night_date")
  missing_required <- setdiff(required, names(nights))
  if (length(missing_required) > 0) {
    rlang::abort(glue::glue(
      "Night table is missing required columns: {paste(missing_required, collapse = ', ')}"
    ))
  }

  if (nrow(nights) == 0) {
    return(empty_night_flags())
  }

  nights <- nights |>
    ensure_columns(c(
      "spt_duration_hours", "sleep_duration_hours", "waso_minutes",
      "sleep_efficiency", "nonwear_fraction_spt", "fraction_night_invalid",
      "duration_sib_wakinghours_atleast15min_hours", "number_sib_wakinghours",
      "ggir_sleep_onset_time", "ggir_wakeup_time", "acc_available"
    ))

  candidate_summary <- summarize_sleep_candidates_for_flags(sleep_candidates)

  nights |>
    dplyr::left_join(candidate_summary, by = c("subject_id", "night_date")) |>
    dplyr::mutate(
      candidate_count = tidyr::replace_na(.data$candidate_count, 0L),
      candidate_total_minutes = tidyr::replace_na(.data$candidate_total_minutes, 0),
      flag_missing_main_sleep = is.na(.data$spt_duration_hours) | is.na(.data$sleep_duration_hours),
      flag_very_short_spt = !is.na(.data$spt_duration_hours) & .data$spt_duration_hours < thresholds$very_short_spt_hours,
      flag_very_long_spt = !is.na(.data$spt_duration_hours) & .data$spt_duration_hours > thresholds$very_long_spt_hours,
      flag_very_short_sleep = !is.na(.data$sleep_duration_hours) & .data$sleep_duration_hours < thresholds$very_short_sleep_hours,
      flag_very_long_sleep = !is.na(.data$sleep_duration_hours) & .data$sleep_duration_hours > thresholds$very_long_sleep_hours,
      flag_low_sleep_efficiency = !is.na(.data$sleep_efficiency) & .data$sleep_efficiency < thresholds$low_sleep_efficiency_pct,
      flag_high_waso = !is.na(.data$waso_minutes) & .data$waso_minutes > thresholds$high_waso_minutes,
      flag_high_nonwear_in_spt = !is.na(.data$nonwear_fraction_spt) & .data$nonwear_fraction_spt > thresholds$high_nonwear_fraction_spt,
      flag_high_invalid_fraction = !is.na(.data$fraction_night_invalid) & .data$fraction_night_invalid > thresholds$high_invalid_fraction_night,
      flag_possible_secondary_sleep = .data$candidate_count > 0,
      flag_many_waking_sibs = !is.na(.data$duration_sib_wakinghours_atleast15min_hours) &
        .data$duration_sib_wakinghours_atleast15min_hours >= thresholds$high_waking_sib_hours_atleast15min,
      flag_acc_unavailable = !is.na(.data$acc_available) & .data$acc_available == 0
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
          high_nonwear_in_spt = .data$flag_high_nonwear_in_spt,
          high_invalid_fraction = .data$flag_high_invalid_fraction,
          possible_secondary_sleep = .data$flag_possible_secondary_sleep,
          many_waking_sibs = .data$flag_many_waking_sibs,
          acc_unavailable = .data$flag_acc_unavailable
        ))[
          c(
            .data$flag_missing_main_sleep,
            .data$flag_very_short_spt,
            .data$flag_very_long_spt,
            .data$flag_very_short_sleep,
            .data$flag_very_long_sleep,
            .data$flag_low_sleep_efficiency,
            .data$flag_high_waso,
            .data$flag_high_nonwear_in_spt,
            .data$flag_high_invalid_fraction,
            .data$flag_possible_secondary_sleep,
            .data$flag_many_waking_sibs,
            .data$flag_acc_unavailable
          )
        ],
        collapse = ";"
      ),
      flagged = .data$flag_reasons != "",
      severity_rank = dplyr::case_when(
        .data$flag_missing_main_sleep | .data$flag_very_short_spt | .data$flag_very_long_spt |
          .data$flag_high_invalid_fraction | .data$flag_acc_unavailable ~ 3L,
        .data$flag_very_short_sleep | .data$flag_very_long_sleep |
          .data$flag_high_nonwear_in_spt | .data$flag_possible_secondary_sleep ~ 2L,
        .data$flag_low_sleep_efficiency | .data$flag_high_waso | .data$flag_many_waking_sibs ~ 1L,
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
      .data$night,
      .data$night_date,
      .data$flagged,
      .data$severity,
      .data$severity_rank,
      .data$flag_reasons,
      .data$candidate_count,
      .data$candidate_total_minutes,
      dplyr::starts_with("flag_")
    )
}

summarize_sleep_candidates_for_flags <- function(sleep_candidates) {
  if (is.null(sleep_candidates) || nrow(sleep_candidates) == 0) {
    return(tibble::tibble(
      subject_id = character(),
      night_date = as.Date(character()),
      candidate_count = integer(),
      candidate_total_minutes = numeric()
    ))
  }

  sleep_candidates |>
    dplyr::filter(!is.na(.data$night_date)) |>
    dplyr::group_by(.data$subject_id, .data$night_date) |>
    dplyr::summarise(
      candidate_count = dplyr::n(),
      candidate_total_minutes = sum(.data$duration_minutes, na.rm = TRUE),
      .groups = "drop"
    )
}

empty_night_flags <- function() {
  tibble::tibble(
    subject_id = character(),
    night = integer(),
    night_date = as.Date(character()),
    flagged = logical(),
    severity = character(),
    severity_rank = integer(),
    flag_reasons = character(),
    candidate_count = integer(),
    candidate_total_minutes = numeric()
  )
}
