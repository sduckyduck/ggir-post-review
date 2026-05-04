#' Derive nap-inclusive sleep metrics
#'
#' Adds optional nap/secondary-sleep candidate duration to GGIR's original sleep
#' duration. This does not overwrite GGIR metrics. It creates separate derived
#' fields that can be used after manual review.
#'
#' @param nights Combined night summary created by [combine_ggir_nights()].
#' @param sleep_candidates Candidate table from [detect_sleep_candidates_from_sib()].
#' @param min_confidence Minimum candidate confidence required for automatic
#'   inclusion in the provisional nap-inclusive total.
#' @param include_types Candidate types to include.
#'
#' @return A tibble with original and nap-inclusive sleep metrics.
#' @export
derive_nap_inclusive_sleep <- function(nights,
                                       sleep_candidates,
                                       min_confidence = 0.60,
                                       include_types = c("nap_candidate", "secondary_sleep_candidate")) {
  if (nrow(nights) == 0) {
    return(empty_nap_inclusive_sleep())
  }

  if (is.null(sleep_candidates) || nrow(sleep_candidates) == 0) {
    nap_summary <- tibble::tibble(
      subject_id = character(),
      night = integer(),
      night_date = as.Date(character()),
      nap_candidate_count = integer(),
      nap_candidate_minutes = numeric(),
      nap_candidate_windows = character()
    )
  } else {
    nap_summary <- sleep_candidates |>
      dplyr::filter(
        .data$candidate_type %in% include_types,
        .data$confidence >= min_confidence,
        !is.na(.data$night_date)
      ) |>
      dplyr::group_by(.data$subject_id, .data$night, .data$night_date) |>
      dplyr::summarise(
        nap_candidate_count = dplyr::n(),
        nap_candidate_minutes = sum(.data$duration_minutes, na.rm = TRUE),
        nap_candidate_windows = paste(
          paste0(format(.data$start_time, "%Y-%m-%d %H:%M"), " to ", format(.data$end_time, "%Y-%m-%d %H:%M")),
          collapse = "; "
        ),
        .groups = "drop"
      )
  }

  nights |>
    dplyr::left_join(nap_summary, by = c("subject_id", "night", "night_date")) |>
    dplyr::mutate(
      nap_candidate_count = tidyr::replace_na(.data$nap_candidate_count, 0L),
      nap_candidate_minutes = tidyr::replace_na(.data$nap_candidate_minutes, 0),
      nap_candidate_hours = .data$nap_candidate_minutes / 60,
      sleep_duration_hours_original = .data$sleep_duration_hours,
      sleep_duration_minutes_original = .data$sleep_duration_minutes,
      sleep_duration_hours_nap_inclusive = .data$sleep_duration_hours_original + .data$nap_candidate_hours,
      sleep_duration_minutes_nap_inclusive = .data$sleep_duration_minutes_original + .data$nap_candidate_minutes,
      nap_inclusive_status = dplyr::case_when(
        .data$nap_candidate_count == 0 ~ "no_candidate_nap",
        .data$nap_candidate_count > 0 ~ "provisional_candidate_added_needs_review",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::select(
      .data$subject_id,
      .data$night,
      .data$night_date,
      .data$sleep_duration_hours_original,
      .data$sleep_duration_minutes_original,
      .data$nap_candidate_count,
      .data$nap_candidate_hours,
      .data$nap_candidate_minutes,
      .data$sleep_duration_hours_nap_inclusive,
      .data$sleep_duration_minutes_nap_inclusive,
      .data$nap_candidate_windows,
      .data$nap_inclusive_status,
      dplyr::everything()
    )
}

empty_nap_inclusive_sleep <- function() {
  tibble::tibble(
    subject_id = character(),
    night = integer(),
    night_date = as.Date(character()),
    sleep_duration_hours_original = numeric(),
    sleep_duration_minutes_original = numeric(),
    nap_candidate_count = integer(),
    nap_candidate_hours = numeric(),
    nap_candidate_minutes = numeric(),
    sleep_duration_hours_nap_inclusive = numeric(),
    sleep_duration_minutes_nap_inclusive = numeric(),
    nap_candidate_windows = character(),
    nap_inclusive_status = character()
  )
}
