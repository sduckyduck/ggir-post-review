#' Detect candidate secondary sleep periods from GGIR SIB reports
#'
#' Uses `meta/ms5.outraw/sib.reports/sib_report_*.csv` files to identify
#' sustained inactivity bouts that occur outside GGIR's main sleep period and may
#' represent naps or secondary sleep periods. These are candidates for manual
#' review, not final adjudicated sleep.
#'
#' @param manifest A run manifest created by [discover_ggir_outputs()].
#' @param nights Combined night summary created by [combine_ggir_nights()].
#' @param config Review configuration list.
#'
#' @return A tibble of candidate sleep periods.
#' @export
detect_sleep_candidates_from_sib <- function(manifest, nights, config = load_review_config(NULL)) {
  sib_files <- manifest |>
    dplyr::filter(.data$ggir_output_type == "sib_report")

  if (nrow(sib_files) == 0 || nrow(nights) == 0) {
    return(empty_sleep_candidates())
  }

  min_duration <- config$candidate_sleep$min_duration_minutes %||% 30
  merge_gap <- config$candidate_sleep$merge_gap_minutes %||% 20
  nap_start_hour <- config$candidate_sleep$nap_start_hour %||% 9
  nap_end_hour <- config$candidate_sleep$nap_end_hour %||% 20

  sibs <- purrr::map_dfr(sib_files$file_path, function(path) {
    readr::read_csv(path, show_col_types = FALSE) |>
      ensure_columns(c("ID", "type", "start", "end", "duration", "mean_acc_1min_before", "mean_acc_1min_after")) |>
      dplyr::mutate(source_file = path)
  }) |>
    dplyr::filter(.data$type == "sib") |>
    dplyr::mutate(
      sib_id = dplyr::row_number(),
      subject_id = as.character(.data$ID),
      start_time = lubridate::ymd_hms(.data$start, quiet = TRUE),
      end_time = lubridate::ymd_hms(.data$end, quiet = TRUE),
      duration_minutes = suppressWarnings(as.numeric(.data$duration)),
      mean_acc_1min_before = suppressWarnings(as.numeric(.data$mean_acc_1min_before)),
      mean_acc_1min_after = suppressWarnings(as.numeric(.data$mean_acc_1min_after))
    ) |>
    dplyr::filter(!is.na(.data$start_time), !is.na(.data$end_time), .data$duration_minutes >= min_duration)

  if (nrow(sibs) == 0) {
    return(empty_sleep_candidates())
  }

  main_windows <- nights |>
    dplyr::filter(!is.na(.data$night_date)) |>
    dplyr::mutate(
      main_sleep_start = decimal_hour_to_datetime(.data$night_date, .data$sleep_onset_decimal),
      main_sleep_end = decimal_hour_to_datetime(.data$night_date, .data$wakeup_decimal)
    ) |>
    dplyr::filter(!is.na(.data$main_sleep_start), !is.na(.data$main_sleep_end)) |>
    dplyr::select(.data$subject_id, .data$night, .data$night_date, .data$main_sleep_start, .data$main_sleep_end)

  # Remove SIBs that overlap any GGIR main sleep period for the same subject.
  overlap_flags <- sibs |>
    dplyr::left_join(main_windows, by = "subject_id", relationship = "many-to-many") |>
    dplyr::mutate(
      overlaps_main_sleep = !is.na(.data$main_sleep_start) &
        .data$start_time <= .data$main_sleep_end + lubridate::minutes(merge_gap) &
        .data$end_time >= .data$main_sleep_start - lubridate::minutes(merge_gap)
    ) |>
    dplyr::group_by(.data$sib_id) |>
    dplyr::summarise(
      overlaps_any_main_sleep = any(.data$overlaps_main_sleep, na.rm = TRUE),
      .groups = "drop"
    )

  waking_sibs <- sibs |>
    dplyr::left_join(overlap_flags, by = "sib_id") |>
    dplyr::filter(!.data$overlaps_any_main_sleep)

  if (nrow(waking_sibs) == 0) {
    return(empty_sleep_candidates())
  }

  # Assign candidate to the nearest GGIR night window for review grouping.
  assigned <- waking_sibs |>
    dplyr::left_join(main_windows, by = "subject_id", relationship = "many-to-many") |>
    dplyr::mutate(
      distance_to_main_sleep_minutes = pmin(
        abs(as.numeric(difftime(.data$start_time, .data$main_sleep_end, units = "mins"))),
        abs(as.numeric(difftime(.data$end_time, .data$main_sleep_start, units = "mins"))),
        na.rm = TRUE
      )
    ) |>
    dplyr::group_by(.data$sib_id) |>
    dplyr::slice_min(.data$distance_to_main_sleep_minutes, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  assigned |>
    dplyr::mutate(
      window_date = as.Date(.data$start_time),
      start_hour = lubridate::hour(.data$start_time) + lubridate::minute(.data$start_time) / 60,
      end_hour = lubridate::hour(.data$end_time) + lubridate::minute(.data$end_time) / 60,
      candidate_type = dplyr::case_when(
        .data$start_hour >= nap_start_hour & .data$start_hour <= nap_end_hour ~ "nap_candidate",
        TRUE ~ "secondary_sleep_candidate"
      ),
      confidence = score_sleep_candidate(.data$duration_minutes, .data$mean_acc_1min_before, .data$mean_acc_1min_after, .data$candidate_type),
      reason_codes = paste0(
        "sustained_inactivity_bout_ge_", min_duration,
        "min_outside_main_sleep;classified_as_", .data$candidate_type
      ),
      candidate_id = dplyr::row_number()
    ) |>
    dplyr::select(
      .data$subject_id,
      .data$window_date,
      .data$candidate_id,
      .data$start_time,
      .data$end_time,
      .data$duration_minutes,
      .data$candidate_type,
      .data$confidence,
      .data$mean_acc_1min_before,
      .data$mean_acc_1min_after,
      .data$night,
      .data$night_date,
      .data$reason_codes,
      .data$source_file
    ) |>
    dplyr::arrange(.data$subject_id, .data$start_time)
}

score_sleep_candidate <- function(duration_minutes, mean_acc_before, mean_acc_after, candidate_type) {
  duration_score <- dplyr::case_when(
    duration_minutes >= 90 ~ 0.40,
    duration_minutes >= 60 ~ 0.32,
    duration_minutes >= 30 ~ 0.24,
    TRUE ~ 0.10
  )

  transition_score <- dplyr::case_when(
    is.na(mean_acc_before) | is.na(mean_acc_after) ~ 0.10,
    mean_acc_before >= 15 & mean_acc_after >= 15 ~ 0.25,
    mean_acc_before >= 10 | mean_acc_after >= 10 ~ 0.18,
    TRUE ~ 0.08
  )

  type_score <- dplyr::case_when(
    candidate_type == "nap_candidate" ~ 0.20,
    TRUE ~ 0.15
  )

  pmin(0.95, duration_score + transition_score + type_score)
}

decimal_hour_to_datetime <- function(date, decimal_hour) {
  date <- as.Date(date)
  decimal_hour <- suppressWarnings(as.numeric(decimal_hour))
  as.POSIXct(date, tz = "UTC") + lubridate::hours(decimal_hour)
}

empty_sleep_candidates <- function() {
  tibble::tibble(
    subject_id = character(),
    window_date = as.Date(character()),
    candidate_id = integer(),
    start_time = as.POSIXct(character()),
    end_time = as.POSIXct(character()),
    duration_minutes = numeric(),
    candidate_type = character(),
    confidence = numeric(),
    mean_acc_1min_before = numeric(),
    mean_acc_1min_after = numeric(),
    night = integer(),
    night_date = as.Date(character()),
    reason_codes = character(),
    source_file = character()
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
