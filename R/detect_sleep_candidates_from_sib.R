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

  sibs <- purrr::map_dfr(sib_files$file_path, function(path) {
    readr::read_csv(path, show_col_types = FALSE) |>
      ensure_columns(c("ID", "type", "start", "end", "duration", "mean_acc_1min_before", "mean_acc_1min_after")) |>
      dplyr::mutate(source_file = path)
  }) |>
    dplyr::filter(.data$type == "sib") |>
    dplyr::mutate(
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
    dplyr::filter(!is.na(.data$night_date), !is.na(.data$ggir_sleep_onset_time), !is.na(.data$ggir_wakeup_time)) |>
    dplyr::mutate(
      main_sleep_start = combine_date_and_clock(.data$night_date, .data$ggir_sleep_onset_time, prefer_next_day = FALSE),
      main_sleep_end = combine_date_and_clock(.data$night_date, .data$ggir_wakeup_time, prefer_next_day = TRUE)
    ) |>
    dplyr::select(.data$subject_id, .data$night, .data$night_date, .data$main_sleep_start, .data$main_sleep_end)

  candidates <- sibs |>
    dplyr::left_join(main_windows, by = "subject_id", relationship = "many-to-many") |>
    dplyr::filter(
      is.na(.data$main_sleep_start) |
        (.data$end_time < .data$main_sleep_start - lubridate::minutes(merge_gap)) |
        (.data$start_time > .data$main_sleep_end + lubridate::minutes(merge_gap))
    ) |>
    dplyr::mutate(
      window_date = as.Date(.data$start_time),
      candidate_type = dplyr::case_when(
        lubridate::hour(.data$start_time) >= 9 & lubridate::hour(.data$start_time) <= 20 ~ "nap_candidate",
        TRUE ~ "secondary_sleep_candidate"
      ),
      confidence = dplyr::case_when(
        .data$duration_minutes >= 90 ~ 0.85,
        .data$duration_minutes >= 60 ~ 0.75,
        .data$duration_minutes >= 30 ~ 0.60,
        TRUE ~ 0.40
      ),
      reason_codes = paste0("sustained_inactivity_bout_ge_", min_duration, "min_outside_main_sleep"),
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

  candidates
}

combine_date_and_clock <- function(date, clock, prefer_next_day = FALSE) {
  clock <- as.character(clock)
  date <- as.Date(date)

  parsed <- lubridate::ymd_hms(paste(date, clock), quiet = TRUE)
  parsed <- dplyr::if_else(
    prefer_next_day & !is.na(parsed) & lubridate::hour(parsed) < 12,
    parsed + lubridate::days(1),
    parsed
  )
  parsed
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
