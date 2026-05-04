#' Combine GGIR night-level sleep outputs
#'
#' Reads GGIR Part 4 night summary files and maps them into a stable schema used
#' by the post-review QC workflow. Cleaned Part 4 night summaries are preferred;
#' full QC Part 4 summaries are used as a fallback.
#'
#' @param manifest A run manifest created by [discover_ggir_outputs()].
#'
#' @return A tibble with one row per subject-night.
#' @export
combine_ggir_nights <- function(manifest) {
  if (!"ggir_output_type" %in% names(manifest)) {
    rlang::abort("Manifest must include ggir_output_type. Run discover_ggir_outputs() first.")
  }

  night_files <- manifest |>
    dplyr::filter(.data$ggir_output_type %in% c("part4_nightsummary_cleaned", "part4_nightsummary_full")) |>
    dplyr::arrange(dplyr::desc(.data$ggir_output_type == "part4_nightsummary_cleaned"))

  if (nrow(night_files) == 0) {
    return(empty_combined_nights())
  }

  purrr::map_dfr(seq_len(nrow(night_files)), function(i) {
    path <- night_files$file_path[[i]]
    source_type <- night_files$ggir_output_type[[i]]
    raw <- readr::read_csv(path, show_col_types = FALSE)
    normalize_part4_nights(raw, source_file = path, source_type = source_type)
  }) |>
    dplyr::distinct(.data$subject_id, .data$night, .data$night_date, .data$source_type, .keep_all = TRUE) |>
    dplyr::arrange(.data$subject_id, .data$night)
}

normalize_part4_nights <- function(raw, source_file, source_type) {
  required <- c("ID", "night")
  missing_required <- setdiff(required, names(raw))
  if (length(missing_required) > 0) {
    rlang::abort(glue::glue(
      "Part 4 night file is missing required columns: {paste(missing_required, collapse = ', ')}"
    ))
  }

  raw |>
    ensure_columns(c(
      "calendar_date", "weekday", "sleeponset", "wakeup", "SptDuration",
      "SleepDurationInSpt", "WASO", "fraction_night_invalid",
      "nonwear_perc_spt", "ACC_spt_mg", "number_sib_sleepperiod",
      "number_sib_wakinghours", "duration_sib_wakinghours",
      "duration_sib_wakinghours_atleast15min", "number_of_awakenings",
      "sleeponset_ts", "wakeup_ts", "guider_onset_ts", "guider_wakeup_ts",
      "sleepparam", "cleaningcode", "sleeplog_used", "acc_available",
      "guider", "daysleeper", "window", "GGIRversion", "filename"
    )) |>
    dplyr::mutate(
      subject_id = as.character(.data$ID),
      night = as.integer(.data$night),
      night_date = suppressWarnings(as.Date(.data$calendar_date)),
      sleep_onset_decimal = suppressWarnings(as.numeric(.data$sleeponset)),
      wakeup_decimal = suppressWarnings(as.numeric(.data$wakeup)),
      spt_duration_hours = suppressWarnings(as.numeric(.data$SptDuration)),
      sleep_duration_hours = suppressWarnings(as.numeric(.data$SleepDurationInSpt)),
      sleep_duration_minutes = .data$sleep_duration_hours * 60,
      waso_hours = suppressWarnings(as.numeric(.data$WASO)),
      waso_minutes = .data$waso_hours * 60,
      sleep_efficiency = dplyr::if_else(
        !is.na(.data$spt_duration_hours) & .data$spt_duration_hours > 0,
        100 * .data$sleep_duration_hours / .data$spt_duration_hours,
        NA_real_
      ),
      fraction_night_invalid = suppressWarnings(as.numeric(.data$fraction_night_invalid)),
      nonwear_fraction_spt = suppressWarnings(as.numeric(.data$nonwear_perc_spt)) / 100,
      acc_spt_mg = suppressWarnings(as.numeric(.data$ACC_spt_mg)),
      number_sib_sleepperiod = suppressWarnings(as.numeric(.data$number_sib_sleepperiod)),
      number_sib_wakinghours = suppressWarnings(as.numeric(.data$number_sib_wakinghours)),
      duration_sib_wakinghours_hours = suppressWarnings(as.numeric(.data$duration_sib_wakinghours)),
      duration_sib_wakinghours_atleast15min_hours = suppressWarnings(as.numeric(.data$duration_sib_wakinghours_atleast15min)),
      number_of_awakenings = suppressWarnings(as.numeric(.data$number_of_awakenings)),
      ggir_sleep_onset_time = as.character(.data$sleeponset_ts),
      ggir_wakeup_time = as.character(.data$wakeup_ts),
      guider_onset_time = as.character(.data$guider_onset_ts),
      guider_wakeup_time = as.character(.data$guider_wakeup_ts),
      sleepparam = as.character(.data$sleepparam),
      cleaningcode = as.character(.data$cleaningcode),
      sleeplog_used = suppressWarnings(as.integer(.data$sleeplog_used)),
      acc_available = suppressWarnings(as.integer(.data$acc_available)),
      guider = as.character(.data$guider),
      daysleeper = suppressWarnings(as.integer(.data$daysleeper)),
      window = as.character(.data$window),
      ggir_version = as.character(.data$GGIRversion),
      source_file = source_file,
      source_type = source_type
    ) |>
    dplyr::select(
      .data$subject_id, .data$night, .data$night_date, .data$weekday,
      .data$ggir_sleep_onset_time, .data$ggir_wakeup_time,
      .data$sleep_onset_decimal, .data$wakeup_decimal,
      .data$spt_duration_hours, .data$sleep_duration_hours,
      .data$sleep_duration_minutes, .data$waso_hours, .data$waso_minutes,
      .data$sleep_efficiency, .data$fraction_night_invalid,
      .data$nonwear_fraction_spt, .data$acc_spt_mg,
      .data$number_sib_sleepperiod, .data$number_sib_wakinghours,
      .data$duration_sib_wakinghours_hours,
      .data$duration_sib_wakinghours_atleast15min_hours,
      .data$number_of_awakenings,
      .data$guider_onset_time, .data$guider_wakeup_time,
      .data$sleepparam, .data$cleaningcode, .data$sleeplog_used,
      .data$acc_available, .data$guider, .data$daysleeper,
      .data$window, .data$ggir_version, .data$filename,
      .data$source_file, .data$source_type
    )
}

empty_combined_nights <- function() {
  tibble::tibble(
    subject_id = character(),
    night = integer(),
    night_date = as.Date(character()),
    weekday = character(),
    ggir_sleep_onset_time = character(),
    ggir_wakeup_time = character(),
    sleep_onset_decimal = numeric(),
    wakeup_decimal = numeric(),
    spt_duration_hours = numeric(),
    sleep_duration_hours = numeric(),
    sleep_duration_minutes = numeric(),
    waso_hours = numeric(),
    waso_minutes = numeric(),
    sleep_efficiency = numeric(),
    fraction_night_invalid = numeric(),
    nonwear_fraction_spt = numeric(),
    acc_spt_mg = numeric(),
    number_sib_sleepperiod = numeric(),
    number_sib_wakinghours = numeric(),
    duration_sib_wakinghours_hours = numeric(),
    duration_sib_wakinghours_atleast15min_hours = numeric(),
    number_of_awakenings = numeric(),
    guider_onset_time = character(),
    guider_wakeup_time = character(),
    sleepparam = character(),
    cleaningcode = character(),
    sleeplog_used = integer(),
    acc_available = integer(),
    guider = character(),
    daysleeper = integer(),
    window = character(),
    ggir_version = character(),
    filename = character(),
    source_file = character(),
    source_type = character()
  )
}

ensure_columns <- function(data, columns) {
  for (col in columns) {
    if (!col %in% names(data)) {
      data[[col]] <- NA
    }
  }
  data
}
