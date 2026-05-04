#' Audit GGIR run parameters
#'
#' Reads `config.csv` from a GGIR output folder and marks parameters that are
#' likely to affect sleep review. This is not a full default comparison yet; it
#' is a first-pass audit that classifies observed parameters by review impact.
#'
#' @param manifest A run manifest created by [discover_ggir_outputs()].
#'
#' @return A tibble with one row per GGIR config argument.
#' @export
audit_ggir_parameters <- function(manifest) {
  config_files <- manifest |>
    dplyr::filter(.data$ggir_output_type == "config")

  if (nrow(config_files) == 0) {
    return(tibble::tibble(
      argument = character(),
      value = character(),
      context = character(),
      impact_area = character(),
      impact_level = character(),
      review_note = character(),
      source_file = character()
    ))
  }

  purrr::map_dfr(config_files$file_path, function(path) {
    cfg <- readr::read_csv(path, show_col_types = FALSE)
    cfg |>
      dplyr::mutate(
        argument = as.character(.data$argument),
        value = as.character(.data$value),
        context = as.character(.data$context),
        impact_area = classify_parameter_impact_area(.data$argument),
        impact_level = classify_parameter_impact_level(.data$impact_area),
        review_note = parameter_review_note(.data$argument, .data$impact_area),
        source_file = path
      ) |>
      dplyr::select(
        .data$argument, .data$value, .data$context,
        .data$impact_area, .data$impact_level, .data$review_note,
        .data$source_file
      )
  })
}

classify_parameter_impact_area <- function(argument) {
  arg <- stringr::str_to_lower(argument)

  dplyr::case_when(
    stringr::str_detect(arg, "timezone|tz|dayborder|hrs.del.start|hrs.del.end|includedaycrit") ~ "time_window",
    stringr::str_detect(arg, "sleep|sleeplog|qwindow|spt|sib|def.noc.sleep") ~ "sleep_detection",
    stringr::str_detect(arg, "nonwear|wear|nwear") ~ "nonwear",
    stringr::str_detect(arg, "calib|autocalib|spherecrit") ~ "calibration",
    stringr::str_detect(arg, "strategy|mode|do.report|windowsizes|threshold|bout") ~ "analysis_setting",
    stringr::str_detect(arg, "datadir|outputdir|studyname") ~ "path_or_io",
    stringr::str_detect(arg, "version") ~ "version",
    TRUE ~ "other"
  )
}

classify_parameter_impact_level <- function(impact_area) {
  dplyr::case_when(
    impact_area %in% c("time_window", "sleep_detection", "nonwear", "calibration") ~ "high",
    impact_area %in% c("analysis_setting", "version") ~ "medium",
    impact_area %in% c("path_or_io") ~ "low",
    TRUE ~ "low"
  )
}

parameter_review_note <- function(argument, impact_area) {
  dplyr::case_when(
    impact_area == "time_window" ~ "Can affect calendar date alignment, valid-day windows, and sleep onset/wake interpretation.",
    impact_area == "sleep_detection" ~ "Can affect SPT, sleep duration, SIB detection, and possible split-sleep review.",
    impact_area == "nonwear" ~ "Can affect whether low-activity periods are interpreted as sleep or device removal.",
    impact_area == "calibration" ~ "Can affect acceleration-derived metrics and downstream sleep/activity summaries.",
    impact_area == "analysis_setting" ~ "May affect derived summaries or reporting thresholds.",
    impact_area == "version" ~ "Record package/tool version because defaults and output columns may differ by GGIR version.",
    TRUE ~ "No specific review note assigned."
  )
}
