#' Build review recommendations from night flags
#'
#' Converts machine flags into researcher-facing review suggestions.
#'
#' @param night_flags Output from [apply_sleep_qc_rules()].
#'
#' @return A tibble of recommendations.
#' @export
build_review_recommendations <- function(night_flags) {
  if (nrow(night_flags) == 0) {
    return(tibble::tibble(
      subject_id = character(),
      night = integer(),
      night_date = as.Date(character()),
      priority = character(),
      recommendation_type = character(),
      recommendation_text = character(),
      supporting_flags = character()
    ))
  }

  night_flags |>
    dplyr::filter(.data$flagged) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      recommendation_type = dplyr::case_when(
        .data$flag_missing_main_sleep ~ "manual_sleep_window_review",
        .data$flag_possible_secondary_sleep ~ "possible_secondary_sleep_or_nap",
        .data$flag_high_nonwear_in_spt | .data$flag_acc_unavailable ~ "wear_validation",
        .data$flag_high_invalid_fraction ~ "invalid_recording_review",
        .data$flag_very_short_spt | .data$flag_very_long_spt ~ "implausible_spt_duration",
        .data$flag_low_sleep_efficiency | .data$flag_high_waso ~ "fragmented_sleep_review",
        .data$flag_many_waking_sibs ~ "waking_inactivity_review",
        TRUE ~ "general_sleep_qc_review"
      ),
      recommendation_text = dplyr::case_when(
        .data$flag_missing_main_sleep ~ "No main sleep period was available. Review the PDF/visualization and diary data before deriving final sleep metrics.",
        .data$flag_possible_secondary_sleep ~ "SIB reports contain sustained inactivity outside the GGIR main sleep window. Review this night for a possible nap or secondary sleep period before finalizing sleep duration.",
        .data$flag_high_nonwear_in_spt | .data$flag_acc_unavailable ~ "Wear validity may affect the sleep window. Check non-wear, temperature if available, and device placement before accepting sleep metrics.",
        .data$flag_high_invalid_fraction ~ "A large fraction of the night is invalid. Consider excluding this night or sending it to manual adjudication.",
        .data$flag_very_short_spt | .data$flag_very_long_spt ~ "SPT duration is outside expected bounds. Check AM/PM, date alignment, day boundary, and sleep diary availability.",
        .data$flag_low_sleep_efficiency | .data$flag_high_waso ~ "Sleep appears highly fragmented. Review activity/light/temperature context and diary notes if available.",
        .data$flag_many_waking_sibs ~ "There are many or long sustained inactivity bouts during waking hours. Check whether these represent quiet wake, non-wear, naps, or missed sleep diary periods.",
        TRUE ~ "Review this night because one or more automated QC flags were triggered."
      ),
      priority = dplyr::case_when(
        .data$severity_rank >= 3 ~ "high",
        .data$severity_rank == 2 ~ "medium",
        .data$severity_rank == 1 ~ "low",
        TRUE ~ "none"
      ),
      supporting_flags = .data$flag_reasons
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      .data$subject_id,
      .data$night,
      .data$night_date,
      .data$priority,
      .data$recommendation_type,
      .data$recommendation_text,
      .data$supporting_flags
    )
}
