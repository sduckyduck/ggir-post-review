load_review_config <- function(config_file = NULL) {
  default_config <- list(
    study = list(
      timezone = "America/New_York",
      day_window = "noon_to_noon",
      min_valid_nights = 3
    ),
    qc_thresholds = list(
      very_short_spt_hours = 2,
      very_long_spt_hours = 14,
      very_short_sleep_hours = 1,
      very_long_sleep_hours = 12,
      low_sleep_efficiency_pct = 65,
      high_sleep_efficiency_pct = 98,
      high_waso_minutes = 180,
      high_nonwear_fraction_spt = 0.20,
      high_invalid_fraction_night = 0.20,
      high_waking_sib_hours_atleast15min = 2
    ),
    candidate_sleep = list(
      min_duration_minutes = 30,
      merge_gap_minutes = 20,
      min_confidence_for_review = 0.60,
      include_daytime_naps = TRUE
    ),
    review = list(
      generate_html = TRUE,
      generate_pdf = FALSE,
      include_only_flagged_nights = FALSE
    )
  )

  if (is.null(config_file)) {
    return(default_config)
  }

  if (!file.exists(config_file)) {
    rlang::abort(glue::glue("Config file does not exist: {config_file}"))
  }

  user_config <- yaml::read_yaml(config_file)
  utils::modifyList(default_config, user_config, keep.null = TRUE)
}
