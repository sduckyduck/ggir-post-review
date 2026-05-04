#' Run GGIR post-review workflow
#'
#' This is the main entry point for the package. It discovers GGIR output files,
#' creates a run manifest, audits the GGIR config, combines Part 4 night summaries,
#' applies sleep QC rules, detects candidate secondary sleep periods from SIB
#' reports, and writes review-ready CSV outputs.
#'
#' @param ggir_output_dir Path to a completed GGIR output directory.
#' @param out_dir Output directory for post-review files.
#' @param config_file Optional YAML configuration file. If `NULL`, package defaults are used.
#'
#' @return A named list containing paths and in-memory result tables.
#' @export
run_ggir_post_review <- function(ggir_output_dir,
                                 out_dir = file.path(ggir_output_dir, "ggir_post_review_outputs"),
                                 config_file = NULL) {
  if (!dir.exists(ggir_output_dir)) {
    rlang::abort(glue::glue("GGIR output directory does not exist: {ggir_output_dir}"))
  }

  fs::dir_create(out_dir)

  config <- load_review_config(config_file)

  cli::cli_h1("GGIR Post Review")
  cli::cli_alert_info("Discovering GGIR output files")
  manifest <- discover_ggir_outputs(ggir_output_dir)

  manifest_path <- file.path(out_dir, "run_manifest.csv")
  readr::write_csv(manifest, manifest_path)

  cli::cli_alert_info("Auditing GGIR config")
  parameter_audit <- audit_ggir_parameters(manifest)
  parameter_audit_path <- file.path(out_dir, "parameter_audit.csv")
  readr::write_csv(parameter_audit, parameter_audit_path)

  cli::cli_alert_info("Combining GGIR Part 4 night summaries")
  nights <- combine_ggir_nights(manifest)

  cli::cli_alert_info("Detecting candidate secondary sleep periods from SIB reports")
  sleep_candidates <- detect_sleep_candidates_from_sib(manifest, nights = nights, config = config)

  cli::cli_alert_info("Applying sleep QC rules")
  night_flags <- apply_sleep_qc_rules(nights, sleep_candidates = sleep_candidates, config = config)

  cli::cli_alert_info("Building recommendations")
  recommendations <- build_review_recommendations(night_flags)

  nights_path <- file.path(out_dir, "combined_nights.csv")
  flags_path <- file.path(out_dir, "night_flags.csv")
  candidates_path <- file.path(out_dir, "sleep_candidates.csv")
  recommendations_path <- file.path(out_dir, "review_recommendations.csv")
  queue_path <- file.path(out_dir, "manual_review_queue.csv")

  readr::write_csv(nights, nights_path)
  readr::write_csv(night_flags, flags_path)
  readr::write_csv(sleep_candidates, candidates_path)
  readr::write_csv(recommendations, recommendations_path)

  review_queue <- night_flags |>
    dplyr::filter(.data$flagged) |>
    dplyr::arrange(dplyr::desc(.data$severity_rank), .data$subject_id, .data$night_date)

  readr::write_csv(review_queue, queue_path)

  cli::cli_alert_success("Post-review files written to {.path {out_dir}}")

  list(
    out_dir = out_dir,
    manifest = manifest,
    parameter_audit = parameter_audit,
    nights = nights,
    sleep_candidates = sleep_candidates,
    night_flags = night_flags,
    recommendations = recommendations,
    paths = list(
      manifest = manifest_path,
      parameter_audit = parameter_audit_path,
      combined_nights = nights_path,
      sleep_candidates = candidates_path,
      night_flags = flags_path,
      review_recommendations = recommendations_path,
      manual_review_queue = queue_path
    )
  )
}
