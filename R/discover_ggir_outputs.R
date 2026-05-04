#' Discover GGIR output files
#'
#' Scans a GGIR output directory and creates a run manifest with GGIR-specific
#' file classifications.
#'
#' @param ggir_output_dir Path to a completed GGIR output directory.
#'
#' @return A tibble with one row per discovered file.
#' @export
discover_ggir_outputs <- function(ggir_output_dir) {
  files <- fs::dir_ls(ggir_output_dir, recurse = TRUE, type = "file", fail = FALSE)

  if (length(files) == 0) {
    return(tibble::tibble(
      file_path = character(),
      relative_path = character(),
      file_name = character(),
      extension = character(),
      file_type = character(),
      ggir_part = character(),
      ggir_output_type = character(),
      subject_id_guess = character(),
      file_size_bytes = numeric(),
      modified_time = as.POSIXct(character())
    ))
  }

  root <- normalizePath(ggir_output_dir, winslash = "/", mustWork = FALSE)

  tibble::tibble(file_path = as.character(files)) |>
    dplyr::mutate(
      file_path = normalizePath(.data$file_path, winslash = "/", mustWork = FALSE),
      relative_path = stringr::str_remove(.data$file_path, paste0("^", root, "/?")),
      file_name = fs::path_file(.data$file_path),
      extension = stringr::str_to_lower(fs::path_ext(.data$file_path)),
      file_type = classify_ggir_file(.data$extension),
      ggir_output_type = classify_ggir_output_type(.data$relative_path, .data$file_name),
      ggir_part = infer_ggir_part(.data$relative_path, .data$ggir_output_type),
      subject_id_guess = guess_subject_id(.data$file_name),
      file_size_bytes = as.numeric(fs::file_size(.data$file_path)),
      modified_time = fs::file_info(.data$file_path)$modification_time
    )
}

classify_ggir_file <- function(extension) {
  dplyr::case_when(
    extension == "csv" ~ "csv",
    extension == "pdf" ~ "pdf",
    extension %in% c("png", "jpg", "jpeg") ~ "image",
    extension == "rdata" ~ "rdata",
    TRUE ~ "other"
  )
}

classify_ggir_output_type <- function(relative_path, file_name) {
  path <- stringr::str_to_lower(relative_path)
  name <- stringr::str_to_lower(file_name)

  dplyr::case_when(
    name == "config.csv" ~ "config",
    stringr::str_detect(path, "results/qc/part4_nightsummary_sleep_full.csv") ~ "part4_nightsummary_full",
    stringr::str_detect(path, "results/part4_nightsummary_sleep_cleaned.csv") ~ "part4_nightsummary_cleaned",
    stringr::str_detect(path, "part4_summary_sleep_cleaned.csv") ~ "part4_summary_cleaned",
    stringr::str_detect(path, "part4_summary_sleep_full.csv") ~ "part4_summary_full",
    stringr::str_detect(path, "part5_daysummary") & stringr::str_detect(path, "results/qc") ~ "part5_daysummary_full",
    stringr::str_detect(path, "part5_daysummary") ~ "part5_daysummary",
    stringr::str_detect(path, "part5_personsummary") ~ "part5_personsummary",
    stringr::str_detect(path, "sib.reports") & stringr::str_detect(path, "sib_report") ~ "sib_report",
    stringr::str_detect(path, "meta/csv") & stringr::str_detect(path, ".csv") ~ "epoch_csv",
    stringr::str_detect(path, "variabledictionary") ~ "variable_dictionary",
    stringr::str_detect(path, "part2_daysummary.csv") ~ "part2_daysummary",
    stringr::str_detect(path, "part2_summary.csv") ~ "part2_summary",
    stringr::str_detect(path, "part6_summary.csv") ~ "part6_summary",
    stringr::str_detect(path, "sleep.qc") & stringr::str_detect(path, ".pdf") ~ "sleep_qc_pdf",
    stringr::str_detect(path, "file summary reports") & stringr::str_detect(path, ".pdf") ~ "file_summary_pdf",
    stringr::str_detect(path, "visualisation_sleep.pdf") ~ "visualisation_sleep_pdf",
    TRUE ~ "other"
  )
}

infer_ggir_part <- function(relative_path, ggir_output_type = NULL) {
  path <- stringr::str_to_lower(relative_path)

  type_part <- dplyr::case_when(
    stringr::str_starts(ggir_output_type, "part2") ~ "part2",
    stringr::str_starts(ggir_output_type, "part4") ~ "part4",
    stringr::str_starts(ggir_output_type, "part5") | ggir_output_type == "sib_report" ~ "part5",
    stringr::str_starts(ggir_output_type, "part6") ~ "part6",
    TRUE ~ NA_character_
  )

  dplyr::case_when(
    !is.na(type_part) ~ type_part,
    stringr::str_detect(path, "part2|meta/basic|ms2.out") ~ "part2",
    stringr::str_detect(path, "part4|ms4.out|sleep") ~ "part4",
    stringr::str_detect(path, "part5|ms5.out|ms5.outraw|report") ~ "part5",
    stringr::str_detect(path, "part6|ms6.out") ~ "part6",
    TRUE ~ "unknown"
  )
}

guess_subject_id <- function(file_name) {
  id_double_underscore <- stringr::str_match(file_name, "^([^_]+)__")[, 2]
  id_first_token <- stringr::str_match(file_name, "^([A-Za-z0-9]+)")[, 2]
  dplyr::coalesce(id_double_underscore, id_first_token, NA_character_)
}
