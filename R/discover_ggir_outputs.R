#' Discover GGIR output files
#'
#' Scans a GGIR output directory and creates a simple run manifest.
#'
#' @param ggir_output_dir Path to a completed GGIR output directory.
#'
#' @return A tibble with one row per discovered file.
#' @export
discover_ggir_outputs <- function(ggir_output_dir) {
  files <- fs::dir_ls(
    ggir_output_dir,
    recurse = TRUE,
    type = "file",
    fail = FALSE
  )

  if (length(files) == 0) {
    return(tibble::tibble(
      file_path = character(),
      file_name = character(),
      extension = character(),
      file_type = character(),
      ggir_part = character(),
      subject_id_guess = character(),
      file_size_bytes = numeric(),
      modified_time = as.POSIXct(character())
    ))
  }

  tibble::tibble(file_path = as.character(files)) |>
    dplyr::mutate(
      file_name = fs::path_file(.data$file_path),
      extension = stringr::str_to_lower(fs::path_ext(.data$file_path)),
      file_type = classify_ggir_file(.data$file_name, .data$extension),
      ggir_part = infer_ggir_part(.data$file_path),
      subject_id_guess = guess_subject_id(.data$file_name),
      file_size_bytes = as.numeric(fs::file_size(.data$file_path)),
      modified_time = fs::file_info(.data$file_path)$modification_time
    )
}

classify_ggir_file <- function(file_name, extension) {
  dplyr::case_when(
    extension == "csv" ~ "csv",
    extension %in% c("pdf") ~ "pdf",
    extension %in% c("png", "jpg", "jpeg") ~ "image",
    extension %in% c("RData", "rdata") ~ "rdata",
    TRUE ~ "other"
  )
}

infer_ggir_part <- function(file_path) {
  lower_path <- stringr::str_to_lower(file_path)

  dplyr::case_when(
    stringr::str_detect(lower_path, "part[ _-]?2|meta/basic") ~ "part2",
    stringr::str_detect(lower_path, "part[ _-]?4|sleep") ~ "part4",
    stringr::str_detect(lower_path, "part[ _-]?5|report") ~ "part5",
    TRUE ~ "unknown"
  )
}

guess_subject_id <- function(file_name) {
  id <- stringr::str_extract(file_name, "[A-Za-z0-9]+(?=\\.(csv|pdf|png|jpg|jpeg|RData|rdata)$)")
  dplyr::coalesce(id, NA_character_)
}
