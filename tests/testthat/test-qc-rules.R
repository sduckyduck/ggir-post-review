test_that("apply_sleep_qc_rules flags implausible nights", {
  nights <- tibble::tibble(
    subject_id = "001",
    night_date = as.Date("2026-01-01"),
    spt_duration_hours = 16,
    sleep_duration_hours = 0.5,
    waso_minutes = 240,
    sleep_efficiency = 50,
    nonwear_fraction_spt = 0.3
  )

  flags <- apply_sleep_qc_rules(nights)

  expect_true(flags$flagged[1])
  expect_equal(flags$severity[1], "high")
  expect_true(stringr::str_detect(flags$flag_reasons[1], "very_long_spt"))
  expect_true(stringr::str_detect(flags$flag_reasons[1], "very_short_sleep"))
})
