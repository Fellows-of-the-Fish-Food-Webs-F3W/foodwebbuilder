test_that("fish_diet_shift has expected structure", {
  data(fish_diet_shift, package = "foodwebbuilder")
  expected_cols <- c(
    "species_code", "species_name", "size_min", "size_max", "stage",
    "light", "det", "biof", "phytob", "macroph", "phytopl", "zoopl", "zoob", "fish"
  )
  expect_true(all(expected_cols %in% names(fish_diet_shift)))
  expect_s3_class(fish_diet_shift, "data.frame")
})

test_that("ind_measure has expected structure", {
  data(ind_measure, package = "foodwebbuilder")
  expected_cols <- c(
    "site_id", "operation_id", "prelevement_id", "batch_id",
    "measure_id", "species_code", "size"
  )
  expect_true(all(expected_cols %in% names(ind_measure)))
  expect_s3_class(ind_measure, "data.frame")
})

test_that("pred_win has expected structure", {
  data(pred_win, package = "foodwebbuilder")
  expected_cols <- c(
    "species_code", "alpha_min", "beta_min",
    "alpha_max", "beta_max", "alpha_mean", "beta_mean"
  )
  expect_true(all(expected_cols %in% names(pred_win)))
  expect_s3_class(pred_win, "data.frame")
})

test_that("resource_diet_shift has expected structure", {
  data(resource_diet_shift, package = "foodwebbuilder")
  expected_cols <- c(
    "species_code", "taxon_name", "size_min", "size_max", "stage",
    "light", "det", "biof", "phytob", "macroph", "phytopl", "zoopl", "zoob", "fish"
  )
  expect_true(all(expected_cols %in% names(resource_diet_shift)))
  expect_s3_class(resource_diet_shift, "data.frame")
})
