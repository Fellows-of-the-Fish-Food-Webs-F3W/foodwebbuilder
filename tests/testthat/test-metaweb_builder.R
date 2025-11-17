test_that("remove_missing_species keeps all species when data is complete", {
  ind_measure <- data.frame(
    species_code = c("A", "B", "A"),
    value = c(1, 2, 3)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    other = 1
  )

  pred_win <- data.frame(
    species_code = c("A", "B"),
    other = 1
  )

  res <- remove_missing_species(ind_measure, fish_diet_shift, pred_win)

  expect_equal(nrow(res), nrow(ind_measure))
  expect_equal(sort(res$species_code), sort(ind_measure$species_code))
})

test_that("remove_missing_species removes species missing in reference tables", {
  ind_measure <- data.frame(
    species_code = c("A", "B", "C", "A"),
    value = c(1, 2, 3, 4)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    other = 1
  )

  pred_win <- data.frame(
    species_code = c("A", "B"),
    other = 1
  )

  res <- remove_missing_species(ind_measure, fish_diet_shift, pred_win)

  expect_false("C" %in% res$species_code)
  expect_true(all(res$species_code %in% c("A", "B")))
  expect_equal(nrow(res), 3)  # les lignes pour A et B seulement
})

test_that("compute_size_classes returns expected columns and structure", {
  ind_measure <- data.frame(
    species_code = c("A", "A", "B", "B"),
    size = c(5, 10, 8, 16)
  )

  num_classes <- 2

  res <- compute_size_classes(ind_measure, num_classes)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(
    colnames(res),
    c("species_code", "lower_bound", "upper_bound_1", "upper_bound_2")
  )
})

test_that("compute_size_classes correctly splits size range per species", {
  ind_measure <- data.frame(
    species_code = c("A", "A", "B", "B"),
    size = c(0, 10, 0, 20)
  )

  num_classes <- 2

  res <- compute_size_classes(ind_measure, num_classes)

  row_A <- res[res$species_code == "A", ]
  expect_equal(as.numeric(row_A[1, "lower_bound"]), 0)
  expect_equal(as.numeric(row_A[1, "upper_bound_1"]), 5)
  expect_equal(as.numeric(row_A[1, "upper_bound_2"]), 10)

  row_B <- res[res$species_code == "B", ]
  expect_equal(as.numeric(row_B[1, "lower_bound"]), 0)
  expect_equal(as.numeric(row_B[1, "upper_bound_1"]), 10)
  expect_equal(as.numeric(row_B[1, "upper_bound_2"]), 20)
})

test_that("build_metaweb errors when selected_resources are not valid columns", {
  small_species <- data.frame(
    species_code  = c("A","B"),
    lower_bound   = c(0,0),
    upper_bound_1 = c(10,10),
    upper_bound_2 = c(15,15)
  )

  small_predwin <- data.frame(
    species_code = c("A","B"),
    beta_min     = c(0.25,0.5),
    beta_max     = c(0.9, 1.0)
  )

  small_fish <- data.frame(
    species_code = c("A","B"),
    size_min     = c(0,0),
    size_max     = c(20,20),
    zooplankton  = c(0,1),
    benthos      = c(0,1),
    fish         = c(0,1)
  )

  small_resource <- data.frame(
    species_code = c("zooplankton", "benthos"),
    zooplankton  = c(0,1),
    benthos      = c(0,1)
  )

  expect_error(
    build_metaweb(
      tab_size_classes    = small_species,
      pred_win            = small_predwin,
      fish_diet_shift     = small_fish,
      resource_diet_shift = small_resource,
      num_classes         = 2,
      selected_resources  = c("zooplankton", "not_a_resource")
    ),
    "not present as columns in both"
  )
})
