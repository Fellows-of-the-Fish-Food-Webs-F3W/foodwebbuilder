testthat::test_that("remove_missing_species keeps all species when data is complete", {
  ind_measure <- data.frame(
    species_code = c("A", "B", "A"),
    size = c(1, 2, 3)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    size_min = c(0, 0),
    size_max = c(10, 10),
    fish = c(0, 0)
  )

  pred_win <- data.frame(
    species_code = c("A", "B"),
    beta_min = c(0.2, 0.2),
    beta_max = c(1.0, 1.0)
  )

  res <- remove_missing_species(ind_measure, fish_diet_shift, pred_win)

  testthat::expect_equal(nrow(res), nrow(ind_measure))
  testthat::expect_equal(sort(res$species_code), sort(ind_measure$species_code))
})

testthat::test_that("remove_missing_species removes species missing in reference tables", {
  ind_measure <- data.frame(
    species_code = c("A", "B", "C", "A"),
    size = c(1, 2, 3, 4)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    size_min = c(0, 0),
    size_max = c(10, 10),
    fish = c(0, 0)
  )

  pred_win <- data.frame(
    species_code = c("A", "B"),
    beta_min = c(0.2, 0.2),
    beta_max = c(1.0, 1.0)
  )

  res <- remove_missing_species(ind_measure, fish_diet_shift, pred_win)

  testthat::expect_false("C" %in% res$species_code)
  testthat::expect_true(all(res$species_code %in% c("A", "B")))
  testthat::expect_equal(nrow(res), 3)  # only rows for A and B
})

testthat::test_that("compute_size_classes returns expected columns and structure", {
  ind_measure <- data.frame(
    species_code = c("A", "A", "B", "B"),
    size = c(5, 10, 8, 16)
  )

  res <- compute_size_classes(ind_measure, num_classes = 2)

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 2)
  testthat::expect_equal(
    colnames(res),
    c("species_code", "lower_bound", "upper_bound_1", "upper_bound_2")
  )

  # numeric columns are numeric
  testthat::expect_true(is.numeric(res$lower_bound))
  testthat::expect_true(is.numeric(res$upper_bound_1))
  testthat::expect_true(is.numeric(res$upper_bound_2))
})

testthat::test_that("compute_size_classes correctly splits size range per species", {
  ind_measure <- data.frame(
    species_code = c("A", "A", "B", "B"),
    size = c(0, 10, 0, 20)
  )

  res <- compute_size_classes(ind_measure, num_classes = 2)

  row_A <- res[res$species_code == "A", ]
  testthat::expect_equal(as.numeric(row_A[1, "lower_bound"]), 0)
  testthat::expect_equal(as.numeric(row_A[1, "upper_bound_1"]), 5)
  testthat::expect_equal(as.numeric(row_A[1, "upper_bound_2"]), 10)

  row_B <- res[res$species_code == "B", ]
  testthat::expect_equal(as.numeric(row_B[1, "lower_bound"]), 0)
  testthat::expect_equal(as.numeric(row_B[1, "upper_bound_1"]), 10)
  testthat::expect_equal(as.numeric(row_B[1, "upper_bound_2"]), 20)
})

testthat::test_that("build_metaweb builds a square matrix with consistent dimnames", {
  # 2 fish species, 2 size classes each -> 4 trophic species
  tab_size_classes <- data.frame(
    species_code  = c("A", "B"),
    lower_bound   = c(0, 0),
    upper_bound_1 = c(10, 10),
    upper_bound_2 = c(20, 20)
  )

  pred_win <- data.frame(
    species_code = c("A", "B"),
    beta_min     = c(0.25, 0.25),
    beta_max     = c(1.0, 1.0)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    size_min     = c(0, 0),
    size_max     = c(25, 25),
    fish         = c(0, 1),        # B piscivorous, A not
    zooplankton  = c(1, 1),
    benthos      = c(0, 1)
  )

  resource_diet_shift <- data.frame(
    species_code = c("zooplankton", "benthos"),
    zooplankton  = c(0, 1),
    benthos      = c(0, 0)
  )

  selected_resources <- c("zooplankton", "benthos")

  mw <- build_metaweb(
    tab_size_classes    = tab_size_classes,
    pred_win            = pred_win,
    fish_diet_shift     = fish_diet_shift,
    resource_diet_shift = resource_diet_shift,
    num_classes         = 2,  # optional check (should pass)
    selected_resources  = selected_resources
  )

  testthat::expect_true(is.matrix(mw))
  testthat::expect_equal(nrow(mw), ncol(mw))
  testthat::expect_identical(rownames(mw), colnames(mw))

  # Expected nodes = trophic species (4) + resources (2) = 6
  testthat::expect_equal(nrow(mw), 6)

  # Basic sanity on values
  testthat::expect_true(all(is.finite(mw)))
})

testthat::test_that("build_metaweb errors when selected_resources are not valid columns", {
  tab_size_classes <- data.frame(
    species_code  = c("A", "B"),
    lower_bound   = c(0, 0),
    upper_bound_1 = c(10, 10),
    upper_bound_2 = c(15, 15)
  )

  pred_win <- data.frame(
    species_code = c("A", "B"),
    beta_min     = c(0.25, 0.5),
    beta_max     = c(0.9,  1.0)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    size_min     = c(0, 0),
    size_max     = c(20, 20),
    fish         = c(0, 1),
    zooplankton  = c(1, 1),
    benthos      = c(0, 1)
  )

  resource_diet_shift <- data.frame(
    species_code = c("zooplankton", "benthos"),
    zooplankton  = c(0, 1),
    benthos      = c(0, 1)
  )

  testthat::expect_error(
    build_metaweb(
      tab_size_classes    = tab_size_classes,
      pred_win            = pred_win,
      fish_diet_shift     = fish_diet_shift,
      resource_diet_shift = resource_diet_shift,
      num_classes         = 2,
      selected_resources  = c("zooplankton", "not_a_resource")
    ),
    "not present as columns in both"
  )
})

testthat::test_that("build_metaweb errors when selected_resources are not found as resource nodes", {
  tab_size_classes <- data.frame(
    species_code  = c("A"),
    lower_bound   = c(0),
    upper_bound_1 = c(10),
    upper_bound_2 = c(20)
  )

  pred_win <- data.frame(
    species_code = c("A"),
    beta_min     = c(0.25),
    beta_max     = c(1.0)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A"),
    size_min     = c(0),
    size_max     = c(30),
    fish         = c(0),
    zooplankton  = c(1),
    benthos      = c(0)
  )

  # Here: 'benthos' exists as a column but not as a node in species_code
  resource_diet_shift <- data.frame(
    species_code = c("zooplankton"),
    zooplankton  = c(0),
    benthos      = c(0)
  )

  testthat::expect_error(
    build_metaweb(
      tab_size_classes    = tab_size_classes,
      pred_win            = pred_win,
      fish_diet_shift     = fish_diet_shift,
      resource_diet_shift = resource_diet_shift,
      selected_resources  = c("zooplankton", "benthos")
    ),
    "present as columns but not as resource nodes"
  )
})

testthat::test_that("build_metaweb errors when num_classes does not match tab_size_classes", {
  tab_size_classes <- data.frame(
    species_code  = c("A"),
    lower_bound   = c(0),
    upper_bound_1 = c(10),
    upper_bound_2 = c(20)
  )

  pred_win <- data.frame(
    species_code = c("A"),
    beta_min     = c(0.25),
    beta_max     = c(1.0)
  )

  fish_diet_shift <- data.frame(
    species_code = c("A"),
    size_min     = c(0),
    size_max     = c(30),
    fish         = c(0),
    zooplankton  = c(1)
  )

  resource_diet_shift <- data.frame(
    species_code = c("zooplankton"),
    zooplankton  = c(0)
  )

  testthat::expect_error(
    build_metaweb(
      tab_size_classes    = tab_size_classes,
      pred_win            = pred_win,
      fish_diet_shift     = fish_diet_shift,
      resource_diet_shift = resource_diet_shift,
      num_classes         = 3,  # inconsistent on purpose
      selected_resources  = "zooplankton"
    ),
    "Inconsistent 'num_classes'"
  )
})
