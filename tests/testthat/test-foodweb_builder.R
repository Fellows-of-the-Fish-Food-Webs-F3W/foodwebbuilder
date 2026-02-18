testthat::test_that("build_local_foodweb handles small example data", {

  ## 1. Size classes (2 fish species, 2 size classes)
  tab_size_classes <- data.frame(
    species_code  = c("A", "B"),
    lower_bound   = c(0, 0),
    upper_bound_1 = c(10, 10),
    upper_bound_2 = c(15, 15)
  )

  ## 2. Predator–prey window
  pred_win <- data.frame(
    species_code = c("A", "B"),
    beta_min     = c(0.25, 0.5),
    beta_max     = c(0.9,  1.0)
  )

  ## 3. Fish ontogenetic diet shifts
  fish_diet_shift <- data.frame(
    species_code = c("A", "B"),
    size_min     = c(0, 0),
    size_max     = c(20, 20),
    zooplankton  = c(0, 1),
    benthos      = c(0, 1),
    fish         = c(0, 1)
  )

  ## 4. Resource diet shifts
  resource_diet_shift <- data.frame(
    species_code = c("zooplankton", "benthos"),
    zooplankton  = c(0, 1),
    benthos      = c(0, 1)
  )

  ## 5. Build global metaweb
  metaweb <- build_metaweb(
    tab_size_classes    = tab_size_classes,
    pred_win            = pred_win,
    fish_diet_shift     = fish_diet_shift,
    resource_diet_shift = resource_diet_shift,
    num_classes         = 2, # optional consistency check
    selected_resources  = c("zooplankton", "benthos")
  )

  ## 6. Individual-level data with a local_id column
  ind_measure <- data.frame(
    local_id     = c("site1", "site1"),
    species_code = c("A", "B"),
    size         = c(5, 12)  # A in class 1, B in class 2
  )

  ## 7. Build local food webs (one per local_id)
  local_fws <- build_local_foodweb(
    ind_measure        = ind_measure,
    local_id           = "local_id",
    metaweb            = metaweb,
    tab_size_classes   = tab_size_classes,
    selected_resources = c("zooplankton", "benthos")
  )

  ## 8. Checks
  testthat::expect_true(is.list(local_fws))
  testthat::expect_equal(length(local_fws), 1L)
  testthat::expect_true("site1" %in% names(local_fws))

  local_fw <- local_fws[["site1"]]

  testthat::expect_true(is.matrix(local_fw) || is.data.frame(local_fw))
  testthat::expect_equal(nrow(local_fw), ncol(local_fw))

  # Local nodes must be subset of metaweb nodes
  testthat::expect_true(all(rownames(local_fw) %in% rownames(metaweb)))
  testthat::expect_true(all(colnames(local_fw) %in% colnames(metaweb)))

  # Dimnames should be identical for adjacency matrices
  testthat::expect_identical(rownames(local_fw), colnames(local_fw))
})

testthat::test_that(
  "build_local_foodweb errors if required columns are missing",
  {

  # Missing 'species_code' column (we use 'species' instead)
  ind_measure <- data.frame(
    local_id = c("site1", "site2"),
    species  = c("A", "B"),  # wrong name
    size     = c(10, 15)
  )

  metaweb <- matrix(0, nrow = 1, ncol = 1)
  dimnames(metaweb) <- list("dummy", "dummy")

  tab_size_classes <- data.frame(
    species_code  = "A",
    lower_bound   = 0,
    upper_bound_1 = 10
  )

  testthat::expect_error(
    build_local_foodweb(
      ind_measure        = ind_measure,
      local_id           = "local_id",
      metaweb            = metaweb,
      tab_size_classes   = tab_size_classes,
      selected_resources = "dummy"
    ),
    "ind_measure must contain"
  )
})

testthat::test_that(
  "build_local_foodweb errors when selected_resources is empty",
  {

  ind_measure <- data.frame(
    local_id     = c("site1"),
    species_code = c("A"),
    size         = c(5)
  )

  metaweb <- matrix(0, nrow = 1, ncol = 1)
  dimnames(metaweb) <- list("dummy", "dummy")

  tab_size_classes <- data.frame(
    species_code  = "A",
    lower_bound   = 0,
    upper_bound_1 = 10
  )

  testthat::expect_error(
    build_local_foodweb(
      ind_measure        = ind_measure,
      local_id           = "local_id",
      metaweb            = metaweb,
      tab_size_classes   = tab_size_classes,
      selected_resources = character()
    ),
    "selected_resources must be a non-empty"
  )
})

testthat::test_that("build_local_foodweb builds one web per local unit", {

  ## 1. Simple size classes for one species (2 size classes)
  tab_size_classes <- data.frame(
    species_code  = "A",
    lower_bound   = 0,
    upper_bound_1 = 10,
    upper_bound_2 = 20
  )

  ## 2. Predator–prey window
  pred_win <- data.frame(
    species_code = "A",
    beta_min     = 0.5,
    beta_max     = 1
  )

  ## 3. Fish ontogenetic diet shifts
  fish_diet_shift <- data.frame(
    species_code = "A",
    size_min     = 0,
    size_max     = 20,
    zoopl        = 1,
    fish         = 0
  )

  ## 4. Resource diet shifts (resource node must exist in species_code)
  resource_diet_shift <- data.frame(
    species_code = "zoopl",
    zoopl        = 0
  )

  ## 5. Build global metaweb
  metaweb <- build_metaweb(
    tab_size_classes    = tab_size_classes,
    pred_win            = pred_win,
    fish_diet_shift     = fish_diet_shift,
    resource_diet_shift = resource_diet_shift,
    num_classes         = 2, # optional check
    selected_resources  = "zoopl"
  )

  ## 6. Two different local units
  ind_measure <- data.frame(
    local_id     = c("site1", "site2"),
    species_code = c("A", "A"),
    size         = c(5, 15)
  )

  ## 7. Build local food webs
  res <- build_local_foodweb(
    ind_measure        = ind_measure,
    local_id           = "local_id",
    metaweb            = metaweb,
    tab_size_classes   = tab_size_classes,
    selected_resources = "zoopl"
  )

  ## 8. Checks
  testthat::expect_true(is.list(res))
  testthat::expect_equal(sort(names(res)), c("site1", "site2"))

  testthat::expect_true(
    all(vapply(res, function(x) {
      (is.matrix(x) || is.data.frame(x)) &&
        (nrow(x) == ncol(x)) &&
        identical(rownames(x), colnames(x))
    }, logical(1)))
  )
})
