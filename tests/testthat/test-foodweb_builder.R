test_that("build_local_foodweb handles small example data", {

  ## 1. Size classes
  small_species <- data.frame(
    species_code  = c("A", "B"),
    lower_bound   = c(0, 0),
    upper_bound_1 = c(10, 10),
    upper_bound_2 = c(15, 15)
  )

  ## 2. Predator–prey window
  small_predwin <- data.frame(
    species_code = c("A", "B"),
    beta_min     = c(0.25, 0.5),
    beta_max     = c(0.9,  1.0)
  )

  ## 3. Fish ontogenetic diet shifts
  small_fish <- data.frame(
    species_code = c("A", "B"),
    size_min     = c(0, 0),
    size_max     = c(20, 20),
    zooplankton  = c(0, 1),
    benthos      = c(0, 1),
    fish         = c(0, 1)
  )

  ## 4. Resource diet shifts
  small_resource <- data.frame(
    species_code = c("zooplankton", "benthos"),
    zooplankton  = c(0, 1),
    benthos      = c(0, 1)
  )

  ## 5. Build global metaweb
  metaweb <- build_metaweb(
    tab_size_classes    = small_species,
    pred_win            = small_predwin,
    fish_diet_shift     = small_fish,
    resource_diet_shift = small_resource,
    num_classes         = 2,
    selected_resources  = c("zooplankton", "benthos")
  )

  ## 6. Individual-level data with a local_id column
  ind_measure <- data.frame(
    local_id     = c("site1", "site1"),
    species_code = c("A", "B"),
    size         = c(5, 12)  # both species present, different size classes
  )

  ## 7. Build local food webs (one per local_id)
  local_fws <- build_local_foodweb(
    ind_measure       = ind_measure,
    local_id          = "local_id",
    metaweb           = metaweb,
    tab_size_classes  = small_species,
    num_classes       = 2,
    selected_resources = c("zooplankton", "benthos")
  )

  ## 8. Checks

  # We expect a named list of local food webs
  expect_true(is.list(local_fws))
  expect_equal(length(local_fws), 1L)
  expect_true("site1" %in% names(local_fws))

  # Extract the local web for "site1"
  local_fw <- local_fws[["site1"]]

  # It should be a matrix or data frame
  expect_true(is.matrix(local_fw) || is.data.frame(local_fw))

  # It should be square
  expect_equal(dim(local_fw)[1], dim(local_fw)[2])

  # All nodes should exist in the global metaweb
  expect_true(all(rownames(local_fw) %in% rownames(metaweb)))
  expect_true(all(colnames(local_fw) %in% colnames(metaweb)))

  # No unexpected new nodes
  expect_false(any(!rownames(local_fw) %in% rownames(metaweb)))
})


test_that("build_local_foodweb errors if required columns are missing", {

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

  expect_error(
    build_local_foodweb(
      ind_measure       = ind_measure,
      local_id          = "local_id",
      metaweb           = metaweb,
      tab_size_classes  = tab_size_classes,
      num_classes       = 1,
      selected_resources = character()
    ),
    "ind_measure must contain"
  )
})


test_that("build_local_foodweb builds one web per local unit", {

  ## 1. Simple size classes for one species
  small_species <- data.frame(
    species_code  = "A",
    lower_bound   = 0,
    upper_bound_1 = 10,
    upper_bound_2 = 20
  )

  ## 2. Predator–prey window
  small_predwin <- data.frame(
    species_code = "A",
    beta_min     = 0.5,
    beta_max     = 1
  )

  ## 3. Fish ontogenetic diet shifts
  small_fish <- data.frame(
    species_code = "A",
    size_min     = 0,
    size_max     = 20,
    zoopl        = 1,
    benthos      = 0,
    fish         = 0
  )

  ## 4. Resource diet shifts
  small_resource <- data.frame(
    species_code = "zoopl",
    zoopl        = 0,
    benthos      = 0
  )

  ## 5. Build global metaweb
  metaweb <- build_metaweb(
    tab_size_classes    = small_species,
    pred_win            = small_predwin,
    fish_diet_shift     = small_fish,
    resource_diet_shift = small_resource,
    num_classes         = 2,
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
    ind_measure       = ind_measure,
    local_id          = "local_id",
    metaweb           = metaweb,
    tab_size_classes  = small_species,
    num_classes       = 2,
    selected_resources = "zoopl"
  )

  ## 8. Checks

  # List with one element per local_id
  expect_true(is.list(res))
  expect_equal(sort(names(res)), c("site1", "site2"))

  # Each element must be a square matrix/data.frame
  expect_true(
    all(vapply(res, function(x) {
      (is.matrix(x) || is.data.frame(x)) && (nrow(x) == ncol(x))
    }, logical(1)))
  )
})
