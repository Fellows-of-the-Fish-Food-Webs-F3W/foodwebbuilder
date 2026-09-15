testthat::test_that(
  ".assign_trophic_species assigns individuals to the correct size classes",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ind_measure <- data.frame(
      species_code = c("A", "A"),
      size         = c(5, 15)
    )

    res <- .assign_trophic_species(
      ind_measure,
      tab_size_classes
    )

    testthat::expect_equal(
      res,
      c("A_1", "A_2")
    )
  }
)

testthat::test_that(
  ".assign_trophic_species assigns boundary values to the correct size classes",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ind_measure <- data.frame(
      species_code = c("A", "A", "A"),
      size         = c(0, 10, 20)
    )

    res <- .assign_trophic_species(
      ind_measure,
      tab_size_classes
    )

    testthat::expect_equal(
      res,
      c(
        "A_1",
        "A_2",
        "A_2"
      )
    )
  }
)

testthat::test_that(
  ".assign_trophic_species handles multiple species",
  {

    tab_size_classes <- data.frame(
      species_code  = c("A", "B"),
      lower_bound   = c(0, 0),
      upper_bound_1 = c(10, 5),
      upper_bound_2 = c(20, 15)
    )

    ind_measure <- data.frame(
      species_code = c("A", "A", "B", "B"),
      size         = c(5, 15, 2, 10)
    )

    res <- .assign_trophic_species(
      ind_measure,
      tab_size_classes
    )

    testthat::expect_equal(
      res,
      c(
        "A_1",
        "A_2",
        "B_1",
        "B_2"
      )
    )
  }
)

testthat::test_that(
  ".assign_trophic_species returns NA for missing values",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ind_measure <- data.frame(
      species_code = c("A", NA, "A"),
      size         = c(5, 10, NA)
    )

    res <- .assign_trophic_species(
      ind_measure,
      tab_size_classes
    )

    testthat::expect_equal(
      res[1],
      "A_1"
    )

    testthat::expect_true(
      is.na(res[2])
    )

    testthat::expect_true(
      is.na(res[3])
    )
  }
)

testthat::test_that(
  ".assign_trophic_species returns NA for species absent from size classes",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ind_measure <- data.frame(
      species_code = c("A", "B"),
      size         = c(5, 5)
    )

    res <- .assign_trophic_species(
      ind_measure,
      tab_size_classes
    )

    testthat::expect_equal(
      res[1],
      "A_1"
    )

    testthat::expect_true(
      is.na(res[2])
    )
  }
)

testthat::test_that(
  ".assign_trophic_species returns NA for sizes outside class bounds",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ind_measure <- data.frame(
      species_code = c("A", "A", "A"),
      size         = c(-1, 5, 21)
    )

    res <- .assign_trophic_species(
      ind_measure,
      tab_size_classes
    )

    testthat::expect_true(
      is.na(res[1])
    )

    testthat::expect_equal(
      res[2],
      "A_1"
    )

    testthat::expect_true(
      is.na(res[3])
    )
  }
)

testthat::test_that(
  ".assign_trophic_species errors if required columns are missing",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ind_measure <- data.frame(
      species_code = "A"
    )

    testthat::expect_error(
      .assign_trophic_species(
        ind_measure,
        tab_size_classes
      ),
      "ind_measure must contain"
    )
  }
)

testthat::test_that(
  "build_local_foodweb handles small example data",
  {

    ## 1. Size classes (2 fish species, 2 size classes)
    tab_size_classes <- data.frame(
      species_code  = c("A", "B"),
      lower_bound   = c(0, 0),
      upper_bound_1 = c(10, 10),
      upper_bound_2 = c(15, 15)
    )

    ## 2. Predator-prey window
    pred_win <- data.frame(
      species_code = c("A", "B"),
      beta_min     = c(0.25, 0.5),
      beta_max     = c(0.9, 1.0)
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
      num_classes         = 2,
      selected_resources  = c(
        "zooplankton",
        "benthos"
      )
    )

    ## 6. Individual-level data with a local_id column
    ind_measure <- data.frame(
      local_id     = c("site1", "site1"),
      species_code = c("A", "B"),
      size         = c(5, 12)
    )

    ## 7. Build local food webs
    local_fws <- build_local_foodweb(
      ind_measure        = ind_measure,
      local_id           = "local_id",
      metaweb            = metaweb,
      tab_size_classes   = tab_size_classes,
      selected_resources = c(
        "zooplankton",
        "benthos"
      )
    )

    ## 8. Checks
    testthat::expect_true(
      is.list(local_fws)
    )

    testthat::expect_equal(
      length(local_fws),
      1L
    )

    testthat::expect_true(
      "site1" %in% names(local_fws)
    )

    local_fw <- local_fws[["site1"]]

    testthat::expect_true(
      is.matrix(local_fw) ||
        is.data.frame(local_fw)
    )

    testthat::expect_equal(
      nrow(local_fw),
      ncol(local_fw)
    )

    ## Local nodes must be a subset of metaweb nodes
    testthat::expect_true(
      all(
        rownames(local_fw) %in%
          rownames(metaweb)
      )
    )

    testthat::expect_true(
      all(
        colnames(local_fw) %in%
          colnames(metaweb)
      )
    )

    ## Dimnames should be identical for adjacency matrices
    testthat::expect_identical(
      rownames(local_fw),
      colnames(local_fw)
    )

    ## Check expected locally represented fish trophic species
    testthat::expect_true(
      "A_1" %in% rownames(local_fw)
    )

    testthat::expect_true(
      "B_2" %in% rownames(local_fw)
    )

    testthat::expect_false(
      "A_2" %in% rownames(local_fw)
    )

    testthat::expect_false(
      "B_1" %in% rownames(local_fw)
    )

    ## Resource nodes must be retained
    testthat::expect_true(
      all(
        c(
          "zooplankton",
          "benthos"
        ) %in% rownames(local_fw)
      )
    )
  }
)

testthat::test_that(
  "build_local_foodweb errors if required columns are missing",
  {

    ## Missing species_code column
    ind_measure <- data.frame(
      local_id = c("site1", "site2"),
      species  = c("A", "B"),
      size     = c(10, 15)
    )

    metaweb <- matrix(
      0,
      nrow = 1,
      ncol = 1
    )

    dimnames(metaweb) <- list(
      "dummy",
      "dummy"
    )

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
  }
)

testthat::test_that(
  "build_local_foodweb errors when selected_resources is empty",
  {

    ind_measure <- data.frame(
      local_id     = "site1",
      species_code = "A",
      size         = 5
    )

    metaweb <- matrix(
      0,
      nrow = 1,
      ncol = 1
    )

    dimnames(metaweb) <- list(
      "dummy",
      "dummy"
    )

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
  }
)

testthat::test_that(
  "build_local_foodweb builds one web per local unit",
  {

    ## 1. Simple size classes for one species
    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ## 2. Predator-prey window
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

    ## 4. Resource diet shifts
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
      ind_measure        = ind_measure,
      local_id           = "local_id",
      metaweb            = metaweb,
      tab_size_classes   = tab_size_classes,
      selected_resources = "zoopl"
    )

    ## 8. Checks
    testthat::expect_true(
      is.list(res)
    )

    testthat::expect_equal(
      sort(names(res)),
      c(
        "site1",
        "site2"
      )
    )

    testthat::expect_true(
      all(
        vapply(
          res,
          function(x) {
            (is.matrix(x) || is.data.frame(x)) &&
              (nrow(x) == ncol(x)) &&
              identical(
                rownames(x),
                colnames(x)
              )
          },
          logical(1)
        )
      )
    )

    ## site1 contains A_1 but not A_2
    testthat::expect_true(
      "A_1" %in% rownames(res[["site1"]])
    )

    testthat::expect_false(
      "A_2" %in% rownames(res[["site1"]])
    )

    ## site2 contains A_2 but not A_1
    testthat::expect_true(
      "A_2" %in% rownames(res[["site2"]])
    )

    testthat::expect_false(
      "A_1" %in% rownames(res[["site2"]])
    )
  }
)

testthat::test_that(
  "build_local_foodweb assigns boundary values to the correct local nodes",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    pred_win <- data.frame(
      species_code = "A",
      beta_min     = 0.5,
      beta_max     = 1
    )

    fish_diet_shift <- data.frame(
      species_code = "A",
      size_min     = 0,
      size_max     = 20,
      zoopl        = 1,
      fish         = 0
    )

    resource_diet_shift <- data.frame(
      species_code = "zoopl",
      zoopl        = 0
    )

    metaweb <- build_metaweb(
      tab_size_classes    = tab_size_classes,
      pred_win            = pred_win,
      fish_diet_shift     = fish_diet_shift,
      resource_diet_shift = resource_diet_shift,
      num_classes         = 2,
      selected_resources  = "zoopl"
    )

    ind_measure <- data.frame(
      local_id = c(
        "lower_bound",
        "class_boundary",
        "upper_bound"
      ),
      species_code = c(
        "A",
        "A",
        "A"
      ),
      size = c(
        0,
        10,
        20
      )
    )

    res <- build_local_foodweb(
      ind_measure        = ind_measure,
      local_id           = "local_id",
      metaweb            = metaweb,
      tab_size_classes   = tab_size_classes,
      selected_resources = "zoopl"
    )

    ## 0 belongs to the first class: [0, 10)
    testthat::expect_true(
      "A_1" %in%
        rownames(res[["lower_bound"]])
    )

    testthat::expect_false(
      "A_2" %in%
        rownames(res[["lower_bound"]])
    )

    ## 10 belongs to the second class: [10, 20]
    testthat::expect_true(
      "A_2" %in%
        rownames(res[["class_boundary"]])
    )

    testthat::expect_false(
      "A_1" %in%
        rownames(res[["class_boundary"]])
    )

    ## 20 must also belong to the final class
    testthat::expect_true(
      "A_2" %in%
        rownames(res[["upper_bound"]])
    )

    testthat::expect_false(
      "A_1" %in%
        rownames(res[["upper_bound"]])
    )
  }
)
