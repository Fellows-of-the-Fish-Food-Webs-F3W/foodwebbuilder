##BASIC NETWORK METRICS
testthat::test_that(
  "compute_S returns the number of nodes",
  {

    M <- matrix(
      0,
      nrow = 3,
      ncol = 3
    )

    testthat::expect_equal(
      compute_S(M),
      3
    )
  }
)

testthat::test_that(
  "compute_L returns the number of non-zero links",
  {

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE
    )

    testthat::expect_equal(
      compute_L(M),
      2
    )
  }
)

testthat::test_that(
  "compute_L counts links rather than interaction values",
  {

    M <- matrix(
      c(
        0, 2, 0,
        0, 0, 5,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE
    )

    testthat::expect_equal(
      compute_L(M),
      2
    )
  }
)

testthat::test_that(
  "compute_linkage_density returns the number of links per node",
  {

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE
    )

    testthat::expect_equal(
      compute_linkage_density(M),
      2 / 3
    )
  }
)

testthat::test_that(
  "compute_connectance returns the fraction of realized links",
  {

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE
    )

    testthat::expect_equal(
      compute_connectance(M),
      2 / 9
    )

    testthat::expect_equal(
      compute_connectance(
        M,
        exclude_self = TRUE
      ),
      2 / 6
    )
  }
)

testthat::test_that(
  "compute_connectance includes self-links by default",
  {

    M <- matrix(
      c(
        1, 1,
        0, 0
      ),
      nrow = 2,
      byrow = TRUE
    )

    testthat::expect_equal(
      compute_connectance(M),
      2 / 4
    )
  }
)

## BASAL, TOP, AND NODE DEGREE
testthat::test_that(
  "get_basal_nodes identifies nodes with no incoming links",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    testthat::expect_equal(
      unname(get_basal_nodes(M)),
      1
    )
  }
)

testthat::test_that(
  "get_leaf_nodes identifies nodes with no outgoing links",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    testthat::expect_equal(
      unname(get_leaf_nodes(M)),
      3
    )
  }
)

testthat::test_that(
  "compute_inward_degree counts incoming links",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 1,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    testthat::expect_equal(
      compute_inward_degree(M),
      c(
        resource = 0,
        A = 1,
        B = 2
      )
    )
  }
)

testthat::test_that(
  "compute_outward_degree counts outgoing links",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 1,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    testthat::expect_equal(
      compute_outward_degree(M),
      c(
        resource = 2,
        A = 1,
        B = 0
      )
    )
  }
)

testthat::test_that(
  "degree functions count non-zero links rather than interaction values",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 2, 5,
        0, 0, 3,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    testthat::expect_equal(
      compute_inward_degree(M),
      c(
        resource = 0,
        A = 1,
        B = 2
      )
    )

    testthat::expect_equal(
      compute_outward_degree(M),
      c(
        resource = 2,
        A = 1,
        B = 0
      )
    )
  }
)

## TROPHIC LEVEL
testthat::test_that(
  "compute_trophic_level returns expected trophic levels",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    ## resource -> A -> B
    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    TL <- compute_trophic_level(M)

    testthat::expect_equal(
      TL,
      c(
        resource = 1,
        A = 2,
        B = 3
      ),
      tolerance = 0.01
    )
  }
)

testthat::test_that(
  "compute_trophic_level retains node names",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    TL <- compute_trophic_level(M)

    testthat::expect_identical(
      names(TL),
      node_names
    )
  }
)

## TROPHIC BREADTH
testthat::test_that(
  "compute_trophic_breadth returns zero for basal nodes and single-resource consumers",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    ## resource -> A -> B
    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    TL <- c(
      resource = 1,
      A = 2,
      B = 3
    )

    TB <- compute_trophic_breadth(
      M,
      TL
    )

    testthat::expect_equal(
      TB,
      c(
        resource = 0,
        A = 0,
        B = 0
      )
    )
  }
)

testthat::test_that(
  "compute_trophic_breadth measures variation in resource trophic levels",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    ## resource -> A
    ## resource -> B
    ## A        -> B
    ##
    ## B therefore consumes resources at TL 1 and TL 2.
    M <- matrix(
      c(
        0, 1, 1,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    TL <- compute_trophic_level(M)

    TB <- compute_trophic_breadth(
      M,
      TL
    )

    testthat::expect_equal(
      unname(TB["resource"]),
      0
    )

    testthat::expect_equal(
      unname(TB["A"]),
      0
    )

    testthat::expect_equal(
      unname(TB["B"]),
      stats::sd(c(1, 2)),
      tolerance = 0.01
    )
  }
)

## OMNIVORY INDEX
testthat::test_that(
  "compute_omnivory_index returns zero for basal nodes and single-resource consumers",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    TL <- c(
      resource = 1,
      A = 2,
      B = 3
    )

    OI <- compute_omnivory_index(
      M,
      TL
    )

    testthat::expect_equal(
      OI,
      c(
        resource = 0,
        A = 0,
        B = 0
      )
    )
  }
)

testthat::test_that(
  "compute_omnivory_index measures variance in resource trophic levels",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    ## resource -> A
    ## resource -> B
    ## A        -> B
    M <- matrix(
      c(
        0, 1, 1,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    TL <- compute_trophic_level(M)

    OI <- compute_omnivory_index(
      M,
      TL
    )

    testthat::expect_equal(
      unname(OI["resource"]),
      0
    )

    testthat::expect_equal(
      unname(OI["A"]),
      0
    )

    testthat::expect_equal(
      unname(OI["B"]),
      stats::var(c(1, 2)),
      tolerance = 0.01
    )
  }
)

## BOTTOM-UP FLUXES
testthat::test_that(
  "compute_bottom_up_fluxes returns a matrix with expected dimensions",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    fluxes <- compute_bottom_up_fluxes(
      M,
      nIt = 10
    )

    testthat::expect_true(
      is.matrix(fluxes)
    )

    testthat::expect_equal(
      dim(fluxes),
      c(3L, 3L)
    )

    testthat::expect_true(
      all(is.finite(fluxes))
    )
  }
)

## SUMMARY METRICS
testthat::test_that(
  "compute_metrics_summary returns expected metric names",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    ## resource -> A -> B
    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    metrics <- compute_metrics_summary(M)

    testthat::expect_identical(
      names(metrics),
      c(
        "S",
        "L",
        "L/S",
        "C",
        "meanTL",
        "maxTL",
        "meanTB",
        "maxTB",
        "meanOI",
        "fracBase",
        "fracTop",
        "fracInt"
      )
    )
  }
)

testthat::test_that(
  "compute_metrics_summary returns expected values for a simple food chain",
  {

    node_names <- c(
      "resource",
      "A",
      "B"
    )

    ## resource -> A -> B
    M <- matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        node_names,
        node_names
      )
    )

    metrics <- compute_metrics_summary(M)

    testthat::expect_equal(
      unname(metrics["S"]),
      3
    )

    testthat::expect_equal(
      unname(metrics["L"]),
      2
    )

    testthat::expect_equal(
      unname(metrics["L/S"]),
      2 / 3
    )

    testthat::expect_equal(
      unname(metrics["C"]),
      2 / 9
    )

    testthat::expect_equal(
      unname(metrics["meanTL"]),
      2,
      tolerance = 0.01
    )

    testthat::expect_equal(
      unname(metrics["maxTL"]),
      3,
      tolerance = 0.01
    )

    testthat::expect_equal(
      unname(metrics["meanTB"]),
      0
    )

    testthat::expect_equal(
      unname(metrics["maxTB"]),
      0
    )

    testthat::expect_equal(
      unname(metrics["meanOI"]),
      0
    )

    testthat::expect_equal(
      unname(metrics["fracBase"]),
      1 / 3
    )

    testthat::expect_equal(
      unname(metrics["fracTop"]),
      1 / 3
    )

    testthat::expect_equal(
      unname(metrics["fracInt"]),
      1 / 3
    )
  }
)

## FISH NODE-LEVEL DATA
testthat::test_that(
  "compute_node_metrics returns metrics only for fish trophic species",
  {

    ## Two fish species, each divided into two size classes
    tab_size_classes <- data.frame(
      species_code  = c("A", "B"),
      lower_bound   = c(0, 0),
      upper_bound_1 = c(10, 10),
      upper_bound_2 = c(20, 20)
    )

    ## Complete local food web:
    ##
    ## resource -> A_1
    ## resource -> B_1
    ## A_1     -> A_2
    ## B_1     -> A_2
    ##
    ## Rows = prey, columns = consumers
    node_names <- c(
      "resource",
      "A_1",
      "A_2",
      "B_1"
    )

    M <- matrix(
      0,
      nrow = 4,
      ncol = 4,
      dimnames = list(
        node_names,
        node_names
      )
    )

    M["resource", "A_1"] <- 1
    M["resource", "B_1"] <- 1
    M["A_1", "A_2"] <- 1
    M["B_1", "A_2"] <- 1

    ## Individual fish observations
    ind_measure <- data.frame(
      species_code = c(
        "A",
        "A",
        "A",
        "B",
        "B"
      ),
      size = c(
        5,
        8,
        15,
        4,
        7
      ),
      weight = c(
        10,
        20,
        50,
        5,
        15
      )
    )

    res <- compute_node_metrics(
      M = M,
      ind_measure = ind_measure,
      tab_size_classes = tab_size_classes
    )

    ##General structure
    testthat::expect_s3_class(
      res,
      "data.frame"
    )

    testthat::expect_identical(
      names(res),
      c(
        "trophic_species",
        "abundance",
        "biomass_g",
        "in_degree",
        "out_degree",
        "degree",
        "TL",
        "TB",
        "OI"
      )
    )

    ##Resource nodes must not be returned
    testthat::expect_false(
      "resource" %in% res$trophic_species
    )

    ##Only fish nodes present in M must be returned
    testthat::expect_identical(
      res$trophic_species,
      c(
        "A_1",
        "A_2",
        "B_1"
      )
    )

    ##Abundance
    testthat::expect_equal(
      res$abundance,
      c(
        2,
        1,
        2
      )
    )


    ##Biomass
    testthat::expect_equal(
      res$biomass_g,
      c(
        30,
        50,
        20
      )
    )

    ##Degree
    testthat::expect_equal(
      res$in_degree,
      c(
        1,
        2,
        1
      )
    )

    testthat::expect_equal(
      res$out_degree,
      c(
        1,
        0,
        1
      )
    )

    testthat::expect_equal(
      res$degree,
      c(
        2,
        2,
        2
      )
    )


    ##Trophic level
    ##
    ## resource = 1
    ## A_1 = 2
    ## B_1 = 2
    ## A_2 = 3
    testthat::expect_equal(
      res$TL,
      c(
        2,
        3,
        2
      ),
      tolerance = 0.01
    )

    ##Trophic breadth and omnivory
    ##
    ## A_1 consumes only resource -> TB = 0, OI = 0
    ## A_2 consumes A_1 and B_1, both TL = 2 -> TB = 0, OI = 0
    ## B_1 consumes only resource -> TB = 0, OI = 0
    testthat::expect_equal(
      res$TB,
      c(
        0,
        0,
        0
      ),
      tolerance = 0.01
    )

    testthat::expect_equal(
      res$OI,
      c(
        0,
        0,
        0
      ),
      tolerance = 0.01
    )
  }
)

testthat::test_that(
  "compute_node_metrics uses resource nodes when computing trophic metrics",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    ## resource -> A_1
    ## resource -> A_2
    ## A_1     -> A_2
    ##
    ## A_2 consumes prey at TL 1 and 2.
    node_names <- c(
      "resource",
      "A_1",
      "A_2"
    )

    M <- matrix(
      0,
      nrow = 3,
      ncol = 3,
      dimnames = list(
        node_names,
        node_names
      )
    )

    M["resource", "A_1"] <- 1
    M["resource", "A_2"] <- 1
    M["A_1", "A_2"] <- 1

    ind_measure <- data.frame(
      species_code = c(
        "A",
        "A"
      ),
      size = c(
        5,
        15
      ),
      weight = c(
        10,
        20
      )
    )

    res <- compute_node_metrics(
      M = M,
      ind_measure = ind_measure,
      tab_size_classes = tab_size_classes
    )

    ##Resource must not appear in the output
    testthat::expect_false(
      "resource" %in% res$trophic_species
    )

    ##A_1 consumes the basal resource only
    testthat::expect_equal(
      res$TL[
        res$trophic_species == "A_1"
      ],
      2,
      tolerance = 0.01
    )

    ##A_2 consumes resource (TL 1) and A_1 (TL 2)
    testthat::expect_equal(
      res$TL[
        res$trophic_species == "A_2"
      ],
      2.5,
      tolerance = 0.01
    )

    testthat::expect_equal(
      res$TB[
        res$trophic_species == "A_2"
      ],
      stats::sd(c(1, 2)),
      tolerance = 0.01
    )

    testthat::expect_equal(
      res$OI[
        res$trophic_species == "A_2"
      ],
      stats::var(c(1, 2)),
      tolerance = 0.01
    )
  }
)

testthat::test_that(
  "compute_node_metrics returns NA biomass when weight is absent",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    node_names <- c(
      "resource",
      "A_1",
      "A_2"
    )

    M <- matrix(
      0,
      nrow = 3,
      ncol = 3,
      dimnames = list(
        node_names,
        node_names
      )
    )

    M["resource", "A_1"] <- 1
    M["A_1", "A_2"] <- 1

    ind_measure <- data.frame(
      species_code = c(
        "A",
        "A",
        "A"
      ),
      size = c(
        5,
        8,
        15
      )
    )

    res <- compute_node_metrics(
      M = M,
      ind_measure = ind_measure,
      tab_size_classes = tab_size_classes
    )

    testthat::expect_equal(
      res$abundance,
      c(
        2,
        1
      )
    )

    testthat::expect_true(
      all(is.na(res$biomass_g))
    )
  }
)

testthat::test_that(
  "compute_node_metrics handles missing individual weights",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    node_names <- c(
      "resource",
      "A_1",
      "A_2"
    )

    M <- matrix(
      0,
      nrow = 3,
      ncol = 3,
      dimnames = list(
        node_names,
        node_names
      )
    )

    M["resource", "A_1"] <- 1
    M["A_1", "A_2"] <- 1

    ind_measure <- data.frame(
      species_code = c(
        "A",
        "A",
        "A",
        "A"
      ),
      size = c(
        5,
        8,
        15,
        18
      ),
      weight = c(
        10,
        NA,
        NA,
        NA
      )
    )

    res <- compute_node_metrics(
      M = M,
      ind_measure = ind_measure,
      tab_size_classes = tab_size_classes
    )

    ##A_1 has one known and one missing weight
    testthat::expect_equal(
      res$biomass_g[
        res$trophic_species == "A_1"
      ],
      10
    )

    ## All A_2 weights are missing
    testthat::expect_true(
      is.na(
        res$biomass_g[
          res$trophic_species == "A_2"
        ]
      )
    )
  }
)

testthat::test_that(
  "compute_node_metrics includes the final upper size bound",
  {

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10,
      upper_bound_2 = 20
    )

    node_names <- c(
      "resource",
      "A_2"
    )

    M <- matrix(
      0,
      nrow = 2,
      ncol = 2,
      dimnames = list(
        node_names,
        node_names
      )
    )

    M["resource", "A_2"] <- 1

    ind_measure <- data.frame(
      species_code = "A",
      size = 20,
      weight = 50
    )

    res <- compute_node_metrics(
      M = M,
      ind_measure = ind_measure,
      tab_size_classes = tab_size_classes
    )

    testthat::expect_identical(
      res$trophic_species,
      "A_2"
    )

    testthat::expect_equal(
      res$abundance,
      1
    )

    testthat::expect_equal(
      res$biomass_g,
      50
    )
  }
)

## COMPUTE_NODE_METRICS
testthat::test_that(
  "compute_node_metrics errors when M is not a matrix or data frame",
  {

    ind_measure <- data.frame(
      species_code = "A",
      size = 5
    )

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10
    )

    testthat::expect_error(
      compute_node_metrics(
        M = c(0, 1),
        ind_measure = ind_measure,
        tab_size_classes = tab_size_classes
      ),
      "M must be a matrix or data frame"
    )
  }
)

testthat::test_that(
  "compute_node_metrics errors for a non-square matrix",
  {

    M <- matrix(
      0,
      nrow = 2,
      ncol = 3
    )

    ind_measure <- data.frame(
      species_code = "A",
      size = 5
    )

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10
    )

    testthat::expect_error(
      compute_node_metrics(
        M = M,
        ind_measure = ind_measure,
        tab_size_classes = tab_size_classes
      ),
      "M must be square"
    )
  }
)

testthat::test_that(
  "compute_node_metrics errors when matrix dimnames are missing",
  {

    M <- matrix(
      0,
      nrow = 2,
      ncol = 2
    )

    ind_measure <- data.frame(
      species_code = "A",
      size = 5
    )

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10
    )

    testthat::expect_error(
      compute_node_metrics(
        M = M,
        ind_measure = ind_measure,
        tab_size_classes = tab_size_classes
      ),
      "M must have identical row and column names"
    )
  }
)

testthat::test_that(
  "compute_node_metrics errors when matrix dimnames are inconsistent",
  {

    M <- matrix(
      0,
      nrow = 2,
      ncol = 2,
      dimnames = list(
        c(
          "resource",
          "A_1"
        ),
        c(
          "resource",
          "wrong_name"
        )
      )
    )

    ind_measure <- data.frame(
      species_code = "A",
      size = 5
    )

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10
    )

    testthat::expect_error(
      compute_node_metrics(
        M = M,
        ind_measure = ind_measure,
        tab_size_classes = tab_size_classes
      ),
      "M must have identical row and column names"
    )
  }
)

testthat::test_that(
  "compute_node_metrics errors when species_code is missing",
  {

    node_names <- c(
      "resource",
      "A_1"
    )

    M <- matrix(
      0,
      nrow = 2,
      ncol = 2,
      dimnames = list(
        node_names,
        node_names
      )
    )

    ind_measure <- data.frame(
      size = 5
    )

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10
    )

    testthat::expect_error(
      compute_node_metrics(
        M = M,
        ind_measure = ind_measure,
        tab_size_classes = tab_size_classes
      ),
      "ind_measure must contain"
    )
  }
)

testthat::test_that(
  "compute_node_metrics errors when size is missing",
  {

    node_names <- c(
      "resource",
      "A_1"
    )

    M <- matrix(
      0,
      nrow = 2,
      ncol = 2,
      dimnames = list(
        node_names,
        node_names
      )
    )

    ind_measure <- data.frame(
      species_code = "A"
    )

    tab_size_classes <- data.frame(
      species_code  = "A",
      lower_bound   = 0,
      upper_bound_1 = 10
    )

    testthat::expect_error(
      compute_node_metrics(
        M = M,
        ind_measure = ind_measure,
        tab_size_classes = tab_size_classes
      ),
      "ind_measure must contain"
    )
  }
)
