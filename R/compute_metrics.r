#######################
## compute_metrics.r ##
#######################

## Goal: Define a set of functions to compute metrics of trophic networks.

###############
## FUNCTIONS ##
###############
#' Number of nodes (S)
#'
#' Number of nodes in the food web.
#'
#' @param M A square adjacency or interaction matrix.
#' @return Integer, number of nodes.
#' @export
compute_S = function(M){
  return(nrow(M))
}

#' Number of links (L)
#'
#' Total number of feeding interactions in the food web.
#'
#' @param M A square adjacency or interaction matrix.
#' @return Integer, number of links.
#' @export
compute_L = function(M){
  return(sum(M != 0))
}

#' Linkage density (L/S)
#'
#' Average number of links per node.
#'
#' @param M A square adjacency or interaction matrix.
#' @return Numeric, linkage density.
#' @export
compute_linkage_density = function(M){
  S = compute_S(M)
  L = compute_L(M)
  return(L / S)
}

#' Connectance (C)
#'
#' Fraction of realized links out of all possible links.
#'
#' @param M A square adjacency or interaction matrix.
#' @param exclude_self Logical, whether to exclude self-links.
#' @return Numeric, connectance.
#' @export
compute_connectance = function(M, exclude_self = FALSE){
  S = compute_S(M)
  L = compute_L(M)

  if(exclude_self){
    return(L / (S * (S - 1)))
  } else {
    return(L / (S^2))
  }
}

#' Identify basal nodes
#'
#' Returns the indices of basal nodes in an interaction matrix.
#' Basal nodes are those with no incoming links (column sum equal to zero).
#'
#' @param M A square adjacency or interaction matrix.
#' @return An integer vector of indices corresponding to basal nodes.
#' @export
get_basal_nodes = function(M){
  return(which(apply(M != 0, 2, sum) == 0))
}

#' Identify leaf nodes
#'
#' Returns the indices of leaf nodes in an interaction matrix.
#' Leaf nodes are those with no outgoing links (row sum equal to zero).
#'
#' @param M A square adjacency or interaction matrix.
#' @return An integer vector of indices corresponding to leaf nodes.
#' @export
get_leaf_nodes = function(M){
  return(which(apply(M != 0, 1, sum) == 0))
}

#' Compute inward degree
#'
#' Computes the inward degree of each node as the number of incoming links.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A numeric vector of inward degrees.
#' @export
compute_inward_degree = function(M){
  return(apply(M != 0, 2, sum))
}

#' Compute outward degree
#'
#' Computes the outward degree of each node as the number of outgoing links.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A numeric vector of outward degrees.
#' @export
compute_outward_degree = function(M){
  return(apply(M != 0, 1, sum))
}

#' Compute trophic breadth
#'
#' Computes the trophic breadth of each node as the standard deviation
#' of trophic levels of its resources.
#'
#' @param M A square adjacency or interaction matrix.
#' @param TL A numeric vector of trophic levels.
#' @return A numeric vector of trophic breadth values. Basal nodes and
#'   consumers with a single resource have a trophic breadth of 0.
#' @export
compute_trophic_breadth = function(M, TL){

  TB = numeric(ncol(M))

  for (j in seq_len(ncol(M))) {

    prey = which(M[,j] != 0)

    if (length(prey) <= 1) {
      TB[j] = 0
    } else {
      TB[j] = stats::sd(TL[prey])
    }
  }

  names(TB) = colnames(M)

  return(TB)
}

#' Compute omnivory index
#'
#' Computes the omnivory index of each node as the variance in trophic
#' levels among its resources.
#'
#' @param M A square adjacency or interaction matrix. Rows correspond to prey
#'   and columns to consumers.
#' @param TL A numeric vector of trophic levels.
#' @return A numeric vector of omnivory index values. Basal nodes and
#'   consumers with a single resource have an omnivory index of 0.
#' @export
compute_omnivory_index = function(M, TL){

  OI = numeric(ncol(M))

  for (j in seq_len(ncol(M))) {

    prey = which(M[,j] != 0)

    if (length(prey) <= 1) {
      OI[j] = 0
    } else {
      OI[j] = stats::var(TL[prey])
    }
  }

  names(OI) = colnames(M)

  return(OI)
}

#' Compute bottom-up fluxes
#'
#' Simulates bottom-up biomass fluxes through a trophic network
#' using an iterative procedure.
#'
#' @param M A square adjacency or interaction matrix.
#' @param nIt Number of iterations for the simulation.
#' @return A matrix of accumulated fluxes.
#' @export
compute_bottom_up_fluxes = function(M, nIt=100){

  ## Initiate
  d = ncol(M)

  ## Check for basal nodes
  check_basal = which(apply(M, 2, sum) == 0)

  ## Check for leaf nodes
  check_leaf = which(apply(M, 1, sum) == 0)

  ## Compute diet matrix
  D = M / apply(M, 1, sum)

  ## Set leaf nodes to zero
  D[check_leaf,] = 0

  ## Initialise biomass vector
  B = rep(0, d)
  B[check_basal] = 1

  ## Simulate biomass fluxes
  fluxes = D

  for (k in 1:nIt)
  {
    fluxes = fluxes + D * as.vector(B)
    B = t(D) %*% B
  }

  return(fluxes)
}

#' Compute trophic level
#'
#' Computes trophic levels iteratively until convergence or until
#' a maximum number of iterations is reached.
#'
#' @param M A square adjacency or interaction matrix.
#' @param nIt Maximum number of iterations.
#' @param output_log Whether to print the number of iterations until convergence.
#' @return A named numeric vector of trophic levels, with names corresponding
#'   to matrix column names when available.
#' @export
compute_trophic_level = function(M, nIt=100, output_log=FALSE){

  ## Initialise
  d = ncol(M)
  TL = rep(0, d)

  for (k in seq_len(nIt)){

    ## Update trophic level vector
    TL_old = TL

    for (j in seq_len(d)){

      denom = sum(M[,j])

      if (denom > 0){
        TL[j] = 1 + 1 / denom * sum(M[,j] * TL)
      } else {
        TL[j] = 1
      }
    }

    ## Check convergence
    loss = mean((TL_old - TL)^2)

    if (loss <= 0.001){

      if (isTRUE(output_log)) {
        message(
          paste(
            "Converged after",
            k,
            "iterations."
          )
        )
      }

      break
    }
  }

  ## Check convergence end
  if (k == nIt && loss > 0.001) {
    message(
      paste(
        "No convergence in",
        k,
        "iterations, consider increasing nIt."
      )
    )
  }

  ## Retain node names
  names(TL) = colnames(M)

  return(TL)
}


#' Compute summary food web metrics
#'
#' Computes a set of standard summary metrics describing the structure
#' of a food web, including size, connectance, trophic structure,
#' trophic breadth, omnivory, and node type composition.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A named numeric vector containing number of nodes (S),
#' number of links (L), linkage density (L/S), connectance (C),
#' mean and maximum trophic level, mean and maximum trophic breadth,
#' mean omnivory index, and fractions of basal, top, and intermediate nodes.
#' @export
compute_metrics_summary = function(M){

  ## Compute basic metrics
  S = compute_S(M)
  L = compute_L(M)
  linkage_density = compute_linkage_density(M)
  C = compute_connectance(M)

  ## Compute fraction of basal, intermediate, and top nodes
  frac_basal = length(get_basal_nodes(M)) / S
  frac_leaf = length(get_leaf_nodes(M)) / S
  frac_intermediate = 1 - (frac_basal + frac_leaf)

  ## Compute trophic level and trophic breadth
  TL = compute_trophic_level(M)
  TB = compute_trophic_breadth(M, TL)

  ## Compute degree of omnivory
  OI = compute_omnivory_index(M, TL)

  ## Collect
  metrics = c(
    S,
    L,
    linkage_density,
    C,
    mean(TL),
    max(TL),
    mean(TB),
    max(TB),
    mean(OI),
    frac_basal,
    frac_leaf,
    frac_intermediate
  )

  names(metrics) = c(
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

  ## Return
  return(metrics)
}


#' Compute fish trophic-species metrics
#'
#' Computes node-level trophic metrics, abundance, and biomass for fish
#' trophic species in a local food web.
#'
#' @description
#' Fish trophic species are defined as species-by-size-class combinations.
#' Network metrics are calculated from the complete local food web, including
#' resource nodes, but only fish trophic species are returned in the output.
#'
#' @param M A square local food-web adjacency matrix. Rows correspond to prey
#'   and columns to consumers.
#' @param ind_measure A data frame containing individual fish observations for
#'   one sampling operation. Must contain `species_code` and `size`.
#'   An optional `weight` column can be provided to calculate fish biomass.
#' @param tab_size_classes A data frame defining fish size classes, typically
#'   produced by [compute_size_classes()].
#'
#' @return A data frame with one row per fish trophic species present in the
#'   local food web and the following columns:
#' \describe{
#'   \item{trophic_species}{Fish trophic-species code.}
#'   \item{abundance}{Number of fish individuals assigned to the trophic species.}
#'   \item{biomass_g}{Total fish biomass in grams when individual weights are
#'     available; otherwise `NA`.}
#'   \item{in_degree}{Number of incoming links of the trophic species.}
#'   \item{out_degree}{Number of outgoing links of the trophic species.}
#'   \item{degree}{Total degree, calculated as inward plus outward degree.}
#'   \item{TL}{Trophic level.}
#'   \item{TB}{Trophic breadth.}
#'   \item{OI}{Omnivory index.}
#' }
#'
#' @details
#' Resource nodes are retained when calculating network metrics because they
#' contribute to the trophic position and diet of fish nodes. Resource nodes
#' are excluded only from the returned table.
#'
#' Abundance corresponds to the number of individual observations assigned to
#' each fish trophic species. Biomass is calculated as the sum of individual
#' weights assigned to each trophic species when a `weight` column is present.
#' If no `weight` column is provided, biomass is returned as `NA`.
#'
#' @export
compute_node_metrics = function(M,
                                ind_measure,
                                tab_size_classes){

  ##Validate food-web matrix
  if (!is.matrix(M) && !is.data.frame(M)) {
    stop(
      "M must be a matrix or data frame.",
      call. = FALSE
    )
  }

  M = as.matrix(M)

  if (nrow(M) != ncol(M)) {
    stop(
      "M must be square.",
      call. = FALSE
    )
  }

  if (
    is.null(rownames(M)) ||
    is.null(colnames(M)) ||
    !identical(rownames(M), colnames(M))
  ) {
    stop(
      "M must have identical row and column names.",
      call. = FALSE
    )
  }

  ##Validate individual measurements
  .assert_has_cols(
    ind_measure,
    c(
      "species_code",
      "size"
    ),
    "ind_measure"
  )

  has_weight = "weight" %in% colnames(ind_measure)

  ##Identify fish trophic species
  num_classes = .infer_num_classes(
    tab_size_classes
  )

  all_fish_nodes = .build_trophic_species(
    tab_size_classes,
    num_classes
  )

  fish_nodes = intersect(
    colnames(M),
    all_fish_nodes
  )

  ##Compute network metrics on the complete food web
  in_degree = compute_inward_degree(M)
  out_degree = compute_outward_degree(M)

  TL = compute_trophic_level(M)
  TB = compute_trophic_breadth(M, TL)
  OI = compute_omnivory_index(M, TL)

  ##Assign individuals to fish trophic species
  individual_trophic_species = .assign_trophic_species(
    ind_measure,
    tab_size_classes
  )


  ##Compute abundance
  abundance = table(
    factor(
      individual_trophic_species[
        !is.na(individual_trophic_species)
      ],
      levels = fish_nodes
    )
  )


  ##Compute biomass
  if (has_weight) {

    biomass = vapply(
      fish_nodes,
      function(node) {

        idx = !is.na(individual_trophic_species) &
          individual_trophic_species == node

        weights = ind_measure$weight[idx]

        ##A trophic species with individuals but no available
        ##individual weights has unknown biomass.
        if (
          length(weights) > 0L &&
          all(is.na(weights))
        ) {
          return(NA_real_)
        }

        sum(
          weights,
          na.rm = TRUE
        )
      },
      numeric(1)
    )

  } else {

    biomass = rep(
      NA_real_,
      length(fish_nodes)
    )

    names(biomass) = fish_nodes
  }

  ##Assemble fish-node table
  out = data.frame(
    trophic_species = fish_nodes,
    abundance = as.numeric(abundance),
    biomass_g = as.numeric(biomass),
    in_degree = as.numeric(
      in_degree[fish_nodes]
    ),
    out_degree = as.numeric(
      out_degree[fish_nodes]
    ),
    degree = as.numeric(
      in_degree[fish_nodes] +
        out_degree[fish_nodes]
    ),
    TL = as.numeric(
      TL[fish_nodes]
    ),
    TB = as.numeric(
      TB[fish_nodes]
    ),
    OI = as.numeric(
      OI[fish_nodes]
    ),
    stringsAsFactors = FALSE
  )

  ##Return
  return(out)
}
