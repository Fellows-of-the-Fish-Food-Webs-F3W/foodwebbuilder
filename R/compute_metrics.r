#######################
## compute_metrics.r ##
#######################

## Goal: Define a set of functions to compute metrics of trophic networks.

###############
## FUNCTIONS ##
###############

#' Species richness (S)
#'
#' Number of species (nodes) in the food web.
#'
#' @param M A square adjacency or interaction matrix.
#' @return Integer, number of species.
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
#' Average number of links per species.
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
  return(which(apply(M, 2, sum) == 0))
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
  return(which(apply(M, 1, sum) == 0))
}

#' Compute inward degree
#'
#' Computes the inward degree (column sums) of each node.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A numeric vector of inward degrees.
#' @export
compute_inward_degree = function(M){
  return(apply(M, 2, sum))
}

#' Compute outward degree
#'
#' Computes the outward degree (row sums) of each node.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A numeric vector of outward degrees.
#' @export
compute_outward_degree = function(M){
  return(apply(M, 1, sum))
}

#' Compute trophic breadth
#'
#' Computes the trophic breadth of each node as the standard deviation
#' of trophic levels of its resources.
#'
#' @param M A square adjacency or interaction matrix.
#' @param TL A numeric vector of trophic levels.
#' @return A numeric vector of trophic breadth values.
#' @export
compute_trophic_breadth = function(M, TL){
  return(apply(M * TL, 2, sd))
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
#' @return A numeric vector of trophic levels.
#' @export
compute_trophic_level = function(M, nIt=100){
  
  ## Initialise
  d = ncol(M)
  TL = rep(0,d)
  for (k in 1:nIt){
    
    ## Update trophic level vector
    TL_old = TL
    for (j in 1:d){
      denom = sum(M[,j])
      if (denom > 0){ 
        TL[j] = 1 + 1/denom*sum(M[,j] * TL)
      }else {
        TL[j] = 1
      }
    }
    
    ## Check convergence
    loss = mean((TL_old - TL)^2)
    if (loss <= 0.001){
      message(paste("Converged after", k, "iterations."))
      break
    }
    
  }
  
  ## Check convergence end
  if (k == nIt) message(paste("No convergence in",k,"iterations, consider increasing nIt."))
  return(TL)
  
}

#' Compute summary food web metrics
#'
#' Computes a set of standard summary metrics describing the structure
#' of a food web, including size, connectance, trophic structure,
#' trophic breadth, omnivory, and node type composition.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A named numeric vector containing species richness (S),
#' number of links (L), linkage density (L/S), connectance (C),
#' mean and maximum trophic level, mean and maximum trophic breadth,
#' mean omnivory index, and fractions of basal, top, and intermediate nodes.
#' @export
compute_metrics_summary = function(M){
  
  ## Compute basic metrics
  S = compute_S(metaweb)
  L = compute_L(metaweb)
  linkage_density = compute_linkage_density(metaweb)
  C = compute_connectance(metaweb)
  
  ## Compute fraction of basal, intermediate, and top nodes
  frac_basal = length(get_basal_nodes(metaweb))/S
  frac_leaf = length(get_leaf_nodes(metaweb))/S
  frac_intermediate = 1 - (frac_basal + frac_leaf)
  
  ## Compute trophic level and trophic breadth
  TL = compute_trophic_level(metaweb)
  TB = compute_trophic_breadth(metaweb, TL)

  ## Compute degree of omnivory
  OI = compute_trophic_breadth(metaweb, TL)
  
  ## Collect
  metrics = c(S, L, linkage_density, C,
              mean(TL), max(TL), 
              mean(TB), max(TB), 
              mean(OI), 
              frac_basal, frac_leaf, frac_intermediate)
  names(metrics) = c("S", "L", "L/S", "C", "meanTL", "maxTL", "meanTB", "maxTB", "meanOI", "fracBase", "fracTop", "fracInt")
  
  ## Return
  return(metrics)
  
}

#
###