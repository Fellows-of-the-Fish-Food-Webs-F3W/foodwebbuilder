#####################
## plot_networks.R ##
#####################

## Goal: Define a set of functions to visualise trophic networks

###############
## FUNCTIONS ##
###############

#' Identify basal nodes
#'
#' Returns the indices of basal nodes in an interaction matrix.
#' Basal nodes are those with no incoming links (column sum equal to zero).
#'
#' @param M A square adjacency or interaction matrix.
#' @return An integer vector of indices corresponding to basal nodes.
#' @export
get_basal_nodes <- function(M) {
  which(apply(M, 2, sum) == 0)
}

#' Identify leaf nodes
#'
#' Returns the indices of leaf nodes in an interaction matrix.
#' Leaf nodes are those with no outgoing links (row sum equal to zero).
#'
#' @param M A square adjacency or interaction matrix.
#' @return An integer vector of indices corresponding to leaf nodes.
#' @export
get_leaf_nodes <- function(M) {
  which(apply(M, 1, sum) == 0)
}

#' Compute inward degree
#'
#' Computes the inward degree (column sums) of each node.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A numeric vector of inward degrees.
#' @export
compute_inward_degree <- function(M) {
  apply(M, 2, sum)
}

#' Compute outward degree
#'
#' Computes the outward degree (row sums) of each node.
#'
#' @param M A square adjacency or interaction matrix.
#' @return A numeric vector of outward degrees.
#' @export
compute_outward_degree <- function(M) {
  apply(M, 1, sum)
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
compute_trophic_breadth <- function(M, TL) {
  apply(M * TL, 2, stats::sd)
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
compute_bottom_up_fluxes <- function(M, nIt = 100) {

  ## Initiate
  d <- ncol(M)

  ## Check for basal nodes
  check_basal <- which(apply(M, 2, sum) == 0)

  ## Check for leaf nodes
  check_leaf <- which(apply(M, 1, sum) == 0)

  ## Compute diet matrix
  D <- M / apply(M, 1, sum)

  ## Set leaf nodes to zero
  D[check_leaf, ] <- 0

  ## Initialise biomass vector
  B <- rep(0, d)
  B[check_basal] <- 1

  ## Simulate biomass fluxes
  fluxes <- D

  for (k in seq_len(nIt)) {
    fluxes <- fluxes + D * as.vector(B)
    B <- t(D) %*% B
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
compute_trophic_level <- function(M, nIt = 100) {

  ## Initialise
  d <- ncol(M)
  TL <- rep(0, d)

  for (k in seq_len(nIt)) {

    ### Update trophic level vector
    TL_old <- TL
    for (j in seq_len(d)) {
      denom <- sum(M[, j])
      if (denom > 0) {
        TL[j] <- 1 + (1 / denom) * sum(M[, j] * TL)
      } else {
        TL[j] <- 1
      }
    }

    ## Check convergence
    loss <- mean((TL_old - TL)^2)
    if (loss <= 0.001) {
      message(paste("Converged after", k, "iterations."))
      break
    }
  }

  ## Check convergence end
  if (k == nIt) {
    message(paste("No convergence in", k, "iterations, consider increasing nIt."))
  }

  TL
}

#' Plot trophic network
#'
#' Plots a trophic interaction network using provided or default
#' node coordinates.
#'
#' @param M A square adjacency or interaction matrix.
#' @param x Optional numeric vector of x-coordinates.
#' @param y Optional numeric vector of y-coordinates.
#' @param labels Optional node labels.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param line_width_max Maximum line width for edges.
#' @param label_space_x Horizontal spacing for labels.
#' @param add_legend Position of legend or "off" to disable.
#' @return Invisibly returns NULL.
#' @export
plot_network <- function(M,
                         x = NULL,
                         y = NULL,
                         labels = NULL,
                         xlab = "",
                         ylab = "",
                         line_width_max = 1,
                         label_space_x = 0.05,
                         add_legend = "topright") {

  ## Dimensions
  d <- dim(M)[1]

  ## Labels
  if (is.null(labels)) labels <- seq_len(d)

  ## Axes
  if (is.null(x)) x <- cos((seq_len(d)) / 0.25)
  if (is.null(y)) y <- sin((seq_len(d)) / 0.25)

  ## Graphical parameters
  delta_x <- (max(x) - min(x)) * label_space_x

  ## Main plotting area
  graphics::plot(
    x, y,
    xlim = c(min(x) - delta_x, max(x) + delta_x * 1.5),
    cex = 0,
    bty = "l",
    xlab = xlab,
    ylab = ylab,
    cex.lab = 1.5
  )

  ## Interactions
  cols <- grDevices::rainbow(ncol(M))
  denom <- max(abs(M))
  if (denom == 0) denom <- 1

  for (j in seq_len(ncol(M))) {
    color_ <- cols[j]
    for (i in seq_len(nrow(M))) {
      line_width <- abs(M[i, j]) / denom * line_width_max
      graphics::lines(
        x = c(x[i], x[j] - delta_x / 2),
        y = c(y[i], y[j]),
        col = color_,
        lwd = line_width
      )
      graphics::arrows(
        x0 = x[i], x1 = x[j] - delta_x / 2,
        y0 = y[i], y1 = y[j],
        col = color_,
        lwd = line_width
      )
    }
  }

  ## Labels and legend
  graphics::points(x - delta_x / 2, y, pch = 1, cex = 2)
  graphics::points(x, y, pch = 16, cex = 2)
  graphics::text(x + delta_x, y, labels = labels, cex = 1.25)

  if (!identical(add_legend, "off")) {
    graphics::legend(
      add_legend,
      legend = c("Ingoing effects", "Outgoing effects"),
      pch = c(1, 16),
      col = c("black", "black"),
      cex = 1.5,
      bg = "white",
      box.col = "white"
    )
  }

  invisible(NULL)
}

#' Plot trophic network (radial layout)
#'
#' Plots a trophic interaction network using a radial layout.
#'
#' @param M A square adjacency or interaction matrix.
#' @param x Optional numeric vector used to order nodes angularly.
#' @param y Optional numeric vector used to scale radial distances.
#' @param labels Optional node labels.
#' @param line_width_max Maximum line width for edges.
#' @param rotations Number of full rotations around the circle.
#' @param scale Radial scaling factor.
#' @param x_offset Horizontal offset.
#' @param y_offset Vertical offset.
#' @param add_legend Position of legend or "off" to disable.
#' @return Invisibly returns NULL.
#' @export
plot_network_radial <- function(M,
                                x = NULL,
                                y = NULL,
                                labels = NULL,
                                line_width_max = 1,
                                rotations = 1.0,
                                scale = 1.0,
                                x_offset = 0.0,
                                y_offset = 0.0,
                                add_legend = "topright") {

  ## Dimensions
  d <- dim(M)[1]

  ## Labels
  if (is.null(labels)) labels <- seq_len(d)

  ## Scale/order inputs
  if (!is.null(x)) {
    x <- (x - min(x)) / (max(x) - min(x))
  } else {
    x <- rep(1, d)
  }

  if (!is.null(y)) {
    r <- (y - min(y)) / (max(y) - min(y))
  } else {
    r <- rep(1, d)
  }

  ## Order
  s <- order(x)
  x <- x[s]
  r <- r[s] * scale
  M <- M[s, s]
  labels <- labels[s]

  ## Angles
  theta <- seq(0, 2 * pi * rotations, length.out = d)
  x <- cos(theta) * r + x_offset
  y <- sin(theta) * r + y_offset
  x_ <- cos(theta + (2 * pi / d * 0.25)) * r + x_offset
  y_ <- sin(theta + (2 * pi / d * 0.25)) * r + y_offset
  x__ <- 1.1 * cos(theta + (2 * pi / d * 0.125)) * r + x_offset
  y__ <- 1.1 * sin(theta + (2 * pi / d * 0.125)) * r + y_offset

  ## Main plotting area
  graphics::plot(
    x = c(-1:1) * 1.5,
    y = c(-1:1) * 1.5,
    cex = 0,
    bty = "n",
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = ""
  )

  ## Interactions
  cols <- grDevices::rainbow(d)
  denom <- max(abs(M))
  if (denom == 0) denom <- 1

  for (j in seq_len(d)) {
    color_ <- cols[j]
    graphics::text(x__[j], y__[j], labels = labels[j])

    for (i in seq_len(d)) {
      line_width <- abs(M[i, j]) / denom * line_width_max
      graphics::lines(
        x = c(x[i], x_[j]),
        y = c(y[i], y_[j]),
        col = color_,
        lwd = line_width
      )

      ## Avoid zero-length arrows
      if (((x[i] - x_[j])^2 + (y[i] - y_[j])^2) > 0) {
        graphics::arrows(
          x0 = x[i], x1 = x_[j],
          y0 = y[i], y1 = y_[j],
          col = color_,
          lwd = line_width
        )
      }
    }
  }

  ## Legend
  graphics::points(x, y, pch = 16)
  graphics::points(x_, y_, pch = 1)

  if (!identical(add_legend, "off")) {
    graphics::legend(
      "bottomright",
      legend = c("Ingoing effects", "Outgoing effects"),
      pch = c(1, 16),
      col = c("black", "black"),
      cex = 1.5,
      bg = "white",
      box.col = "white"
    )
  }

  invisible(NULL)
}
