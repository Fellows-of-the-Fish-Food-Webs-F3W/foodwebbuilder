#####################
## plot_networks.r ##
#####################

## Goal: Define a set of functions to visualise trophic networks.

###############
## FUNCTIONS ##
###############

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
plot_network = function(M, x=NULL, y=NULL, labels=NULL, xlab="", ylab="", line_width_max=1, label_space_x=0.05, add_legend="topright"){
  
  ## Dimensions
  d = dim(M)[1]
  
  ## Labels
  if (is.null(labels) == T) labels = 1:d
  
  ## Axes
  if (is.null(x) == T) x = cos((1:d)/0.25)
  if (is.null(y) == T) y = sin((1:d)/0.25)
  
  ## Graphical parameters
  delta_x = (max(x)-min(x)) * label_space_x
  
  ## Main plotting area
  plot(x,y,xlim=c(min(x)-delta_x,max(x)+delta_x*1.5),cex=0,bty="n", xlab=xlab, ylab=ylab, cex.lab=1.5, bty="l")
  
  ## Interactions
  for(j in 1:ncol(M))
  {
    color_ = rainbow(ncol(M))[j]
    for(i in 1:nrow(M))
    {
      line_width = abs(M[i,j])/max(abs(M)) * line_width_max
      lines(x=c(x[i],x[j]-delta_x/2), y=c(y[i],y[j]), col=color_, lwd=line_width)
      arrows(x0=x[i], x1=x[j]-delta_x/2, y0=y[i], y1=y[j], col=color_, lwd=line_width)
    }
  }
  
  ## Labels and legend
  points(x-delta_x/2, y, pch=1, cex=2)
  points(x, y, pch=16, cex=2)
  text(x+delta_x, y, labels=labels, cex=1.25)
  if (add_legend != "off"){
    legend(add_legend, legend=c("Ingoing effects","Outgoing effects"), pch=c(1,16), col=c("black","black"), cex=1.5, bg="white", box.col="white")  
  }
  
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
plot_network_radial = function(M, x=NULL, y=NULL, labels=NULL, line_width_max=1, rotations=1.0, scale=1.0, x_offset=0.0, y_offset=0.0, add_legend="topright"){
  
  ## Dimensions
  d = dim(M)[1]
  
  ## Labels
  if (is.null(labels) == T) labels = 1:d
  
  ## Scale
  if (is.null(x) == F) x = (x-min(x))/(max(x)-min(x)) else x = rep(1,d)
  if (is.null(y) == F) r = (y-min(y))/(max(y)-min(y)) else r = rep(1,d)
  
  ## Order
  s = order(x)
  x = x[s]
  r = r[s] * scale
  M = M[s,s]
  labels = labels[s]
  
  ## Angles
  theta = seq(0, 2*pi*rotations, length.out=d)
  x = cos(theta) * r + x_offset
  y = sin(theta) * r + y_offset
  x_ = cos(theta + (2*pi/d*0.25)) * r + x_offset
  y_ = sin(theta + (2*pi/d*0.25)) * r + y_offset
  x__ = 1.1 * cos(theta + (2*pi/d*0.125)) * r + x_offset
  y__ = 1.1 * sin(theta + (2*pi/d*0.125)) * r + y_offset
  
  ## Main plotting area
  plot(x=c(-1:1)*1.5,y=c(-1:1)*1.5,cex=0,bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
  
  ## Interactions
  for(j in 1:d)
  {
    color_ = rainbow(d)[j]
    text(x__[j],y__[j], labels=labels[j])
    for(i in 1:d)
    {
      line_width = abs(M[i,j])/max(abs(M))*line_width_max
      lines(x=c(x[i],x_[j]), y=c(y[i],y_[j]), col=color_, lwd=line_width)
      if (((x[i] - x_[i])^2 + (y[i] - y_[j])^2) > 0) arrows(x0=x[i], x1=x_[j], y0=y[i], y1=y_[j], col=color_, lwd=line_width)
    }
  }
  
  ## Legend
  points(x,y,pch=16)
  points(x_,y_,pch=1)
  if (add_legend != "off"){
    legend("bottomright", legend=c("Ingoing effects","Outgoing effects"), pch=c(1,16), col=c("black","black"), cex=1.5, bg="white", box.col="white")
  }
}

#
###