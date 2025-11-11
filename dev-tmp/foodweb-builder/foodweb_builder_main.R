############
## main.R ##
############

## Goal: Development file for the build_metaweb function

##############
## INITIATE ##
##############

## Load packages
library("foodwebbuilder")

## Load functions
source("metaweb_builder-V2025-11-11-2.R")
source("foodweb_builder-V2025-11-11-1.R")

#
###

###############
## LOAD DATA ##
###############

## Individual body length measurements
data(ind_measure)
head(ind_measure)
nrow(ind_measure)

## Fish ontogenetic diet shifts
data(fish_diet_shift)
head(fish_diet_shift)
nrow(fish_diet_shift)

## Resource ontogenetic diet shifts
data(resource_diet_shift)
head(resource_diet_shift)
nrow(resource_diet_shift)

## Fish predation windows
data(pred_win)
head(pred_win)
nrow(pred_win)

#
###

###########################
## BUILD LOCAL FOOD WEBS ##
###########################

## Random fish sample
num_samples = 30
u = round(runif(num_samples,0,1) * nrow(ind_measure))
ind_measure_local = ind_measure[u,]

## Build local foodweb
local_fw = build_local_foodweb(ind_measure_local, metaweb, tab_size_classes, NUM_CLASSES, SELECTED_RESOURCES)

## Visualise
image(t(as.matrix(local_fw)), ylim=c(1,0), col = colorRampPalette(c("blue", "white", "red"))(100))

#
###









































