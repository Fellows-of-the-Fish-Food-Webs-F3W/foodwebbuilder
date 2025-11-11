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
# source("metaweb_builder-V2025-11-11-3.R")
# source("foodweb_builder-V2025-11-11-1.R")

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

###################
## BUILD METAWEB ##
###################

## Global variables
NUM_CLASSES = 3
SELECTED_RESOURCES = c("det", "biof", "phytob", "macroph", "phytopl", "zoopl", "zoob")

## Remove missing species
ind_measure = remove_missing_species(ind_measure, fish_diet_shift, pred_win)

## Compute size classes
tab_size_classes = compute_size_classes(ind_measure, NUM_CLASSES)

## Build metaweb
metaweb = build_metaweb(tab_size_classes, pred_win, NUM_CLASSES, SELECTED_RESOURCES)

## Visualise
image(t(as.matrix(metaweb)), ylim=c(1,0), col = colorRampPalette(c("blue", "white", "red"))(100))

#
###









































