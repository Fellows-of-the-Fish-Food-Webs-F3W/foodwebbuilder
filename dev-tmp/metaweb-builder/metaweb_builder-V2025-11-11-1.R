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
source("metaweb_build.R")

#
###

####################
## INITIAL CHECKS ##
####################

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

######################
## GLOBAL VARIABLES ##
######################

NUM_CLASSES = 3
SELECTED_RESOURCES = c("det", "biof", "phytob", "macroph", "phytopl", "zoopl", "zoob")

#
###

#####################################
## CHECK FOR MISSING DATA AND TRIM ##
#####################################

## All species codes
species_code_samples = unique(ind_measure$species_code)
species_code_ods = unique(fish_diet_shift$species_code)
species_code_pred = unique(pred_win$species_code)

## Check samples -> ods
check_1 = is.na(match(species_code_samples, species_code_ods))*1
check_2 = is.na(match(species_code_samples, species_code_pred))*1
missing = species_code_samples[(check_1 | check_2)]

## Remove
for(missing_ in missing){
  s = which(ind_measure$species_code == missing_)
  ind_measure = ind_measure[-s,]
}

#
###

##########################
## COMPUTE SIZE CLASSES ##
##########################

## All species codes
species_code = unique(ind_measure$species_code)

## Get body length classes
tab_size_classes = NULL
for (species_code_ in species_code){
  
  ## Subset data
  s = ind_measure$species_code == species_code_
  ind_measure_ = ind_measure[s,]
  
  ## Compute max length
  size_max_ = max(ind_measure_$size)
  
  ## Divide in size classes
  size_classes_ = seq(0, size_max_, size_max_/NUM_CLASSES)
  
  ## Collect
  tab_size_classes = rbind(tab_size_classes, c(species_code_, size_classes_))
  
}
tab_size_classes = data.frame(tab_size_classes)
colnames(tab_size_classes) = c("species_code", "lower_bound", paste("upper_bound", 1:NUM_CLASSES, sep="_"))

#       
###

############################
## DEFINE TROPHIC SPECIES ##
############################

## Flatten
lb_size_classes = as.numeric(t(as.matrix(tab_size_classes[,-c(1,ncol(tab_size_classes))])))
ub_size_classes = as.numeric(t(as.matrix(tab_size_classes[,-c(1,2)])))
mp_size_classes = 0.5 * (ub_size_classes + lb_size_classes)
trophic_species_code = paste(rep(species_code, 1, each=NUM_CLASSES), 1:NUM_CLASSES, sep="_")

#
###

###############################
## COMPUTE PREY SIZE CLASSES ##
###############################

## Subset rows in predation window file
species_code = tab_size_classes$species_code
s = match(species_code, pred_win$species_code)
pred_win_ = pred_win[s,]

## reshape class midpoints as matrix
tab_size_classes_midpoints = matrix(mp_size_classes, ncol=NUM_CLASSES, byrow=T)

## Compute prey lower bound and upper bound
lb_prey = tab_size_classes_midpoints * pred_win_$beta_min
ub_prey = tab_size_classes_midpoints * pred_win_$beta_max

## Vectorise
lb_prey = as.numeric(t(lb_prey))
ub_prey = as.numeric(t(ub_prey))

#
###

####################################################
## DETERMINE ALL POTENTIAL FISH-FISH INTERACTIONS ##
####################################################

## Build fish-fish interaction matrix
ff_interactions = NULL
for (i in 1:length(mp_size_classes)){
  ff_interactions_ = (mp_size_classes >= vec_lb_prey[i]) * (mp_size_classes < vec_ub_prey[i])  
  ff_interactions = rbind(ff_interactions, ff_interactions_)
}

## Format matrix
colnames(ff_interactions) = trophic_species_code
rownames(ff_interactions) = trophic_species_code
ff_interactions = t(ff_interactions) # i --> j (row i is consumed by column j)

#
###

############################
## FISH-FISH INTERACTIONS ##
############################

## For each trophic species check piscivory status
piscivory = NULL
for (i in 1:length(trophic_species_code)){
  
  ## Subset ODS data
  species_code_ = unlist(strsplit(trophic_species_code[i],"_"))[1]
  s = fish_diet_shift$species_code == species_code_
  fish_diet_shift_ = fish_diet_shift[s,]
  
  ## Find match
  check = (mp_size_classes[i] >= fish_diet_shift_$size_min) & (mp_size_classes[i] < fish_diet_shift_$size_max)
  
  ## Check status
  piscivory_ = fish_diet_shift_[check, "fish"]
  piscivory = c(piscivory, piscivory_)
  
}

## Apply filter
ff_interactions = ff_interactions %*% diag(piscivory)

## Format
colnames(ff_interactions) = trophic_species_code
rownames(ff_interactions) = trophic_species_code

#
###

################################
## RESOURCE-FISH INTERACTIONS ##
################################

## For each trophic species check piscivory status
rf_interactions = NULL
for (i in 1:length(trophic_species_code)){
  
  ## Subset ODS data
  species_code_ = unlist(strsplit(trophic_species_code[i],"_"))[1]
  s = fish_diet_shift$species_code == species_code_
  fish_diet_shift_ = fish_diet_shift[s,]
  
  ## Find match
  check = (mp_size_classes[i] >= fish_diet_shift_$size_min) & (mp_size_classes[i] < fish_diet_shift_$size_max)
  
  ## Check status
  rf_interactions_ = fish_diet_shift_[check, ]
  rf_interactions = rbind(rf_interactions, rf_interactions_)
  
}

## Format
rf_interactions = t(rf_interactions[,SELECTED_RESOURCES])
colnames(rf_interactions) = trophic_species_code
rownames(rf_interactions) = SELECTED_RESOURCES

#
###

######################
## ASSEMBLE METAWEB ##
######################

## Resource-resource interactions
rr_interactions = resource_diet_shift[, SELECTED_RESOURCES]
rownames(rr_interactions) = SELECTED_RESOURCES
colnames(rr_interactions) = SELECTED_RESOURCES

## fish-resource interactions
fr_interactions = matrix(0, ncol=ncol(rr_interactions), nrow=ncol(ff_interactions))
rownames(fr_interactions) = trophic_species_code
colnames(fr_interactions) = SELECTED_RESOURCES

## Metaweb
metaweb = cbind(
  rbind(ff_interactions, rf_interactions),
  rbind(fr_interactions, rr_interactions)
)

## Visualise matrix
# image(as.matrix(metaweb))

#
###

#####################
## LOCAL FOOD WEBS ##
#####################

## Random fish sample
num_samples = 30
u = round(runif(num_samples,0,1) * nrow(ind_measure))
ind_measure_local = ind_measure[u,]

## Check presence
check = NULL
for (i in 1:nrow(ind_measure_local)){
  
  ## Check presence
  check_ = (
    grepl(pattern = ind_measure_local$species_code[i], x = trophic_species_code) & 
    (ind_measure_local$size[i] >= lb_size_classes) & 
    (ind_measure_local$size[i] < ub_size_classes)  
    )*1
  
  ## Collect
  check = rbind(check, check_)
}

## Collapse
check = apply(check, 2, sum)
cbind(trophic_species_code, check)

## Subset
s = which(check > 0)
trophic_species_code_subset = c(trophic_species_code[s], SELECTED_RESOURCES)

## Subset metaweb
metaweb[trophic_species_code_subset, trophic_species_code_subset]

#
###









































