#######################
## metaweb_builder.R ##
#######################

## Goal: functions file for the build_metaweb function

#####################################
## CHECK FOR MISSING DATA AND TRIM ##
#####################################

#' Remove Species with Missing Data Across Input Tables
#'
#' @description
#' Cleans the individual measurement dataset by removing species that are not
#' present in the auxiliary datasets (`fish_diet_shift` or `pred_win`).
#' This ensures that all species retained have complete information across
#' all relevant data sources.
#'
#' @param ind_measure A data frame containing individual-level measurements
#'   with a column `species_code` identifying each species.
#' @param fish_diet_shift A data frame containing species dietary information,
#'   with a column `species_code`.
#' @param pred_win A data frame containing predator window or prey availability
#'   data, with a column `species_code`.
#'
#' @return
#' A filtered version of `ind_measure` containing only species that appear in
#' both `fish_diet_shift` and `pred_win`.
#'
#' @details
#' The function checks whether each species in `ind_measure` is represented in
#' both reference datasets. Any species missing from either is removed, and a
#' message is printed listing which species were excluded.
#'
#' @examples
#' \dontrun{
#' filtered <- remove_missing_species(ind_measure, fish_diet_shift, pred_win)
#' }
#'
#' @export
remove_missing_species = function(ind_measure, fish_diet_shift, pred_win){
  
  ## All species codes
  species_code_samples = unique(ind_measure$species_code)
  species_code_ods = unique(fish_diet_shift$species_code)
  species_code_pred = unique(pred_win$species_code)
  
  ## Check samples -> ods
  check_1 = is.na(match(species_code_samples, species_code_ods))*1
  check_2 = is.na(match(species_code_samples, species_code_pred))*1
  missing = species_code_samples[(check_1 | check_2)]
  message("missing species found and removed: ")
  message(paste(missing, sep=" ", collapse=" "))
  
  ## Remove
  for(missing_ in missing){
    s = which(ind_measure$species_code == missing_)
    ind_measure = ind_measure[-s,]
  }
  
  ## End
  return(ind_measure)
  
}

#
###

##########################
## COMPUTE SIZE CLASSES ##
##########################

#' Compute Size Class Boundaries for Each Species
#'
#' @description
#' Divides the observed body length range of each species into a specified
#' number of size classes. This is typically used to discretize continuous
#' size data for subsequent analysis or modeling.
#'
#' @param ind_measure A data frame of individual-level measurements
#'   containing at least the columns `species_code` and `size`.
#' @param num_classes Integer indicating the number of size classes
#'   to divide each species' observed size range into.
#'
#' @return
#' A data frame listing each species with the corresponding size class
#' boundaries. The output includes a column for the species code,
#' a lower bound, and upper bound columns for each class.
#'
#' @details
#' For each species, the function identifies the maximum observed size and
#' constructs evenly spaced size class intervals from 0 to that maximum.
#' These intervals are returned in a tabular form suitable for further use
#' in population or trophic modeling.
#'
#' @examples
#' \dontrun{
#' size_classes <- compute_size_classes(ind_measure, num_classes = 5)
#' head(size_classes)
#' }
#'
#' @export
compute_size_classes = function(ind_measure, num_classes){

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
    size_classes_ = seq(0, size_max_, size_max_/num_classes)
    
    ## Collect
    tab_size_classes = rbind(tab_size_classes, c(species_code_, size_classes_))
    
  }
  
  ## Format
  tab_size_classes = data.frame(tab_size_classes)
  colnames(tab_size_classes) = c("species_code", "lower_bound", paste("upper_bound", 1:num_classes, sep="_"))

  ## End
  return(tab_size_classes)

}

#       
###

###################
## BUILD METAWEB ##
###################

#' Build a Metaweb Interaction Network
#'
#' @description
#' Constructs a complete metaweb—an integrated species interaction network—
#' by combining size‐class information, predator–prey relationships, dietary
#' data, and resource links. The resulting matrix describes all potential
#' trophic interactions among size‐structured fish species and their resources.
#'
#' @param tab_size_classes A data frame of size‐class boundaries for each
#'   species, typically produced by [compute_size_classes()]. Must contain a
#'   `species_code` column and the lower and upper bounds for each size class.
#' @param pred_win A data frame describing the predator–prey window for each
#'   species, including columns `species_code`, `beta_min`, and `beta_max`,
#'   defining the lower and upper ratios of prey to predator body size.
#' @param fish_diet_shift A data frame describing ontogenetic diet shifts for
#'   each fish species. Must include `species_code`, `size_min`, `size_max`,
#'   and columns indicating dietary components (e.g., `fish`, `benthos`, etc.).
#'   Used to determine piscivory status and resource consumption.
#' @param resource_diet_shift A data frame describing resource‐to‐resource
#'   interactions (e.g., basal resource dependencies). Must include one row per
#'   resource and columns corresponding to the resources listed in
#'   `selected_resources`.
#' @param num_classes Integer indicating the number of size classes per species,
#'   matching the value used in [compute_size_classes()].
#' @param selected_resources Character vector giving the names of resource
#'   types (columns) to include in the metaweb.
#'
#' @return
#' A square adjacency matrix (data frame or matrix) representing all potential
#' trophic interactions among trophic species (size classes) and resources.
#' Rows correspond to prey items and columns to consumers.
#'
#' @details
#' The function proceeds through several steps:
#' 1. Defines *trophic species* as combinations of species and size classes.
#' 2. Computes prey size‐class limits using the predator–prey window parameters.
#' 3. Builds fish–fish interaction matrices based on size overlap and
#'    piscivory status from `fish_diet_shift`.
#' 4. Builds resource–fish and resource–resource interaction matrices using
#'    `fish_diet_shift` and `resource_diet_shift`.
#' 5. Combines all matrices into a single metaweb adjacency matrix.
#'
#' The metaweb thus represents the complete potential trophic network
#' integrating all modeled size classes and resource categories.
#'
#' @examples
#' \dontrun{
#' metaweb <- build_metaweb(
#'   tab_size_classes = size_classes,
#'   pred_win = predation_window,
#'   fish_diet_shift = fish_diet_data,
#'   resource_diet_shift = resource_links,
#'   num_classes = 5,
#'   selected_resources = c("zooplankton", "benthos")
#' )
#' dim(metaweb)
#' }
#'
#' @seealso [remove_missing_species()], [compute_size_classes()]
#'
#' @export
build_metaweb = function(tab_size_classes, pred_win, fish_diet_shift, resource_diet_shift, num_classes, selected_resources)
{
  
  ############################
  ## DEFINE TROPHIC SPECIES ##
  ############################
  
  ## Flatten
  lb_size_classes = as.numeric(t(as.matrix(tab_size_classes[,-c(1,ncol(tab_size_classes))])))
  ub_size_classes = as.numeric(t(as.matrix(tab_size_classes[,-c(1,2)])))
  mp_size_classes = 0.5 * (ub_size_classes + lb_size_classes)
  trophic_species_code = paste(rep(tab_size_classes[,1], 1, each=num_classes), 1:num_classes, sep="_")
  
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
  tab_size_classes_midpoints = matrix(mp_size_classes, ncol=num_classes, byrow=T)
  
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
    ff_interactions_ = (mp_size_classes >= lb_prey[i]) * (mp_size_classes < ub_prey[i])  
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
  rf_interactions = t(rf_interactions[,selected_resources])
  colnames(rf_interactions) = trophic_species_code
  rownames(rf_interactions) = selected_resources
  
  #
  ###
  
  ######################
  ## ASSEMBLE METAWEB ##
  ######################
  
  ## Resource-resource interactions
  rr_interactions = resource_diet_shift[, selected_resources]
  rownames(rr_interactions) = selected_resources
  colnames(rr_interactions) = selected_resources
  
  ## fish-resource interactions
  fr_interactions = matrix(0, ncol=ncol(rr_interactions), nrow=ncol(ff_interactions))
  rownames(fr_interactions) = trophic_species_code
  colnames(fr_interactions) = selected_resources
  
  ## Metaweb
  metaweb = cbind(
    rbind(ff_interactions, rf_interactions),
    rbind(fr_interactions, rr_interactions)
  )
  
  #
  ###
  
  #########
  ## END ##
  
  ## End
  return(metaweb)
  
  #
  ###
  
}

#
###
