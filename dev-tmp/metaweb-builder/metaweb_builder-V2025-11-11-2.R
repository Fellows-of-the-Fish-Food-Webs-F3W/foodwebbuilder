#######################
## metaweb_builder.R ##
#######################

## Goal: functions file for the build_metaweb function

#####################################
## CHECK FOR MISSING DATA AND TRIM ##
#####################################

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

build_metaweb = function(tab_size_classes, pred_win, num_classes, selected_resources)
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