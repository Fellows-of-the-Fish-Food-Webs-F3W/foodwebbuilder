#######################
## foodweb_builder.R ##
#######################

## Goal: functions file for building local foodwebs

###############
## FUNCTIONS ##
###############

build_local_foodweb = function(ind_measure_local, metaweb, tab_size_classes, num_classes, selected_resources){
  
  ## Flatten
  trophic_species_code = paste(rep(tab_size_classes[,1], 1, each=num_classes), 1:num_classes, sep="_")
  lb_size_classes = as.numeric(t(as.matrix(tab_size_classes[,-c(1,ncol(tab_size_classes))])))
  ub_size_classes = as.numeric(t(as.matrix(tab_size_classes[,-c(1,2)])))
  
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
  trophic_species_code_subset = c(trophic_species_code[s], selected_resources)
  
  ## Subset metaweb
  metaweb_ = metaweb[trophic_species_code_subset, trophic_species_code_subset]
  
  ## End
  return(metaweb_)
  
}

#
###









































