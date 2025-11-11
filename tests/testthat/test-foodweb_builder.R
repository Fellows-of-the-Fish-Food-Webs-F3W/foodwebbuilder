test_that("build_local_foodweb handles small example data", {
  
  ## Table with size classes
  small_species <- data.frame(
    species_code = c("A", "B"), 
    lower_bound = c(0, 0),  
    upper_bound_1 = c(10, 10),
    upper_bound_2 = c(15, 15)
  )
  
  ## Table with predation window parameters
  small_predwin <- data.frame(
    species_code = c("A", "B"),
    beta_min = c(0.25, 0.5),  
    beta_max = c(0.9, 1.0)
  )
  
  ## Fish ontogenetic diet shifts
  small_fish <- data.frame(
    species_code = c("A", "B"), 
    size_min = c(0, 0),   
    size_max = c(20, 20),  
    zooplankton = c(0, 1),  
    benthos = c(0, 1), 
    fish = c(0, 1)
  )
  
  ## Resource ontogenetic diet shifts
  small_resource <- data.frame(
    species_code = c("zooplankton", "benthos"), 
    zooplankton = c(0, 1),  
    benthos = c(0, 1)
  )
  
  ## Build global metaweb first
  metaweb <- build_metaweb(
    tab_size_classes = small_species,
    pred_win = small_predwin,
    fish_diet_shift = small_fish,
    resource_diet_shift = small_resource,
    num_classes = 2,
    selected_resources = c("zooplankton", "benthos")
  )
  
  ## Create a local individual-level dataset
  local_ind <- data.frame(
    species_code = c("A", "B"),
    size = c(5, 12)  # both species present, different size classes
  )
  
  ## Build local food web
  local_fw <- build_local_foodweb(
    ind_measure_local = local_ind,
    metaweb = metaweb,
    tab_size_classes = small_species,
    num_classes = 2,
    selected_resources = c("zooplankton", "benthos")
  )
  
  ## ---- Tests ----
  
  ## Check it returns a matrix or data frame
  expect_true(is.matrix(local_fw) || is.data.frame(local_fw))
  
  ## Check it is square
  expect_equal(dim(local_fw)[1], dim(local_fw)[2])
  
  ## Check that all node names are present in the global metaweb
  expect_true(all(rownames(local_fw) %in% rownames(metaweb)))
  expect_true(all(colnames(local_fw) %in% colnames(metaweb)))
  
  ## Optional: check no new species/resources appear
  expect_false(any(!rownames(local_fw) %in% rownames(metaweb)))

})

