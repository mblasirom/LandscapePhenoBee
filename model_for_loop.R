for (row_id in 1:nrow(sim_design)){
  print(row_id) 
 
  ## 1.Generate land use maps that contain semi-natural habitats, early crop and late crop patches 
  source('2.generate_maps.R') 
  
  ## 2.Generate nest map in the semi-natural habitat patches
  source("3.generate_nests.R")  
  
  ## 3.Generate floral resources based on GDD, for a normal (flor_val_sim) and a drought year (flor_val_sim_ext)
  source("4.generate_floral_values.R")
  
  ## 4.Assign the floral resource values from #3, to the corresponding land use patch created in #1 
  source("5.landmaps_into_floralvalues.R")
  
  ## 5. Growth population model 
  source("6.population_model_GDD.R") 
}