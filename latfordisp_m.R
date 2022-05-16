# foraging function from Häussler et al. 2017

latfordisp <- function(X,
                       FLOR,
                       decay)
{
  FLOR[is.na(FLOR)] <- 0
  X[is.na(X)] <- 0
  num.nest <- sum(X>0)
  # cell-specific sum of the distance-weighted resources
  distWeigthedResources = EBImage::filter2(FLOR, decay) 
  distWeigthedResources[distWeigthedResources < 1e-15] <- 0 # replace values that are smaller than the zero-machine by zero

  relativeResources <- X*0 # initializing the matrix to have the same size as N
  relativeResources[distWeigthedResources > 0] = X[distWeigthedResources > 0] /  distWeigthedResources[distWeigthedResources > 0]
  relativeResources[distWeigthedResources < 0] = 0 #
  
  # dispersion of relative resources
  distWeightedRelativeResources = EBImage::filter2(relativeResources, decay)
  distWeightedRelativeResources[distWeightedRelativeResources < 1e-15] <- 0
  
  visitationRate = distWeightedRelativeResources * FLOR
  # the resources accessed by foragers are the distance-weighted resources but only at cells where foragers are actually nesting
  Racc_per_nest = distWeigthedResources * (X>0) ## the original code here is Racc_per_forager
  
  return(list(Racc_per_nest = Racc_per_nest, 
              visitation_rate = visitationRate, 
              distance_weighted_resources = distWeigthedResources))
}
