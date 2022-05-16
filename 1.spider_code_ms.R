
rm(list=ls())

# Install the necessary packages
library(landscapeR)
library(landscapemetrics) # package to produce edge length metric, calculate_lsm(count_boundary = TRUE)
library(landscapetools)
library(raster)
library(ggplot2)
library(sjPlot)

library(scico)
library(gridExtra)
library(caTools)# calculate the area under the curve - for visitation rates VR
library(lhs)# Latin Hypercube Sample
library(pollen)
library(dplyr)
#BiocManager::install("EBImage")
library('EBImage')   
source('kerncalc.R') # dispersal kernel
source('latfordisp_m.R') # foraging function 

pop_param <- list(
  gdd.emerge = 150/25,#divided by 25 to have a generalized function of 1 to 100
  gdd.workerforage = 400/25,
  gdd.queenprod = 1000/25,
  alpha = 10, 
  beta = 1, 
  delta = 0.9,
  pw = 0.8, 
  epsilon = 0.2,
  kappa = 1,
  nest_int = 0.01)
save(pop_param,file='pop_param.Rdata')

resource_param <- data.frame(
  lu = c('nest','crop1','crop2'), # crop 1 is early flowering crop, crop 2 is late flowering crop
  gdd.start.flowering = c(200/25,300/25,1500/25), #divided by 25 to have a generalized function of 1 to 100
  gdd.end.flowering = c(1900/25,600/25,1700/25),
  max.flowers = c(1,10,5)
  )

## Generate the simulation design by latin hypercube sampling
ranges_design <- data.frame(
  d_length = c(3,6),
  snh_fract = c(5,25), snh_size = c(50,150))

create_design_one_iter <- function(iter,n,ranges_design){
  sim_design <- LHS_design <- improvedLHS(n = n,k = length(ranges_design), dup = 1)
  for(k in 1:length(ranges_design)){
    sim_design[,k] <- ranges_design[1,k] + LHS_design[,k]*(ranges_design[2,k]-ranges_design[1,k])
  }
  sim_design <- as.data.frame(sim_design)
  colnames(sim_design) <- colnames(ranges_design)
  sim_design$d_length <- round(sim_design$d_length )
  sim_design$iter <- iter
  return(sim_design)
}


number_of_iterations = 5 
number_of_samples = 20 
list_design <- lapply(1:number_of_iterations,create_design_one_iter,n=number_of_samples,ranges_design)
sim_design <- do.call(rbind,list_design)

save(sim_design,file='sim_design.Rdata')

sim_df <- sim_design
sim_df$VR <- sim_df$Q_Ne <- sim_df$Wmax_Ne <-sim_df$Wtot_Ne <-sim_df$poll<-  sim_df$prop_snh <- sim_df$prop_crop <- sim_df$prop_late_crop  <- NA 
sim_df$VR_D <- sim_df$Q_Ne_D <- sim_df$Wmax_Ne_D <-sim_df$Wtot_Ne_D <- sim_df$poll_D <- NA 


# SA stands for Sensitivity analysis
SA = FALSE

# the design is now loaded and ready to start the model in "model_for_loop.R". The script contains: 
#source('2.generate_maps.R') # To generate the landscapes
#source("3.generate_nests.R")# To generate the nests
#source("4.generate_floral_values.R")# To generate floral resources based on GDD, for a normal (flor_val_sim) and a drought year (flor_val_sim_ext)
#source("5.landmaps_into_floralvalues.R") # assign the floral values generated to the land use type
#source("6.population_model_GDD.R") # the colony-population model
source('model_for_loop.R')

extract_df <- function(sim_df){
## Store simulation outputs into a data frame for analysis: 
temp0 <- sim_df[,c("prop_snh","prop_crop","prop_late_crop","snh_fract","snh_size","iter","d_length")]
temp1 <- sim_df[,c("Wtot_Ne","Wmax_Ne","Q_Ne","VR","poll","flor")]
temp2 <- sim_df[,c("Wtot_Ne_D","Wmax_Ne_D","Q_Ne_D","VR_D","poll_D","flor_D")]
colnames(temp2) <- colnames(temp1)
temp3 <- data.frame(id = 1:nrow(temp0))
df <- cbind(rbind(temp0,temp0),rbind(temp1,temp2),rbind(temp3,temp3))
df$drought <- rep(c('No','Yes'),each=nrow(temp1))
df$d_length[df$drought=='No'] <- 0

df <- df[is.finite(df$prop_snh),]# remove empty lines
return(df)
}

df_nominal <- extract_df(sim_df)
save(df_nominal,file = paste("df_nominal_code.Rdata"))

# run the regression analysis in:
#9.regression_analysis.R


# run the sensitivity analysis here: 
source('10.sensitivity_analysis_GDD.R')

source('11.compare_outputs_GDD.R')

if(FALSE){
  # Example plot
df <- df_nominal
{
  f1 <- ggplot(df,aes(x=prop_snh,y=poll,color=drought)) +
    labs(x = "Semi-natural habitat %", y= "Pollination") +
    geom_point() +
    geom_smooth(method = "lm")+ 
    theme_classic()  + ggtitle("")
  
  f2 <- ggplot(df,aes(x=prop_snh,y=Q_Ne,color=drought)) +
    labs(x = "Semi-natural habitat %", y= "New queens") +
    geom_point() +
    geom_smooth(method = "lm")+ 
    theme_classic()  + ggtitle("")
  
  f3 <- ggplot(df,aes(x=prop_snh,y=Wmax_Ne,color=drought)) +
    labs(x = "Semi-natural habitat %", y= "Max workers per nest") +
    geom_point() +
    geom_smooth(method = "loess")+ 
    theme_classic()  + ggtitle("")
  
  f4 <- ggplot(df,aes(x=prop_snh,y=flor,color=drought)) +
    labs(x = "Semi-natural habitat %", y= "Floral resources") +
    geom_point() +
    geom_smooth(method = "loess")+ 
    theme_classic()  + ggtitle("")
  
  grid.arrange(f1, f2, f3, f4)
}


#Plots
#Visitation rates

{
  ggplot(df,aes(x=prop_snh,y=prop_crop)) +
    labs(x = "Semi-natural habitat %", y= "Prop crop") +
    geom_point()
}

{
  plot(df[,c("B","Wmax_Ne","Q_Ne","flor")])
}

plot(df[,c("flor","B")])


{
  a <- ggplot(df,aes(x=prop_snh,y=VR,fill=drought)) +
    labs(x = "Semi-natural habitat %", y= "Visitation rates") +
    geom_point()+
    geom_smooth()+ theme_classic()
  
  b <- ggplot(df,aes(x=prop_crop,y=VR,color=drought)) +
    labs(x = "Flowering crop % ", y= "Visitation rates") +
    geom_point()+
    geom_smooth()+ theme_classic()
  
  #Qvect_Ne_sum
  c <-ggplot(df,aes(x=prop_snh,y=Q_Ne,color=drought)) +
    labs(x = "Semi-natural habitat %", y= "New queens per nest") +
    geom_point()+
    geom_smooth()+ theme_classic()
  
  #Wvect_Ne_sum
  d <- ggplot(df,aes(x=prop_snh,y=Wmax_Ne,color=drought)) +
    labs(x = "Semi-natural habitat %", y= "Max workers per nest") +
    geom_point()+
    geom_smooth()+ 
    theme_classic()
  
  e <- grid.arrange(a, b, c, d)
  e
}
}



