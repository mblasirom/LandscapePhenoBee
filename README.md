# LandscapePhenoBee

Name of the file and Description


bees_extreme_events.Rproj	R project
1.spider_code_ms.R	Code that contains all the necessary packages to run the code. This file prepares the design of the simulation (“sim_design.Rdata”), and values to the population parameters (“pop_param.Rdata”) and land use resources.  
This is the first code to run.
The script calls for “model_for_loop.R”


model_for_loop.R	Script that runs on a loop for all the different following parts of the model: 
1) '2.generate_maps.R'
2)”3.generate_nests.R"
3)"4.generate_floral_values.R"
4)"5.landmaps_into_floralvalues.R"
5)"6.population_model_GDD.R”


2.generate_maps.R	To generate the landscapes


3.generate_nests.R	To generate the nests


4.generate_floral_values.R	To generate floral resources based on GDD, for a normal (flor_val_sim) and a drought year (flor_val_sim_ext)


5.landmaps_into_floralvalues.R	Assigns the floral values generated in “4.generate_floral_values.R” to the chosen land use type from “2.generate_maps.R”


6.population_model_GDD.R	The colony-population model. The model calls for the parameters generated in “1.spider_code_ms.R”, and kernalc.R and latfordisp_m.R.
kerncalc.R	Dispersal kernel, from Häussler et al. 2017*
latfordisp_m.R	Foraging function adapted from Häussler et al. 2017*


9.regression_analysis.R	Multivariate analysis of the results. The script calls for the previously saved dataframe with the results “df_nominal_code.RData”


10.sensitivity_analysis_GDD.R	Code that calls for the population parameters and design previously saved as pop_param.Rdata and sim_design.Rdata respectively. Since we used a local sensitivity analysis, this code allows to vary +10% (“max”) and -10% (“min”) around their nominal value one at a time, keeping all the other parameters fixed. The results are saved as “df_SA1GDD_220513.Rdata”


11.compare_outputs_GDD.R	This code analyses the results from the sensitivity analysis. 

pop_param.Rdata	Population parameters

sim_design.Rdata	Simulation design 

df_nominal_code.Rdata	Dataframe of the simulated results

df_SA1GDD_220513.Rdata	Dataframe of the sensitivity analysis
