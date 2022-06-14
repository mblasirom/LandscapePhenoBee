# LandscapePhenoBee
Description of dataset
We developed a spatially and temporally explicit theoretical model of wild bee populations, called LandscapePhenoBee. The model is developed for Bombus sp. and captures within-season colony dynamics. The model describes mechanistically foraging at the colony level and temporal population dynamics for an average colony at the landscape level. Stages in population dynamics are temperature-dependent triggered with a theoretical generalized seasonal progression, which can be informed by growing degree days (GDD). The purpose of the LandscapePhenoBee model is to evaluate the impact of systematic changes and within-season variability in resources on bee population sizes and crop visitation rates. In a simulation study, we used the model to evaluate the impact of the shortage of food resources in the landscape arising from drought events in different types of landscapes (ranging from different proportions of semi-natural habitats and early and late flowering crops) on bumblebee populations.
DATA-SPECIFIC INFORMATION FOR: df_nominal_code.Rdata

1. Number of variables: 15

2. Number of cases/rows: 200

3. Variable List: 
	prop_snh: proportion of Semi-natural habitat in the landscape
prop_crop: proportion of early crop habitat in the landscape
prop_late_crop: proportion of late crop habitat in the landscape
snh_fract:  percentage of Semi-natural habitat in the landscape
snh_size: size of semi-natural habitat patches
iter: number of iteration
d_length:  drought length
Wtot_Ne:  maximum number of workers produced in the season
Wmax_Ne:  maximum number of workers produced in peak growth
Q_Ne: number of new queens produced
VR: visitation rates
Poll: pollination potential
Flor: floral resources
Id: landscape id
drought: if drought conditions; Yes/No

4. Missing data codes: 
	None

5. Abbreviations used: 
	N/A; not applicable

6. Other relevant information: NA
	

DATA-SPECIFIC INFORMATION FOR: df_SA1GDD_220513.Rdata

1. Number of variables: 17

2. Number of cases/rows: 6000

3. Variable List: 
	
prop_snh: proportion of Semi-natural habitat in the landscape
prop_crop: proportion of early crop habitat in the landscape
prop_late_crop: proportion of late crop habitat in the landscape
snh_fract:  percentage of Semi-natural habitat in the landscape
snh_size: size of semi-natural habitat patches
iter: number of iteration
d_length:  drought length
Wtot_Ne: total number of workers 
Wmax_Ne: maximum number of workers at the peak of population growth
Q_Ne: the total number of daughter queens produced per colony
VR: visitation rates
Poll: pollination potential
Flor: floral resources
Id: landscape id
drought: if drought conditions; Yes/No
Value: nominal, min, max
Parameter: parameter name used in the sensitivity analysis 

4. Missing data codes: 
	None

5. Abbreviations used: 
	N/A; not applicable

6. Other relevant information: 


DATA-SPECIFIC INFORMATION FOR: pop_param.Rdata

1. Number of variables: 10 (list)

2. Number of cases/rows: 1

3. Variable List: 
gdd.emerge: temperature sum for queen emergence
gdd.workerforage: temperature sum for worker emergence
gdd.queenprod: temperature sum for production of worker queens
alpha: Maximum number of workers produced during stages A2 to B2
beta: For which amount of resources half of the potential number of workers is being produced during stages A2 to B2
gamma: Weekly survival rate of workers and daughter queens during stages A2 to B2
epsilon: Proportion of colony workers among newly produced individuals in the nest
kappa: In the pollination score, is the parameter adjusting how quickly the visitation rate per floral resource reaches the maximum pollination
pw: The proportion of workers foraging used to calculate the total number of visitation rate when workers are foraging
nest_int: Density of nests in the semi-natural habitats
	
4. Missing data codes: 
	None

5. Abbreviations used: 
	N/A; not applicable

6. Other relevant information: 

DATA-SPECIFIC INFORMATION FOR: sim_design.Rdata

1. Number of variables: 4

2. Number of cases/rows: 100

3. Variable List: 
	d_length: Duration of drought, between 1 to 4 weeks.
snh_size: SNH size, within a range of 5000 to 15000 m2
snh_fract: Proportion of SNH, between 5 to 25%
iter: iteration number 

4. Missing data codes: 
	None

5. Abbreviations used: 
	N/A; not applicable

6. Other relevant information: 


List of the files: 


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
Spatial variability in nesting and floral resources is represented by spatially explicit maps with a resolution of 10 x 10 m. The size of a landscape is 2010 x 2010 m (mapsize= 201).

The number and size of the patches of SNH were used to randomly create landscapes with different proportions of SNH between 5 to 25% which was used to describe spatial heterogeneity between simple and complex landscapes. 

The average size of crop patches was set to 500 m2 (patchsize_crop=50) and the proportion of flowering crops was set such that these constitute together with SNH, 60% of the area in the landscape (prop_crop=(1-prop_snh-0.4)).


3.generate_nests.R	To generate the nests


4.generate_floral_values.R	To generate floral resources based on GDD, for a normal (flor_val_sim) and a drought year (flor_val_sim_ext)


5.landmaps_into_floralvalues.R	Assigns the floral values generated in “4.generate_floral_values.R” to the chosen land use type from “2.generate_maps.R”


6.population_model_GDD.R	The colony-population model. The model calls for the parameters generated in “1.spider_code_ms.R”, and kernalc.R and latfordisp_m.R.
kerncalc.R	Dispersal kernel, from Häussler et al. 2017*
latfordisp_m.R	Foraging function adapted from Häussler et al. 2017*


9.regression_analysis.R	Multivariate analysis of the results. The script calls for the previously saved dataframe with the results “df_nominal_code.RData”


10.sensitivity_analysis_GDD.R	Code that calls for the population parameters and design previously saved as pop_param.Rdata and sim_design.Rdata respectively. Since we used a local sensitivity analysis, this code allows to vary +10% (“max”) and -10% (“min”) around their nominal value one at a time, keeping all the other parameters fixed. The results are saved as “df_SA1GDD_220513.Rdata”


11.compare_outputs_GDD.R	This code analyses the results from the sensitivity analysis. 


