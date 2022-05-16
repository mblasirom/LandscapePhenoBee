
## Sensitivity analysis script
#(make sure to run 1.spider_code_ms until the SA=FALSE)


load('pop_param.Rdata')
pop_param_nominal <- pop_param
load('sim_design.Rdata')

sim_df <- sim_design
sim_df$VR <- sim_df$Q_Ne <- sim_df$Wmax_Ne <-sim_df$Wtot_Ne <-sim_df$poll<-  sim_df$prop_snh <- sim_df$prop_crop  <- NA 
sim_df$VR_D <- sim_df$Q_Ne_D <- sim_df$Wmax_Ne_D <-sim_df$Wtot_Ne_D <- sim_df$poll_D <- NA 

SA_list <- vector('list',length(pop_param))
for (p in 1:length(pop_param)){
    print(paste('SA',p)) 

SA = TRUE #cue to reuse landscapes when we call the model_for_loop    
pop_param = pop_param_nominal

pop_param[[p]] = pop_param_nominal[[p]]*0.9
source('model_for_loop.R')
df_low <- extract_df(sim_df)
df_low$value <- "min"
pop_param[[p]] = pop_param_nominal[[p]]*1.1
source('model_for_loop.R')
df_max <- extract_df(sim_df)
df_max$value <- "max"
load("df_nominal_code.Rdata")
df_nominal$value <- "nominal"
SA_list[[p]] <- rbind(df_nominal, df_low, df_max)
SA_list[[p]]$parameter <- names(pop_param[p])
}

df_SA <- do.call('rbind',SA_list)
save(df_SA, file = "df_SA1GDD_220513.Rdata")

