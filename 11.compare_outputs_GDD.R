
rm(list=ls())
## Compare the sensitivity analysis outputs
## 1. Plot the different combinations per parameter
## 2. Calculate % of change between each new combination and the estimate (baseline)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)

load("df_SA1GDD_220513.Rdata")
df_SA$parameter = as.factor(df_SA$parameter)

df_SA$value = as.factor(df_SA$value)
levels(df_SA$value)
df_SA$param_change = c(1,-1,0)[as.numeric(df_SA$value)]

df_SA$drought = as.factor(df_SA$drought)

pnam <- levels(df_SA$parameter)


SA2 <- do.call('rbind',lapply(1:2,
  function(did){
  do.call('rbind',lapply(1:length(pnam),function(pid){
    
modW <- lmer(Wtot_Ne ~ param_change + (1|id), control = lmerControl(optimizer ="bobyqa"), 
             data = df_SA %>% filter(parameter == pnam[pid] & drought == c('Yes','No')[did]))
sW <- summary(modW)

modQ <- lmer(Q_Ne ~ param_change + (1|id), control = lmerControl(optimizer ="bobyqa"), 
             data = df_SA %>% filter(parameter == pnam[pid] & drought == c('Yes','No')[did]))
sQ <- summary(modQ)

modP <- lmer(poll ~ param_change + (1|id), control = lmerControl(optimizer ="bobyqa"), 
             data = df_SA %>% filter(parameter == pnam[pid] & drought == c('Yes','No')[did]))
sP <- summary(modP)

temp <- rbind(data.frame(fit = sW$coefficients[2,],drought=c('Yes','No')[did],parameter=pnam[pid],output='maxW'),
      data.frame(fit = sQ$coefficients[2,],drought=c('Yes','No')[did],parameter=pnam[pid],output='TQ'),
      data.frame(fit = sP$coefficients[2,],drought=c('Yes','No')[did],parameter=pnam[pid],output='PS'))
temp$what = rep(names(sQ$coefficients[2,]),3)
return(temp)
}))
}))

SA2 %>% 
  filter(what == "Estimate") %>%
  ggplot(aes(x=parameter,y = fit,col=parameter)) + 
  geom_point() +
  facet_wrap(~drought*output, scale = "free") 

y <- SA2 %>% 
  filter(what == "Std. Error")

dd <- SA2 %>% 
  filter(what == "Estimate")
dd$SE <- y$fit
dd %>% 
  ggplot(aes(col=parameter)) + 
  geom_errorbarh( aes(xmin = fit-SE,xmax = fit+SE,y = parameter)) +
  facet_wrap(~output*drought, scale = "free",ncol=2)+
  geom_vline(xintercept = 0) +
  theme(legend.position = "none") 

ggsave('SA2.png',width = 7, height = 10)


