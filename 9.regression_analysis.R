
## Regression analysis

## Multivariate analysis of the results:
rm(list=ls())
library(lme4)
library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())
library(lmerTest)
library(ggpubr)
library(performance)
library(car)
library(vegan)
library(optimx)#control = lmerControl(optimizer ="Nelder_Mead")
library(gridExtra)

# Call for the sim_df and develop the models:
{
load("df_nominal_code.RData")
df_nominal$prop_early_crop <- df_nominal$prop_crop
range(df_nominal$prop_early_crop)
range(df_nominal$prop_late_crop)
df_nominal$prop_crop_total <- df_nominal$prop_early_crop + df_nominal$prop_late_crop 
df_nominal$duration <- cut(df_nominal$d_length,breaks = c(-1,2,3,10))
levels(df_nominal$duration) <- c('no','short','long')
ordered(df_nominal$duration)

landscape_area = 404.01 #sqrmetres of the total landscape landscape is 2010m x 2010m, 4 040 100m2 = 404.01ha

}

df=df_nominal
df$drought= as.factor(df$drought)


# exploration plots


{
plot(prop_early_crop~ prop_snh, data = df)
plot(prop_late_crop ~ prop_crop_total, data = df ) # negative correlated
plot(prop_late_crop/prop_crop_total ~ prop_early_crop, data = df ) # negative correlated
plot(prop_late_crop/prop_crop_total ~ prop_snh, data = df ) # negative correlated
plot(prop_crop_total ~ prop_snh, data = df ) # negative correlated
plot(prop_snh  ~ snh_size, data = df )#non correlated
plot(Wtot_Ne  ~ Wmax_Ne, data = df ) # as expected, correlated
plot(prop_early_crop ~ prop_snh, data = df )
histogram((df$crop_diversity))
head(df)


histogram(df$poll)
histogram(df$Wmax_Ne) 
histogram(df$Q_Ne) 

df %>% ggplot(aes(x= Q_Ne, col= drought)) +  geom_density()
df %>% ggplot(aes(x= Wmax_Ne, col= drought)) +  geom_density()
df %>% ggplot(aes(x= poll, col= drought)) +  geom_density()

df %>% ggplot(aes(y= Q_Ne, x = prop_snh, col= drought)) +  geom_point()
df %>% ggplot(aes(y= Wmax_Ne, x = prop_snh, col= drought)) +  geom_point()
df %>% ggplot(aes(y= poll, x = prop_snh, col= drought)) +  geom_point()

df %>% ggplot(aes(y= Q_Ne, x = prop_early_crop, col= drought)) +  geom_point()
df %>% ggplot(aes(y= Wmax_Ne, x = prop_early_crop, col= drought)) +  geom_point()
df %>% ggplot(aes(y= poll, x = prop_early_crop, col= drought)) +  geom_point()

df %>% ggplot(aes(y= Q_Ne, x = prop_late_crop, col= drought)) +  geom_point()
df %>% ggplot(aes(y= Wmax_Ne, x = prop_late_crop, col= drought)) +  geom_point()
df %>% ggplot(aes(y= poll, x = prop_late_crop, col= drought)) +  geom_point()

df %>% ggplot(aes(y= Q_Ne, x = prop_late_crop/(prop_late_crop+prop_snh), col= drought)) +  geom_point()

df %>% ggplot(aes(y= Q_Ne, x = (prop_late_crop+prop_early_crop)/prop_snh, col= drought)) +  geom_point()
df %>% ggplot(aes(y= Wmax_Ne, x = (prop_late_crop+prop_early_crop)/prop_snh, col= drought)) +  geom_point()
df %>% ggplot(aes(y= poll, x = (prop_late_crop+prop_early_crop)/prop_snh, col= drought)) +  geom_point()

df %>% ggplot(aes(y= Q_Ne, x = d_length, col= drought)) +  geom_point()
df %>% ggplot(aes(y= Wmax_Ne, x = d_length, col= drought)) +  geom_point()
df %>% ggplot(aes(y= poll, x = d_length, col= drought)) +  geom_point()

df %>% ggplot(aes(y= Q_Ne, x = duration, col= drought)) +  geom_point()
df %>% ggplot(aes(y= Wmax_Ne, x = duration, col= drought)) +  geom_point()
df %>% ggplot(aes(y= poll, x = duration, col= drought)) +  geom_point()
}

{
test_result <- NULL
poll_model <- lmer(poll ~ (prop_snh  + prop_early_crop + I(prop_early_crop^2)) * drought + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
#snh
model0 <- lmer(poll ~ (prop_early_crop + I(prop_early_crop^2)) * drought + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
te <- anova(poll_model, model0)
test_result <- rbind(test_result,data.frame(output = 'PS',
                                            var = 'SNH',
                                            df = te$Df[2],
                                            Chisq = te$Chisq[2],
                                            p = te$`Pr(>Chisq)`[2]))
#prop early_crop
model0 <- lmer(poll ~ prop_snh * drought  + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
te <- anova(poll_model, model0)
test_result <- rbind(test_result,data.frame(output = 'PS',var = 'prop early_crop',
                                            df = te$Df[2],
                                            Chisq = te$Chisq[2],
                                            p = te$`Pr(>Chisq)`[2]))
#drought
model0 <- lmer(poll ~ (prop_snh  + prop_early_crop + I(prop_early_crop^2))  + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
te <- anova(poll_model, model0)
test_result <- rbind(test_result,data.frame(output = 'PS',var = 'drought',
                                            df = te$Df[2],
                                            Chisq = te$Chisq[2],
                                            p = te$`Pr(>Chisq)`[2]))

if(TRUE){
worker_model <- lmer(Wmax_Ne ~ (prop_snh  + prop_early_crop+ I(prop_early_crop^2)) * drought + (1|id) , 
                     REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)

  #snh
  model0 <- lmer(Wmax_Ne ~ (prop_early_crop+ I(prop_early_crop^2)) * drought + (1|id), REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
  te <- anova(worker_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'Wmax',var = 'SNH',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
  #prop_early_crop
  model0 <- lmer(Wmax_Ne ~ prop_snh * drought  + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
  te <- anova(worker_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'Wmax',var = 'prop early_crop',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
  #drought
  model0 <- lmer(Wmax_Ne ~ (prop_snh  + prop_early_crop+ I(prop_early_crop^2)) + (1|id), REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
  te <- anova(worker_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'Wmax',var = 'drought',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
}else{
  worker_model <- lmer(Wmax_Ne ~ (prop_snh  + prop_early_crop+ I(prop_early_crop^2)) * duration + (1|id), 
                       REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
  #snh
  model0 <- lmer(Wmax_Ne ~ (prop_early_crop+ I(prop_early_crop^2)) * duration+ (1|id), REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
  te <- anova(worker_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'Wmax',var = 'SNH',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
  #crop_diversity
  model0 <- lmer(Wmax_Ne ~ prop_snh * duration  + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
  te <- anova(worker_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'Wmax',var = 'prop early_crop',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
  #drought
  model0 <- lmer(Wmax_Ne ~ (prop_snh  + prop_early_crop+ I(prop_early_crop^2)) + (1|id), REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
  te <- anova(worker_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'Wmax',var = 'drought',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
}
  ## gueens
  queen_model <- lmer(Q_Ne ~ (prop_snh  + prop_early_crop+ I(prop_early_crop^2)) * drought + (1|id) , REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
  #snh
  model0 <- lmer(Q_Ne ~ (prop_early_crop + I(prop_early_crop^2)) * drought + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
  te <- anova(queen_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'QT',var = 'SNH',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
  #crop_diversity
  model0 <- lmer(Q_Ne ~ prop_snh * drought + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
  te <- anova(queen_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'QT',var = 'prop early_crop',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
  #drought
  model0 <- lmer(Q_Ne ~ (prop_snh  + prop_early_crop+ I(prop_early_crop^2)) + (1|id), REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
  te <- anova(queen_model, model0)
  test_result <- rbind(test_result,data.frame(output = 'QT',var = 'drought',
                                              df = te$Df[2],
                                              Chisq = te$Chisq[2],
                                              p = te$`Pr(>Chisq)`[2]))
}

test_result$pv = round(test_result$p*10000)
test_result

anova(worker_model, type= 2)
anova(poll_model, type= 2)
anova(queen_model, type= 2)

model = worker_model #
model = poll_model # 
model = queen_model#

summary(model)
par(mfrow= c(2, 2))
plot(model)
qqPlot(resid(model))
hist(resid(model)) #good
vif(model)
model_performance (model)


## plots ms
{
a = plot_model(poll_model, type = "pred",terms = c("prop_snh [all]","drought"), show.data = TRUE,dot.size = 0.5)+ theme_bw() +
  xlab("% SNH") +
  ylab("PS") + 
  theme(axis.text = element_text(size=7), axis.title = element_text(size=7))+
  scale_x_continuous(breaks=c(0.05,0.15,0.25))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 7),
         legend.text = element_text(size = 7), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")
a

b= plot_model(poll_model, type = "pred", terms = c("prop_early_crop [all]","drought"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("% Early MFC") +
  ylab("PS")+ 
  scale_x_continuous(breaks=c(0.1,0.3,0.5))+
  theme(axis.text = element_text(size=7), axis.title = element_text(size=7))+
  #scale_fill_manual(values= c("#1e96a6","#a6721e")) +
  #scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 7),
         legend.text = element_text(size = 7), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")
b



d = plot_model(worker_model, type = "pred", terms = c("prop_snh [all]","drought"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("% SNH") +
  ylab("MaxW")+ 
  scale_x_continuous(breaks=c(0.05,0.15,0.25))+
  theme(axis.text = element_text(size=7), axis.title = element_text(size=7))+
  #scale_fill_manual(values= c("#1e96a6","#a6721e")) +
  #scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 7),
         legend.text = element_text(size = 7), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")
d
e= plot_model(worker_model, type = "pred", terms = c("prop_early_crop [all]", "drought"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("% Early MFC") +
  ylab("MaxW")+ 
  scale_x_continuous(breaks=c(0.1,0.3,0.5))+
  theme(axis.text = element_text(size=7), axis.title = element_text(size=7))+
  #scale_fill_manual(values= c("#1e96a6","#a6721e")) +
  #scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 7),
         legend.text = element_text(size = 7), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")
e




g = plot_model(queen_model, type = "pred", terms = c("prop_snh [all]","drought"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("% SNH") +
  ylab("TQ")+ 
  scale_x_continuous(breaks=c(0.05,0.15,0.25))+
  theme(axis.text = element_text(size=7), axis.title = element_text(size=7))+
  #scale_fill_manual(values= c("#1e96a6","#a6721e")) +
  #scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 7),
         legend.text = element_text(size = 7), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")
g
h= plot_model(queen_model, type = "pred", terms = c("prop_early_crop [all]","drought"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("% Early MFC") +
  ylab("TQ")+ 
  scale_x_continuous(breaks=c(0.1,0.3,0.5))+
  theme(axis.text = element_text(size=7), axis.title = element_text(size=7))+
  #scale_fill_manual(values= c("#1e96a6","#a6721e")) +
  #scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 7),
         legend.text = element_text(size = 7), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")
h
}

#extra plots supplementary for the drought duration
poll_model2 <- lmer(poll ~ d_length + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
summary(poll_model2)
worker_model2 <- lmer(Wmax_Ne ~ d_length  + (1|id) , REML = FALSE, control = lmerControl(optimizer ="bobyqa"), data = df)
summary(worker_model2)
queen_model2 <- lmer(Q_Ne ~ d_length  + (1|id) , REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"), data = df)
summary(queen_model2)

# drought plots
{

poll_model2_plot= plot_model(poll_model2, type = "pred", terms = c("d_length[all]"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
      xlab("Drought length") +
      ylab("PS")+ 
      theme(axis.text = element_text(size=9), axis.title = element_text(size=9))+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
      theme (legend.position = "bottom",legend.title = element_text(size = 8),
             legend.text = element_text(size = 8), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
      geom_line(key_glyph = draw_key_rect)+
      ggtitle ("")
poll_model2_plot
worker_model2_plot= plot_model(worker_model2, type = "pred", terms = c("d_length[all]"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
      xlab("Drought length") +
      ylab("MaxW")+ 
      
      theme(axis.text = element_text(size=9), axis.title = element_text(size=9))+
      scale_fill_manual(values= c("#1e96a6","#a6721e")) +
      scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
      theme (legend.position = "bottom",legend.title = element_text(size = 8),
             legend.text = element_text(size = 8), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
      geom_line(key_glyph = draw_key_rect)+
      ggtitle ("")
worker_model2_plot

queen_model2_plot= plot_model(queen_model2, type = "pred", terms = c("d_length[all]"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("Drought length") +
  ylab("TQ")+ 
  theme(axis.text = element_text(size=9), axis.title = element_text(size=9))+
  scale_fill_manual(values= c("#1e96a6","#a6721e")) +
  scale_color_manual(name= "Drought" , values =  c("#1e96a6","#a6721e"),breaks= c("No", "Yes"),labels=c("No", "Yes")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  guides(color = guide_legend(title.position = "top",title.hjust = 0.5))+
  theme (legend.position = "bottom",legend.title = element_text(size = 8),
         legend.text = element_text(size = 8), legend.key.height = unit(0.2, 'cm'),legend.key.width = unit(0.2, 'cm'))+
  geom_line(key_glyph = draw_key_rect)+
  ggtitle ("")

queen_model2_plot
}



## 

plots = cowplot::plot_grid (d, g, a, e, h, b, ncol=3,
                            labels = c("(a)", "(b)","(c)","(d)","(e)","(f)"), align = "h",label_size = 12)
plots

ggsave(
  "regression_plots_final2.png",
  plot = plots,
  width = 13,
  height = 12,
  units = "cm")
dev.off()



pdf("drought_length_plots.pdf", width = 7.5, height = 2.5)
plots_drought_length <- cowplot::plot_grid (poll_model2_plot, worker_model2_plot, queen_model2_plot, ncol=3,
                                            labels = c("(a)", "(b)","(c)"), align = "v",label_size = 12)
plots_drought_length                                            
dev.off()



mod <- lm(flor*1000000 ~ d_length, data=df)
summary(mod)

a1= plot_model(mod, type = "pred", terms = c("d_length"), show.data = TRUE,dot.size = 0.5)+ theme_bw()+
  xlab("Drought length") +
  ylab("Floral resources")+ 
  theme(axis.text = element_text(size=9), axis.title = element_text(size=9))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ theme(legend.position = "right")+
  ggtitle ("")
a1

