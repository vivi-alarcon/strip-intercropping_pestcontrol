##
# pest control in canola cultivars data analisis
# master thesis - strip-intercropping 
# agroecology lab unigöttingen - 2019
###


#directory
setwd("~/Dropbox/Msc_ECOLOGY_Bremen/a.Master_Thesis/A_Multicropping/b_thesis/r/intermediate")
library(glmmTMB)
library(DHARMa)
library(effects)
larvae.df <-(read.csv("larvae_df_complete2.csv",sep= ","))
head(larvae.df)
nrow(larvae.df)
str(larvae.df)
larvae.df$Farm_id <- as.factor(larvae.df$Farm_id)
larvae.df$total.larvae  <- as.numeric(larvae.df$total.larvae )
larvae.df$parasited_larvae  <- as.numeric(larvae.df$parasited_larvae )
larvae.df$total_eggs  <- as.numeric(larvae.df$total_eggs )
larvae.df$Trap_ID  <- as.factor(larvae.df$Trap_ID)

# add the field column 

field = substr(larvae.df$Trap_ID, 1,3)
field = factor(field)
larvae.df$field <- field
head(larvae.df)

strips.larvae <- subset(larvae.df,type_id=="S")
head(strips.larvae)

nrow(larvae.df)
mean(larvae.df$total.larvae) # 23 larvae per plot 
max(larvae.df$total.larvae) # 56 larvae per plot 
min(larvae.df$total.larvae) # 5 larvae per plot 
 # larvae density ####


larvae_summary11 <- larvae.df %>% # the names of the new data frame and the data frame to be summarised
  group_by(type_id) %>%   # the grouping variable
  summarise(mean = mean(total.larvae),  # calculates the mean of each group
            sd = sd(total.larvae), # calculates the standard deviation of each group
            sample_size = n(),  # calculates the sample size per group
            SE = sd(total.larvae)/sqrt(n())) # calculates the standard error of each group
head(larvae_summary11)


# cultivation method 
ab.larvae <- glmmTMB(total.larvae~t1+(1|field), data=larvae.df, family = nbinom2(link = "log"))
ab.larvaesimulationOutput <- simulateResiduals(fittedModel = ab.larvae, n = 250)
testUniformity(simulationOutput = ab.larvaesimulationOutput)
testDispersion(ab.larvaesimulationOutput) 
testResiduals(ab.larvaesimulationOutput)
testZeroInflation(ab.larvaesimulationOutput) 
plot(allEffects(ab.larvae))
summary(ab.larvae)
drop1(ab.larvae,test="Chi")

# insecticide use 
head(strips.larvae)
str(strips.larvae)
strips.larvae$insecticide <- as.factor(strips.larvae$insecticide)

ab.larvae2 <- glmmTMB(total.larvae~insecticide+(1|field), data=strips.larvae, family = nbinom2(link = "log"))
ab.larvae2simulationOutput <- simulateResiduals(fittedModel = ab.larvae2, n = 250)
testUniformity(simulationOutput = ab.larvae2simulationOutput)
testDispersion(ab.larvae2simulationOutput) 
testResiduals(ab.larvae2simulationOutput)
testZeroInflation(ab.larvae2simulationOutput) 
plot(allEffects(ab.larvae2))
summary(ab.larvae2)
drop1(ab.larvae2,test="Chi")


# global model larvae - parasitism rate  M vs S####
head(larvae.df)
m2_larvae <- glmmTMB(cbind(parasited_larvae, total.larvae-parasited_larvae) ~ t1 + (1 | field),
                     data=larvae.df, family="binomial") 

m2_larvaesimulationOutput <- simulateResiduals(fittedModel = m2_larvae, n = 250)
testUniformity(simulationOutput = m2_larvaesimulationOutput)
testDispersion(m2_larvaesimulationOutput) 
testResiduals(m2_larvaesimulationOutput)
testZeroInflation(m2_larvaesimulationOutput) 
plot(m2_larvaesimulationOutput)
plot(allEffects(m2_larvae)) 

summary(m2_larvae)
drop1(m2_larvae,test="Chi") # no differences 

# model S1 vs S2 larvae #### 
m3_larvae <- glmmTMB(cbind(parasited_larvae, non_par_larvae) ~ insecticide + (1 | field),
                     data=strips.larve, family="binomial") 

m3_larvaesimulationOutput <- simulateResiduals(fittedModel = m3_larvae, n = 250)
testUniformity(simulationOutput = m3_larvaesimulationOutput)
testDispersion(m3_larvaesimulationOutput) #under or overdispersed ¿?
testResiduals(m3_larvaesimulationOutput)
testZeroInflation(m3_larvaesimulationOutput) 
plot(m3_larvaesimulationOutput)
plot(allEffects(m3_larvae)) 

summary(m3_larvae)
drop1(m3_larvae,test="Chi") 

# model E vs M ####

# various tests for larvae infestation data <-  binomial vs betabinomial <-  glm vs glmtb

# parasited larvae in canola <-  model testing 

# the binomial model gives me a very significant result, but by checking the raw data it seems it is not like that 
m4_larvae <- glmmTMB(cbind(parasited_larvae, total.larvae-parasited_larvae) ~ loc_strip1 + (1 | field),
                     data=strips.larvae, family="binomial") 
droplevels(strips.larvae$loc_strip1)
m4_larvaesimulationOutput <- simulateResiduals(fittedModel = m4_larvae, n = 250)
testUniformity(simulationOutput = m4_larvaesimulationOutput)
testDispersion(m4_larvaesimulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m4_larvaesimulationOutput)
testZeroInflation(m4_larvaesimulationOutput) 
plot(m4_larvaesimulationOutput)
plot(allEffects(m4_larvae)) 

summary(m4_larvae)
drop1(m4_larvae,test="Chi") 

head(strips.larvae)
strips.larvae$proportion.par.larvae <- (strips.larvae$parasited_larvae)/(strips.larvae$total.larvae)

plot(strips.larvae$loc_strip1,strips.larvae$proportion.par.larvae)

# tryiong binomial with proportions <-  ver bad fiitted model 
m4_larvae2 <- glmmTMB( proportion.par.larvae ~ loc_strip1 + (1 | field), weights = total.larvae,
                       data=strips.larvae, family="binomial") 

m4_larvae2simulationOutput <- simulateResiduals(fittedModel = m4_larvae2, n = 250)
testUniformity(simulationOutput = m4_larvae2simulationOutput)
testDispersion(m4_larvae2simulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m4_larvae2simulationOutput)
testZeroInflation(m4_larvae2simulationOutput) 
plot(m4_larvae2simulationOutput)
plot(allEffects(m4_larvae2)) 
summary(m4_larvae2)
drop1(m4_larvae2,test="Chi") 

# tring with the old way <-  glmer <-  same significante (high and oversdispersion)
library(lme4)
library(car)
m4_larvae3 <- glmer(cbind(parasited_larvae, total.larvae-parasited_larvae) ~ loc_strip1 + (1 | field),
                    data=strips.larvae, family="binomial") 
plot(m4_larvae3)
plot(allEffects(m4_larvae3))
summary(m4_larvae3)
drop1(m4_larvae3,test="Chi") 

m4_larvae3simulationOutput <- simulateResiduals(fittedModel = m4_larvae3, n = 250)
testUniformity(simulationOutput = m4_larvae3simulationOutput)
testDispersion(m4_larvae3simulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m4_larvae3simulationOutput)
testZeroInflation(m4_larvae3simulationOutput) 
plot(m4_larvae3simulationOutput)

strips.larvae$proportion.par.larvae
# oldstyle with proportions  <-  very bad fitted model 
m4_larvae4 <- glmer( proportion.par.larvae ~ loc_strip1 + (1 | field), weights = total.larvae,
                     data=strips.larvae, family="binomial") 
plot(m4_larvae4)
plot(allEffects(m4_larvae4))
summary(m4_larvae4)
drop1(m4_larvae4,test="Chi") 

m4_larvae4simulationOutput <- simulateResiduals(fittedModel = m4_larvae4, n = 250)
testUniformity(simulationOutput = m4_larvae4simulationOutput)
testDispersion(m4_larvae4simulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m4_larvae4simulationOutput)
testZeroInflation(m4_larvae4simulationOutput) 
plot(m4_larvae4simulationOutput)

# trying betabinomial
betabinomial(link = "logit")
# with the proportion <-  very bad fitted model 
m4_larvaebeta1 <- glmmTMB( proportion.par.larvae ~ loc_strip1 + (1 | field), weights = total.larvae,
                           data=strips.larvae, family=betabinomial(link = "logit")) 
m4_larvaebeta1simulationOutput <- simulateResiduals(fittedModel = m4_larvaebeta1, n = 250)
testUniformity(simulationOutput = m4_larvaebeta1simulationOutput)
testDispersion(m4_larvaebeta1simulationOutput,alternative = "greater") 
testResiduals(m4_larvaebeta1simulationOutput)
testZeroInflation(m4_larvaebeta1simulationOutput) 
plot(m4_larvaebeta1simulationOutput)


# with cbind  <-  well fitted <-  still significant results 
m4_larvaebeta2 <- glmmTMB(cbind(parasited_larvae, total.larvae-parasited_larvae) ~ loc_strip1 + (1 | field),
                          data=strips.larvae, family=betabinomial(link = "logit")) 
m4_larvaebeta2simulationOutput <- simulateResiduals(fittedModel = m4_larvaebeta2, n = 250)
testUniformity(simulationOutput = m4_larvaebeta2simulationOutput)
testDispersion(m4_larvaebeta2simulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m4_larvaebeta2simulationOutput)
testZeroInflation(m4_larvaebeta2simulationOutput) 
plot(m4_larvaebeta2simulationOutput)
plot(allEffects(m4_larvaebeta2))
summary(m4_larvaebeta2)
drop1(m4_larvaebeta2,test="Chi") 

# checking old style <-  not supported betabinommial 
m4_larvaebeta2.2 <- glmer(cbind(parasited_larvae, total.larvae-parasited_larvae) ~ loc_strip1 + (1 | field),
                          data=strips.larvae, family=betabinomial(link = "logit")) 

## ploting raw data as a barplot of the means with SE 
larvae_summary3 <- strips.larvae %>% # the names of the new data frame and the data frame to be summarised
  group_by(loc_strip1) %>%   # the grouping variable
  summarise(mean = mean(proportion.par.larvae),  # calculates the mean of each group
            sd = sd(proportion.par.larvae), # calculates the standard deviation of each group
            sample_size = n(),  # calculates the sample size per group
            SE = sd(proportion.par.larvae)/sqrt(n())) # calculates the standard error of each group
head(larvae_summary3)

dodge <- position_dodge(width = 0.9)
larvae3 <- ggplot(larvae_summary3, aes(loc_strip1, mean, fill= loc_strip1)) + 
  geom_col(position=dodge,width = 0.9) +  
  scale_fill_manual(values=c("gray42","gray70"),name = "", labels = c("Edge traps", "Interior traps")) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.2,alpha=0.9, size=0.5,position=dodge)  + 
  labs(y="Proportion of parasited larvae per plot", x = "") + coord_cartesian (ylim = c(0, 1)) +
  theme_cowplot() + theme(axis.title.x = element_text(size=1, color = "black"),
                          axis.title.y = element_text(size=11, color = "black"),
                          axis.text.x = element_blank(), axis.ticks = element_blank(),legend.position = c(0.1, 0.9))
larvae3

## fig 7 plor of the model ####
Fig7 <- plot(ggpredict(m4_larvaebeta2, "loc_strip1"),show.title = FALSE,connect.lines = TRUE) + coord_cartesian (ylim = c(0, 1)) + 
  theme_cowplot() + theme(axis.title.x = element_text(size=10, color = "black"),
                          axis.title.y = element_text(size=12, color = "black"),
                          axis.text.x = element_text(size=14)) + labs(title = "Oilseed rape \n strip-intercropping",y="Parasitism rate \n (infested larvae per plot)", x = "") +
  theme(plot.title = element_text(hjust = 0.5,size=15,color = "black",face="plain"))
Fig7

plot_grid(Fig6,Fig7, 
          labels=c('a)', 'b)'))

# parasitisim rate 2 : mean eggs per larvae 
str(larvae.df)
hist(larvae.df$mean.egg.larvae)

m.eggs <- glmmTMB( mean.egg.larvae ~ t1 + (1 | field),
                   data=larvae.df, family="gaussian") 

m.eggssimulationOutput <- simulateResiduals(fittedModel = m.eggs, n = 250)
testUniformity(simulationOutput = m.eggssimulationOutput)
testDispersion(m.eggssimulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m.eggssimulationOutput)
testZeroInflation(m.eggssimulationOutput) 
plot(m.eggssimulationOutput)
plot(allEffects(m.eggs))
summary(m.eggs)
drop1(m.eggs,test="Chi") 


m.eggs2 <- glmmTMB( mean.egg.larvae ~ loc_strip1 + (1 | field),
                    data=strips.larvae, family="gaussian") 

m.eggs2simulationOutput <- simulateResiduals(fittedModel = m.eggs2, n = 250)
testUniformity(simulationOutput = m.eggs2simulationOutput)
testDispersion(m.eggs2simulationOutput,alternative = "greater") # seems to be overdispersed
testResiduals(m.eggs2simulationOutput)
testZeroInflation(m.eggs2simulationOutput) 
plot(m.eggs2simulationOutput)
plot(allEffects(m.eggs2))
summary(m.eggs2)
drop1(m.eggs2,test="Chi") 

dat=ggpredict(m.eggs2, terms = c("loc_strip1"))

fig7d <- ggplot(dat, aes(x, predicted, fill= x)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=0.2,alpha=0.9, size=0.8,position=dodge)  + 
  geom_point(data=strips.larvae,aes(y=strips.larvae$mean.egg.larvae,
                                    x=loc_strip1, fill=loc_strip1,colour=loc_strip1))+
  scale_color_manual(values=c("gray70","gray42")) +
  labs(y="Proportion of parasited larvae per plot", x = "") + coord_cartesian (ylim = c(0, 5)) +
  theme_cowplot() + theme(axis.title.x = element_text(size=1, color = "black"),
                          axis.title.y = element_text(size=11, color = "black"),
                          axis.text.x = element_blank(),legend.position = "none")+
  labs(title = "Oilseed rape \n strip-intercropping",y="Mean number of eggs per larvae", x = "") +
  theme(plot.title = element_text(hjust = 0.5,size=15,color = "black",face="plain"))
fig7d



