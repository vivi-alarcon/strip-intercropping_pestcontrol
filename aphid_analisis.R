##
# pest control in wheat cultivars data analisis
# master thesis - strip-intercropping 
# agroecology lab unigöttingen - 2019
###

setwd("~/Dropbox/Msc_ECOLOGY_Bremen/a.Master_Thesis/A_Multicropping/b_thesis/r")
library(glmmTMB)
library(DHARMa)
library(effects)


# wheat pest control
aphids.df <-(read.csv("aphids_df_complete.csv",sep= ","))
head(aphids.df)
str(aphids.df)
nrow(aphids.df)

aphids.df$Farm_id <- as.factor(aphids.df$Farm_id)
aphids.df$N.aphids[is.na(aphids.df$N.aphids)] <- 0
aphids.df$N.mummies.plot[is.na(aphids.df$N.mummies.plot)] <- 0
aphids.df$N.mummies.out[is.na(aphids.df$N.mummies.out)] <- 0
# adding the field column 
field = substr(aphids.df$Trap_ID, 1,3)
field = factor(field)
aphids.df$field <- field
# for using cbind binomial <-  create the sucess colum 
aphids.df$total_aphids <- aphids.df$N.aphids
aphids.df$total_mummies <- aphids.df$N.mummies.plot

aphids.df$non_parasited_aphids <- (aphids.df$total_aphids)-(aphids.df$N.mummies.plot)
#here i have some negative values 
aphids.df$non_parasited_aphids[aphids.df$non_parasited_aphids<0] <- 0

# for ploing purposes… adding the proportion of mummified aphids per plot 
aphids.df$proportion <- (aphids.df$total_mummies)/(aphids.df$total_aphids)
aphids.df$proportion[!is.finite(aphids.df$proportion)] <- 0
# adding aphid density 
aphids.df$aphid_density <- (aphids.df$total_aphids)/50
# plot raw data proportion aphids #### 
plot(aphids.df$proportion~aphids.df$t1)

### general data description for aphids ####
head(aphids.df)
mean(aphids.df$total_aphids) # 12 aphids per plot 

###### description stat of monocultures and strip general #### 
head(aphids.df)
mono.aphids.summary <- aphids.df %>% # the names of the new data frame and the data frame to be summarised
  group_by(type_id) %>%   # the grouping variable
  summarise(mean = mean(total_aphids),  # calculates the mean of each group
            sd = sd(total_aphids), # calculates the standard deviation of each group
            sample_size = n(),  # calculates the sample size per group
            SE = sd(total_aphids)/sqrt(n())) # calculates the standard error of each group
head(mono.aphids.summary)

## separating insecticide window 
insecticide=subset(aphids.df, insecticide>=1)
head(insecticide)
str(insecticide)
insecticide$insecticide=as.factor(insecticide$insecticide)


# aphid density: S1 vs S2 ####
ab.aphids <- glmmTMB(total_aphids~insecticide+(1|field), data=insecticide, family = nbinom2(link = "log"))
ab.aphidssimulationOutput <- simulateResiduals(fittedModel = ab.aphids, n = 250)
testUniformity(simulationOutput = ab.aphidssimulationOutput)
testDispersion(ab.aphidssimulationOutput) 
testResiduals(ab.aphidssimulationOutput)
testZeroInflation(ab.aphidssimulationOutput) 
plot(allEffects(ab.aphids))
summary(ab.aphids)
drop1(ab.aphids,test="Chi") 
# aphid densities did not differed between S1 and S2

# aphid density: M vs S (including all) ####
ab.aphids <- glmmTMB(total_aphids~t1+(1|field), data=aphids.df, family = nbinom2(link = "log"))
ab.aphidssimulationOutput <- simulateResiduals(fittedModel = ab.aphids, n = 250)
testUniformity(simulationOutput = ab.aphidssimulationOutput)
testDispersion(ab.aphidssimulationOutput) 
testResiduals(ab.aphidssimulationOutput)
testZeroInflation(ab.aphidssimulationOutput) 
plot(allEffects(ab.aphids))
summary(ab.aphids)
drop1(ab.aphids,test="Chi") 
# aphid densities did not differed between M and S 

##### BINOMIAL MODELS - APHID PARASITISM ####

# parasitism rate M vs S  ####
head(aphids.df)
m2_aphids <- glmmTMB(cbind(total_mummies, non_parasited_aphids) ~ t1 + (1 | field),
                     data=aphids.df, family="binomial")
unique(aphids.df$t1) #M S

m2_aphidssimulationOutput <- simulateResiduals(fittedModel = m2_aphids, n = 250)
testUniformity(simulationOutput = m2_aphidssimulationOutput)
testDispersion(m2_aphidssimulationOutput) 
testResiduals(m2_aphidssimulationOutput)
testZeroInflation(m2_aphidssimulationOutput) 
plot(m2_aphidssimulationOutput)
plot(allEffects(m2_aphids))

summary(m2_aphids)
drop1(m2_aphids,test="Chi") # not significant

# trying with the minus in the binomial fit #### 
head(aphids.df)
m1_aphids <- glmmTMB(cbind(total_mummies, total_aphids-total_mummies) ~ t1 + (1 | field),
                     data=aphids.df, family="binomial")

m1_aphidssimulationOutput <- simulateResiduals(fittedModel = m1_aphids, n = 250)
testUniformity(simulationOutput = m1_aphidssimulationOutput)
testDispersion(m1_aphidssimulationOutput) 
testResiduals(m1_aphidssimulationOutput)
testZeroInflation(m1_aphidssimulationOutput) 
plot(m1_aphidssimulationOutput)
plot(allEffects(m1_aphids))
ggpredict(m1_aphids)
summary(m1_aphids)
drop1(m1_aphids,test="Chi") # not significant



# fig s2 ####
# testing if insecticide window affect mummification rates  Fig. S2d ####
insecticide.m <- glmmTMB(cbind(total_mummies, non_parasited_aphids) ~ insecticide + (1 | field),
                         data=insecticide, family="binomial")
insecticide.msimulationOutput <- simulateResiduals(fittedModel = insecticide.m, n = 250)
testUniformity(simulationOutput = insecticide.msimulationOutput)
testDispersion(insecticide.msimulationOutput) 
testResiduals(insecticide.msimulationOutput)
testZeroInflation(insecticide.msimulationOutput) 
plot(insecticide.msimulationOutput)
plot(allEffects(insecticide.m))
summary(insecticide.m)
drop1(insecticide.m,test="Chi") # no differences between S1 and S1 so I will pool all the data for the next analisis. 




# fig 7a <-  edge vs middle traps ####


head(aphids.df)
df.strips.aphids <- subset(aphids.df, type_id=="S")
str(df.strips.aphids)
df.strips.aphids$proportion.par.aphids <- (df.strips.aphids$total_mummies)/(df.strips.aphids$total_aphids)

plot(df.strips.aphids$proportion.par.aphids~df.strips.aphids$loc_strip1)

m42_aphids <- glmmTMB(cbind(total_mummies, total_aphids-total_mummies) ~ loc_strip1 + (1 | field),
                      data=df.strips.aphids, family="binomial")

m42_aphidssimulationOutput <- simulateResiduals(fittedModel = m42_aphids, n = 250)
testUniformity(simulationOutput = m42_aphidssimulationOutput)
testDispersion(m42_aphidssimulationOutput, alternative = "greater") # seems not overdispersed?
testDispersion(m42_aphidssimulationOutput, alternative = "less")
testResiduals(m42_aphidssimulationOutput)
testOutliers(m42_aphidssimulationOutput)
plot(m42_aphidssimulationOutput)
plot(allEffects(m42_aphids))
summary(m42_aphids) 
drop1(m42_aphids,test="Chi") # significant (the graph looks weird )

dat=ggpredict(m42_aphids, terms = c("loc_strip1"))
fig7a <- ggplot(dat, aes(x, predicted, fill= x)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=0.2,alpha=0.9, size=0.8,position=dodge)  + 
  geom_point(data=strips.larvae,aes(y=strips.larvae$parasited_larvae/strips.larvae$total.larvae,
                                    x=loc_strip1, fill=loc_strip1,colour=loc_strip1))+
  scale_color_manual(values=c("gray70","gray42")) +
  labs(y="Proportion of parasited aphids per plot", x = "") + coord_cartesian (ylim = c(0, 1)) +
  theme_cowplot() + theme(axis.title.x = element_text(size=1, color = "black"),
                          axis.title.y = element_text(size=11, color = "black"),
                          axis.text.x = element_blank(),legend.position = c(0.1, 0.9))+
  labs(title = "Wheat \n strip-intercropping",y="Parasitism rate \n (infested aphids per plot)", x = "") +
  theme(plot.title = element_text(hjust = 0.5,size=15,color = "black",face="plain"))
fig7a


# tring with the old way <-  glmer <-  same significante (high and oversdispersion)
library(lme4)
library(car)
m4_aphidsold<- glmer(cbind(total_mummies, total_aphids-total_mummies) ~ loc_strip1 + (1 | field),
                    data=df.strips.aphids, family="binomial") 
plot(m4_aphidsold)
plot(allEffects(m4_aphidsold))
summary(m4_aphidsold)
drop1(m4_aphidsold,test="Chi")  # sig 0.04

m4_aphidsoldsimulationOutput <- simulateResiduals(fittedModel = m4_aphidsold, n = 250)
testUniformity(simulationOutput = m4_aphidsoldsimulationOutput)
testDispersion(m4_aphidsoldsimulationOutput,alternative = "less") # seems to be overdispersed
testResiduals(m4_aphidsoldsimulationOutput)
testZeroInflation(m4_aphidsoldsimulationOutput) 
plot(m4_aphidsoldsimulationOutput)


# with proportion 
# head(df.strips.aphids)
# m4_larvae2 <- glmmTMB( proportion.par.aphids ~ loc_strip1 + (1 | field), weights = total_aphids,
#                        data=df.strips.aphids, family="binomial") 
# 
# m4_larvae2simulationOutput <- simulateResiduals(fittedModel = m4_larvae2, n = 250)
# testUniformity(simulationOutput = m4_larvae2simulationOutput)
# testDispersion(m4_larvae2simulationOutput,alternative = "greater") # seems to be overdispersed
# testResiduals(m4_larvae2simulationOutput)
# testZeroInflation(m4_larvae2simulationOutput) 
# plot(m4_larvae2simulationOutput)
# plot(allEffects(m4_larvae2)) 
# summary(m4_larvae2)
# drop1(m4_larvae2,test="Chi") # deviated 

# betabinomial with cbind  <-  well fitted <-  still significant results 
m4_aphidsbeta2 <- glmmTMB(cbind(total_mummies, total_aphids-total_mummies) ~ loc_strip1 + (1 | field),
                          data=df.strips.aphids, family=betabinomial(link = "logit")) 

m4_aphidsbeta2simulationOutput <- simulateResiduals(fittedModel = m4_aphidsbeta2, n = 250)
testUniformity(simulationOutput = m4_aphidsbeta2simulationOutput)
testDispersion(m4_aphidsbeta2simulationOutput,alternative = "less") # seems to be overdispersed
testResiduals(m4_aphidsbeta2simulationOutput)
testZeroInflation(m4_aphidsbeta2simulationOutput) 
plot(m4_aphidsbeta2simulationOutput)
plot(allEffects(m4_aphidsbeta2))
summary(m4_aphidsbeta2)
drop1(m4_aphidsbeta2,test="Chi") # not significant!!!



#### Proportion of ecloded wasps??

# with the whole dataframe <-  probably zero inflated

m_wasps1 <- glmmTMB(cbind(N.wasp, N.mummies.out-N.wasp) ~ t1 + (1 | field),
                    data=aphids.df, family="binomial")
m_wasps1simulationOutput <- simulateResiduals(fittedModel = m_wasps1, n = 250)
testUniformity(simulationOutput = m_wasps1simulationOutput)
testDispersion(m_wasps1simulationOutput) 
testResiduals(m_wasps1simulationOutput)
testOutliers(m_wasps1simulationOutput)
plot(m_wasps1simulationOutput)
plot(allEffects(m_wasps1))
summary(m_wasps1)
drop1(m_wasps1,test="Chi") 
# checking plots 
plot(aphids.df$N.mummies.out~aphids.df$N.wasp)
plot(aphids.df$N.mummies.out~aphids.df$t1)
plot(aphids.df$N.wasp~aphids.df$t1)

Fig6b <- plot(ggpredict(m_wasps1, "t1"),show.title = FALSE,connect.lines = TRUE) + coord_cartesian (ylim = c(0, 1)) + 
  theme_cowplot() + theme(axis.title.x = element_text(size=10, color = "black"),
                          axis.title.y = element_text(size=12, color = "black"),
                          axis.text.x = element_text(size=14)) + labs(title = "Cultivation Method",y="Eclosion rate \n (ecloded wasps per plot) ", x = "") +
  theme(plot.title = element_text(hjust = 0.5,size=14,color = "black",face="plain"))
Fig6b


wasp.test <- aphids.df %>% # the names of the new data frame and the data frame to be summarised
  group_by(t1) %>%   # the grouping variable
  summarise(mean = mean(N.wasp),  # calculates the mean of each group
            sd = sd(N.wasp), # calculates the standard deviation of each group
            sample_size = n(),  # calculates the sample size per group
            SE = sd(N.wasp)/sqrt(n())) # calculates the standard error of each group
head(wasp.test)
dodge <- position_dodge(width=0.9)
fig.wasp.test <- ggplot(wasp.test, aes(t1, mean, fill= t1)) + 
  geom_col(position=dodge) +  
  scale_fill_manual(values=c("gray70","gray42")) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.2,alpha=0.9, size=0.5,position=dodge)  + 
  labs(title="",y="mean wasp per plot", x = "") + coord_cartesian (ylim = c(0, 1)) +
  theme_cowplot() + theme(axis.title.x = element_text(size=1, color = "black"),
                          axis.title.y = element_text(size=12, color = "black"),
                          axis.text.x = element_text(size=12, color = "black"),legend.position = "none") 
fig.wasp.test



# using only the points were we found the wasps.. 
head(aphids.df)
wasp.df <- subset(aphids.df,N.wasp >=1)
wasp.df$eclosion_rate <- wasp.df$N.wasp/wasp.df$N.mummies.out
head(wasp.df)
nrow(wasp.df) # 16 datapoints were we got wasps
wasp.df$no.wasp <- wasp.df$N.mummies.out-wasp.df$N.wasp
# checking plots 
plot(wasp.df$N.mummies.out~wasp.df$N.wasp)
plot(wasp.df$N.mummies.out~wasp.df$t1)
plot(wasp.df$N.wasp~wasp.df$t1)


# model
m_wasps <- glmmTMB(cbind(N.wasp, no.wasp) ~ t1 + (1 | field),
                     data=wasp.df, family="binomial")
m_waspssimulationOutput <- simulateResiduals(fittedModel = m_wasps, n = 250)
testUniformity(simulationOutput = m_waspssimulationOutput)
testDispersion(m_waspssimulationOutput) 
testResiduals(m_waspssimulationOutput)
plot(m_waspssimulationOutput)
plot(allEffects(m_wasps))
summary(m_wasps)
drop1(m_wasps,test="Chi")


Fig6b <- plot(ggpredict(m_wasps, "t1"),show.title = FALSE,connect.lines = TRUE) + coord_cartesian (ylim = c(0, 1)) + 
  theme_cowplot() + theme(axis.title.x = element_text(size=10, color = "black"),
                          axis.title.y = element_text(size=12, color = "black"),
                          axis.text.x = element_text(size=14)) + labs(title = "",y="wasp eclosion rate", x = "") +
  theme(plot.title = element_text(hjust = 0.5,size=14,color = "black",face="plain"))
Fig6b

# wasp eclosion according to trap location ####
head(df.strips.aphids)
df.strips.aphids$loc_strip1
# m_wasps1 <- glmmTMB(cbind(N.wasp, N.mummies.out-N.wasp) ~ loc_strip1 + (1 | field),
#                     data=df.strips.aphids, family="binomial")

m_wasps1 <- glmmTMB(cbind(N.wasp, N.mummies.out-N.wasp) ~ loc_strip1 + (1 | field),
                    data=df.strips.aphids, family=betabinomial(link = "logit"))

m_wasps1simulationOutput <- simulateResiduals(fittedModel = m_wasps1, n = 250)
testUniformity(simulationOutput = m_wasps1simulationOutput) # not too well fitted 
testDispersion(m_wasps1simulationOutput,alternative = "greater") 
testResiduals(m_wasps1simulationOutput)
testOutliers(m_wasps1simulationOutput)
plot(m_wasps1simulationOutput)
plot(allEffects(m_wasps1))
summary(m_wasps1)
drop1(m_wasps1,test="Chi")

dat=ggpredict(m_wasps1, terms = c("loc_strip1"))
nrow(df.strips.aphids)
df.strips.aphids$test2 <- df.strips.aphids$N.wasp/df.strips.aphids$N.mummies.out
df.strips.aphids$test3 <- df.strips.aphids$N.wasp/df.strips.aphids$N.mummies.plot


fig7c <- ggplot(dat, aes(x, predicted, fill= x)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=0.2,alpha=0.9, size=0.8,position=dodge)  + 
  geom_point(data=df.strips.aphids,aes(y=df.strips.aphids$N.wasp/df.strips.aphids$N.mummies.out,
                                       x=loc_strip1, fill=loc_strip1,colour=loc_strip1))+
  scale_color_manual(values=c("gray70","gray42")) +
  labs(y="Proportion of mummified aphid per plot", x = "") + coord_cartesian (ylim = c(0, 1)) +
  theme_cowplot() + theme(axis.title.x = element_text(size=1, color = "black"),
                          axis.title.y = element_text(size=11, color = "black"),
                          axis.text.x = element_blank(),legend.position = "none")+
  labs(title = "Wheat \n strip-intercropping",y="Parasitism rate \n (mummified aphid per plot)", x = "") +
  theme(plot.title = element_text(hjust = 0.5,size=15,color = "black",face="plain"))
fig7c
