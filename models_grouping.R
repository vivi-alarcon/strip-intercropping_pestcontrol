# BEFORE RUNNING THIS, LOAD GROUPING.R FILE - same folder -  (all the dataframes come from there)
# r script of the models after deciding to run the analisis grouping the traps
library(glmmTMB)
library(DHARMa)
library(nlme) # Old way 
library(lme4)
library(lattice)
library(MASS)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)
library(car)
library(MASS)

# dataframes ####
# diversity by season 
head(beetle.df.s1) # beetle diversity season 1 
min(beetle.df.s1$S)
head(beetle.df.s2) # beetle diversity season 2
head(spider.df.s1) # spider diversity season 1 
head(spider.df.s2) # spider diversiy season 2 
# diversity both seasons 
tail(spider.seasons) # spider diversity in both seasons 
head(beetle.seasons) # beetle diversity in both seasons 

#       Models results 

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- -
# a) predator diversity differed between seasons? pooling seasons ####

# beetles 
head(beetle.seasons) 
beetle.seasons$season <- as.factor(beetle.seasons$season)
beetle.seasons$total.N <- as.numeric(beetle.seasons$total.N)

m1 <- glmmTMB(total.N~season+(1|farm_id2), data=beetle.seasons, family = poisson)
m1.2 <- glmmTMB(total.N~season+(1|farm_id2), data=beetle.seasons, family = nbinom2(link = "log")) 
m2 <- glmmTMB(S~season+(1|farm_id2), data=beetle.seasons, family = poisson)
m2.2 <- glmmTMB(S~season+(1|farm_id2), data=beetle.seasons, family = nbinom2(link = "log")) 
m2.3 <- glmer.nb(S~season+(1|farm_id2), data=beetle.seasons) 

mtest <- m1.2 # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) # p-value 0.6 not overdispe!
testDispersion(m.testsimulationOutput, alternative = c("less")) # is underdispersed 
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi") 

# spiders 
head(spider.seasons)
spider.seasons$season <- as.factor(spider.seasons$season)
spider.seasons$total.N <- as.numeric(spider.seasons$total.N)
m1 <- glmmTMB(total.N~season+(1|farm_id2), data=spider.seasons, family = poisson)
m1.2 <- glmmTMB(total.N~season+(1|farm_id2), data=spider.seasons, family = nbinom2(link = "log")) 

m2 <- glmmTMB(S~season+(1|farm_id2), data=spider.seasons, family = poisson)
m2.2 <- glmmTMB(S~season+(1|farm_id2), data=spider.seasons, family = nbinom2(link = "log")) 
m2.3 <- glmer.nb(S~season+(1|farm_id2), data=spider.seasons) 

mtest <- m1.2 # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) # p-value 0.6 not overdispe!
testDispersion(m.testsimulationOutput, alternative = c("less"))  
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi") 

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
## in general, predator diversity differed in between types of cultivtion? polled seasons ###
str(beetle.seasons) 
nrow(spider.seasons)

# beetles 
m1 <- glmmTMB(total.N~t3+(1|farm_id2), data=beetle.seasons, family = poisson)
m1.2 <- glmmTMB(total.N~t3+(1|farm_id2), data=beetle.seasons, family = nbinom2(link = "log")) 
#Para cambiar nivel de referencia:
beetle.seasons$t3= relevel(beetle.seasons$t3,"MC")

m2 <- glmmTMB(S~t3+(1|farm_id2), data=beetle.seasons, family = poisson)
m2.2 <- glmmTMB(S~t3+(1|farm_id2), data=beetle.seasons, family = nbinom2(link = "log")) 
m2.3 <- glmer.nb(S~t3+(1|farm_id2), data=beetle.seasons) 

mtest <- m2.2 # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) # p-value 0.6 not overdispe!
testDispersion(m.testsimulationOutput, alternative = c("less")) # is underdispersed 
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi") 

# spiders 
head(spider.seasons)

m1 <- glmmTMB(total.N~t3+(1|farm_id2), data=spider.seasons, family = poisson)
m1.2 <- glmmTMB(total.N~t3+(1|farm_id2), data=spider.seasons, family = nbinom2(link = "log")) 

m2 <- glmmTMB(S~t3+(1|farm_id2), data=spider.seasons, family = poisson)
m2.2 <- glmmTMB(S~t3+(1|farm_id2), data=spider.seasons, family = nbinom2(link = "log")) 
m2.3 <- glmer.nb(S~t3+(1|farm_id2), data=spider.seasons) 

mtest <- m2.2 # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) # p-value 0.6 not overdispe!
testDispersion(m.testsimulationOutput, alternative = c("less"))  
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi")

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# b) does predator diversity differ between monoculture and strip? testing per season #### 
# dataframes 
head(beetle.df.s1) # beetle diversity season 1 
head(beetle.df.s2) # beetle diversity season 2
head(spider.df.s1) # spider diversity season 1 
head(spider.df.s2) # spider diversiy season 2 

# BEETLES ####
beetle.df.s1$total.N <- as.numeric(beetle.df.s1$total.N)
beetle.df.s2$total.N <- as.numeric(beetle.df.s2$total.N)

# beetle ab s1 and s2 
m.beetle.s1.ab <- glmmTMB(total.N~t3+(1|farm_id2), data=beetle.df.s1, family = nbinom2(link = "log"))
m.beetle.s2.ab <- glmmTMB(total.N~t3+(1|farm_id2), data=beetle.df.s2, family = nbinom2(link = "log")) 


mtest <- m.beetle.s2.ab # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) # p-value 0.6 not overdispe!
testDispersion(m.testsimulationOutput, alternative = c("less"))  
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi")

library(emmeans)
mc=cld(emmeans(model1, ~t3, sort=FALSE, Letters=letters))
mc # mean and standard error of each group

# beetle richness s1 and s2 
str(beetle.df.s1)
m.beetle.s1.S <- glmmTMB(S~t3+(1|farm_id2), data=beetle.df.s1, family = poisson) 
m.beetle.s1.S2 <- glmmTMB(S~t3+(1|farm_id2), data=beetle.df.s1, family = nbinom2(link = "log")) # model do not converge

m.beetle.s2.S <- glmmTMB(S~t3+(1|farm_id2), data=beetle.df.s2, family = poisson) 
m.beetle.s2.S2 <- glmmTMB(S~t3+(1|farm_id2), data=beetle.df.s2, family = nbinom2(link = "log")) 

mtest <- m.beetle.s2.S # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) # p-value 0.6 not overdispe!
testDispersion(m.testsimulationOutput, alternative = c("less"))  
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi")


### SPIDER  ####
# spider ab s1 and s2 
m.spider.s1.ab2 <- glmmTMB(total.N~t3+(1|farm_id2), data=spider.df.s1, family = poisson)
m.spider.s1.ab <- glmmTMB(total.N~t3+(1|farm_id2), data=spider.df.s1, family = nbinom2(link = "log")) 
m.spider.s2.ab <- glmmTMB(total.N~t3+(1|farm_id2), data=spider.df.s2, family = nbinom2(link = "log")) 

mtest <- m.spider.s2.ab # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) 
testDispersion(m.testsimulationOutput, alternative = c("less"))  
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi")

# spider richness s1 and s2  ####
str(spider.df.s1)

m.spider.s1.S <- glmmTMB(S~t3+(1|farm_id2), data=spider.df.s1, family = poisson) 
m.spider.s1.S2 <- glmmTMB(S~t3+(1|farm_id2), data=spider.df.s1, family = nbinom2(link = "log")) # Do not converge

m.spider.s2.S <- glmmTMB(S~t3+(1|farm_id2), data=spider.df.s2, family = poisson) 
m.spider.s2.S2 <- glmmTMB(S~t3+(1|farm_id2), data=spider.df.s2, family = nbinom2(link = "log")) 

mtest <- m.spider.s2.S # to check the models just copy the name of the model you want here 

m.testsimulationOutput <- simulateResiduals(fittedModel = mtest, n = 250)
testUniformity(simulationOutput = m.testsimulationOutput)
testDispersion(m.testsimulationOutput, alternative = c("greater")) 
testDispersion(m.testsimulationOutput, alternative = c("less"))  
testResiduals(m.testsimulationOutput)
plot(m.testsimulationOutput)
library(effects)
plot(allEffects(mtest))
summary(mtest)
drop1(mtest,test="Chi")

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# dataframes for EDGE EFFECTS ####
head(beetle.df.edge.s1)  # beetle edge season 1 df ####
head(beetle.df.edge.s2) # beetle edge season 2 df #### 
head(spider.df.edge.s1)  # spider edge season 1 df ####
head(spider.df.edge.s2) # spider edge season 2 df ####

beetle.df.edge.s1$season <- "S1"
beetle.df.edge.s2$season <- "S2"
spider.df.edge.s1$season <- "S1"
spider.df.edge.s2$season <- "S2"
# dataframe edge and interior both seasons pooled
beetes_both_S <- rbind(beetle.df.edge.s1,beetle.df.edge.s2)
spiders_both_S <- rbind(spider.df.edge.s1,spider.df.edge.s2)
beetes_both_S$total.N <- as.numeric(beetes_both_S$total.N)
beetes_both_S$species <- as.factor(beetes_both_S$species)
beetes_both_S$season <- as.factor(beetes_both_S$season)
spiders_both_S$total.N <- as.numeric(spiders_both_S$total.N)
spiders_both_S$species <- as.factor(spiders_both_S$species)
spiders_both_S$season <- as.factor(spiders_both_S$season)
a <- rbind(beetes_both_S,spiders_both_S)

# in general: how edge (trap location) affects predator diversity depends on the type of cultivar (MC:MW:SW:SC)
# Q: does the way in which edge affect diversity differ between type of cultivars? 
# Q: does edge affect diversity when the type of cultivar is taken into account?
library(car)

# models edge effects ####
##### beetles edge <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

beetle.df.edge.s1$total.N <- as.numeric(beetle.df.edge.s1$total.N)
beetle.df.edge.s2$total.N <- as.numeric(beetle.df.edge.s2$total.N)
str(beetle.df.edge.s2)


# interaction model 
model=glmer(S~t10*t_edge + (1|farm_id2), family = "poisson", beetle.df.edge.s2)
summary(model) 
Anova(model)
drop1(model)

#additive model 
model=glmer(S~t10+t_edge + (1|farm_id2), family = "poisson", beetle.df.edge.s2)
summary(model) 
Anova(model)
drop1(model)

#check for overdispersion 
overdisp_fun <- function(model) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model) # p<0.05 means that can be over or underdispersed
# model for abundance beetles S1 was over-disp  so i used negative binomial
# model for richness beetles S1 was ok with a poisson error dist 

model1.nb=glmer.nb(total.N~t10*t_edge + (1|farm_id2), data=beetle.df.edge.s1)
summary(model1.nb) # not converging
# negative binomial interaction model 
model1.nb3=glmer.nb(S~t10*t_edge + (1|farm_id2), 
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
                    ,data=beetle.df.edge.s2)
summary(model1.nb3)  
Anova(model1.nb3)
drop1(model)

# negative binomial additive model 
model1.nb2=glmer.nb(total.N~t10+t_edge + (1|farm_id2), data=beetle.df.edge.s1)
summary(model1.nb2) # not sig
Anova(model1.nb2)

# results: 
# model for abundance beetles was over-disp  so i used negative binomial  
# model for richness beetles  was ok with a poisson error dist 



##### spiders  edge <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

spider.df.edge.s2$total.N <- as.numeric(spider.df.edge.s2$total.N)
str(spider.df.edge.s2)

# interaction model - poisson 
model=glmer(S~t10*t_edge + (1|farm_id2), family = "poisson", spider.df.edge.s2)
summary(model) 
Anova(model)
drop1(model)

#additive model - poisson 
model=glmer(S~t10+t_edge + (1|farm_id2), family = "poisson", spider.df.edge.s2)
summary(model) 
Anova(model)
drop1(model)

#check for overdispersion 
overdisp_fun <- function(model) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model) # p<0.05 means that can be over or underdispersed
# model for abundance beetles S1 was over-disp  so i used negative binomial
# model for richness beetles S1 was ok with a poisson error dist 

model1.nb=glmer.nb(total.N~t10*t_edge + (1|farm_id2), data=spider.df.edge.s2)
summary(model1.nb) # not converging
# negative binomial interaction model 
model1.nb3=glmer.nb(S~t10*t_edge + (1|farm_id2), 
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
                    ,data=spider.df.edge.s2)
summary(model1.nb3) # not sig 
Anova(model1.nb3)
drop1(model)
# negative binomial additive model 

model1.nb2=glmer.nb(total.N~t10+t_edge + (1|farm_id2), data=spider.df.edge.s2)
summary(model1.nb2) # not sig

# results: 
# model for abundance beetles S1 was over-disp  so i used negative binomial <-  not sig 
# model for richness beetles S1 was ok with a poisson error dist <-  not sig

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <  
## models APHIDS (wheat biological control) ####

head(aphid.density)


# aphid and mummie density 
# for running aphid abundance models add: total.aphids as a response variable 
# for running mummies abundance models add: total.mummies as a response variable

model=glmer(total.mummies~t10 + (1|farm_id2), family = "poisson", aphid.density)
summary(model) 
Anova(model)
drop1(model, test = "Chisq")


#check for overdispersion 
overdisp_fun <- function(model) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model) # p<0.05 means that can be over or underdispersed

# aphid abundance model poisson is overdispersed 
# mummies abundance model poission is overdispersed

model1.nb=glmer.nb(total.mummies~t10 + (1|farm_id2), 
                   glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
                   ,data=aphid.density)
summary(model1.nb) 
Anova(model1.nb)
drop1(model1.nb,test="Chi")

m1 <- glmmTMB(total.mummies~t10 + (1|farm_id2), data=aphid.density, family = nbinom2(link = "log"))
summary(m1) 
Anova(m1)
drop1(m1,test="Chi") # this one 

## parasitisim rate wheat ####

# a) proportion of mummified aphids 

# binomial cbind 
m1.aphids <- glmer(cbind(total.mummies,total.aphids-total.mummies)~
                     t10+(1|farm_id2),
                   data=aphid.density,family="binomial")
summary(m1.aphids) 
Anova(m1.aphids)
drop1(m1.aphids,test="Chi")

m2.aphids <- glmmTMB(cbind(total.mummies,total.aphids-total.mummies) ~ t10 + (1 | farm_id2),
                     data=aphid.density, family="binomial")
summary(m2.aphids) 
Anova(m2.aphids)
drop1(m2.aphids,test="Chi")

# Testing using proportions as sucesses and failures 

m1.aphids <- glmer(cbind(percentage.mummified,percetage.not.mummified)~
                     t10+(1|farm_id2),
                   data=aphid.density,family="binomial")
summary(m1.aphids) 
Anova(m1.aphids)
drop1(m1.aphids,test="Chi")

m2.aphids <- glmmTMB(cbind(percentage.mummified,percetage.not.mummified) ~ t10 + (1 | farm_id2),
                     data=aphid.density, family="binomial")
summary(m2.aphids) 
Anova(m2.aphids)
drop1(m2.aphids,test="Chi") # not good fitting model

# b) proportion of wasp ecloded 

# binomial cbind 
m1.aphids <- glmer(cbind(total.wasps,total.aphids-total.wasps)~
                     t10+(1|farm_id2),
                   data=aphid.density,family="binomial")
summary(m1.aphids) 
Anova(m1.aphids)
drop1(m1.aphids,test="Chi")

m2.aphids <- glmmTMB(cbind(total.wasps,total.aphids-total.wasps) ~ t10 + (1 | farm_id2),
                     data=aphid.density, family="binomial")
summary(m2.aphids) 
Anova(m2.aphids)
drop1(m2.aphids,test="Chi")

# edge on wheat aphids 

str(aphid.edge)

# interaction model - poisson <-  for total aphids
model=glmer(total.aphids~t10*t_edge + (1|farm_id2), family = "poisson", aphid.edge)
summary(model) 
Anova(model)
drop1(model,test="Chi")

model=glmer(total.aphids~t_edge + (1|farm_id2), family = "poisson", aphid.edge)


library(emmeans)
mc=cld(emmeans(model, ~t10, sort=FALSE, Letters=letters))
mc

#check for overdispersion 
overdisp_fun <- function(model) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model) # p<0.05 means that can be over or underdispersed

# total aphids model poisson is overdispersed. 

m1.2 <- glmmTMB(total.aphids~t10*t_edge+(1|farm_id2), 
                data=aphid.edge, 
                family = nbinom2(link = "log")) 
Anova(m1.2)
summary(m1.2)
drop1(m1.2,test="Chi")

# edge on parasitism of aphids####
aphid.edge
m1.aphids.edge <- glmer(cbind(total.mummies,total.aphids-total.mummies)~
                     t_edge+(1|farm_id2),
                   data=aphid.edge,family="binomial")
summary(m1.aphids.edge) 
Anova(m1.aphids.edge)
drop1(m1.aphids.edge,test="Chi")

m2.aphids.edge <- glmmTMB(cbind(total.mummies,total.aphids-total.mummies) ~ t10*t_edge + (1 | farm_id2),
                     data=aphid.edge, family="binomial")
summary(m2.aphids.edge) 
Anova(m2.aphids.edge)
drop1(m2.aphids.edge,test="Chi")

## <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <-
## parasitisim rate Canola ####

# larvae density and mean number of eggs per larvae 
# for running larvae density models add: total.larvae as a response variable 
# for running number of eggs per larvae models add: eggs.per.larvae as a response variable
head(larvae.density)

model=glmer(eggs.per.larvae.intg~t10 + (1|farm_id2), family = "poisson", larvae.density)
summary(model) 
Anova(model)
drop1(model, test = "Chisq")

#check for overdispersion 
overdisp_fun <- function(model) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model) # p<0.05 means that can be over or underdispersed

# larvae abundance model poisson is overdispersed 
# mean eggs  model was poisson 

model1.nb=glmer.nb(eggs.per.larvae~t10 + (1|farm_id2), 
                   glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
                   ,data=larvae.density)
summary(model1.nb) 
Anova(model1.nb)
drop1(model1.nb,test="Chi")

m1 <- glmmTMB(eggs.per.larvae~t10 + (1|farm_id2), data=larvae.density, family = nbinom2(link = "log"))
summary(m1) 
Anova(m1)
drop1(m1,test="Chi") # this one 


# proportion of infested larvae  

# binomial cbind 
m1.larvae <- glmer(cbind(parasited.larvae,total.larvae-parasited.larvae)~
                     t10+(1|farm_id2),
                   data=larvae.density,family="binomial")
summary(m1.larvae) 
Anova(m1.larvae)
drop1(m1.larvae,test="Chi")

m2.larvae <- glmmTMB(cbind(parasited.larvae,total.larvae-parasited.larvae) ~ t10 + (1 | farm_id2),
                     data=larvae.density, family="binomial")
summary(m2.larvae) 
Anova(m2.larvae)
drop1(m2.larvae,test="Chi")

# edge on canola larvae #### 

str(larvae.edge)

# interaction model - poisson <-  for total aphids
head(larvae.edge)
model=glmer(parasited.larvae~t10 + (1|farm_id2), family = "poisson", larvae.edge)
summary(model) 
Anova(model)
drop1(model,test="Chi")

model=glmer(total.larvae~t_edge + (1|farm_id2), family = "poisson", larvae.edge)
summary(model) 
Anova(model)
drop1(model,test="Chi")


#check for overdispersion 
overdisp_fun <- function(model) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model) # p<0.05 means that can be over or underdispersed


m1.2 <- glmmTMB(total.larvae~t10*t_edge+(1|farm_id2), 
                data=larvae.edge, 
                family = nbinom2(link = "log")) 
Anova(m1.2)
summary(m1.2)
drop1(m1.2,test="Chi")


# edge on parasitism of pollen beetle larvae ####
larvae.edge
m1.aphids.edge <- glmer(cbind(parasited.larvae,total.larvae-parasited.larvae)~
                          t_edge*t10+(1|farm_id2),
                        data=larvae.edge,family="binomial")
summary(m1.aphids.edge) 
Anova(m1.aphids.edge)
drop1(m1.aphids.edge,test="Chi")

m2.aphids.edge <- glmmTMB(cbind(parasited.larvae,total.larvae-parasited.larvae) ~ t10 + (1 | farm_id2),
                          data=larvae.edge, family="binomial")
summary(m2.aphids.edge) 
Anova(m2.aphids.edge)
drop1(m2.aphids.edge,test="Chi")


m1.aphids.edge2 <- glmer(cbind(parasited.larvae,total.larvae-parasited.larvae)~
                          t_edge+t10+(1|farm_id2),
                        data=larvae.edge,family="binomial")
summary(m1.aphids.edge2) 
Anova(m1.aphids.edge2)
drop1(m1.aphids.edge2,test="Chi")
# Testing multiple comparisons ####
warpbreaks.mc <- glht(m1.aphids.edge2, linfct=mcp(t10="Tukey"))
summary(glht(m1.aphids.edge2, linfct=mcp(t10="Tukey"))) # do not understand so post-hoc test?
warpbreaks.cld <- cld(warpbreaks.mc)
plot(warpbreaks.cld)

# testing only for strips canola 
strip.canola <- subset(larvae.edge,t10=="S C")
m2.aphids.edge.canola <- glmmTMB(cbind(parasited.larvae,total.larvae-parasited.larvae) ~ t_edge + (1 | farm_id2),
                          data=strip.canola, family="binomial")
summary(m2.aphids.edge.canola) 
Anova(m2.aphids.edge.canola)
drop1(m2.aphids.edge.canola,test="Chi")

# testing for mono  canola 
mono.canola <- subset(larvae.edge,t10=="M C")
m2.aphids.edge.canola.mono <- glmmTMB(cbind(parasited.larvae,total.larvae-parasited.larvae) ~ t_edge + (1 | farm_id2),
                                 data=mono.canola, family="binomial")
summary(m2.aphids.edge.canola.mono) 
Anova(m2.aphids.edge.canola.mono)
drop1(m2.aphids.edge.canola.mono,test="Chi")

# INSECTICIDE INFLUENCE <- <- <- <- <- <- <- <- <- <- <- <- <- <- ####
head(beetle.df.strips) # both seasons 
head(spider.df.strips) # both seasons 


head(spider.df.s1.strips)
head(spider.df.s2.strips)

head(beetle.df.s1.strips)
head(beetle.df.s2.strips)

# beetles seasons pooled 
head(beetle.df.strips)

model1=glmer(total.N~insecticide + (1|farm_id2), family = "poisson", beetle.df.strips)
summary(model1) 
Anova(model1)
drop1(model1)

model1.1 <- glmmTMB(total.N~insecticide+(1|farm_id2), data=beetle.df.strips, family = poisson)
summary(model1.1) 
Anova(model1.1)
drop1(model1.1)

#check for overdispersion 
overdisp_fun <- function(model1) {
  # number of variance parameters in
  #   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model1),vpars))+length(fixef(model1))
  rdf <- nrow(model.frame(model1))-model.df
  rp <- residuals(model1,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model1) # p<0.05 means that can be over or underdispersed

model1.nb=glmer.nb(total.N~insecticide + (1|farm_id2), data=beetle.df.strips)
summary(model1.nb)  
Anova(model1.nb)
drop1(model1.nb,,test = "Chisq")

model1.2nb <- glmmTMB(total.N~insecticide+(1|farm_id2), data=beetle.df.strips, family = nbinom2(link = "log")) 
summary(model1.2nb)  
Anova(model1.2nb)
drop1(model1.2nb,test = "Chisq")

model1.3nb=glmer.nb(total.N~insecticide + (1|farm_id2), 
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
                    ,data=beetle.df.strips)
summary(model1.3nb)  
Anova(model1.3nb)
drop1(model1.3nb)


# Beta diversity #### 
df.beta.spider.s1
df.beta.spider.s2

df.beta.beetle.s1
df.beta.beetle.s2


# betadiversity + edge <- <-  




