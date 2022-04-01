## grouping biodiversity 
setwd("~/Dropbox/Msc_ECOLOGY_Bremen/a.Master_Thesis/A_Multicropping/b_thesis/r/grouping_df")

trap_id <-(read.csv("trap_ids.csv",sep= ","))
str(trap_id)
trap_id$insecticide <- as.factor(trap_id$insecticide)
trap_id$farm2 <- as.factor(trap_id$farm2)
trap_id$field_season <- as.factor(trap_id$field_season)


beetles <- (read.csv("beetle_ID_final.csv",sep= ","))
head(beetles)
beetles <-beetles [,-c(1:11)]
spiders <- (read.csv("spider_ID_final.csv",sep= ","))
head(spiders)

beetles.all <- merge(trap_id,beetles, by="Trap_ID")
head(beetles.all)

spiders.all <- merge(trap_id,spiders, by="Trap_ID")
head(spiders.all)

# taking out insecticide fee window 
head(beetles.all)
levels(beetles.all$insecticide)
beetles <- subset(beetles.all, insecticide == "0" | insecticide == "2",
                  select=c(5,9,13,17,21:65))
head(beetles)
beetles$insecticide # beetles df general (no insect)

# beetles only insecticide window 
head(beetles.all)
levels(beetles.all$insecticide)
beetles.only.insect <- subset(beetles.all, insecticide == "1",
                  select=c(5,9,13,17,21:65))
head(beetles.only.insect)
beetles.only.insect$insecticide # beetles df general (no insect)

beetles.only.insect.melt = melt(beetles.only.insect, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetles.only.insect.melt)[match("value", names(beetles.only.insect.melt))]="N"
head(beetles.only.insect.melt) # beetles only insecticide window data 
sum(beetles.only.insect.melt$N) # 633

# beetles strips df 
beetles.strips <- subset(beetles.all, type_id == "S",
                         select=c(5,9,13,17,21:65))
beetles.strips$insecticide
head(beetles.strips) # beetles strips df ####

# spiders
colnames(spiders.all)
levels(spiders.all$insecticide)
spiders <- subset(spiders.all, insecticide == "0" | insecticide == "2",
                  select=c(5,9,13,17,21:65))
spiders$insecticide # spiders df general (no insect)

# spiders only insecticide window 
head(spiders.all)
levels(spiders.all$insecticide)
spiders.only.insect <- subset(spiders.all, insecticide == "1",
                              select=c(5,9,13,17,21:65))
head(spiders.only.insect)
spiders.only.insect$insecticide # spiders only insecticide window df 

spiders.only.insect.melt = melt(spiders.only.insect, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spiders.only.insect.melt)[match("value", names(spiders.only.insect.melt))]="N"
head(spiders.only.insect.melt) # spiders only insecticide window data 

sum(spiders.only.insect.melt$N) # 185 


# spider strips df 
spiders.strips <- subset(spiders.all, type_id == "S",
                         select=c(5,9,13,17,21:65))
spiders.strips$insecticide
head(spiders.strips) # spiders strips df ####


beetles.s1 <- subset(beetles,field_season==1)
head(beetles.s1) ## beetles season 1 df ####
beetles.s2 <- subset(beetles,field_season==2) # beetles season 2 df ####

spiders.s1 <- subset(spiders,field_season==1) ## spiders season 1 df ####
spiders.s2 <- subset(spiders,field_season==2) ## spiders season 2 df ####


##                  Beetles season 1      ######
# 1) transpose the dataframe 
head(beetles.s1)
library(reshape)
colnames(beetles.s1)
beetle.s1.melt = melt(beetles.s1, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.s1.melt)[match("value", names(beetle.s1.melt))]="N"
head(beetle.s1.melt)

#old way <- ddply brazil 
# I decided to use this way because i tested and it gives me the same result as the 
# one with aggregate. - on the bottom part its the checking

head(beetle.s1.melt) # with the melted dataframe of abundance per species 
beetle.s1.melt$presence = ifelse(beetle.s1.melt$N>=1, 1, 0) #new colum called presence. 
head(beetle.s1.melt)

farm_id=substr(beetle.s1.melt$field_group, 2,2)
farm_id = factor(farm_id)
beetle.s1.melt$farm_id <- farm_id
farm_id2=substr(beetle.s1.melt$field_group, 2,4)
farm_id2 = factor(farm_id2)
beetle.s1.melt$farm_id2 <- farm_id2
tail(beetle.s1.melt)

library(plyr)
beetle.df.s1 = ddply(beetle.s1.melt, .(farm_id2,t3,field_group), summarize, 
                    total.N = sum(N), 
                    S = sum(presence))

beetle.df.s1 # beetle diversity per GROUP season 1 df ### 36 points 


head(beetle.s1.melt)
beetle.df.s1.farm = ddply(beetle.s1.melt, .(farm_id2), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))

beetle.df.s1.farm # beetle diversity per FARM season 1 df ### 36 points 


### EDGE #####
head(beetle.s1.melt)
beetle.df.edge.s1 = ddply(beetle.s1.melt, .(t3,group_edge1), summarize, 
                          total.N = sum(N), 
                          S = sum(presence))

beetle.df.edge.s1
# remove nas 
beetle.df.edge.s1 <- beetle.df.edge.s1[-c(13,14),]

farm_id=substr(beetle.df.edge.s1$group_edge1, 5,5)
farm_id = factor(farm_id)
beetle.df.edge.s1$farm_id <- farm_id
farm_id2=substr(beetle.df.edge.s1$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
beetle.df.edge.s1$farm_id2 <- farm_id2
beetle.df.edge.s1 # beetle edge season 1 df ####

head(beetle.df.edge.s1)
t_edge=substr(beetle.df.edge.s1$group_edge1, 2,2)
t_edge = factor(t_edge)
beetle.df.edge.s1$t_edge <- t_edge

head(beetle.df.edge.s1)  # beetle edge season 1 df ####
cultivation <- substr(beetle.df.edge.s1$group_edge1, 1,1)
cultivation = factor(cultivation)
beetle.df.edge.s1$cultivation <- cultivation
t6=substr(beetle.df.edge.s1$t3, 1,1)
t6 = factor(t6)
beetle.df.edge.s1$t6 <- t6
beetle.df.edge.s1$t10 <- paste(beetle.df.edge.s1$t6,beetle.df.edge.s1$cultivation)
beetle.df.edge.s1$t10 <- as.factor(beetle.df.edge.s1$t10)


#                      beetles <-  season 2 ####
# 1) transpose the dataframe 
head(beetles.s2)
library(reshape)
colnames(beetles.s2)
beetle.s2.melt = melt(beetles.s2, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.s2.melt)[match("value", names(beetle.s2.melt))]="N"
head(beetle.s2.melt)

#old way <- ddply brazil 
str(beetle.s2.melt) # with the melted dataframe of abundance per species 
beetle.s2.melt$presence = ifelse(beetle.s2.melt$N>=1, 1, 0) #new colum called presence. 
head(beetle.s2.melt)

farm_id=substr(beetle.s2.melt$field_group, 2,2)
farm_id = factor(farm_id)
beetle.s2.melt$farm_id <- farm_id

farm_id2=substr(beetle.s2.melt$field_group, 2,4)
farm_id2 = factor(farm_id2)
beetle.s2.melt$farm_id2 <- farm_id2
beetle.s2.melt

beetle.df.s2 = ddply(beetle.s2.melt, .(farm_id2,t3,field_group), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
beetle.df.s2 # beetle diversity per GROUP S2 ###

head(beetle.s2.melt)
beetle.df.s2.farm = ddply(beetle.s2.melt, .(farm_id2), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
beetle.df.s2.farm # beetle diversity per FARM S2 ### <-  strange, more spp than the actual list 

### edge 
head(beetle.s2.melt) # with the melted dataframe of abundance per species 
beetle.s2.melt$presence = ifelse(beetle.s2.melt$N>=1, 1, 0) #new colum called presence. 
head(beetle.s2.melt)
beetle.df.edge.s2 = ddply(beetle.s2.melt, .(t3,group_edge1), summarize, 
                          total.N = sum(N), 
                          S = sum(presence))
beetle.df.edge.s2 
# remove nas 
beetle.df.edge.s2 <- beetle.df.edge.s2[-c(13,14),]

farm_id=substr(beetle.df.edge.s2$group_edge1, 5,5)
farm_id = factor(farm_id)
beetle.df.edge.s2$farm_id <- farm_id
farm_id2=substr(beetle.df.edge.s2$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
beetle.df.edge.s2$farm_id2 <- farm_id2
beetle.df.edge.s2 # beetle edge season 2 df ####

head(beetle.df.edge.s2)
t_edge=substr(beetle.df.edge.s2$group_edge1, 2,2)
t_edge = factor(t_edge)
beetle.df.edge.s2$t_edge <- t_edge

head(beetle.df.edge.s2)  # beetle edge season 1 df ####
cultivation <- substr(beetle.df.edge.s2$group_edge1, 1,1)
cultivation = factor(cultivation)
beetle.df.edge.s2$cultivation <- cultivation
t6=substr(beetle.df.edge.s2$t3, 1,1)
t6 = factor(t6)
beetle.df.edge.s2$t6 <- t6
beetle.df.edge.s2$t10 <- paste(beetle.df.edge.s2$t6,beetle.df.edge.s2$cultivation)
beetle.df.edge.s2$t10 <- as.factor(beetle.df.edge.s2$t10)

##                  Spiders season 1      ######
# 1) transpose the dataframe 
head(spiders.s1)
library(reshape)
colnames(spiders.s1)
spider.s1.melt = melt(spiders.s1, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spider.s1.melt)[match("value", names(spider.s1.melt))]="N"
head(spider.s1.melt)

#old way <- ddply brazil 
# I decided to use this way because i tested and it gives me the same result as the 
# one with aggregate. - on the bottom part its the checking

head(spider.s1.melt) # with the melted dataframe of abundance per species 
spider.s1.melt$presence = ifelse(spider.s1.melt$N>=1, 1, 0) #new colum called presence. 
head(spider.s1.melt)

farm_id=substr(spider.s1.melt$field_group, 2,2)
farm_id = factor(farm_id)
spider.s1.melt$farm_id <- farm_id

farm_id2=substr(spider.s1.melt$field_group, 2,4)
farm_id2 = factor(farm_id2)
spider.s1.melt$farm_id2 <- farm_id2


spider.df.s1 = ddply(spider.s1.melt, .(farm_id2,t3,field_group), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
spider.df.s1 # spider diversity PER GROUP S1####


head(spider.s1.melt)
spider.df.s1.farm = ddply(spider.s1.melt, .(farm_id2), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
spider.df.s1.farm # spider diversity PER FARM S1####

### edge
head(spider.s1.melt) # with the melted dataframe of abundance per species 
spider.s1.melt$presence = ifelse(spider.s1.melt$N>=1, 1, 0) #new colum called presence. 
head(spider.s1.melt)
spider.df.edge.s1 = ddply(spider.s1.melt, .(t3,group_edge1), summarize, 
                          total.N = sum(N), 
                          S = sum(presence))
spider.df.edge.s1  # spider edge S1 df ####
spider.df.edge.s1 <- spider.df.edge.s1[-c(13,14),]

farm_id=substr(spider.df.edge.s1$group_edge1, 5,5)
farm_id = factor(farm_id)
spider.df.edge.s1$farm_id <- farm_id

farm_id2=substr(spider.df.edge.s1$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
spider.df.edge.s1$farm_id2 <- farm_id2

head(spider.df.edge.s1)
t_edge=substr(spider.df.edge.s1$group_edge1, 2,2)
t_edge = factor(t_edge)
spider.df.edge.s1$t_edge <- t_edge

cultivation <- substr(spider.df.edge.s1$group_edge1, 1,1)
cultivation = factor(cultivation)
spider.df.edge.s1$cultivation <- cultivation
t6=substr(spider.df.edge.s1$t3, 1,1)
t6 = factor(t6)
spider.df.edge.s1$t6 <- t6
spider.df.edge.s1$t10 <- paste(spider.df.edge.s1$t6,spider.df.edge.s1$cultivation)
spider.df.edge.s1$t10 <- as.factor(spider.df.edge.s1$t10)


##                  Spiders season 2      ######
# 1) transpose the dataframe 
head(spiders.s2)
library(reshape)
colnames(spiders.s2)
spider.s2.melt = melt(spiders.s2, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spider.s2.melt)[match("value", names(spider.s2.melt))]="N"
head(spider.s2.melt)

#old way <- ddply brazil 
# I decided to use this way because i tested and it gives me the same result as the 
# one with aggregate. - on the bottom part its the checking

head(spider.s2.melt) # with the melted dataframe of abundance per species 
spider.s2.melt$presence = ifelse(spider.s2.melt$N>=1, 1, 0) #new colum called presence. 
head(spider.s2.melt)

farm_id=substr(spider.s2.melt$field_group, 2,2)
farm_id = factor(farm_id)
spider.s2.melt$farm_id <- farm_id

farm_id2=substr(spider.s2.melt$field_group, 2,4)
farm_id2 = factor(farm_id2)
spider.s2.melt$farm_id2 <- farm_id2


spider.df.s2 = ddply(spider.s2.melt, .(farm_id2,t3,field_group), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
spider.df.s2 # df spider diversity PER GROUP s2####


head(spider.s2.melt)
spider.df.s2.farm = ddply(spider.s2.melt, .(farm_id2), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
spider.df.s2.farm # df spider diversity PER FARM S2 ####



## edge 
head(spider.s2.melt) # with the melted dataframe of abundance per species 
spider.s2.melt$presence = ifelse(spider.s2.melt$N>=1, 1, 0) #new colum called presence. 
head(spider.s2.melt)
spider.df.edge.s2 = ddply(spider.s2.melt, .(t3,group_edge1), summarize, 
                          total.N = sum(N), 
                          S = sum(presence))
spider.df.edge.s2 # spider edge season 2 df ####
spider.df.edge.s2 <- spider.df.edge.s2[-c(13,14),]

farm_id=substr(spider.df.edge.s2$group_edge1, 5,5)
farm_id = factor(farm_id)
spider.df.edge.s2$farm_id <- farm_id

farm_id2=substr(spider.df.edge.s2$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
spider.df.edge.s2$farm_id2 <- farm_id2

head(spider.df.edge.s2)
t_edge=substr(spider.df.edge.s2$group_edge1, 2,2)
t_edge = factor(t_edge)
spider.df.edge.s2$t_edge <- t_edge

cultivation <- substr(spider.df.edge.s2$group_edge1, 1,1)
cultivation = factor(cultivation)
spider.df.edge.s2$cultivation <- cultivation
t6=substr(spider.df.edge.s2$t3, 1,1)
t6 = factor(t6)
spider.df.edge.s2$t6 <- t6
spider.df.edge.s2$t10 <- paste(spider.df.edge.s2$t6,spider.df.edge.s2$cultivation)
spider.df.edge.s2$t10 <- as.factor(spider.df.edge.s2$t10)


### pooling both seasons together #### just to try 
# beetles 
head(beetles)
head(spiders)

library(reshape)
colnames(beetles)
beetle.melt = melt(beetles, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.melt)[match("value", names(beetle.melt))]="N"
head(beetle.melt)

#old way <- ddply brazil 
# I decided to use this way because i tested and it gives me the same result as the 
# one with aggregate. - on the bottom part its the checking

head(beetle.melt) # with the melted dataframe of abundance per species 
beetle.melt$presence = ifelse(beetle.melt$N>=1, 1, 0) #new colum called presence. 
head(beetle.melt)

beetle.df.alls = ddply(beetle.melt, .(t3,field_group), summarize, 
                     total.N = sum(N), 
                     S = sum(presence))
beetle.df.alls # df beetles seasons polled ####

farm_id=substr(beetle.df.alls$field_group, 2,2)
farm_id = factor(farm_id)
beetle.df.alls$farm_id <- farm_id

farm_id2=substr(beetle.df.alls$field_group, 2,4)
farm_id2 = factor(farm_id2)
beetle.df.alls$farm_id2 <- farm_id2
head(beetle.df.alls)
sum(beetle.df.alls$total.N)

# edge 
head(beetle.melt) # with the melted dataframe of abundance per species 
beetle.melt$presence = ifelse(beetle.melt$N>=1, 1, 0) #new colum called presence. 
head(beetle.melt)
beetle.df.alls.edge = ddply(beetle.melt, .(t3,group_edge1), summarize, 
                            total.N = sum(N), 
                            S = sum(presence))
beetle.df.alls.edge # df beetles edge <- seasons polled ####

beetle.df.alls.edge <- beetle.df.alls.edge[-c(13,14),]

farm_id=substr(beetle.df.alls.edge$group_edge1, 5,5)
farm_id = factor(farm_id)
beetle.df.alls.edge$farm_id <- farm_id

farm_id2=substr(beetle.df.alls.edge$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
beetle.df.alls.edge$farm_id2 <- farm_id2

head(beetle.df.alls.edge)
t_edge=substr(beetle.df.alls.edge$group_edge1, 2,2)
t_edge = factor(t_edge)
beetle.df.alls.edge$t_edge <- t_edge

cultivation <- substr(beetle.df.alls.edge$group_edge1, 1,1)
cultivation = factor(cultivation)
beetle.df.alls.edge$cultivation <- cultivation
t6=substr(beetle.df.alls.edge$t3, 1,1)
t6 = factor(t6)
beetle.df.alls.edge$t6 <- t6
beetle.df.alls.edge$t10 <- paste(beetle.df.alls.edge$t6,beetle.df.alls.edge$cultivation)
beetle.df.alls.edge$t10 <- as.factor(beetle.df.alls.edge$t10)

#<- <- <- <- <- <- <- <- <- <- <- <- 


# spiders 
head(spiders)
library(reshape)
colnames(spiders)
spider.melt = melt(spiders, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spider.melt)[match("value", names(spider.melt))]="N"
head(spider.melt)

#old way <- ddply brazil 
# I decided to use this way because i tested and it gives me the same result as the 
# one with aggregate. - on the bottom part its the checking

head(spider.melt) # with the melted dataframe of abundance per species 
spider.melt$presence = ifelse(spider.melt$N>=1, 1, 0) #new colum called presence. 
head(spider.melt)
spider.df.alls = ddply(spider.melt, .(t3,field_group), summarize, 
                       total.N = sum(N), 
                       S = sum(presence))
spider.df.alls # df spiders seasons polled ####

farm_id=substr(spider.df.alls$field_group, 2,2)
farm_id = factor(farm_id)
spider.df.alls$farm_id <- farm_id

farm_id2=substr(spider.df.alls$field_group, 2,4)
farm_id2 = factor(farm_id2)
spider.df.alls$farm_id2 <- farm_id2

### edge 
head(spider.melt) # with the melted dataframe of abundance per species 
spider.melt$presence = ifelse(spider.melt$N>=1, 1, 0) #new colum called presence. 
head(spider.melt)
spider.df.alls.edge = ddply(spider.melt, .(t3,group_edge1), summarize, 
                            total.N = sum(N), 
                            S = sum(presence))
spider.df.alls.edge # df spiders edge <- seasons polled ####

spider.df.alls.edge <- spider.df.alls.edge[-c(13,14),]

farm_id=substr(spider.df.alls.edge$group_edge1, 2,2)
farm_id = factor(farm_id)
spider.df.alls.edge$farm_id <- farm_id

farm_id2=substr(spider.df.alls.edge$group_edge1, 2,4)
farm_id2 = factor(farm_id2)
spider.df.alls.edge$farm_id2 <- farm_id2

head(spider.df.alls.edge)
t_edge=substr(spider.df.alls.edge$group_edge1, 2,2)
t_edge = factor(t_edge)
spider.df.alls.edge$t_edge <- t_edge

cultivation <- substr(spider.df.alls.edge$group_edge1, 1,1)
cultivation = factor(cultivation)
spider.df.alls.edge$cultivation <- cultivation
t6=substr(spider.df.alls.edge$t3, 1,1)
t6 = factor(t6)
spider.df.alls.edge$t6 <- t6
spider.df.alls.edge$t10 <- paste(spider.df.alls.edge$t6,spider.df.alls.edge$cultivation)
spider.df.alls.edge$t10 <- as.factor(spider.df.alls.edge$t10)

# data frame to compare between seasons ####

head(beetle.df.s1) # beetle diversity season 1 
head(beetle.df.s2) # beetle diversity season 2
head(spider.df.s1) # spider diversity season 1 
head(spider.df.s2) # spider diversiy season 2 

# diversity both seasons 
beetle.df.s1$season <-1  
beetle.df.s2$season <-2  
spider.df.s1$season <-1  
spider.df.s2$season <-2 

spider.seasons <- rbind(spider.df.s1,spider.df.s2)  
head(spider.seasons) # spider diversity in both seasons 
beetle.seasons <- rbind(beetle.df.s1,beetle.df.s2)
head(beetle.seasons) # beetle diversity in both seasons 
max(beetle.seasons$total.N)
  

# APHIDS DATA ####


aphids.df <-(read.csv("aphids_df_complete.csv",sep= ","))
head(aphids.df)

aphids <- merge(trap_id,aphids.df,by="Trap_ID")
head(aphids)
colnames(aphids)
aphids <- aphids[,c(1,16:22,26,26:36)]
aphids$insecticide.y

aphids.strips <- subset(aphids,insecticide.y == "1" | insecticide.y == "2")

aphids <- subset(aphids, insecticide.y == "0" | insecticide.y == "2")
colnames(aphids)
aphids <- aphids[,-c(10)]

names(aphids)[names(aphids) == "insecticide.y"] <- "insecticide"
head(aphids)

cultivation <- substr(aphids$group4, 1,1)
cultivation = factor(cultivation)
aphids$cultivation <- cultivation

aphids$t6 <- aphids$t1.y
aphids$t10 <- paste(aphids$t6,aphids$cultivation)
aphids$t10 <- as.factor(aphids$t10)
head(aphids)

aphid.density = ddply(aphids, .(t10,field_group), summarize, 
                      total.aphids = sum(N.aphids), 
                      total.mummies.plot = sum(N.mummies.plot),
                      total.mummies.out = sum(N.mummies.out),
                      total.wasps = sum(N.wasp))
aphid.density$total.mummies <- aphid.density$total.mummies.plot+aphid.density$total.mummies.out
aphid.density
aphid.density$proportion.mummified <- aphid.density$total.mummies/aphid.density$total.aphids
aphid.density$proportion.wasps <- aphid.density$total.wasps/aphid.density$total.mummies
aphid.density$proportion.NOT.mummified <- 1-aphid.density$proportion.mummified

aphid.density$percentage.mummified <- aphid.density$proportion.mummified*100
aphid.density$percetage.not.mummified <- aphid.density$proportion.NOT.mummified*100

aphid.density$percentage.mummified <- as.integer(aphid.density$percentage.mummified)
aphid.density$percetage.not.mummified <- as.integer(aphid.density$percetage.not.mummified)

farm_id2=substr(aphid.density$field_group, 2,4)
farm_id2 = factor(farm_id2)
aphid.density$farm_id2 <- farm_id2
str(aphid.density)

# summary statistics aphids ####
aphid.monoculture <- subset(aphid.density, t10=="M W")
head(aphid.monoculture)
mean(aphid.monoculture$total.mummies)
mean(aphid.monoculture$proportion.mummified)

aphid.strips <- subset(aphid.density, t10=="S W")
sd(aphid.strips$total.aphids)
mean(aphid.strips$total.mummies)
mean(aphid.strips$proportion.mummified)

# edge effects on aphids ####
head(aphids)

aphid.edge = ddply(aphids, .(t10,group_edge1), summarize, 
                      total.aphids = sum(N.aphids), 
                      total.mummies.plot = sum(N.mummies.plot),
                      total.mummies.out = sum(N.mummies.out),
                      total.wasps = sum(N.wasp))
aphid.edge <- aphid.edge[-1,]

aphid.edge$total.mummies <- aphid.edge$total.mummies.plot+aphid.edge$total.mummies.out
aphid.edge
aphid.edge$proportion.mummified <- aphid.edge$total.mummies/aphid.edge$total.aphids
aphid.edge$proportion.wasps <- aphid.edge$total.wasps/aphid.edge$total.mummies
aphid.edge$proportion.NOT.mummified <- 1-aphid.edge$proportion.mummified

aphid.edge$percentage.mummified <- aphid.edge$proportion.mummified*100
aphid.edge$percetage.not.mummified <- aphid.edge$proportion.NOT.mummified*100

aphid.edge$percentage.mummified <- as.integer(aphid.edge$percentage.mummified)
aphid.edge$percetage.not.mummified <- as.integer(aphid.edge$percetage.not.mummified)

farm_id2=substr(aphid.edge$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
aphid.edge$farm_id2 <- farm_id2

t_edge=substr(aphid.edge$group_edge1, 2,2)
t_edge = factor(t_edge)
aphid.edge$t_edge <- t_edge

# wheat aphids insecticide #### 
head(aphids.strips)
aphids.strips <- aphids.strips[,-c(10)]
names(aphids.strips)[names(aphids.strips) == "insecticide.y"] <- "insecticide"

colnames(aphids.strips)
aphids.strips <- aphids.strips[,c(3,7,9,13:17)]

aphids.strips$total.mummies <- aphids.strips$N.mummies.plot+aphids.strips$N.mummies.out
aphids.strips$proportion.mummified <- aphids.strips$total.mummies/aphids.strips$N.aphids
aphids.strips$proportion.wasps <- aphids.strips$N.wasp/aphids.strips$total.mummies
aphids.strips$proportion.NOT.mummified <- 1-aphids.strips$proportion.mummified

aphids.strips

farm_id2=substr(aphids.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
aphids.strips$farm_id2 <- farm_id2
aphids.strips

t_edge=substr(aphids.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
aphids.strips$t_edge <- t_edge
aphids.strips

t10=substr(aphids.strips$group_edge1, 1,1)
t10 = factor(t10)
aphids.strips$t10 <- t10
aphids.strips

t.test=substr(aphids.strips$group_edge1, 1,2)
t.test = factor(t.test)
aphids.strips$t.test <- t.test
aphids.strips

head(aphids.strips)
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# LARVAE DATA ####

larvae.df <-(read.csv("larvae_data.csv",sep= ","))
head(larvae.df)

larvae <- merge(trap_id,larvae.df,by="Trap_ID")
head(larvae)

larvae.strips <- subset(larvae, insecticide == "1" | insecticide == "2")

larvae <- subset(larvae, insecticide == "0" | insecticide == "2")
tail(larvae)

cultivation <- substr(larvae$group4, 1,1)
cultivation = factor(cultivation)
larvae$cultivation <- cultivation

t6 <- substr(larvae$t3, 1,1)
t6 = factor(t6)
larvae$t6 <- t6

larvae$t10 <- paste(larvae$t6,larvae$cultivation)
larvae$t10 <- as.factor(larvae$t10)
head(larvae)


larvae.density = ddply(larvae, .(t10,field_group), summarize, 
                       total.larvae = sum(total.larvae), 
                       total.eggs = sum(total_eggs),
                       parasited.larvae= sum(parasited.larvae))

farm_id2=substr(larvae.density$field_group, 2,4)
farm_id2 = factor(farm_id2)
larvae.density$farm_id2 <- farm_id2
head(larvae.density)
larvae.density

larvae.density$proportion <- larvae.density$parasited.larvae/larvae.density$total.larvae
larvae.density$eggs.per.larvae <- larvae.density$total.eggs/larvae.density$parasited.larvae

str(larvae.density)

larvae.density$eggs.per.larvae.intg <- as.integer(larvae.density$eggs.per.larvae)

#summary statistics larvae density ####
head(larvae.density)
larvae.monoculture <- subset(larvae.density, t10=="M C")
head(larvae.monoculture)
sd(larvae.monoculture$total.larvae)
mean(larvae.monoculture$eggs.per.larvae)
mean(larvae.monoculture$proportion)


larvae.strips <- subset(larvae.density, t10=="S C")
sd(larvae.strips$total.larvae)
mean(larvae.strips$eggs.per.larvae)
mean(larvae.strips$proportion)


#write.csv(larvae.density,file = "larvae_density.csv")
larvae.density <- read.csv("larvae_density.csv",sep=",")
larvae.density$farm_id2 <- as.factor(larvae.density$farm_id2)

# edge effects on larvae ####
head(larvae)

larvae.edge = ddply(larvae, .(t10,group_edge1), summarize, 
                   total.larvae = sum(total.larvae), 
                   parasited.larvae = sum(parasited.larvae),
                   total_eggs = sum(total_eggs))
larvae.edge <- larvae.edge[-13,]

farm_id2=substr(larvae.edge$group_edge1, 5,7)
farm_id2 = factor(farm_id2)
larvae.edge$farm_id2 <- farm_id2

t_edge=substr(larvae.edge$group_edge1, 2,2)
t_edge = factor(t_edge)
larvae.edge$t_edge <- t_edge
larvae.edge$proportion <- larvae.edge$parasited.larvae/larvae.edge$total.larvae
larvae.edge

### pollen beetle larvae insecticide ####
head(larvae.strips)
larvae.strips$proportion.parasited <- larvae.strips$parasited.larvae/larvae.strips$total.larvae

colnames(larvae.strips)
larvae.strips <- larvae.strips[,c(17,21,5,23:26)]

farm_id2=substr(larvae.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
larvae.strips$farm_id2 <- farm_id2
larvae.strips

t_edge=substr(larvae.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
larvae.strips$t_edge <- t_edge
larvae.strips

t10=substr(larvae.strips$group_edge1, 1,1)
t10 = factor(t10)
larvae.strips$t10 <- t10
larvae.strips

t.test=substr(larvae.strips$group_edge1, 1,2)
t.test = factor(t.test)
larvae.strips$t.test <- t.test
larvae.strips



# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# insecticide influence biodiversity dataframes####


# choosing data only from the strips 
head(beetles.strips)
beetles.strips.S1 <- subset(beetles.strips,field_season==1)
beetles.strips.S2 <- subset(beetles.strips,field_season==2)

head(spiders.strips)
spiders.strips.S1 <- subset(spiders.strips,field_season==1)
spiders.strips.S2 <- subset(spiders.strips,field_season==2)

# Beetles season 1 <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

head(beetles.strips.S1)
library(reshape)
colnames(beetles.strips.S1)
beetle.s1.melt.strips = melt(beetles.strips.S1, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.s1.melt.strips)[match("value", names(beetle.s1.melt.strips))]="N"
head(beetle.s1.melt.strips)

head(beetle.s1.melt.strips) # with the melted dataframe of abundance per species 
beetle.s1.melt.strips$presence = ifelse(beetle.s1.melt.strips$N>=1, 1, 0) #new colum called presence. 
head(beetle.s1.melt.strips)

beetle.df.s1.strips = ddply(beetle.s1.melt.strips, .(field_group,group_edge1,insecticide), summarize, 
                            total.N = sum(N), 
                            S = sum(presence))

beetle.df.s1.strips # beetle diversity on strips 

farm_id2=substr(beetle.df.s1.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
beetle.df.s1.strips$farm_id2 <- farm_id2
beetle.df.s1.strips

t_edge=substr(beetle.df.s1.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
beetle.df.s1.strips$t_edge <- t_edge
beetle.df.s1.strips

t10=substr(beetle.df.s1.strips$group_edge1, 1,1)
t10 = factor(t10)
beetle.df.s1.strips$t10 <- t10
beetle.df.s1.strips

t.test=substr(beetle.df.s1.strips$group_edge1, 1,2)
t.test = factor(t.test)
beetle.df.s1.strips$t.test <- t.test
beetle.df.s1.strips


# Beetles season 2 <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

head(beetles.strips.S2)
library(reshape)
colnames(beetles.strips.S2)
beetle.s2.melt.strips = melt(beetles.strips.S2, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.s2.melt.strips)[match("value", names(beetle.s2.melt.strips))]="N"
head(beetle.s2.melt.strips)

head(beetle.s2.melt.strips) # with the melted dataframe of abundance per species 
beetle.s2.melt.strips$presence = ifelse(beetle.s2.melt.strips$N>=1, 1, 0) #new colum called presence. 
head(beetle.s2.melt.strips)

beetle.df.s2.strips = ddply(beetle.s2.melt.strips, .(field_group,group_edge1,insecticide), summarize, 
                            total.N = sum(N), 
                            S = sum(presence))

beetle.df.s2.strips # beetle diversity on strips 

farm_id2=substr(beetle.df.s2.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
beetle.df.s2.strips$farm_id2 <- farm_id2
beetle.df.s2.strips

t_edge=substr(beetle.df.s2.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
beetle.df.s2.strips$t_edge <- t_edge
beetle.df.s2.strips

t10=substr(beetle.df.s2.strips$group_edge1, 1,1)
t10 = factor(t10)
beetle.df.s2.strips$t10 <- t10
beetle.df.s2.strips

t.test=substr(beetle.df.s2.strips$group_edge1, 1,2)
t.test = factor(t.test)
beetle.df.s2.strips$t.test <- t.test
beetle.df.s2.strips

# Beetles seasons pooled <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

head(beetles.strips)
library(reshape)
colnames(beetles.strips)
beetle.melt.strips = melt(beetles.strips, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.melt.strips)[match("value", names(beetle.melt.strips))]="N"
head(beetle.melt.strips)

head(beetle.melt.strips) # with the melted dataframe of abundance per species 
beetle.melt.strips$presence = ifelse(beetle.melt.strips$N>=1, 1, 0) #new colum called presence. 
head(beetle.melt.strips)

beetle.df.strips = ddply(beetle.melt.strips, .(field_group,group_edge1,insecticide), summarize, 
                         total.N = sum(N), 
                         S = sum(presence))

beetle.df.strips # beetle diversity on strips 

farm_id2=substr(beetle.df.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
beetle.df.strips$farm_id2 <- farm_id2
beetle.df.strips

t_edge=substr(beetle.df.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
beetle.df.strips$t_edge <- t_edge
beetle.df.strips

t10=substr(beetle.df.strips$group_edge1, 1,1)
t10 = factor(t10)
beetle.df.strips$t10 <- t10
beetle.df.strips

t.test=substr(beetle.df.strips$group_edge1, 1,2)
t.test = factor(t.test)
beetle.df.strips$t.test <- t.test
beetle.df.strips

##########################################3

# Spiders season 1 <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

head(spiders.strips.S1)
library(reshape)
colnames(spiders.strips.S1)
spider.s1.melt.strips = melt(spiders.strips.S1, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spider.s1.melt.strips)[match("value", names(spider.s1.melt.strips))]="N"
head(spider.s1.melt.strips)

head(spider.s1.melt.strips) # with the melted dataframe of abundance per species 
spider.s1.melt.strips$presence = ifelse(spider.s1.melt.strips$N>=1, 1, 0) #new colum called presence. 
head(spider.s1.melt.strips)

spider.df.s1.strips = ddply(spider.s1.melt.strips, .(field_group,group_edge1,insecticide), summarize, 
                            total.N = sum(N), 
                            S = sum(presence))

spider.df.s1.strips # beetle diversity on strips 

farm_id2=substr(spider.df.s1.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
spider.df.s1.strips$farm_id2 <- farm_id2
spider.df.s1.strips

t_edge=substr(spider.df.s1.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
spider.df.s1.strips$t_edge <- t_edge
spider.df.s1.strips

t10=substr(spider.df.s1.strips$group_edge1, 1,1)
t10 = factor(t10)
spider.df.s1.strips$t10 <- t10
spider.df.s1.strips

t.test=substr(spider.df.s1.strips$group_edge1, 1,2)
t.test = factor(t.test)
spider.df.s1.strips$t.test <- t.test
spider.df.s1.strips




# Beetles season 2 <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

head(spiders.strips.S2)
library(reshape)
colnames(spiders.strips.S2)
spider.s2.melt.strips = melt(spiders.strips.S2, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spider.s2.melt.strips)[match("value", names(spider.s2.melt.strips))]="N"
head(spider.s2.melt.strips)

head(spider.s2.melt.strips) # with the melted dataframe of abundance per species 
spider.s2.melt.strips$presence = ifelse(spider.s2.melt.strips$N>=1, 1, 0) #new colum called presence. 
head(spider.s2.melt.strips)

spider.df.s2.strips = ddply(spider.s2.melt.strips, .(field_group,group_edge1,insecticide), summarize, 
                            total.N = sum(N), 
                            S = sum(presence))

spider.df.s2.strips # beetle diversity on strips 

farm_id2=substr(spider.df.s2.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
spider.df.s2.strips$farm_id2 <- farm_id2
spider.df.s2.strips

t_edge=substr(spider.df.s2.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
spider.df.s2.strips$t_edge <- t_edge
spider.df.s2.strips

t10=substr(spider.df.s2.strips$group_edge1, 1,1)
t10 = factor(t10)
spider.df.s2.strips$t10 <- t10
spider.df.s2.strips

t.test=substr(spider.df.s2.strips$group_edge1, 1,2)
t.test = factor(t.test)
spider.df.s2.strips$t.test <- t.test
spider.df.s2.strips


# Beetles seasons pooled <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

head(spiders.strips)
library(reshape)
colnames(spiders.strips)
spider.melt.strips = melt(spiders.strips, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spider.melt.strips)[match("value", names(spider.melt.strips))]="N"
head(spider.melt.strips)

head(spider.melt.strips) # with the melted dataframe of abundance per species 
spider.melt.strips$presence = ifelse(spider.melt.strips$N>=1, 1, 0) #new colum called presence. 
head(spider.melt.strips)

spider.df.strips = ddply(spider.melt.strips, .(field_group,group_edge1,insecticide), summarize, 
                         total.N = sum(N), 
                         S = sum(presence))

spider.df.strips # beetle diversity on strips 

farm_id2=substr(spider.df.strips$field_group, 2,4)
farm_id2 = factor(farm_id2)
spider.df.strips$farm_id2 <- farm_id2
spider.df.strips

t_edge=substr(spider.df.strips$group_edge1, 2,2)
t_edge = factor(t_edge)
spider.df.strips$t_edge <- t_edge
spider.df.strips

t10=substr(spider.df.strips$group_edge1, 1,1)
t10 = factor(t10)
spider.df.strips$t10 <- t10
spider.df.strips

t.test=substr(spider.df.strips$group_edge1, 1,2)
t.test = factor(t.test)
spider.df.strips$t.test <- t.test
spider.df.strips

# BETA DIVERSITY #### 

beta.div <- read.csv("betadiversity.csv",sep = ",")
str(beta.div)

beta.beetles <- subset(beta.div,species=="beetles ")
beta.beetle.s1 <- subset(beta.beetles,season==1)
beta.beetle.s2 <- subset(beta.beetles,season==2)

df.beta.beetle.s1 <- merge(beetle.df.s1,beta.beetle.s1, by="farm_id2")
df.beta.beetle.s2 <- merge(beetle.df.s2,beta.beetle.s2, by="farm_id2")

df.beta.beetle.s1$proportion.species.rich <- df.beta.beetle.s1$S/df.beta.beetle.s1$betadiv2
df.beta.beetle.s2$proportion.species.rich <- df.beta.beetle.s2$S/df.beta.beetle.s2$betadiv2 # values higher¿? <-  might be because of the duplicate samples  
df.beta.beetle.s2$proportion.species.rich[df.beta.beetle.s2$proportion.species.rich>1] <- 1

beta.spider <- subset(beta.div,species=="spiders")
beta.spider.s1 <- subset(beta.spider,season==1)
beta.spider.s2 <- subset(beta.spider,season==2)

df.beta.spider.s1 <- merge(spider.df.s1,beta.spider.s1, by="farm_id2")
df.beta.spider.s2 <- merge(spider.df.s2,beta.spider.s2, by="farm_id2")

df.beta.spider.s1$proportion.species.rich <- df.beta.spider.s1$S/df.beta.spider.s1$betadiv2
df.beta.spider.s2$proportion.species.rich <- df.beta.spider.s2$S/df.beta.spider.s2$betadiv2 # values higher¿? 

# dataframe to plot 
both_betas1 <- rbind(df.beta.beetle.s1,df.beta.spider.s1)
both_betas2 <- rbind(df.beta.beetle.s2,df.beta.spider.s2)

# edge + betadiversity 

df.beta.beetle.s1.edge <- merge(beetle.df.edge.s1,beta.beetle.s1, by="farm_id2")
df.beta.beetle.s1.edge$proportion.species.rich <- df.beta.beetle.s1.edge$S/df.beta.beetle.s1.edge$betadiv2

df.beta.beetle.s2.edge <- merge(beetle.df.edge.s2,beta.beetle.s2, by="farm_id2")
df.beta.beetle.s2.edge$proportion.species.rich <- df.beta.beetle.s2.edge$S/df.beta.beetle.s2.edge$betadiv2

df.beta.spider.s1.edge <- merge(spider.df.edge.s1,beta.spider.s1, by="farm_id2")
df.beta.spider.s1.edge$proportion.species.rich <- df.beta.spider.s1.edge$S/df.beta.spider.s1.edge$betadiv2

df.beta.spider.s2.edge <- merge(spider.df.edge.s2,beta.spider.s2, by="farm_id2")
df.beta.spider.s2.edge$proportion.species.rich <- df.beta.spider.s2.edge$S/df.beta.spider.s2.edge$betadiv2

# dataframe to plot 
colnames(df.beta.beetle.s1.edge)
both_betas1.edge <- rbind(df.beta.beetle.s1.edge,df.beta.spider.s1.edge)
str(both_betas1.edge)
both_betas2.edge <- rbind(df.beta.beetle.s2.edge,df.beta.spider.s2.edge)

# checking species captured only on strips ####

head(beetles.strips) 
beetles.strips.metl = melt(beetles.strips, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetles.strips.metl)[match("value", names(beetles.strips.metl))]="N"
head(beetles.strips.metl)

spp_strips.beetles <- subset(beetles.strips.metl, N>=1)
unique(spp_strips.beetles$spp)
head(spp_strips.beetles)

beetle.species.strips = ddply(spp_strips, .(spp), summarize, 
                              total.N = sum(N))

write.csv(beetle.species.strips,file = "beetles_pecies_strips.csv")

head(spiders.strips)
spiders.strips.metl = melt(spiders.strips, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spiders.strips.metl)[match("value", names(spiders.strips.metl))]="N"
head(spiders.strips.metl)

spp_strips.spiders <- subset(spiders.strips.metl, N>=1)
unique(spp_strips.spiders$spp)
head(spp_strips.spiders)

spiders.species.strips = ddply(spp_strips.spiders, .(spp), summarize, 
                               total.N = sum(N))

write.csv(spiders.species.strips,file = "spider_pecies_strips.csv")

# beetles canola and wheat spearated ####
head(beetles)
Beetles.MC <- subset(beetles.all, t3 == "MC",
                     select=c(5,9,13,17,21:65))
head(Beetles.MC)

beetle.mc1 = melt(Beetles.MC, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.mc1)[match("value", names(beetle.mc1))]="N"
head(beetle.mc1)

spp_mc.beetles <- subset(beetle.mc1, N>=1)
unique(spp_mc.beetles$spp)
head(spp_mc.beetles)

beetle.species.mc = ddply(spp_mc.beetles, .(spp), summarize, 
                          total.N = sum(N))

write.csv(beetle.species.mc,file = "beetles_pecies_MC.csv")

# WHEAT
Beetles.W <- subset(beetles.all, t3 == "MW",
                    select=c(5,9,13,17,21:65))
head(Beetles.W)

beetle.W1 = melt(Beetles.W, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(beetle.W1)[match("value", names(beetle.W1))]="N"
head(beetle.W1)

spp_W.beetles <- subset(beetle.W1, N>=1)
unique(spp_W.beetles$spp)
head(spp_W.beetles)

beetle.species.W = ddply(spp_W.beetles, .(spp), summarize, 
                         total.N = sum(N))

write.csv(beetle.species.W,file = "beetles_pecies_W.csv")

# spider species per monoculture separatedly ####
spiders.MC <- subset(spiders.all, t3 == "MC",
                     select=c(5,9,13,17,21:65))
head(spiders.MC)

spiders.mc1 = melt(spiders.MC, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spiders.mc1)[match("value", names(spiders.mc1))]="N"
head(spiders.mc1)

spp_mc.spiders <- subset(spiders.mc1, N>=1)
unique(spp_mc.spiders$spp)
head(spp_mc.spiders)

spiders.species.mc = ddply(spp_mc.spiders, .(spp), summarize, 
                           total.N = sum(N))

write.csv(spiders.species.mc,file = "spiders_pecies_MC.csv")

# WHEAT
spiders.W <- subset(spiders.all, t3 == "MW",
                    select=c(5,9,13,17,21:65))
head(spiders.W)

spiders.W1 = melt(spiders.W, id.vars=1:6, measure.vars=7:49, variable_name="spp")
names(spiders.W1)[match("value", names(spiders.W1))]="N"
head(spiders.W1)

spp_W.spiders <- subset(spiders.W1, N>=1)
unique(spp_W.spiders$spp)
head(spp_W.spiders)

spider.species.W = ddply(spp_W.spiders, .(spp), summarize, 
                         total.N = sum(N))

write.csv(spider.species.W,file = "spider_pecies_W.csv")

