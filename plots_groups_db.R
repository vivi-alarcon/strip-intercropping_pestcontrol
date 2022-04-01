library(sciplot)
## par margins correct !! #####
output=par(mfrow=c(2,2),mar=c(1.1,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# beetles in both seasons ####
# beetle abundance S1
bargraph.CI(t3,total.N, data=beetle.df.s1, 
            ylab="Beetle Abundance  S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)
# beetle abundance S2
bargraph.CI(t3,total.N, data=beetle.df.s2, 
            ylab="Beetle Abundance  S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,120), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)

# beetle richness S1
bargraph.CI(t3,S, data=beetle.df.s1, 
            ylab="Beetle richness S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)

# beetle richness S2
bargraph.CI(t3,S, data=beetle.df.s2, 
            ylab="Beetle richness S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,30), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)

# spiders in both seasons ####
output=par(mfrow=c(2,2),mar=c(1.1,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# spider abundance S1
bargraph.CI(t3,total.N, data=spider.df.s1, 
            ylab="Spider Abundance  S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)
# spider abundance S2
bargraph.CI(t3,total.N, data=spider.df.s2, 
            ylab="Spider Abundance  S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)

# spider richness S1
bargraph.CI(t3,S, data=spider.df.s1, 
            ylab="Spider richness S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)

# spider richness S2
bargraph.CI(t3,S, data=spider.df.s2, 
            ylab="spider richness S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=0.5, y.leg=15,err.width=0.04)

# trying to plot all together but separated by season ###
head(beetle.df.s2)
beetle.df.s2$species <- "Beetles"
head(beetle.df.s1)
beetle.df.s1$species <- "Beetles"

head(spider.df.s1)
spider.df.s1$species <- "Spiders"
head(spider.df.s2)
spider.df.s2$species <- "Spiders"

both.s1 <- rbind(spider.df.s1,beetle.df.s1)
both.s2 <- rbind(spider.df.s2,beetle.df.s2)
nrow(both.s1)
both.s1$total.N <- as.numeric(both.s1$total.N)
both.s1$species <- as.factor(both.s1$species)
str(both.s2)
both.s2$total.N <- as.numeric(both.s2$total.N)
both.s2$species <- as.factor(both.s2$species)

output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# abundance S1 <-  both species 
bargraph.CI(species,
            total.N, 
            group=t3,
            data=both.s1, 
            ylab="Abundance  S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,100), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5, y.leg=100,err.width=0.04)

# abundance S2 <-  both species 
bargraph.CI(species,
            total.N, 
            group=t3,
            data=both.s2, 
            ylab="Abundance  S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,100), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5, y.leg=100,err.width=0.04)

# both species richness S1
bargraph.CI(species,
            S, 
            group=t3,
            data=both.s1, 
            ylab="Richness S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,25), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5, y.leg=25,err.width=0.04)


# both species richness S2
bargraph.CI(species,
            S,
            group=t3,
            data=both.s2, 
            ylab="Richness S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,25), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5, y.leg=25,err.width=0.04)

# plot seasons pooled 
head(beetle.df.alls)
beetle.df.alls$species <- "Beetles"
head(spider.df.alls)
spider.df.alls$species <- "Spiders"

both.alls <- rbind(spider.df.alls,beetle.df.alls)
nrow(both.alls)
both.alls$total.N <- as.numeric(both.alls$total.N)
both.alls$species <- as.factor(both.alls$species)


output=par(mfrow=c(2,1),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# abundance season pooled <-  both species 
bargraph.CI(species,
            total.N, 
            group=t3,
            data=both.alls, 
            ylab="Abundance  both seasons", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,120), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=6, y.leg=125,err.width=0.04)

# spider season pooled <-  both species 
bargraph.CI(species,
            S, 
            group=t3,
            data=both.alls, 
            ylab="Richness both seasons", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,35), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=6, y.leg=35,err.width=0.04)

##########EDGE PLOTS ####
# trying to plot all together but separated by season <-  EDGE!###
beetle.df.edge.s1$species <- "Beetles"
head(beetle.df.edge.s1)
beetle.df.edge.s2$species <- "Beetles"
head(beetle.df.edge.s2)

spider.df.edge.s1$species <- "Spiders"
head(spider.df.edge.s1)
spider.df.edge.s2$species <- "Spiders"
head(spider.df.edge.s2)


both.s1.edge <- rbind(spider.df.edge.s1,beetle.df.edge.s1)
both.s2.edge <- rbind(spider.df.edge.s2,beetle.df.edge.s2)
str(both.s1.edge)
str(both.s2.edge)

both.s1.edge$total.N <- as.numeric(both.s1.edge$total.N)
both.s1.edge$species <- as.factor(both.s1.edge$species)
str(both.s2)
both.s2.edge$total.N <- as.numeric(both.s2.edge$total.N)
both.s2.edge$species <- as.factor(both.s2.edge$species)

head(both.s1.edge)

output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# abundance S1 <-  both species 
bargraph.CI(species:t10,
            total.N, 
            group=t_edge,
            data=both.s1.edge, 
            ylab="Abundance  S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=13, y.leg=14,err.width=0.04)



# abundance S2 <-  both species 
bargraph.CI(species:t10,
            total.N, 
            group=t_edge,
            data=both.s2.edge, 
            ylab="Abundance  S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,80), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=13, y.leg=80,err.width=0.04)

# richness S1 <-  both species 
bargraph.CI(species:t10,
            S, 
            group=t_edge,
            data=both.s1.edge, 
            ylab="Richness S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=13, y.leg=10,err.width=0.04)


# richness S2 <-  both species
bargraph.CI(species:t10,
            S,
            group=t_edge,
            data=both.s2.edge, 
            ylab="Richness S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=13, y.leg=15,err.width=0.04)



## plot abundance and richness per species and cultivar <-  edge effects ####
head(both.s1.edge)
head(both.s2.edge)

output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

bargraph.CI(species:t10,
            total.N, 
            group=t_edge,
            data=both.s1.edge, 
            ylab="Abundance  S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=18, y.leg=15,err.width=0.04)


bargraph.CI(species:t10,
            total.N, 
            group=t_edge,
            data=both.s2.edge, 
            ylab="Abundance  S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,80), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=18, y.leg=80,err.width=0.04)

bargraph.CI(species:t10,
            S, 
            group=t_edge,
            data=both.s1.edge, 
            ylab="Richness  S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=18, y.leg=10,err.width=0.04)

bargraph.CI(species:t10,
            S, 
            group=t_edge,
            data=both.s2.edge, 
            ylab="Richness  S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,15), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=18, y.leg=14,err.width=0.04)


## plots both species pooled seasons ####
beetle.df.edge.s1$season <- "S1"
beetle.df.edge.s2$season <- "S2"

spider.df.edge.s1$season <- "S1"
spider.df.edge.s2$season <- "S2"

beetes_both_S <- rbind(beetle.df.edge.s1,beetle.df.edge.s2)
spiders_both_S <- rbind(spider.df.edge.s1,spider.df.edge.s2)

str(beetes_both_S)

beetes_both_S$total.N <- as.numeric(beetes_both_S$total.N)
beetes_both_S$species <- as.factor(beetes_both_S$species)
beetes_both_S$season <- as.factor(beetes_both_S$season)


spiders_both_S$total.N <- as.numeric(spiders_both_S$total.N)
spiders_both_S$species <- as.factor(spiders_both_S$species)
spiders_both_S$season <- as.factor(spiders_both_S$season)

a <- rbind(beetes_both_S,spiders_both_S)
write.csv(a, file = "a.csv")
a <- read.csv("a.csv",sep = ",")

output=par(mfrow=c(2,1),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

bargraph.CI(species:t11,
            total.N, 
            group=t_edge,
            data=a, 
            ylab="Abundance (pooled seasons)", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,50), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=20, y.leg=50,err.width=0.04)


bargraph.CI(species:t11,
            S, 
            group=t_edge,
            data=a, 
            ylab="Richness (pooled seasons)", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=20, y.leg=10,err.width=0.04)


# aphids ####
output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

head(aphid.density)
# aphid density ####
bargraph.CI(t10,
            total.aphids, 
            data=aphid.density, 
            ylab="Aphid density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,100), 
            las=1, col=c("darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)

bargraph.CI(t10,
            total.mummies, 
            data=aphid.density, 
            ylab="Mummies density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,16), 
            las=1, col=c("darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)

# pest predation wheat ###
bargraph.CI(t10,
            proportion.mummified, 
            data=aphid.density, 
            ylab="Proportion of mummified aphids", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)

bargraph.CI(t10,
            proportion.wasps, 
            data=aphid.density, 
            ylab="Proportion of hatched wasps", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)


output=par(mfrow=c(1,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# abundance aphids ~edge ####
bargraph.CI(t10,
            total.aphids,
            group = t_edge,
            data=aphid.edge, 
            ylab="Aphid density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,50), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=50,err.width=0.04)
# mummified aphids ~edge 
bargraph.CI(t10,
            proportion.mummified, 
            group = t_edge,
            data=aphid.edge, 
            ylab="Proportion of mummified aphids", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

# checking for correlation between aphid adundance and mummification rate ####
plot(aphid.edge$total.aphids~aphid.edge$proportion.mummified)
plot(aphid.edge$proportion.mummified~aphid.edge$total.aphids)
cor.test(proportion.mummified~total.aphids,aphid.edge)

library("ggpubr")
ggscatter(aphid.edge, x = "proportion.mummified", y = "total.aphids", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "proportion mummified", ylab = "aphid density")
res <- cor.test(aphid.edge$proportion.mummified, aphid.edge$total.aphids, 
                method = "pearson")
res

library("ggpubr")
ggscatter(aphid.density, x = "proportion.mummified", y = "total.aphids", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "proportion mummified", ylab = "aphid density")
res <- cor.test(aphid.density$proportion.mummified, aphid.density$total.aphids, 
                method = "pearson")
res



output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# pollen beetle larvae density #### 
bargraph.CI(t10,
            total.larvae, 
            data=larvae.density, 
            ylab="Pollen beetle larvae density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,100), 
            las=1, col=c("darkgoldenrod","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)


bargraph.CI(t10,
            eggs.per.larvae, 
            data=larvae.density, 
            ylab="Mean num eggs per larvae ", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,5), 
            las=1, col=c("darkgoldenrod","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)

# pest predation CANOLA ####
bargraph.CI(t10,
            proportion, 
            data=larvae.density, 
            ylab="Proportion of infested larvae", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","#6E8DAB"),legend=TRUE,leg.lab=c("Wheat monoculture","Strip-intercropping"),
            x.leg=1, y.leg=10,err.width=0.04)

 
output=par(mfrow=c(1,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

# larvae density ~edge ####
bargraph.CI(t10,
            total.larvae,
            group = t_edge,
            data=larvae.edge, 
            ylab="Pollen beetle larvae density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,80), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=80,err.width=0.04)
# infested larvae ~edge ####
bargraph.CI(t10,
            proportion, 
            group = t_edge,
            data=larvae.edge, 
            ylab="Proportion of infested larvae", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

# checking for correlation between aphid adundance and mummification rate ####

plot(larvae.edge$proportion~larvae.edge$total.larvae)


library("ggpubr")
ggscatter(larvae.edge, x = "proportion", y = "total.larvae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "proportion infested larvae", ylab = "larvae density")
res <- cor.test(larvae.edge$proportion, larvae.edge$total.larvae, 
                method = "pearson")
res

library("ggpubr")
ggscatter(larvae.density, x = "proportion", y = "total.larvae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "proportion infested larvae", ylab = "larvae density")
res <- cor.test(larvae.density$proportion, larvae.density$total.larvae, 
                method = "pearson")
res

# parasite rates both crops together #### 
output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text
bargraph.CI(t10,
            total.aphids,
            group = t_edge,
            data=aphid.edge, 
            ylab="Aphid density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,50), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=50,err.width=0.04)

bargraph.CI(t10,
            total.larvae,
            group = t_edge,
            data=larvae.edge, 
            ylab="Pollen beetle larvae density", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,80), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=80,err.width=0.04)


bargraph.CI(t10,
            proportion.mummified, 
            group = t_edge,
            data=aphid.edge, 
            ylab="Proportion of mummified aphids", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)


bargraph.CI(t10,
            proportion, 
            group = t_edge,
            data=larvae.edge, 
            ylab="Proportion of infested larvae", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

# insecticide influence ####
output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

bargraph.CI(insecticide:t_edge,
            S, 
            data=beetle.df.strips, 
            ylab="beetle richness ALL S", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)


bargraph.CI(insecticide:t_edge,
            S, 
            data=beetle.df.s1.strips, 
            ylab="beetle richness S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(insecticide:t_edge,
            S, 
            data=beetle.df.s2.strips, 
            ylab="beetle richness S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(insecticide,
            S, 
            data=spider.df.s2.strips, 
            ylab="spider richness S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,10), 
            las=1, col=c("cyan4","cornsilk4"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

# insecticide - aphids ####
str(aphids.strips)
aphids.strips$insecticide <- as.factor(aphids.strips$insecticide)
bargraph.CI(insecticide,
            proportion.mummified, 
            data=aphids.strips, 
            ylab="proportion of mummified aphids", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("cyan4","cornsilk4"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(insecticide:t_edge,
            proportion.mummified, 
            data=aphids.strips, 
            ylab="proportion of mummified aphids", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

# insecticide - larvae  ####
str(larvae.strips)
larvae.strips$insecticide <- as.factor(larvae.strips$insecticide)
bargraph.CI(insecticide,
            proportion.parasited, 
            data=larvae.strips, 
            ylab="proportion of infested larvae", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("cyan4","cornsilk4"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(insecticide:t_edge,
            proportion.parasited, 
            data=larvae.strips, 
            ylab="proportion of infested larvae", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkseagreen","darksalmon"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

## Beta diversity ####

bargraph.CI(t3,
            proportion.species.rich, 
            data=df.beta.beetle.s1, 
            ylab="Beta beetles S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(t3,
            proportion.species.rich, 
            data=df.beta.beetle.s2, 
            ylab="Beta beetles S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(t3,
            proportion.species.rich, 
            data=df.beta.spider.s1, 
            ylab="Beta spider S1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

bargraph.CI(t3,
            proportion.species.rich, 
            data=df.beta.spider.s2, 
            ylab="Beta spider S2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Edge","Interior"),
            x.leg=3.5, y.leg=1,err.width=0.04)

output=par(mfrow=c(2,2),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

bargraph.CI(species,
            proportion.species.rich, 
            group = t3,
            data=both_betas1, 
            ylab="Proportion from the farm pool of spp s1", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5.5, y.leg=1,err.width=0.04)

bargraph.CI(species,
            proportion.species.rich, 
            group = t3,
            data=both_betas2, 
            ylab="Proportion from the farm pool of spp s2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","#6E8DAB"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5.5, y.leg=1,err.width=0.04)

# beta richness per cultivar <-  using the edge dataframe 

output=par(mfrow=c(2,1),mar=c(2.2,4.1,2.1,2.1)) # sets the bottom, left, top and right margins respectively of the plot region in number of lines of text

bargraph.CI(species:t10,
            proportion.species.rich, 
            #group = t10,
            data=both_betas2.edge, 
            ylab="Proportion from the farm pool of spp s2", xlab="", 
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,ylim=c(0,1), 
            las=1, col=c("darkgoldenrod","darkolivegreen","darkgoldenrod2","darkseagreen"),legend=TRUE,leg.lab=c("Oilseed rape","Wheat","Strip-intercrop"),
            x.leg=5.5, y.leg=1,err.width=0.04)

