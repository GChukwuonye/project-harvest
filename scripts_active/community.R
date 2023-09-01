#Authors: Kunal Palawat and Gift Chukwuonye
#Description: Code to analyze PH community specific variables

#load libraries ----
#base
library(readxl) #read excel files
library(tidyverse)
library(ggplot2)

#models
library(EnvStats)
library(lme4)
library(lmerTest)
library(ggeffects)
library(performance)
library(effects)
library(ggpubr)
library(emmeans)
library(multcomp)
library(car)


#dewey ----
dewey <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "dewey", col_names = TRUE)
ddat <- iw.dm.long[iw.dm.long$community=="Dewey-Humboldt",]
deweydat <- full_join(ddat, dewey, by = c("site"))
deweydat <- deweydat[!is.na(deweydat$community),]
deweydat <- deweydat[!is.na(deweydat$location),]
deweydat$analyte.ln <- log(deweydat$value)
deweydat$location<- factor(deweydat$location, levels = c("South", "East", "North East", "North", "North West"))
# deweydat.edist <- deweydat[!is.na(deweydat$edist.rounded),]
# deweydat.wdist <- deweydat[!is.na(deweydat$wdist.rounded),]
deweydat.long <- pivot_longer(data = deweydat,
                         cols = edist.rounded:wdist.rounded,
                         values_to = "dist.rounded",
                         names_to = "tailings")
deweydat.long[deweydat.long$tailings=="edist.rounded",]$tailings <- "Eastern Tailings"
deweydat.long[deweydat.long$tailings=="wdist.rounded",]$tailings <- "Western Tailings"
deweydat.long$tailings <- factor(deweydat.long$tailings, levels = c("Western Tailings", "Eastern Tailings"))

deweydat$edist.rounded.km <- deweydat$edist.rounded/1000
deweydat$wdist.rounded.km <- deweydat$wdist.rounded/1000

ggplot(data = deweydat.long, mapping = aes(y = analyte.ln, x = dist.rounded/1000, color = tailings)) + 
  geom_point(size = 1)+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

#it seems like there is not much of a difference between the distances from each of the tailings piles, so kind of hard to see an association. there is also not much coverage between 5 and 10 km away from either tailings.


dsource1 <- lmer(data = deweydat,
                 log(value) ~ edist.rounded + wdist.rounded 
                 + edist.rounded:analyte 
                 + wdist.rounded:analyte
                 + (1|site),
                 REML = F) #ML for comparison, REML for final
summary(dsource1)
plot(dsource1)
model.effects <- allEffects(dsource1)
anova(dsource1)
plot(model.effects) #not good comparisons because of scale differences and automatic set up

#Anova tells us that e tilings and e tailings:analyte interaction is not significant. move forwrd with w tailings as point source.


#globe ----
globe <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "globe", col_names = TRUE)
gdat <- iw.dm.long[iw.dm.long$community=="Globe/Miami",]
globedat <- full_join(gdat, globe, by = c("site"))
globedat <- globedat[!is.na(globedat$community),]
globedat <- globedat[!is.na(globedat$location),]

globedat$location <- factor(globedat$location, levels = c("Claypool", "Globe District Two", "Globe District Three", "Globe District Four", "Globe District Five", "Globe District Six", "Globe District One", "Miami", "Central Heights-Midland City", "Icehouse Canyon", "Six Shooter Canyon"))

globedat$location_2 <- factor(globedat$location_2, levels = c("Miami/Claypool Area", "Globe Area", "Canyons Area"))

globedat$nearest.dist.km <- globedat$nearest.dist.rounded/1000

#graph to viz associations by source
ggplot(data = globedat, mapping = aes(y = log(value), x = nearest.dist.km)) + 
  geom_point(size = 1)+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

#only one pollution source identified in Globe - the mine

#hayden ----

#load water data from contaminant descriptions file, subset hayden/winkelman data only
hayden <- iw.dm.long[iw.dm.long$community=="Hayden/Winkelman",]

#assign water samples pre and post strike
hayden$collection_date <- substr(hayden$sample.name, 11, 18) #subset sample name to create collection date
hayden$strike <- as.numeric(hayden$collection_date) #make numeric
hayden[hayden$strike<20191013,]$strike <- 0 #assign pre strike values 0
hayden[hayden$strike>=20191013,]$strike <- 1 #assign post strike values 1
hayden$strike <- as.factor(hayden$strike) #make factor
hayden$strike <- ifelse(hayden$strike==0, "Operating", "Not Operating") #label with words
hayden$strike <- factor(hayden$strike, levels = c("Operating", "Not Operating")) #order the labels

#load hayden distance data
hdistance <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "hayden", col_names = TRUE)
hdistance <- subset(hdistance, select = c("site", "siteID", "distance.rounded", "location", "distance.to.tailings.round", "dist.tailings.km"))

#merge with water and soil data by site
haydendat <- full_join(hayden, hdistance, by = c("site"))
#is.haydendat <- full_join(hayden.is, hdistance, by = c("site"))

#remove rows without data
haydendat <- haydendat[!is.na(haydendat$community),]
#is.haydendat <- is.haydendat[!is.na(is.haydendat$community),]

haydendat$direction <- factor(haydendat$location, levels = c("Hayden", "Winkelman"))
#is.haydendat$direction <- factor(is.haydendat$location, levels = c("Hayden", "Winkelman"))

#remove kit H222
haydendat <- haydendat[haydendat$site!="H222",]

#subset winter data because only winter data exists post strike
haydendat.winter <- haydendat[haydendat$season=="Winter",]

#distance from smelter smoke stack
haydendat$distance.rounded.km <- haydendat$distance.rounded/1000
haydendat.long <- pivot_longer(data = haydendat,
                              cols = c("distance.rounded.km","dist.tailings.km"),
                              values_to = "dist.rounded.km",
                              names_to = "source")
haydendat.long$dist.rounded.km <- as.numeric(haydendat.long$dist.rounded.km)
haydendat.long[haydendat.long$source=="distance.rounded.km",]$source <- "Smelter"
haydendat.long[haydendat.long$source=="dist.tailings.km",]$source <- "Tailings"
haydendat.long$source <- factor(haydendat.long$source, levels = c("Tailings", "Smelter"))

#graph to viz associations by source
ggplot(data = haydendat.long, mapping = aes(y = log(value), x = dist.rounded.km, color = source)) + 
  geom_point(size = 1)+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

#it seems like there is an association with both sources. i think we should run a model to compare the affect of them.

hsource1 <- lmer(data = haydendat,
            log(value) ~ distance.rounded.km + dist.tailings.km 
            + distance.rounded.km:analyte 
            + dist.tailings.km:analyte
            + (1|site),
            REML = F) #ML for comparison, REML for final
summary(hsource1)
model.effects <- allEffects(hsource1)
anova(hsource1)
plot(model.effects) #not good comparisons because of scale differences and automatic set up

#Anova tells us that tailigns and tailings:analyte interaction is not significant. move forwrd with smelter as point source.


# tucson ----
tucson <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "tucson", col_names = TRUE)
tdat <- iw.dm.long[iw.dm.long$community=="Tucson",]

tucsondat <- full_join(tdat, tucson, by = c("site"))
tucsondat <- tucsondat[!is.na(tucsondat[c("community")]),]
tucsondat <- tucsondat[!is.na(tucsondat[c("ward")]),]
tucsondat <- tucsondat[!is.na(tucsondat[c("location")]),]

tucsondat$ward <- factor(tucsondat$ward, levels = c("One", "Two", "Three", "Four", "Five", "Six"))

tucsondat.long <- pivot_longer(data = tucsondat,
                               cols = c("dist.jensen.km","dist.fci.km", "dist.tia.km", "dist.afb.km"),
                               values_to = "dist.km",
                               names_to = "source")

#graph to viz associations by source
ggplot(data = tucsondat.long, mapping = aes(y = log(value), x = dist.km, color = source)) + 
  #geom_point(size = 1)+
  geom_smooth()+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

ggplot(data = tucsondat.long, mapping = aes(y = log(value), x = ward)) + 
  geom_boxplot()+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

#it seems like there is an association with many sources. i think we should run a model to compare the affect of them.

tsource1 <- lmer(data = tucsondat,
                 log(value) ~ dist.jensen.km + dist.fci.km + dist.tia.km + dist.afb.km + location + ward + season
                 + dist.jensen.km:analyte
                 + dist.fci.km:analyte
                 + dist.tia.km:analyte
                 + dist.afb.km:analyte
                 + ward:analyte
                 + location:analyte
                 + (1|site),
                 REML = F) #ML for comparison, REML for final
summary(tsource1)
model.effects <- allEffects(tsource1)
plot(model.effects) #not good comparisons because of scale differences and automatic set up
anova(tsource1)

#model effects make it seem like tia and afb and jensen are influential. anova tells us that jensen is influential but that the trend is different for each analyte

#try stepwise
tsource0 <- lmer(data = tucsondat,
                 log(value) ~ 
                 + (1|site),
                 REML = F) #ML for comparison, REML for final

tsource.step <- step(tsource1, scope=list(lower=tsource0), direction="both")

#remove jensen and remove location
tsource2 <- lmer(data = tucsondat,
                 log(value) ~ dist.fci.km + dist.tia.km + dist.afb.km + ward + season
                 + dist.fci.km:analyte
                 + dist.tia.km:analyte
                 + dist.afb.km:analyte
                 + ward:analyte
                 + (1|site),
                 REML = F) #ML for comparison, REML for final

summary(tsource2)
model.effects <- allEffects(tsource2)
plot(model.effects) #not good comparisons because of scale differences and automatic set up
anova(tsource2)
