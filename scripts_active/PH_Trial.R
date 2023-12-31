#Authors: Gift Chukwuonye, Kunal Palawat
#Description: Code to test analyzing multiple metal(loid)s at once with linear mixed models - Project Harvest rooftop harvested rainwater data

#Notes
# add pH into model with relevant interactions
# Al, As, Ba, Be, Cd, Cr, Cu, Mn, Ni, Pb, Zn

#load libraries ----
library(readxl)
library(tidyverse)
library(ggplot2)
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

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data ----
#IW DM
iw.dm <- read_xlsx("data/data_clean/IW_DM_Y123.xlsx", sheet = "Corrected", col_names = TRUE) #corrected mean the corrected tab in the excel sheet
iw.dm.detects <- read_xlsx("data/data_clean/IW_DM_Y123.xlsx", sheet = "Detection", col_names = TRUE)
mlod <- read_xlsx("data/data_processing/IPSW_MLODS.xlsx", sheet = "corrected - 12.22.20", col_names = TRUE)
iw.mlod <- mlod[mlod$`Sample Type`=="IW",]
iw.mlod.dm <- iw.mlod[iw.mlod$Analysis=="DM",]

#wrangle data
#add period and season variables
iw.dm$period <- iw.dm$samplings
iw.dm$season <- iw.dm$samplings

#redefine them
iw.dm[iw.dm$period=="First Winter",]$period <- "First"
iw.dm[iw.dm$period=="Last Winter",]$period <- "Last"
iw.dm[iw.dm$period=="First Monsoon",]$period <- "First"
iw.dm[iw.dm$period=="Last Monsoon",]$period <- "Last"

iw.dm[iw.dm$season=="First Winter",]$season <- "Winter"
iw.dm[iw.dm$season=="Last Winter",]$season <- "Winter"
iw.dm[iw.dm$season=="First Monsoon",]$season <- "Monsoon"
iw.dm[iw.dm$season=="Last Monsoon",]$season <- "Monsoon"


#changing year #Make a new variable with this

iw.dm$year <- iw.dm$sampling_year
iw.dm[iw.dm$year=="2017-2018",]$year <- "Water Year 1"
iw.dm[iw.dm$year=="2018-2019",]$year <- "Water Year 2"
iw.dm[iw.dm$year=="2019-2020",]$year <- "Water Year 3"

#remove field blanks. to remove anything, use a type!="VAlue" to remove them.
iw.dm <- iw.dm[iw.dm$type!="B", ]
iw.dm.detects <- iw.dm.detects[iw.dm.detects$type!="B", ]

#remove ATS samples because ATS samples are not included in PH research. 
iw.dm <- iw.dm[iw.dm$site!="ATS1", ]
iw.dm.detects <- iw.dm.detects[iw.dm.detects$site!="ATS1", ]

#remove year 3 monsoon samples
iw.dm$ssnyear <- paste(iw.dm$season, iw.dm$year)
iw.dm <- iw.dm[iw.dm$ssnyear!="Monsoon Water Year 3", ]


#confirm correct order of categorical variables
#iw.dm$samplings <- factor(iw.dm$samplings, levels = c("FW", "LW", "FM", "LM"))
iw.dm$samplings <- factor(iw.dm$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
iw.dm$period <- factor(iw.dm$period, levels = c("First", "Last"))
iw.dm$season <- factor(iw.dm$season, levels = c("Winter", "Monsoon"))
iw.dm$sampling_year <- factor(iw.dm$sampling_year, levels = c("Water Year 1", "Water Year 2", "Water Year 3"))
iw.dm$community <- factor(iw.dm$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))



#For the detection data,
#detect data
#add period and season variables
iw.dm.detects$period <- iw.dm.detects$samplings
iw.dm.detects$season <- iw.dm.detects$samplings


#redefine them
iw.dm.detects[iw.dm.detects$period=="First Winter",]$period <- "First"
iw.dm.detects[iw.dm.detects$period=="Last Winter",]$period <- "Last"
iw.dm.detects[iw.dm.detects$period=="First Monsoon",]$period <- "First"
iw.dm.detects[iw.dm.detects$period=="Last Monsoon",]$period <- "Last" 

iw.dm.detects[iw.dm.detects$season=="First Winter",]$season <- "Winter"
iw.dm.detects[iw.dm.detects$season=="Last Winter",]$season <- "Winter"
iw.dm.detects[iw.dm.detects$season=="First Monsoon",]$season <- "Monsoon"
iw.dm.detects[iw.dm.detects$season=="Last Monsoon",]$season <- "Monsoon"


#changing year
iw.dm.detects$year<-iw.dm.detects$sampling_year

iw.dm.detects[iw.dm.detects$sampling_year=="2017-2018",]$sampling_year <- "Water Year 1"
iw.dm.detects[iw.dm.detects$sampling_year=="2018-2019",]$sampling_year <- "Water Year 2"
iw.dm.detects[iw.dm.detects$sampling_year=="2019-2020",]$sampling_year <- "Water Year 3"

#remove year 3 monsoon samples
iw.dm.detects$ssnyear <- paste(iw.dm.detects$season, iw.dm.detects$sampling_year)
iw.dm.detects <- iw.dm.detects[iw.dm.detects$ssnyear!="Monsoon Water Year 3", ]


#confirm correct order of categorical variables
#iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("FW", "LW", "FM", "LM"))
iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
iw.dm.detects$period <- factor(iw.dm.detects$period, levels = c("First", "Last"))
iw.dm.detects$season <- factor(iw.dm.detects$season, levels = c("Winter", "Monsoon"))
iw.dm.detects$sampling_year <- factor(iw.dm.detects$sampling_year, levels = c("Water Year 1", "Water Year 2", "Water Year 3"))
iw.dm.detects$community <- factor(iw.dm.detects$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))



#pH and EC data
#append pH and EC data
iw.pHec <- read_xlsx("data/data_clean/IW_pHEC_Y123.xlsx", sheet = 1, col_names = TRUE)
iw.pHec <- iw.pHec[iw.pHec$type!="B",] #removing field blanks
iw.dm <- full_join(iw.dm, iw.pHec, by = c("sample.name", "type")) #joins the phec data with the original iw.dm we had before
iw.dm <- iw.dm[!is.na(iw.dm$community),]
na.omit(iw.dm$community)

iw.pHec <- iw.dm[!is.na(iw.dm$pH),]
na.omit(iw.dm$pH)
iw.pHec <- iw.dm[!is.na(iw.dm$EC),]
na.omit(iw.dm$EC)
aggregate(iw.pHec$EC,
          by = list(iw.pHec$season),
          FUN = max)



median(iw.pHec$EC)
iw.dm.long <- pivot_longer(iw.dm,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")

iw.dm.detects.long <- pivot_longer(iw.dm.detects,
                                   cols = Be:Pb,
                                   values_to = "detection",
                                   names_to = "analyte")


iw.mlod.dm.long <- pivot_longer(iw.mlod.dm,
                                cols=Be:Pb,
                                values_to = "value",
                                names_to = "analyte")



#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#Multiple Metals Test ----
dat.long <- pivot_longer(data = iw.dm,
                         cols = c("Al", "As", "Ba", "Be", "Cd", "Cr", "Cu", "Mn", "Ni", "Pb", "Zn"),
                         values_to = "concentration",
                         names_to = "analyte")

#quick data viz
ggplot(data = dat.long, mapping = aes(y = log(concentration), x = community)) + 
  geom_boxplot()+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

ggplot(data = dat.long, mapping = aes(y = log(concentration), x = period)) + 
  geom_boxplot()+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

ggplot(data = dat.long, mapping = aes(y = log(concentration), x = community, fill = season)) + 
  geom_boxplot()+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

ggplot(data = dat.long, mapping = aes(y = log(concentration), x = pH, color = community)) + 
  geom_point(size = 1)+
  facet_wrap(analyte~., scales = "free") #ln transformed just to be able to compare better

#community:analyte, season:community, pH:analyte:community

#for Al, Cd, and Zn it does seem like there are different community trends. There are not very different period or season trends. Scales are different though. There are pretty similar season and period trends by community for analytes, meaning that H/W concentrations of Cd are highest in first and last periods, etc. This could mean that community:analyte is important but period:analyte and season:analyte and maybe season:period:analyte are not.


mm0 <- lmer(data = dat.long,
            concentration ~ (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(mm0)

mm1 <- lmer(data = dat.long,
            concentration ~
            + community + community:period + community:season + community:analyte
            + period + period:season + period:analyte 
            + season + season:analyte
            + period:analyte:season + period:analyte:community + season:analyte:community
            + (1|community:site),
            REML = T) #ML for comparison, REML for final
summary(mm1)
plot(mm1) #not heteroscedastic when untransformed
model.effects <- allEffects(mm1)
plot(model.effects) #not good comparisons because of scale differences
vif(mm1)

mm2 <- lmer(data = dat.long,
            concentration ~
            + community + community:analyte
            + period + period:season + period:analyte 
            + season + season:analyte
            + period:analyte:season
            + (1|community:site),
            REML = T)
summary(mm2)
plot(mm2) #not heteroscedastic when untransformed
model.effects <- allEffects(mm2)
plot(model.effects) #not good comparisons because of scale differences
vif(mm2)

#try backwards stepwise method to get best model
step(mm1, scope=list(lower=mm0), direction="backward")

#copy it and summarize
mm1.1 <- lmer(data = dat.long,
             concentration ~ community + season
             + analyte:community + analyte:season
             + (1 | community:site),
             REML = T)
summary(mm1.1)
plot(mm1.1) #not heteroscedastic when untransformed
model.effects <- allEffects(mm1.1)
plot(model.effects) #not good comparisons because of scale differences
vif(mm1.1)

#Dewey-Humboldt=======
#load data
dewey <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "dewey", col_names = TRUE)
deweydat <- iw.dm.long[iw.dm.long$community=="Dewey-Humboldt",]
dat <- full_join(deweydat, dewey, by = c("site"))
dat <- dat[!is.na(dat$community),]
dat <- dat[!is.na(dat$location),]
dat$analyte.ln <- log(dat$value)
dat$location<- factor(dat$location, levels = c("South", "East", "North East", "North", "North West"))
# dat.edist <- dat[!is.na(dat$edist.rounded),]
# dat.wdist <- dat[!is.na(dat$wdist.rounded),]
dat.long <- pivot_longer(data = dat,
                         cols = edist.rounded:wdist.rounded,
                         values_to = "dist.rounded",
                         names_to = "tailings")
dat.long[dat.long$tailings=="edist.rounded",]$tailings <- "Eastern Tailings"
dat.long[dat.long$tailings=="wdist.rounded",]$tailings <- "Western Tailings"
dat.long$tailings <- factor(dat.long$tailings, levels = c("Western Tailings", "Eastern Tailings"))

dat$edist.rounded.km <- dat$edist.rounded/1000
dat$wdist.rounded.km <- dat$wdist.rounded/1000


setwd("/users/godsgiftnkechichukwuonye/Desktop/PhD_Stuff/PH_Figures/Modelling")
Al0 <- lmer(data = dat,
            log(value) ~ (1|site),
            REML = T)

Al1 <- lmer(data = dat,
            log(value) ~ location+analyte + (1|site))
anova(Al1)
summary(Al1)
Al2 <- lmer(data = dat,
            log(value) ~ location+ analyte+ edist.rounded.km + wdist.rounded.km + season + period
            + location:season:analyte + season:period+ pH+ EC
            +(1|site),
            REML = T)
anova(Al2)
vif(Al2)
Al3 <- lmer(data = dat,
            log(value) ~ season + period
            + location:season:period + season:period
            +(1|site),
            REML = F)
anova(Al2)
