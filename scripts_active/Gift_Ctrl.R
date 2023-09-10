#Kunal Palawat | kpalawat@email.arizona.edu
#Date Created: April 13th, 2021
#Description: Code to analyze arsenic and lead overall with control data
#Notes
  #mixed model cf to assess contributions
  #look at ej screen to assess point source for tucson

#load libraries----
library(car)
library(tidyverse)
library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(lmerTest)
library(performance)
library(effects)
library(ggeffects)
library(ggpubr)
library(emmeans)
library(multcomp)
library(patchwork)

#set working directory----
#setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles/data/data_processing")
setwd("~/Documents/GitHub/ProjectHarvest/WorkingFiles//data/data_processing")
#load data ----
ic.dm <- read_xlsx("IC_DMTM_Y23.xlsx", sheet = "Corrected - DM", col_names = TRUE)
ic.dm$community <- "AZ-Background" #changes community to AZ background

#add period and season variables
ic.dm$period <- ic.dm$samplings
ic.dm$season <- ic.dm$samplings

#redefine them
ic.dm[ic.dm$period=="First Winter",]$period <- "First"
ic.dm[ic.dm$period=="Last Winter",]$period <- "Last"
ic.dm[ic.dm$period=="First Monsoon",]$period <- "First"
ic.dm[ic.dm$period=="Last Monsoon",]$period <- "Last"

ic.dm[ic.dm$season=="First Winter",]$season <- "Winter"
ic.dm[ic.dm$season=="Last Winter",]$season <- "Winter"
ic.dm[ic.dm$season=="First Monsoon",]$season <- "Monsoon"
ic.dm[ic.dm$season=="Last Monsoon",]$season <- "Monsoon"

#combine water dm files together
dat <- bind_rows(iw.dm, ic.dm) #binds iw to ic

#define factors
dat$community <- factor(dat$community, levels = c("AZ-Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))
#dat$community <- factor(dat$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson", "Chiricahua", "Grand Canyon", "Oliver Knoll", "Organ Pipe", "Petrified Forest"))
dat$samplings <- factor(dat$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
dat$period <- factor(dat$period, levels = c("First", "Last"))
dat$season <- factor(dat$season, levels = c("Winter", "Monsoon"))

dat.long <- pivot_longer(data = dat,
                         cols = Be:Pb,
                         names_to = "analyte",
                         values_to = "value")
setwd("/users/godsgiftnkechichukwuonye/Desktop/PhD Stuff/PH_Figures")
setwd("~/Documents/GitHub/project-harvest-GC/Figures")

#summary stats community ----

pli.summary <- comdat %>%
  group_by(season, community) %>%
  summarise(mean = mean(na.omit(pli)))
view(pli.summary)            

plot(log(comdat$pli) ~ comdat$proximity.km)

#graph to viz associations by source
ggplot(data = comdat, mapping = aes(y = pli, x = proximity.km, color = community)) + 
  #geom_point(size = 1)+
  facet_grid(season~., scales = "free")+
  geom_smooth()

#boxplot viz
ggplot(data = comdat, mapping = aes(y = log(pli), x = community, fill = season)) + 
  geom_boxplot()

#pli modeling ----
pli0 <- lmer(data = iw.dm,
            pli ~ (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(pli0)

#including relevant variables - period not included based on MFA and previous modeling. proximity.km:season interaction not included because proximity to pollutant does not change by season
pli1 <- lmer(data = iw.dm,
            pli ~
            + community + season + proximity.km
            + community:season + proximity.km:community
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(pli1)
plot(pli1) #not heteroscedastic when untransformed
model.effects <- allEffects(pli1)
plot(model.effects)
vif(pli1)
anova(pli1)

#try backwards stepwise method to get best model
step(pli1, scope=list(lower=pli0), direction="both")
#all variables are signif based on stepwise

pli2 <- lmer(data = iw.dm,
             pli ~
               + community + season + proximity.km
             + proximity.km:community
             + (1|community:site),
             REML = F) #ML for comparison, REML for final

anova(pli1, pli2) #pli1 is better

#keep all variables in the model???

#remove tucson from analysis
iw.dm.nt <- iw.dm[iw.dm$community!="Tucson",]
pli0.nt <- lmer(data = iw.dm.nt,
             pli ~ (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(pli0.nt)

pli1.nt <- lmer(data = iw.dm.nt,
             pli ~
               + community + season + proximity.km
             + community:season + proximity.km:community
             + (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(pli1.nt)
plot(pli1.nt) #not heteroscedastic when untransformed
model.effects <- allEffects(pli1.nt)
plot(model.effects)
vif(pli1.nt)
anova(pli1.nt)
#community:season not signif meaning monsoon trend is pmuch the same across communities
#community:proximity not signif, all mining communities are relatively close to point source (esp compared to tucson)
#proximity is not signif...

#try backwards stepwise method to get best model
step(pli1.nt, scope=list(lower=pli0.nt), direction="both")
#season and proximity are signif, meaning no significant pli differences between the three communities
#comm not significant, the legacy of the mine has a signif impact - DH is not sig fig from GM and HW which have active sources

#copy it and summarize, but keep community a part of the model?

pli1.1.nt <- lmer(data = iw.dm.nt,
               pli ~ season + proximity.km
               + (1 | community:site),
               REML = F)
summary(pli1.1.nt)
plot(pli1.1.nt) #not heteroscedastic when untransformed
model.effects <- allEffects(pli1.1.nt)
plot(model.effects)
vif(pli1.1.nt)

#double check community
pli1.2.nt <- lmer(data = iw.dm.nt,
               pli ~ season + proximity.km + community
               + (1 | community:site),
               REML = F)

anova(pli1.1.nt, pli1.2.nt)

#double check community:season
pli1.3.nt <- lmer(data = iw.dm.nt,
                  pli ~ season + proximity.km + community:season
                  + (1 | community:site),
                  REML = F)

anova(pli1.1.nt, pli1.3.nt)

#double community:proximity
pli1.4.nt <- lmer(data = iw.dm.nt,
                  pli ~ season + proximity.km + community:proximity.km
                  + (1 | community:site),
                  REML = F)

anova(pli1.1.nt, pli1.4.nt)

#double check community:season + community
pli1.5.nt <- lmer(data = iw.dm.nt,
                  pli ~ season + proximity.km + community
                  + community:season
                  + (1 | community:site),
                  REML = F)

anova(pli1.1.nt, pli1.5.nt)

#community is not signif when looking at mining communities, even if it looks like it should be in the effect plot summaries

#compare land use - mining and urban
  #because mining communities (DH, GM, HW) were not signif different frm one another based on above pli models
pli0.lu <- lmer(data = iw.dm,
             pli ~ (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(pli0.lu)

#instead of community, we include landuse in the maximal model
pli1.lu <- lmer(data = iw.dm,
             pli ~
               + landuse + season + proximity.km
             + landuse:season + proximity.km:landuse
             + (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(pli1.lu)
plot(pli1.lu) #not heteroscedastic when untransformed
model.effects <- allEffects(pli1.lu)
plot(model.effects)
vif(pli1.lu)
anova(pli1.lu)
#all significant, similar to inital pli models with DH, GM, HW, TU

#try backwards stepwise method to get best model
step(pli1.lu, scope=list(lower=pli0.lu), direction="both")
#all significant, similar to inital pli models with DH, GM, HW, TU

#remove landuse:season and compre
pli2.lu <- lmer(data = iw.dm,
                pli ~
                  + landuse + season + proximity.km
                + proximity.km:landuse
                + (1|community:site),
                REML = F) #ML for comparison, REML for final

anova(pli1.lu, pli2.lu) #pli1 still more signif

#based on these three sets of models,
  #1 there are signif community differences, which were primarily due to land use (mining vs. urban)
  #2 differences between mining communities were not significant based on models, 
performance(pli1.1.nt)


#scratch ----
dat %>% count(community)
comdat.long <- pivot_longer(data = comdat,
                        cols = pH:`Hg_(ng/kg)`,
                        names_to = "analyte",
                        values_to = "value")
is.long$count <- ifelse(is.long$value == "NA", 0, 1)
is.long$value <- is.long$value + 0.0000000000000000000000001
is.long.sum <- is.long %>%
  group_by(analyte) %>%
  summarise(across(everything(),
                   .f = list(n = n(), mean = mean(value), sd = sd(value)),
                   na.rm = TRUE)) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ic=se*qt((1-.05)/2+.5, n-1))

is.long.sum <- is.long %>%
  group_by(analyte) %>%
  summarise(n = sum(na.omit(count)),
            min = min(na.omit(value)),
            max = max(na.omit(value)),
            mean = mean(na.omit(value)),
            sd = sd(na.omit(value)),
            geomean = geoMean(as.numeric(na.omit(value))),
            gsd = geoSD(as.numeric(na.omit(value)))) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ic=se*qt((1-.05)/2+.5, n-1))

is.long.sum$geomean <- as.numeric(is.long.sum$geomean)

is.long.sum.print <- cbind(analyte=is.long.sum$analyte,
                           GeometricMean=formatC(signif(is.long.sum$geomean,digits=3), digits=3,format="fg", flag="#"),
                           Min=formatC(signif(is.long.sum$min,digits=3), digits=3,format="fg", flag="#"),
                           Max=formatC(signif(is.long.sum$max,digits=3), digits=3,format="fg", flag="#")
)



# gmean <- aggregate(dat$Pb,
#                    by = list(dat$community),
#                    FUN = geoMean) #this gives the gmean of pb conc by community
# colnames(gmean) <- c("community", "gmean") #this changes the names of the columns to gmean and community
# gmean$gmean <- formatC(signif(gmean$gmean,digits=4), digits=4,format="fg", flag="#") #this approximates to four significant figures
# 
# gsd <- aggregate(dat$Pb,
#                  by = list(dat$community),
#                  FUN = geoSD) #geometric standard deviation
# colnames(gsd) <- c("community", "gsd")
# gsd$gsd <- formatC(signif(gsd$gsd,digits=2), digits=2,format="fg", flag="#")
# 
# median <- aggregate(dat$Pb,
#                     by = list(dat$community),
#                     FUN = median)
# colnames(median) <- c("community", "median")
# 
# min <- aggregate(dat$Pb,
#                  by = list(dat$community),
#                  FUN = min)
# colnames(min) <- c("community", "min")
# 
# max <- aggregate(dat$Pb,
#                  by = list(dat$community),
#                  FUN = max)
# colnames(max) <- c("community", "max")
# 
# a <- full_join(gmean, gsd, by = c("community"))
# b <- full_join(a, median, by = c("community"))
# c <- full_join(b, min, by = c("community"))
# d <- full_join(c, max, by = c("community"))
# #joining by community
# 
# write.csv(d, "PbStats.csv")
# #produces csv file in working directory
# 
# count <- dat %>% count(samplings, community)
# 
# gmean <- aggregate(dat$Pb,
#                    by = list(dat$community, dat$samplings),
#                    FUN = geoMean)
# colnames(gmean) <- c("community", "samplings", "gmean")
# gmean$gmean <- formatC(signif(gmean$gmean,digits=4), digits=4,format="fg", flag="#")
# 
# gsd <- aggregate(dat$Pb,
#                  by = list(dat$community, dat$samplings),
#                  FUN = geoSD)
# colnames(gsd) <- c("community", "samplings", "gsd")
# gsd$gsd <- formatC(signif(gsd$gsd,digits=2), digits=2,format="fg", flag="#")
# 
# median <- aggregate(dat$Pb,
#                     by = list(dat$community, dat$samplings),
#                     FUN = median)
# colnames(median) <- c("community", "samplings", "median")
# 
# min <- aggregate(dat$Pb,
#                  by = list(dat$community, dat$samplings),
#                  FUN = min)
# colnames(min) <- c("community", "samplings", "min")
# 
# max <- aggregate(dat$Pb,
#                  by = list(dat$community, dat$samplings),
#                  FUN = max)
# colnames(max) <- c("community", "samplings", "max")
# 
# a <- full_join(gmean, gsd, by = c("community", "samplings"))
# b <- full_join(a, median, by = c("community", "samplings"))
# c <- full_join(b, min, by = c("community", "samplings"))
# d <- full_join(c, max, by = c("community", "samplings"))
# e <- full_join(d, count, by = c("community", "samplings"))
# write.csv(e, "PbStatsSamp.csv")
# 
# 
# 
# #graphing
# dat$community_short <- as.character(dat$community)
# dat[dat$community_short=="Dewey-Humboldt",]$community_short <- "DH"
# dat[dat$community_short=="Globe/Miami",]$community_short <- "GM"
# dat[dat$community_short=="Hayden/Winkelman",]$community_short <- "HW"
# dat[dat$community_short=="Tucson",]$community_short <- "TU"
# dat[dat$community_short=="AZ-Background",]$community_short <- "AZ"
# 
# 
# dat$community_short <- factor(dat$community_short, levels = c("DH", "GM", "HW", "TU", "AZ"))
# summary(dat$community_short)
# 
# ggplot(data = dat,
#        mapping = aes(x=community_short,
#                      y=Pb))+
#   geom_boxplot() +
#   stat_boxplot(geom = 'errorbar') +
#   facet_wrap(.~samplings)+
#   labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
#        x = paste("\nCommunity"))+
#   theme_bw() +
#   theme(text = element_text(size=7, family = "Arial"),
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.line.x = element_blank())
# dev.print(jpeg, "Fig3.jpeg", res=500, height=140, width=140, units="mm")
# 
# ggplot(data = dat,
#        mapping = aes(x=community_short,
#                      y=Pb))+
#   geom_boxplot() +
#   stat_boxplot(geom = 'errorbar') +
#   facet_wrap(.~samplings)+
#   labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
#        x = paste("\nCommunity"))+
#   coord_cartesian(ylim = c(0, 40)) +
#   theme_bw() +
#   theme(text = element_text(size=7, family = "Arial"),
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.line.x = element_blank())
# dev.print(jpeg, "Fig4.jpeg", res=500, height=140, width=140, units="mm")

#Al--------
#summary stats community ----
dat %>% count(community)

Algmean <- aggregate(dat$Al,
                   by = list(dat$community),
                   FUN = geoMean) #this gives the gmean of pb conc by community
colnames(Algmean) <- c("community", "gmean") #this changes the names of the columns to gmean and community
Algmean$gmean <- formatC(signif(gmean$gmean,digits=4), digits=4,format="fg", flag="#") #this approximates to four significant figures

Algsd <- aggregate(dat$Al,
                 by = list(dat$community),
                 FUN = geoSD) #geometric standard deviation
colnames(Algsd) <- c("community", "gsd")
Algsd$gsd <- formatC(signif(gsd$gsd,digits=2), digits=2,format="fg", flag="#")

Almedian <- aggregate(dat$Al,
                    by = list(dat$community),
                    FUN = median)
colnames(Almedian) <- c("community", "median")

Almin <- aggregate(dat$Al,
                 by = list(dat$community),
                 FUN = min)
colnames(Almin) <- c("community", "min")

Almax <- aggregate(dat$Al,
                 by = list(dat$community),
                 FUN = max)
colnames(Almax) <- c("community", "max")

e <- full_join(Algmean, Algsd, by = c("community"))
f <- full_join(e, Almedian, by = c("community"))
g <- full_join(f, Almin, by = c("community"))
h <- full_join(g, Almax, by = c("community"))
#joining by community

write.csv(h, "AlStats.csv")
#produces csv file in working directory

count <- dat %>% count(samplings, community)

Algmean <- aggregate(dat$Al,
                   by = list(dat$community, dat$samplings),
                   FUN = geoMean)
colnames(Algmean) <- c("community", "samplings", "gmean")
Algmean$gmean <- formatC(signif(gmean$gmean,digits=4), digits=4,format="fg", flag="#")

Algsd <- aggregate(dat$Al,
                 by = list(dat$community, dat$samplings),
                 FUN = geoSD)
colnames(Algsd) <- c("community", "samplings", "gsd")
Algsd$gsd <- formatC(signif(gsd$gsd,digits=2), digits=2,format="fg", flag="#")

Almedian <- aggregate(dat$Al,
                    by = list(dat$community, dat$samplings),
                    FUN = median)
colnames(Almedian) <- c("community", "samplings", "median")

Almin <- aggregate(dat$Al,
                 by = list(dat$community, dat$samplings),
                 FUN = min)
colnames(Almin) <- c("community", "samplings", "min")

Almax <- aggregate(dat$Al,
                 by = list(dat$community, dat$samplings),
                 FUN = max)
colnames(Almax) <- c("community", "samplings", "max")


i <- full_join(Algmean, Algsd, by = c("community", "samplings"))
j <- full_join(i, median, by = c("community", "samplings"))
k <- full_join(j, min, by = c("community", "samplings"))
l <- full_join(k, max, by = c("community", "samplings"))
m <- full_join(l, count, by = c("community", "samplings"))
write.csv(m, "AlStatsSamp.csv")



#graphing
dat$community_short <- as.character(dat$community)
dat[dat$community_short=="Dewey-Humboldt",]$community_short <- "DH"
dat[dat$community_short=="Globe/Miami",]$community_short <- "GM"
dat[dat$community_short=="Hayden/Winkelman",]$community_short <- "HW"
dat[dat$community_short=="Tucson",]$community_short <- "TU"
dat[dat$community_short=="AZ-Background",]$community_short <- "AZ"


dat$community_short <- factor(dat$community_short, levels = c("DH", "GM", "HW", "TU", "AZ"))
summary(dat$community_short)

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Al))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings, scales = "free_y")+
  labs(y = expression(paste("Al Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "Fig3.jpeg", res=500, height=140, width=140, units="mm")

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Al))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Al Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  coord_cartesian(ylim = c(0, 1000)) +
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "AlSummary.jpeg", res=500, height=140, width=140, units="mm")



#Mn--------
#summary stats community ----
dat %>% count(community)

Mngmean <- aggregate(dat$Mn,
                     by = list(dat$community),
                     FUN = geoMean) #this gives the gmean of pb conc by community
colnames(Mngmean) <- c("community", "gmean") #this changes the names of the columns to gmean and community
Mngmean$gmean <- formatC(signif(Mngmean$gmean,digits=4), digits=4,format="fg", flag="#") #this approximates to four significant figures

Mngsd <- aggregate(dat$Mn,
                   by = list(dat$community),
                   FUN = geoSD) #geometric standard deviation
colnames(Mngsd) <- c("community", "gsd")
Mngsd$gsd <- formatC(signif(Mngsd$gsd,digits=2), digits=2,format="fg", flag="#")

Mnmedian <- aggregate(dat$Mn,
                      by = list(dat$community),
                      FUN = median)
colnames(Mnmedian) <- c("community", "median")

Mnmin <- aggregate(dat$Mn,
                   by = list(dat$community),
                   FUN = min)
colnames(Mnmin) <- c("community", "min")

Mnmax <- aggregate(dat$Mn,
                   by = list(dat$community),
                   FUN = max)
colnames(Mnmax) <- c("community", "max")

n <- full_join(Mngmean, Mngsd, by = c("community"))
o <- full_join(n, Mnmedian, by = c("community"))
p <- full_join(o, Mnmin, by = c("community"))
q <- full_join(p, Mnmax, by = c("community"))
#joining by community

write.csv(q, "AlStats.csv")
#produces csv file in working directory

count <- dat %>% count(samplings, community)

Mngmean <- aggregate(dat$Mn,
                     by = list(dat$community, dat$samplings),
                     FUN = geoMean)
colnames(Mngmean) <- c("community", "samplings", "gmean")
Mngmean$gmean <- formatC(signif(Mngmean$gmean,digits=4), digits=4,format="fg", flag="#")

Mngsd <- aggregate(dat$Mn,
                   by = list(dat$community, dat$samplings),
                   FUN = geoSD)
colnames(Mngsd) <- c("community", "samplings", "gsd")
Mngsd$gsd <- formatC(signif(Mngsd$gsd,digits=2), digits=2,format="fg", flag="#")

Mnmedian <- aggregate(dat$Mn,
                      by = list(dat$community, dat$samplings),
                      FUN = median)
colnames(Mnmedian) <- c("community", "samplings", "median")

Mnmin <- aggregate(dat$Mn,
                   by = list(dat$community, dat$samplings),
                   FUN = min)
colnames(Mnmin) <- c("community", "samplings", "min")

Mnmax <- aggregate(dat$Mn,
                   by = list(dat$community, dat$samplings),
                   FUN = max)
colnames(Mnmax) <- c("community", "samplings", "max")


n <- full_join(Mngmean, Mngsd, by = c("community", "samplings"))
o <- full_join(n, median, by = c("community", "samplings"))
p <- full_join(o, min, by = c("community", "samplings"))
q <- full_join(p, max, by = c("community", "samplings"))
r <- full_join(q, count, by = c("community", "samplings"))
write.csv(r, "MnStatsSamp.csv")



#graphing
dat$community_short <- as.character(dat$community)
dat[dat$community_short=="Dewey-Humboldt",]$community_short <- "DH"
dat[dat$community_short=="Globe/Miami",]$community_short <- "GM"
dat[dat$community_short=="Hayden/Winkelman",]$community_short <- "HW"
dat[dat$community_short=="Tucson",]$community_short <- "TU"
dat[dat$community_short=="AZ-Background",]$community_short <- "AZ"


dat$community_short <- factor(dat$community_short, levels = c("DH", "GM", "HW", "TU", "AZ"))
summary(dat$community_short)

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Mn))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Mn Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "MnSummary.jpeg", res=500, height=140, width=140, units="mm")

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Al))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Mn Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  coord_cartesian(ylim = c(0, 500)) +
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "MnSummary.jpeg", res=500, height=140, width=140, units="mm")

#Ni--------
dat %>% count(community)

Nigmean <- aggregate(dat$Ni,
                     by = list(dat$community),
                     FUN = geoMean) #this gives the gmean of pb conc by community
colnames(Nigmean) <- c("community", "gmean") #this changes the names of the columns to gmean and community
Nigmean$gmean <- formatC(signif(Nigmean$gmean,digits=4), digits=4,format="fg", flag="#") #this approximates to four significant figures

Nigsd <- aggregate(dat$Ni,
                   by = list(dat$community),
                   FUN = geoSD) #geometric standard deviation
colnames(Nigsd) <- c("community", "gsd")
Nigsd$gsd <- formatC(signif(Nigsd$gsd,digits=2), digits=2,format="fg", flag="#")

Nimedian <- aggregate(dat$Ni,
                      by = list(dat$community),
                      FUN = median)
colnames(Nimedian) <- c("community", "median")

Nimin <- aggregate(dat$Ni,
                   by = list(dat$community),
                   FUN = min)
colnames(Nimin) <- c("community", "min")

Nimax <- aggregate(dat$Ni,
                   by = list(dat$community),
                   FUN = max)
colnames(Nimax) <- c("community", "max")

r <- full_join(Nigmean, Nigsd, by = c("community"))
s <- full_join(r, Nimedian, by = c("community"))
t <- full_join(s, Nimin, by = c("community"))
u <- full_join(t, Nimax, by = c("community"))
#joining by community

write.csv(u, "AlStats.csv")
#produces csv file in working directory

count <- dat %>% count(samplings, community)

Nigmean <- aggregate(dat$Ni,
                     by = list(dat$community, dat$samplings),
                     FUN = geoMean)
colnames(Nigmean) <- c("community", "samplings", "gmean")
Nigmean$gmean <- formatC(signif(Nigmean$gmean,digits=4), digits=4,format="fg", flag="#")

Nigsd <- aggregate(dat$Ni,
                   by = list(dat$community, dat$samplings),
                   FUN = geoSD)
colnames(Nigsd) <- c("community", "samplings", "gsd")
Nigsd$gsd <- formatC(signif(Nigsd$gsd,digits=2), digits=2,format="fg", flag="#")

Nimedian <- aggregate(dat$Ni,
                      by = list(dat$community, dat$samplings),
                      FUN = median)
colnames(Nimedian) <- c("community", "samplings", "median")

Nimin <- aggregate(dat$Ni,
                   by = list(dat$community, dat$samplings),
                   FUN = min)
colnames(Nimin) <- c("community", "samplings", "min")

Nimax <- aggregate(dat$Ni,
                   by = list(dat$community, dat$samplings),
                   FUN = max)
colnames(Nimax) <- c("community", "samplings", "max")


r <- full_join(Nigmean, Nigsd, by = c("community", "samplings"))
s <- full_join(r, median, by = c("community", "samplings"))
t <- full_join(s, min, by = c("community", "samplings"))
u <- full_join(t, max, by = c("community", "samplings"))
u <- full_join(u, count, by = c("community", "samplings"))
write.csv(u, "NiStatsSamp.csv")



#graphing
dat$community_short <- as.character(dat$community)
dat[dat$community_short=="Dewey-Humboldt",]$community_short <- "DH"
dat[dat$community_short=="Globe/Miami",]$community_short <- "GM"
dat[dat$community_short=="Hayden/Winkelman",]$community_short <- "HW"
dat[dat$community_short=="Tucson",]$community_short <- "TU"
dat[dat$community_short=="AZ-Background",]$community_short <- "AZ"


dat$community_short <- factor(dat$community_short, levels = c("DH", "GM", "HW", "TU", "AZ"))
summary(dat$community_short)

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Ni))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Ni Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "NiSummary.jpeg", res=500, height=140, width=140, units="mm")

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Al))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Ni Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  coord_cartesian(ylim = c(0, 500)) +
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "NiSummary.jpeg", res=500, height=140, width=140, units="mm")


#Cd--------
#summary stats community ----
dat %>% count(community)

Cdgmean <- aggregate(dat$Cd,
                     by = list(dat$community),
                     FUN = geoMean) #this gives the gmean of pb conc by community
colnames(Cdgmean) <- c("community", "gmean") #this changes the names of the coluCds to gmean and community
Cdgmean$gmean <- formatC(signif(Cdgmean$gmean,digits=4), digits=4,format="fg", flag="#") #this approximates to four significant figures

Cdgsd <- aggregate(dat$Cd,
                   by = list(dat$community),
                   FUN = geoSD) #geometric standard deviation
colnames(Cdgsd) <- c("community", "gsd")
Cdgsd$gsd <- formatC(signif(Cdgsd$gsd,digits=2), digits=2,format="fg", flag="#")

Cdmedian <- aggregate(dat$Cd,
                      by = list(dat$community),
                      FUN = median)
colnames(Cdmedian) <- c("community", "median")

Cdmin <- aggregate(dat$Cd,
                   by = list(dat$community),
                   FUN = min)
colnames(Cdmin) <- c("community", "min")

Cdmax <- aggregate(dat$Cd,
                   by = list(dat$community),
                   FUN = max)
colnames(Cdmax) <- c("community", "max")

Cd1 <- full_join(Cdgmean, Cdgsd, by = c("community"))
Cd2 <- full_join(Cd1, Cdmedian, by = c("community"))
Cd3 <- full_join(Cd2, Cdmin, by = c("community"))
Cd4 <- full_join(Cd3, Cdmax, by = c("community"))
#joining by community

write.csv(q, "AlStats.csv")
#produces csv file in working directory

count <- dat %>% count(samplings, community)

Cdgmean <- aggregate(dat$Cd,
                     by = list(dat$community, dat$samplings),
                     FUN = geoMean)
colnames(Cdgmean) <- c("community", "samplings", "gmean")
Cdgmean$gmean <- formatC(signif(Cdgmean$gmean,digits=4), digits=4,format="fg", flag="#")

Cdgsd <- aggregate(dat$Cd,
                   by = list(dat$community, dat$samplings),
                   FUN = geoSD)
colnames(Cdgsd) <- c("community", "samplings", "gsd")
Cdgsd$gsd <- formatC(signif(Cdgsd$gsd,digits=2), digits=2,format="fg", flag="#")

Cdmedian <- aggregate(dat$Cd,
                      by = list(dat$community, dat$samplings),
                      FUN = median)
colnames(Cdmedian) <- c("community", "samplings", "median")

Cdmin <- aggregate(dat$Cd,
                   by = list(dat$community, dat$samplings),
                   FUN = min)
colnames(Cdmin) <- c("community", "samplings", "min")

Cdmax <- aggregate(dat$Cd,
                   by = list(dat$community, dat$samplings),
                   FUN = max)
colnames(Cdmax) <- c("community", "samplings", "max")


Cd1 <- full_join(Cdgmean, Cdgsd, by = c("community", "samplings"))
Cd2 <- full_join(Cd1, Cdmedian, by = c("community", "samplings"))
Cd3 <- full_join(Cd2, Cdmin, by = c("community", "samplings"))
Cd4 <- full_join(Cd3, Cdmax, by = c("community", "samplings"))
Cd5 <- full_join(Cd4, Cdcount, by = c("community", "samplings"))
write.csv(r, "CdStatsSamp.csv")



#graphing
dat$community_short <- as.character(dat$community)
dat[dat$community_short=="Dewey-Humboldt",]$community_short <- "DH"
dat[dat$community_short=="Globe/Miami",]$community_short <- "GM"
dat[dat$community_short=="Hayden/Winkelman",]$community_short <- "HW"
dat[dat$community_short=="Tucson",]$community_short <- "TU"
dat[dat$community_short=="AZ-Background",]$community_short <- "AZ"


dat$community_short <- factor(dat$community_short, levels = c("DH", "GM", "HW", "TU", "AZ"))
summary(dat$community_short)

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Cd))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Cd Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "CdSummary.jpeg", res=500, height=140, width=140, units="mm")

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Cd))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Cd Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  coord_cartesian(ylim = c(0, 15)) +
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "CdSummary.jpeg", res=500, height=140, width=140, units="mm")




newfull<- full_join(Cd5, u, e, m, r, by = c("community", "samplings"))

#detection
ic.dm.detects <- read_xlsx("IC_DMTM_Y23.xlsx", sheet = "Detection - DM", col_names = TRUE)
ic.dm.detects$community <- "AZ-Background"

ic.dm.mlods <- read_xlsx("IPSW_MLODS.xlsx", sheet = "nadp - corrected - 2.14.21", col_names = TRUE)
ic.dm.mlods$community <- "AZ Background"
#combine water dm files together
dat.mlods <- bind_rows(iw.mlod.dm, ic.dm.mlods)
#define factors
dat.mlods$community <- factor(dat.mlods$community, levels = c("AZ Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))

dat.mlods.long <- pivot_longer(data = dat.mlods,
                               cols = Be:Pb,
                               names_to = "analyte",
                               values_to = "value")
write.csv(dat.mlods.long, "mlods.appendixA_v6.csv")


dat.detects <- bind_rows(iw.dm.detects, ic.dm.detects)

#define factors
dat.detects$community <- factor(dat.detects$community, levels = c("AZ Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))
dat$community <- factor(dat$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson", "Chiricahua", "Grand Canyon", "Oliver Knoll", "Organ Pipe", "Petrified Forest"))
dat.detects$samplings <- factor(dat.detects$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))

dat.detects.long <- pivot_longer(data = dat.detects,
                                 cols = Be:Pb,
                                 names_to = "analyte",
                                 values_to = "detection")





#Al Ctrl Modeling ----
#basic viz
boxplot(dat$Al ~ dat$community, log = "y")
boxplot(dat$Al ~ dat$samplings, log = "y")


Al0 <- lmer(data = dat,
            log(Al) ~ (1|community:site),
            REML = T)
Al1 <- lmer(data = dat,
            log(Al) ~ community+period+season+community:period+community:season+period:season+community:period:season
            + (1|community:site),
            REML = T)
summary(Al1)
vif(Al1)

#try backwards stepwise method to get best model
step(Al1, scope=list(lower=Al0), direction="backward")

#copy it and summarize
Al1b <- lmer(data = dat,
             log(Al) ~ community + period + season + (1 | community:site) + community:season + period:season,
             REML = T)
summary(Al1b)

Al2 <- lmer(data = dat,
            log(As) ~ community+period+season+community:period+period:season+community:period:season
            + (1|community:site),
            REML = F)
vif(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ community+period+season+community:period+period:season
            + (1|community:site),
            REML = F)
vif(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ community+period+season+period:season
            + (1|community:site),
            REML = T)
vif(Al4)
anova(Al4)
summary(Al4)

#why was period:season taken out and not just period?
Al5 <- lmer(data = dat,
            log(Al) ~ community+season
            + (1|community:site),
            REML = T)
anova(Al5)
summary(Al5)

Al5b <- lmer(data = dat,
            log(Al) ~ community+season+period:season
            + (1|community:site),
            REML = T)
anova(Al5b)
summary(Al5b)

anova(Al1,Al5)
anova(Al4, Al5)
anova(Al4, Al5b)
anova(Al5, Al5b)

anova(Al6, Al5)
anova(Al4, Al6)
anova(Al7, Al5)

check_model(Al5)
dev.print(png, "Al_checkcontrol5.png", res=100, height=12, width=30, units="in")


model.effects <- allEffects(Al5)
plot(model.effects)
model.effects <- ggeffect(model = Al5,
                          back.transform = F,
                          type = "re")
comm.effect <- model.effects$community
comm.effect$community <- factor(comm.effect$x, levels = c("AZ-Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))


comm.plot <- ggplot(data= dat,
                    mapping = aes(x=community,y=log(Al))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~community, scales = "free")+
  geom_pointrange(data = comm.effect, aes(x=community, y = predicted, color = community, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCommunity",
       y = expression(paste("ln[Al] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        legend.position = "none")
comm.plot

# ggplot(data= dat,
#        mapping = aes(x=community,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = comm.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nCommunity",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
#   #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlcommEffectln.png", res=100, height=5, width=7, units="in")

lsmeans(Al5, pairwise~community, adjust = "tukey")# 
al.sum <- summary(Al5)# 
write.csv(al.sum$coefficients, "al5coef.csv")
al.commmeans <- lsmeans(Al5, pairwise~community, adjust = "tukey")# 
write.csv(al.commmeans$lsmeans, "al5lsmeanscomm.csv")
write.csv(al.commmeans$contrasts, "al5contrastscomm.csv")

period.effect <- model.effects$period
period.effect$period <- factor(period.effect$x, levels = c("First", "Last"))

per.plot <- ggplot(data= dat,
                   mapping = aes(x=period,y=log(Al))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~period, scales = "free")+
  geom_pointrange(data = period.effect, aes(x=period, y = predicted, color = period, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSampling Period",
       y = expression(paste("ln[Al] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#C5D7D2","#F5D1C4"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        #axis.title.y = element_blank(),
        legend.position = "none")
per.plot

# ggplot(data= dat,
#        mapping = aes(x=period,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = period.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nSampling Period",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#C5D7D2","#F5D1C4"))+
#   coord_cartesian(ylim = c(-0.2,.2), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlperiodEffectln.png", res=100, height=4, width=4, units="in")

lsmeans(Al5, pairwise~period, adjust = "tukey")# first greater than last
al.periodmeans <- lsmeans(Al5, pairwise~period, adjust = "tukey")
write.csv(al.periodmeans$lsmeans, "al5lsmeansper.csv")
write.csv(al.periodmeans$contrasts, "al5contrastsper.csv")

season.effect <- model.effects$season
season.effect$season <- factor(season.effect$x, levels = c("Winter", "Monsoon"))

ssn.plot <- ggplot(data= dat,
                   mapping = aes(x=season,y=log(Al))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~season, scales = "free")+
  geom_pointrange(data = season.effect, aes(x=season, y = predicted, color = season, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSampling Season",
       y = expression(paste("ln[Al] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#A2A9D6","#F2C893"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        #axis.title.y = element_blank(),
        legend.position = "none")
ssn.plot


comm.plot +  ssn.plot + plot_layout(ncol=3,widths=c(3,1,1))
dev.print(png, "Al_commperssnEffectln.png", res=300, height=5, width=7, units="in")

comm.plot
dev.print(png, "Al_commEffectln.png", res=300, height=5, width=5, units="in")

per.plot
dev.print(png, "Al_perEffectln.png", res=300, height=5, width=3, units="in")

ssn.plot
dev.print(png, "Al_ssnEffectln.png", res=300, height=5, width=3, units="in")


# ggplot(data= dat,
#        mapping = aes(x=season,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = season.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nSampling Season",
#        y = "ln(As) [ug/L]\n")+  
#   scale_color_manual(values=c("#A2A9D6","#F2C893"))+
#   #coord_cartesian(ylim = c(-0.3,.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")

# title="Modeled Effect of Community",
# subtitle = "This figure displays modeled, not real data",

dev.print(png, "Al_ctrlseasonEffectln.png", res=100, height=4, width=4, units="in")

lsmeans(Al5, pairwise~season, adjust = "tukey")# first greater than last
al.seasonmeans <- lsmeans(Al5, pairwise~season, adjust = "tukey")
write.csv(al.seasonmeans$lsmeans, "al5lsmeansseason.csv")
write.csv(al.seasonmeans$contrasts, "al5contrastsseason.csv")


#Cd Ctrl Modeling ----
#basic viz
boxplot(dat$Cd ~ dat$community, log = "y")
boxplot(dat$Cd ~ dat$season, log = "y")


Cd0 <- lmer(data = dat,
            log(Cd) ~ (1|community:site),
            REML = T)
Cd1 <- lmer(data = dat,
            log(Cd) ~ community+period+season+community:period+community:season+period:season+community:period:season
            + (1|community:site),
            REML = T)
step(Cd1, scope=list(lower=Cd0), direction="backward")
Cd1b <- lmer(data = dat,
        log(Cd) ~ community + season + (1 | community:site) +  period:season,
        REML = T)
anova(Cd1b)
summary(Cd1b)
vif(Cd1b)
Cd2 <- lmer(data = dat,
            log(Cd) ~ community+period+season+community:period+period:season+community:season
            + (1|community:site),
            REML = F)

Cd3 <- lmer(data = dat,
            log(Cd) ~ community+period+season+community:season+period:season
            + (1|community:site),
            REML = F)

Cd4 <- lmer(data = dat,
            log(Cd) ~ community+period+season+period:season
            + (1|community:site),
            REML = T)
anova(Cd4)
summary(Cd4)
Cd5 <- lmer(data = dat,
            log(Cd) ~ community+period+season
            + (1|community:site),
            REML = F)
anova(Cd5)
summary(Cd5)
Cd6 <- lmer(data = dat,
            log(Cd) ~ community+season
            + (1|community:site),
            REML = T)
anova(Cd6)
summary(Cd6)

Cd7 <- lmer(data = dat,
            log(Cd) ~ period+season
            + (1|community:site),
            REML = F)
anova(Cd7)
summary(Cd7)

Cd8 <- lmer(data = dat,
            log(Cd) ~ community+period:season
            + (1|community:site),
            REML = F)
anova(Cd8)
summary(Cd8)

Cd9 <- lmer(data = dat,
            log(Cd) ~ community+period
            + (1|community:site),
            REML = F)
anova(Cd9)
summary(Cd9)

Cd10 <- lmer(data = dat,
             log(Cd) ~ community+samplings
             + (1|community:site),
             REML = F)
anova(Cd10)
summary(Cd10)

Cd11 <- lmer(data = dat,
             log(Cd) ~ season
             + (1|community:site),
             REML = F)


anova(Cd4, Cd5)
anova(Cd5, Cd6)
anova(Cd6, Cd7)
anova(Cd6, Cd8)
anova(Cd8, Cd9)
anova(Cd8, Cd10)
anova(Cd8, Cd11)
anova(Cd8, Cd1b)
anova(Cd6, Cd1b)

performance(Cd8)
check_model(Cd8)
dev.print(png, "Cd_checkcontrol8.png", res=100, height=12, width=30, units="in")

summary(Cd10)
model.effects <- allEffects(Cd10)
plot(model.effects)


model.effects<- ggeffect(model = Cd10,
                          back.transform = F,
                          type = "re")
comm.effect <- model.effects$community
comm.effect$community <- factor(comm.effect$x, levels = c("AZ-Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))


comm.plot <- ggplot(data= dat,
                    mapping = aes(x=community,y=log(Cd))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~community, scales = "free")+
  geom_pointrange(data = comm.effect, aes(x=community, y = predicted, color = community, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCommunity",
       y = expression(paste("ln[Cd] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        legend.position = "none")
comm.plot


# comm.effect$x <- factor(comm.effect$x, levels = c("AZ Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))
# 
# ggplot(data= dat,
#        mapping = aes(x=community,y=log(Pb))) +
#   #geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = comm.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nCommunity",
#        y = "ln(Pb) [µg/L]\n")+
#   scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
#   coord_cartesian(ylim = c(-2.1,.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")

# title="Modeled Effect of Community",
# subtitle = "This figure displays modeled, not real data",

dev.print(png, "Cd_ctrlcommEffectln_no points.png", res=100, height=5, width=7, units="in")

lsmeans(Cd10, pairwise~community, adjust = "tukey")# 
cd.sum <- summary(Cd10)# 
write.csv(cd.sum$coefficients, "cd10coef.csv")
cd.commmeans <- lsmeans(Cd6, pairwise~community, adjust = "tukey")#
write.csv(cd.commmeans$lsmeans, "cd10lsmeanscomm.csv")
write.csv(cd.commmeans$contrasts, "cd10contrastscomm.csv")

samplings.effect <- model.effects$samplings
samplings.effect$samplings <- factor(samplings.effect$x, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))

ssn.plot <- ggplot(data= dat,
                   mapping = aes(x=samplings,y=log(Cd))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~samplings, scales = "free")+
  geom_pointrange(data = samplings.effect, aes(x=samplings, y = predicted, color = samplings, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSamplings",
       y = expression(paste("ln[Cd] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#A2A9D6","#F2C893", "#EE4B2B", "#A020F0"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        axis.title.y = element_blank(),
        legend.position = "none")
ssn.plot


lsmeans(Cd10, pairwise~samplings, adjust = "tukey")# first greater than last
cd.seasonmeans <- lsmeans(Cd10, pairwise~samplings, adjust = "tukey")
write.csv(cd.seasonmeans$lsmeans, "cd10lsmeansseason.csv")
write.csv(cd.seasonmeans$contrasts, "cd10contrastsseason.csv")

comm.plot + ssn.plot + plot_layout(ncol=2,widths=c(2,1))
dev.print(png, "Cd_commssnEffectln.png", res=300, height=5, width=9, units="in")

comm.plot
dev.print(png, "Cd_commEffectln.png", res=300, height=5, width=5, units="in")

ssn.plot
dev.print(png, "Cd_ssnEffectln.png", res=300, height=5, width=3, units="in")




#Mn Modeling ----
#basic viz
boxplot(iw.dm$Mn ~ iw.dm$community, log = "y")
boxplot(iw.dm$Mn ~ iw.dm$samplings, log = "y")


Mn0 <- lmer(data = iw.dm,
            log(Mn) ~ (1|community:site),
            REML = F)
Mn1 <- lmer(data = iw.dm,
            log(Mn) ~ community+period+season+community:period+community:season+period:season+community:period:season
            + (1|community:site),
            REML = F)

Mn2 <- lmer(data = iw.dm,
            log(Mn) ~ community+period+season+community:period+period:season+community:season
            + (1|community:site),
            REML = F)
vif(Mn2)

Mn3 <- lmer(data = iw.dm,
            log(Mn) ~ community+period+season+period:season+community:season
            + (1|community:site),
            REML = F)
vif(Mn3)
anova(Mn3)
summary(Mn3)

Mn4 <- lmer(data = iw.dm,
            log(Mn) ~ community+period+season+period:season
            + (1|community:site),
            REML = F)
vif(Mn4)
anova(Mn4)
summary(Mn4)
Mn5 <- lmer(data = iw.dm,
            log(Mn) ~ community+period+season
            + (1|community:site),
            REML = F)
anova(Mn5)
summary(Mn5)
Mn6 <- lmer(data = iw.dm,
            log(Mn) ~ community+season
            + (1|community:site),
            REML = F)

Mn7 <- lmer(data = iw.dm,
            log(Mn) ~ community+samplings
            + (1|community:site),
            REML = F)

anova(Mn4, Mn5)
anova(Mn4, Mn6)
anova(Mn5, Mn6)
anova(Mn4, Mn7)

check_model(Mn7)
dev.print(png, "Mn_check7.png", res=100, height=12, width=30, units="in")



model.effects <- allEffects(Mn7)
plot(model.effects)
model.effects <- ggeffect(model = Mn7,
                          back.transform = F,
                          type = "re")
comm.effect <- model.effects$community
comm.effect$community <- factor(comm.effect$x, levels = c("AZ-Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))


comm.plot <- ggplot(data= dat,
                    mapping = aes(x=community,y=log(Mn))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~community, scales = "free")+
  geom_pointrange(data = comm.effect, aes(x=community, y = predicted, color = community, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCommunity",
       y = expression(paste("ln[Mn] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        legend.position = "none")
comm.plot

# ggplot(data= dat,
#        mapping = aes(x=community,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = comm.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nCommunity",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
#   #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlcommEffectln.png", res=100, height=5, width=7, units="in")

lsmeans(Mn7, pairwise~community, adjust = "tukey")# 
mn.sum <- summary(Mn7)# 
write.csv(mn.sum$coefficients, "mn7coef.csv")
mn.commmeans <- lsmeans(Mn7, pairwise~community, adjust = "tukey")# 
write.csv(mn.commmeans$lsmeans, "mn7lsmeanscomm.csv")
write.csv(mn.commmeans$contrasts, "mn7contrastscomm.csv")

samplings.effect <- model.effects$samplings
samplings.effect$samplings <- factor(samplings.effect$x, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))

ssn.plot <- ggplot(data= dat,
                   mapping = aes(x=samplings,y=log(Mn))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~samplings, scales = "free")+
  geom_pointrange(data = samplings.effect, aes(x=samplings, y = predicted, color = samplings, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSamplings",
       y = expression(paste("ln[Mn] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#A2A9D6","#F2C893", "#EE4B2B", "#A020F0"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        axis.title.y = element_blank(),
        legend.position = "none")
ssn.plot


lsmeans(Mn7, pairwise~samplings, adjust = "tukey")# first greater than last
mn.seasonmeans <- lsmeans(Mn7, pairwise~samplings, adjust = "tukey")
write.csv(mn.seasonmeans$lsmeans, "mn7lsmeansseason.csv")
write.csv(mn.seasonmeans$contrasts, "mn7contrastsseason.csv")

comm.plot + ssn.plot + plot_layout(ncol=2,widths=c(2,1))
dev.print(png, "mn_commssnEffectln.png", res=300, height=5, width=9, units="in")

comm.plot
dev.print(png, "mn_commEffectln.png", res=300, height=5, width=5, units="in")

ssn.plot
dev.print(png, "mn_ssnEffectln.png", res=300, height=5, width=3, units="in")

#Cu Modelling=======
boxplot(dat$Cu ~ dat$community, log = "y")
boxplot(dat$Cu ~ dat$samplings, log = "y")


Cu0 <- lmer(data = dat,
            log(Cu) ~ (1|community:site),
            REML = F)
Cu1 <- lmer(data = iw.dm,
            log(Cu) ~ community+period+season+community:period+community:season+period:season+community:period:season
            + (1|community:site),
            REML = F)
vif(Cu1)

Cu2 <- lmer(data = iw.dm,
            log(Cu) ~ community+period+season+community:period+period:season+community:season
            + (1|community:site),
            REML = F)
vif(Cu2)

Cu3 <- lmer(data = iw.dm,
            log(Cu) ~ community+period+season+period:season+community:season
            + (1|community:site),
            REML = F)
vif(Cu3)


Cu4 <- lmer(data = iw.dm,
            log(Cu) ~ community+period+season+period:season
            + (1|community:site),
            REML = F)
vif(Cu4)
anova(Cu4)
summary(Ni4)
Cu5 <- lmer(data = dat,
            log(Cu) ~ community+period+season
            + (1|community:site),
            REML = F)
anova(Cu5)
summary(Cu5)
Cu6 <- lmer(data = iw.dm,
            log(Cu) ~ community+season
            + (1|community:site),
            REML = F)

Cu7 <- lmer(data = iw.dm,
            log(Cu) ~ community+samplings
            + (1|community:site),
            REML = F)

anova(Cu4, Cu5)
anova(Cu5, Cu6)
anova(Cu5, Cu7)


check_model(Cu5)
dev.print(png, "Cu_check5.png", res=100, height=12, width=30, units="in")



model.effects <- allEffects(Cu5)
plot(model.effects)
model.effects <- ggeffect(model = Cu5,
                          back.transform = F,
                          type = "re")
comm.effect <- model.effects$community
comm.effect$community <- factor(comm.effect$x, levels = c("AZ-Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))


comm.plot <- ggplot(data= dat,
                    mapping = aes(x=community,y=log(Cu))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~community, scales = "free")+
  geom_pointrange(data = comm.effect, aes(x=community, y = predicted, color = community, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCommunity",
       y = expression(paste("ln[Cu] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        legend.position = "none")
comm.plot

# ggplot(data= dat,
#        mapping = aes(x=community,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = comm.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nCommunity",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
#   #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlcommEffectln.png", res=100, height=5, width=7, units="in")

lsmeans(Cu5, pairwise~community, adjust = "tukey")# 
cu.sum <- summary(Cu5)# 
write.csv(cu.sum$coefficients, "cu5coef.csv")
cu.commmeans <- lsmeans(Cu5, pairwise~community, adjust = "tukey")# 
write.csv(cu.commmeans$lsmeans, "cu5lsmeanscomm.csv")
write.csv(cu.commmeans$contrasts, "cu5contrastscomm.csv")

period.effect <- model.effects$period
period.effect$period <- factor(period.effect$x, levels = c("First", "Last"))

per.plot <- ggplot(data= dat,
                   mapping = aes(x=period,y=log(Cu))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~period, scales = "free")+
  geom_pointrange(data = period.effect, aes(x=period, y = predicted, color = period, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSampling Period",
       y = expression(paste("ln[Cu] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#C5D7D2","#F5D1C4"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        #axis.title.y = element_blank(),
        legend.position = "none")
per.plot

# ggplot(data= dat,
#        mapping = aes(x=period,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = period.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nSampling Period",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#C5D7D2","#F5D1C4"))+
#   coord_cartesian(ylim = c(-0.2,.2), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlperiodEffectln.png", res=100, height=4, width=4, units="in")

lsmeans(Cu5, pairwise~period, adjust = "tukey")# first greater than last
cu.periodmeans <- lsmeans(Cu5, pairwise~period, adjust = "tukey")
write.csv(cu.periodmeans$lsmeans, "cu5lsmeansper.csv")
write.csv(cu.periodmeans$contrasts, "cu5contrastsper.csv")

season.effect <- model.effects$season
season.effect$season <- factor(season.effect$x, levels = c("Winter", "Monsoon"))

ssn.plot <- ggplot(data= dat,
                   mapping = aes(x=season,y=log(Cu))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~season, scales = "free")+
  geom_pointrange(data = season.effect, aes(x=season, y = predicted, color = season, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSampling Season",
       y = expression(paste("ln[Cu] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#A2A9D6","#F2C893"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        #axis.title.y = element_blank(),
        legend.position = "none")
ssn.plot


comm.plot +  ssn.plot + per.plot+ plot_layout(ncol=3,widths=c(3,1,1))
dev.print(png, "Cu_commperssnEffectln.png", res=300, height=7, width=10, units="in")

comm.plot
dev.print(png, "Ni_commEffectln.png", res=300, height=5, width=5, units="in")

per.plot
dev.print(png, "Ni_perEffectln.png", res=300, height=5, width=3, units="in")

ssn.plot
dev.print(png, "Ni_ssnEffectln.png", res=300, height=5, width=3, units="in")


# ggplot(data= dat,
#        mapping = aes(x=season,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = season.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nSampling Season",
#        y = "ln(As) [ug/L]\n")+  
#   scale_color_manual(values=c("#A2A9D6","#F2C893"))+
#   #coord_cartesian(ylim = c(-0.3,.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")

# title="Modeled Effect of Community",
# subtitle = "This figure displays modeled, not real data",

dev.print(png, "Ni_ctrlseasonEffectln.png", res=100, height=4, width=4, units="in")

lsmeans(Cu5, pairwise~season, adjust = "tukey")# first greater than last
cu.seasonmeans <- lsmeans(Cu5, pairwise~season, adjust = "tukey")
write.csv(cu.seasonmeans$lsmeans, "cu5lsmeansseason.csv")
write.csv(cu.seasonmeans$contrasts, "cu5contrastsseason.csv")



#Zn Modelling========
boxplot(iw.dm$Zn ~ iw.dm$community, log = "y")
boxplot(iw.dm$Zn ~ iw.dm$samplings, log = "y")


Zn0 <- lmer(data = iw.dm,
            log(Zn) ~ (1|community:site),
            REML = F)
Zn1 <- lmer(data = iw.dm,
            log(Zn) ~ community+period+season+community:period+community:season+period:season+community:period:season
            + (1|community:site),
            REML = F)

Zn2 <- lmer(data = iw.dm,
            log(Zn) ~ community+period+season+community:period+period:season+community:season
            + (1|community:site),
            REML = F)
vif(Zn2)

Zn3 <- lmer(data = iw.dm,
            log(Zn) ~ community+period+season+period:season+community:season
            + (1|community:site),
            REML = F)
vif(Zn3)


Zn4 <- lmer(data = iw.dm,
            log(Zn) ~ community+period+season+period:season
            + (1|community:site),
            REML = F)
vif(Zn4)
anova(Zn4)
summary(Zn4)
Zn5 <- lmer(data = iw.dm,
            log(Zn) ~ community+period+season
            + (1|community:site),
            REML = F)
anova(Zn5)
summary(Zn5)
Zn6 <- lmer(data = iw.dm,
            log(Zn) ~ community+season
            + (1|community:site),
            REML = F)

Zn7 <- lmer(data = iw.dm,
            log(Zn) ~ community+samplings
            + (1|community:site),
            REML = F)

anova(Zn4, Zn5)
anova(Zn4, Zn6)
anova(Zn5, Zn6)
anova(Zn4, Zn7)

check_model(Zn5)
dev.print(png, "Zn_check5.png", res=100, height=12, width=30, units="in")



model.effects <- allEffects(Zn5)
plot(model.effects)
model.effects <- ggeffect(model = Zn5,
                          back.transform = F,
                          type = "re")
comm.effect <- model.effects$community
comm.effect$community <- factor(comm.effect$x, levels = c("AZ-Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))


comm.plot <- ggplot(data= dat,
                    mapping = aes(x=community,y=log(Zn))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~community, scales = "free")+
  geom_pointrange(data = comm.effect, aes(x=community, y = predicted, color = community, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCommunity",
       y = expression(paste("ln[Zn] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        legend.position = "none")
comm.plot

# ggplot(data= dat,
#        mapping = aes(x=community,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = comm.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nCommunity",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
#   #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlcommEffectln.png", res=100, height=5, width=7, units="in")

lsmeans(Zn5, pairwise~community, adjust = "tukey")# 
zn.sum <- summary(Zn5)# 
write.csv(zn.sum$coefficients, "zn5coef.csv")
zn.commmeans <- lsmeans(Zn5, pairwise~community, adjust = "tukey")# 
write.csv(zn.commmeans$lsmeans, "zn5lsmeanscomm.csv")
write.csv(zn.commmeans$contrasts, "zn5contrastscomm.csv")

period.effect <- model.effects$period
period.effect$period <- factor(period.effect$x, levels = c("First", "Last"))

per.plot <- ggplot(data= dat,
                   mapping = aes(x=period,y=log(Zn))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~period, scales = "free")+
  geom_pointrange(data = period.effect, aes(x=period, y = predicted, color = period, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSampling Period",
       y = expression(paste("ln[Zn] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#C5D7D2","#F5D1C4"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        #axis.title.y = element_blank(),
        legend.position = "none")
per.plot

# ggplot(data= dat,
#        mapping = aes(x=period,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = period.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nSampling Period",
#        y = "ln(As) [µg/L]\n")+
#   scale_color_manual(values=c("#C5D7D2","#F5D1C4"))+
#   coord_cartesian(ylim = c(-0.2,.2), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# dev.print(png, "As_ctrlperiodEffectln.png", res=100, height=4, width=4, units="in")

lsmeans(Zn5, pairwise~period, adjust = "tukey")# first greater than last
zn.periodmeans <- lsmeans(Zn5, pairwise~period, adjust = "tukey")
write.csv(zn.periodmeans$lsmeans, "zn5lsmeansper.csv")
write.csv(zn.periodmeans$contrasts, "zn5contrastsper.csv")

season.effect <- model.effects$season
season.effect$season <- factor(season.effect$x, levels = c("Winter", "Monsoon"))

ssn.plot <- ggplot(data= dat,
                   mapping = aes(x=season,y=log(Zn))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~season, scales = "free")+
  geom_pointrange(data = season.effect, aes(x=season, y = predicted, color = season, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nSampling Season",
       y = expression(paste("ln[Zn] (µg ",L^-1, ")")))+
  scale_color_manual(values=c("#A2A9D6","#F2C893"))+
  #oord_cartesian(ylim = c(-1.5,1.5), expand = c(0, .05))+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 9, color = "black", face = "bold"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black"),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 6.5, color = "black", hjust = .5, vjust = .5),
        #axis.title.y = element_blank(),
        legend.position = "none")
ssn.plot


comm.plot +  ssn.plot + per.plot+ plot_layout(ncol=3,widths=c(3,1,1))
dev.print(png, "Zn_commperssnEffectln.png", res=300, height=7, width=10, units="in")

comm.plot
dev.print(png, "Ni_commEffectln.png", res=300, height=5, width=5, units="in")

per.plot
dev.print(png, "Ni_perEffectln.png", res=300, height=5, width=3, units="in")

ssn.plot
dev.print(png, "Ni_ssnEffectln.png", res=300, height=5, width=3, units="in")


# ggplot(data= dat,
#        mapping = aes(x=season,y=log(As))) +
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = season.effect, aes(x=x, y = predicted, color = x, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1.5) +
#   labs(x = "\nSampling Season",
#        y = "ln(As) [ug/L]\n")+  
#   scale_color_manual(values=c("#A2A9D6","#F2C893"))+
#   #coord_cartesian(ylim = c(-0.3,.5), expand = c(0, .05))+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")

# title="Modeled Effect of Community",
# subtitle = "This figure displays modeled, not real data",

dev.print(png, "Ni_ctrlseasonEffectln.png", res=100, height=4, width=4, units="in")

lsmeans(Zn5, pairwise~season, adjust = "tukey")# first greater than last
zn.seasonmeans <- lsmeans(Zn5, pairwise~season, adjust = "tukey")
write.csv(zn.seasonmeans$lsmeans, "zn5lsmeansseason.csv")
write.csv(zn.seasonmeans$contrasts, "zn5contrastsseason.csv")




