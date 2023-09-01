#Kunal Palawat | kpalawat@email.arizona.edu
#Date Created: April 7th, 2021
#Description: Code to analyze globe/miami specific concentrations
#Notes
#


#load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(performance)
library(effects)
library(ggpubr)
library(emmeans)
library(multcomp)
library(ggeffects)
library(lmerTest)
library(car)

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data
globe <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "globe", col_names = TRUE)
globedat <- iw.dm[iw.dm$community=="Globe/Miami",]
dat <- full_join(globedat, globe, by = c("site"))
dat <- dat[!is.na(dat$community),]
dat <- dat[!is.na(dat$location.y),]

dat$location.y <- factor(dat$location.y, levels = c("Claypool", "Globe District Two", "Globe District Three", "Globe District Four", "Globe District Five", "Globe District Six", "Globe District One", "Miami", "Central Heights-Midland City", "Icehouse Canyon", "Six Shooter Canyon"))

dat$location_2 <- factor(dat$location_2, levels = c("Miami/Claypool Area", "Globe Area", "Canyons Area"))

dat$nearest.dist.km <- dat$nearest.dist.rounded/1000



#Al modeling location_2 ----
Al0 <- lmer(data = dat,
        log(Al) ~ (1|site))
Al1 <- lmer(data = dat,
            log(Al) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ location_2  + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al2)


Al3 <- lmer(data = dat,
            log(Al) ~  location_2   + season + period 
            + location_2:season + location_2:period 
            + season:period  + nearest.dist.km + 
            + location_2:season:period
            + (1|site))
vif(Al3)
summary(Al3)
anova(Al3)
step(Al3, scope=list(lower=Al0), direction="backward")
step(Al2, scope=list(lower=Al0), direction="backward")
step(Al1, scope=list(lower=Al0), direction="backward")

Al4 <- lmer(data = dat,
            log(Al) ~ nearest.dist.km  + (1 | site))
anova(Al4)
summary(Al4)
al.sum <- summary(Al4)
write.csv(al.sum$coefficients, "alglobe4coef.csv")
al.ssnmeans <- lsmeans(Al4, pairwise~season, adjust = "tukey")#
write.csv(cu.ssnmeans$lsmeans, "Cu1lsmeansssnHW.csv")
write.csv(cu.ssnmeans$contrasts, "Cu7contrastsssnHW.csv")


plot(allEffects(Al4))


predict.dat <- ggeffect(model = Al4,
                        terms = c("nearest.dist.km"),
                        back.transform = F,
                        type = "re")


ggplot(data = dat, aes(x = nearest.dist.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Al by Distance From Smelter and Effect Trendline\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_Gldisteffectln.png", res=100, height=6, width=8, units="in")





#Cd modeling ----
Cd0 <- lmer(data = dat,
            log(Cd) ~ (1|site))
Cd1 <- lmer(data = dat,
            log(Cd) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(Cd1)

Cd2 <- lmer(data = dat,
            log(Cd) ~ location_2  + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Cd2)

Cd2b <- lmer(data = dat,
            log(Cd) ~ location_2  + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period 
            + location_2:season:period
            + (1|site))
vif(Cd2b)

anova(Cd2b)

step(Cd2b, scope=list(lower=Cd0), direction="backward")

Cd3 <- lmer(data = dat,
             log(Cd) ~ season + period + season:period
             + (1|site))
vif(Cd3)
anova(Cd3)
summary(Cd3)
cd.sum <- summary(Cd3)
write.csv(cd.sum$coefficients, "cdglobe3coef.csv")
plot(allEffects(Cd3))
dev.print(png, "Cd_GMdisteffectln.png", res=100, height=6, width=8, units="in")

cd.ssnmeans <- lsmeans(Cd3, pairwise~season, adjust = "tukey")#
write.csv(cd.ssnmeans$contrasts, "Cd3contrastsssnHW.csv")

cd.ssnmeans <- lsmeans(Cd3, pairwise~season:period, adjust = "tukey")#
write.csv(cd.ssnmeans$contrasts, "Cd3contrastsssnHW.csv")


#Cu====
Cu0 <- lmer(data = dat,
            log(Cu) ~ (1|site))
Cu1 <- lmer(data = dat,
            log(Cu) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(Cu1)

Cu2 <- lmer(data = dat,
            log(Cu) ~ location_2  + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Cu2)


Cu3 <- lmer(data = dat,
            log(Cu) ~  location_2   + season + period + + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period  
            + location_2:season:period
            + (1|site))
vif(Cu3)
summary(Cu3)
anova(Cu3)
step(Cu3, scope=list(lower=Cu0), direction="backward")


Cu4 <- lmer(data = dat,
            log(Cu) ~ season + period + (1 | site))
anova(Cu4)
summary(Cu4)
cu.sum <- summary(Cu4)
write.csv(cu.sum$coefficients, "cuglobe4coef.csv")

cu.seasonmeans <- lsmeans(Cu4, pairwise~season, adjust = "tukey")
write.csv(cu.seasonmeans$lsmeans, "cuglobe93lsmeansseason.csv")
write.csv(cu.seasonmeans$contrasts, "cuglobe93contrastsseason.csv")

cu.periodmeans <- lsmeans(Cu4, pairwise~period, adjust = "tukey")
write.csv(cu.periodmeans$lsmeans, "cuglobe93lsmeansseason.csv")
write.csv(cu.periodmeans$contrasts, "cuglobe95contrastsseason.csv")


plot(allEffects(Cu4))
dev.print(png, "Cu_GMdisteffectln.png", res=100, height=6, width=8, units="in")





#Zn=====
Zn0 <- lmer(data = dat,
            log(Zn) ~ (1|site))
Zn1 <- lmer(data = dat,
            log(Zn) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(Zn1)

Zn2 <- lmer(data = dat,
            log(Zn) ~ location_2  + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Zn2)


Zn3 <- lmer(data = dat,
            log(Zn) ~  location_2   + season + period +nearest.dist.km
            + location_2:season + location_2:period 
            + season:period  
            + location_2:season:period
            + (1|site))
vif(Zn3)
summary(Zn3)
anova(Zn3)
step(Zn3, scope=list(lower=Zn0), direction="backward")


Zn4 <- lmer(data = dat,
            log(Zn) ~ season +(1 | site))
anova(Zn4)
summary(Zn4)

zn.sum <- summary(Zn4)
write.csv(zn.sum$coefficients, "znglobe3coef.csv")
plot(allEffects(Zn4))
dev.print(png, "Zn_GMdisteffectln.png", res=100, height=6, width=8, units="in")
zn.ssnmeans <- lsmeans(Zn4, pairwise~season, adjust = "tukey")#
write.csv(zn.ssnmeans$contrasts, "Zn4contrastsssnHW.csv")

#Mn=======
Mn0 <- lmer(data = dat,
            log(Mn) ~ (1|site))
Mn1 <- lmer(data = dat,
            log(Mn) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(Mn1)

Mn2 <- lmer(data = dat,
            log(Mn) ~ location_2  + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Mn2)


Mn3 <- lmer(data = dat,
            log(Mn) ~  location_2   + season + period + nearest.dist.km
            + location_2:season + location_2:period 
            + season:period  
            + location_2:season:period
            + (1|site))
vif(Mn3)
summary(Mn3)
anova(Mn3)
step(Mn3, scope=list(lower=Mn0), direction="backward")
step(Mn2, scope=list(lower=Mn0), direction="backward")
step(Mn1, scope=list(lower=Mn0), direction="backward")

Mn4 <- lmer(data = dat,
            log(Mn) ~ season + period +(1 | site))
anova(Mn4)
summary(Mn4)
mn.sum <- summary(Mn4)
write.csv(mn.sum$coefficients, "mnglobe93coef.csv")

plot(allEffects(Mn4))

dev.print(png, "Mn_GMdisteffectln.png", res=100, height=6, width=8, units="in")
mn.ssnmeans <- lsmeans(Mn4, pairwise~period, adjust = "tukey")#
write.csv(zn.ssnmeans$contrasts, "Mn4contrastsssnHW.csv")





#Barplot Season ----
GMssnbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$season),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$season),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("season","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("season","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=season)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+    
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = season), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = season, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nSeason",
         fill = "",         
         shape = "") +
    guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_GMssn.png"), res=100, height=6, width=8, units="in")
  
}
GMssnbarplotFX(dataDF = dat,
               name.string = "As")
lapply(X=Contamlist,
       GMssnbarplotFX,
       dataDF = hayden.dist)



GMlocationbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$location),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$location),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$location),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("location", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$location),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$location),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$location),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("location", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=location, y=gmean, x=location)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = 'black', mapping = aes(shape=range, y=value, x=location))+
    geom_text(aes(label = paste("n =", count),
                  group = location),
              y = 0,
              vjust = 1.5,
              position = "identity",
              family="Avenir") +
    scale_fill_manual(values = c("#52a90f", "#005ce6", "#ea4530", "#a3ec23", "#ec63cf", "#A2A9D6", "#735746", "#523087", "#F2C893", "#fdf734"))+
    labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations by Location", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nLocation",
         shape = "") +
    guides(fill = "none")+
    #scale_x_discrete(drop = FALSE)+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle=45, hjust = 1),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_globelocation_nopoints.png"), res=150, height=8, width=11, units="in")
  
}

GMlocationbarplotFX(dataDF = dat,
                    name.string = "Pb")
lapply(X=Contamlist,
       GMlocationbarplotFX,
       dataDF = dat)

GMlocationssnbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$location, dat.long$season),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$location, dat.long$season),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$location, dat.long$season),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
  
  names(gdat) <- c("location", "season", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$location, dat.long$season),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$location, dat.long$season),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$location, dat.long$season),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  
  names(rdat) <- c("location", "season", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=location)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = 'black', mapping = aes(shape=range, y=value, x=location, group = season))+
    geom_text(aes(label = paste("n =", count),
                  x = location,
                  group = season),
              y = 0,
              vjust = 1.5,
              position = "identity",
              family="Avenir") +
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations by Location and Season", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nLocation",
         shape = "",
         fill = "Season") +
    # guides(fill = "none")+
    facet_grid(.~season)+
    #scale_x_discrete(drop = FALSE)+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle=45, hjust = 1),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_globelocationssn_nopoints.png"), res=150, height=9, width=12, units="in")
  
}

GMlocationssnbarplotFX(dataDF = dat,
                       name.string = "As")
lapply(X=Contamlist,
       GMlocationssnbarplotFX,
       dataDF = dat)

GMsummaryStatFX <- function(dataDF, analyte.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- analyte.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$season, dat.long$nearest.dist.rounded),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$season, dat.long$nearest.dist.rounded),
                    FUN = geoSD)
  
  gdat <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  names(gdat) <- c("season", "nearest.dist.rounded", "gmean", "gsd")
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$season, dat.long$nearest.dist.rounded),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$season, dat.long$nearest.dist.rounded),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$season, dat.long$nearest.dist.rounded),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  names(rdat) <- c("season", "nearest.dist.rounded", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=as.factor(nearest.dist.rounded))) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = "black", mapping = aes(shape=range, y=value, x=as.factor(nearest.dist.rounded)))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste("Globe/Miami Geometric Mean", analyte, "Concentrations"),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep=""),
         x = "\nNearest Distance To Mine (m)",
         fill = "Season",
         shape = "") +
    facet_grid(.~season, scales = "free_x")+
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          strip.text = element_blank(),
          panel.grid = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.text.x = element_text(angle=90, vjust = .5),
          plot.title = element_text(size = 15, hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_GMminessn_nopoints.png"), res=150, height=8, width=10, units="in")
  
}
lapply(X = Contamlist,
       GMsummaryStatFX,
       dataDF = dat)

GMsummaryStatFX(dataDF = dat,
                analyte.string = "As")

GMscatterFX <- function(dataDF, analyte.string, x.string, test.string, xlabel.string){
  library(ggpubr)
  library(ggplot2)
  
  dat <- dataDF
  analyte <- analyte.string
  xvar <- x.string
  test <- test.string
  xlabel <- xlabel.string
  
  p <- ggscatter(data=dat, x = xvar, y = analyte,
                 add = "reg.line",
                 conf.int = TRUE,
                 cor.coef = FALSE,
                 cor.coeff.args = list(method = test, label.x.npc = "middle"),
                 ylab = paste(analyte, "Concentration (µg/L)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(size = 2.5, fill = "blue", color = "black", shape = 21) +
    labs(title = paste(analyte, "Concentrations in Rooftop Harvested Rainwater\nby Nearest Distance To Mine")) +
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
          plot.caption = element_text(size = 13, face="italic", hjust = 0),
          axis.text = element_text(size=15, color = "black", face = "bold"),
          axis.title = element_text(size=15, color = "black", face = "bold"))
  print(p)
  dev.print(png, paste(analyte, "_IWDM_GMscatter.jpeg", sep = ""), res=100, height=6, width=8, units="in")
  print("Great Job!")
}

GMsubsetScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
  library(ggpubr)
  library(ggplot2)
  
  dat <- dataDF
  analyte <- analyte.string
  xvar <- x.string
  subset <- subset.string
  subset.title <- subset.title.string
  test <- test.string
  xlabel <- xlabel.string
  
  p <- ggscatter(data=dat, x = xvar, y = analyte, color = subset,
                 add = "reg.line",
                 conf.int = TRUE,
                 #cor.coef = FALSE,
                 cor.coeff.args = list(method = test, label.x.npc = "middle"),
                 ylab = paste(analyte, "Concentration (µg/L)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             mapping = aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), color = season),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste(analyte, "Concentrations in Rooftop Harvested Rainwater\nby Nearest Distance To Mine"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
          legend.position = "bottom",
          plot.caption = element_text(size = 13, face="italic", hjust = 0),
          axis.text = element_text(size=15, color = "black", face = "bold"),
          axis.title = element_text(size=15, color = "black", face = "bold"))
  print(p)
  dev.print(png, paste(analyte, "_IWDM_GMssnscatter.jpeg", sep = ""), res=100, height=6, width=8, units="in")
  print("Great Job!")
}
GMscatterFX(dataDF = dat,
            analyte.string = "Pb",
            x.string = "nearest.dist.rounded",
            test.string = "spearman",
            xlabel.string = "Nearest Distance To Mine (m)")

lapply(X=Contamlist,
       GMscatterFX,
       dataDF = dat,
       x.string = "nearest.dist.rounded",
       test.string = "spearman",
       xlabel.string = "Nearest Distance To Mine (m)")

GMsubsetScatterFX(dataDF = dat,
                  analyte.string = "As",
                  x.string = "nearest.dist.rounded",
                  subset.string = "season",
                  subset.title.string = "Season",
                  test.string = "spearman",
                  xlabel.string = "Nearest Distance To Mine (m)")

lapply(X=Contamlist,
       GMsubsetScatterFX,
       dataDF = dat,
       x.string = "nearest.dist.rounded",
       subset.string = "season",
       subset.title.string = "Season",
       test.string = "spearman",
       xlabel.string = "Nearest Distance To Mine (m)")


GMlocation_2barplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$location_2),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$location_2),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$location_2),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("location_2", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$location_2),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$location_2),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$location_2),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("location_2", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=location_2, y=gmean, x=location_2)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = 'black', mapping = aes(shape=range, y=value, x=location_2))+
    geom_text(aes(label = paste("n =", count),
                  group = location_2),
              y = 0,
              vjust = 1.2,
              position = "identity",
              family="Avenir") +
    scale_fill_manual(values = c("#A2A9D6", "#a3ec23", "#fdf734"))+
    labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations by Location", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nLocation",
         shape = "") +
    guides(fill = "none")+
    #scale_x_discrete(drop = FALSE)+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_globelocation_2_nopoints.png"), res=100, height=6, width=8, units="in")
  
}

GMlocation_2barplotFX(dataDF = dat,
                      name.string = "As")
lapply(X=Contamlist,
       GMlocation_2barplotFX,
       dataDF = dat)

# GMlocation_2ssnbarplotFX <- function(dataDF, name.string){
#   library(ggplot2)
#   library(EnvStats)
#   library(tidyverse)
#   
#   dat <- dataDF
#   analyte <- name.string
#   
#   dat.long <- pivot_longer(data = dat,
#                            cols = Be:Pb,
#                            values_to = "value",
#                            names_to = "analyte")
#   
#   dat.long <- dat.long[dat.long$analyte == analyte,]
#   
#   dat.long$count <- 1
#   
#   gmeans <- aggregate(dat.long$value,
#                       by = list(dat.long$location_2, dat.long$season),
#                       FUN = geoMean)
#   
#   gsds <- aggregate(dat.long$value,
#                     by = list(dat.long$location_2, dat.long$season),
#                     FUN = geoSD)
#   
#   gcounts <- aggregate(dat.long$count,
#                        by = list(dat.long$location_2, dat.long$season),
#                        FUN = sum)
#   
#   gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
#   gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
#   
#   names(gdat) <- c("location_2", "season", "gmean", "gsd", "count")
#   
#   gdat$gmean <- as.numeric(gdat$gmean)
#   gdat$gsd <- as.numeric(gdat$gsd)
#   gdat$count <- as.numeric(gdat$count)
#   
#   rmin <- aggregate(dat.long$value,
#                     by = list(dat.long$location_2, dat.long$season),
#                     FUN = min)
#   
#   rmax <- aggregate(dat.long$value,
#                     by = list(dat.long$location_2, dat.long$season),
#                     FUN = max)
#   
#   rmed <- aggregate(dat.long$value,
#                     by = list(dat.long$location_2, dat.long$season),
#                     FUN = median)
#   
#   rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
#   rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
#   
#   names(rdat) <- c("location_2", "season", "min", "max", "median")
#   
#   rdat.long <- pivot_longer(data = rdat,
#                             cols = min:median,
#                             values_to = "value",
#                             names_to = "range")
#   rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
#   
#   p <- ggplot(gdat,
#               mapping = aes(fill=season, y=gmean, x=location_2)) +
#     geom_bar(position="dodge", stat="identity", color = "black") +
#     geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
#     #geom_point(data = rdat.long, fill = 'black', mapping = aes(shape=range, y=value, x=location_2, group = season))+
#     geom_text(aes(label = paste("n =", count),
#                   x = location_2,
#                   group = season),
#               y = 0,
#               vjust = 1.5,
#               position = "identity",
#               family="Avenir") +
#     scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
#     labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations by Location and Season", sep = ""),
#          subtitle = paste("Rooftop Harvested Rainwater - DM"),
#          y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
#          x = "\nLocation",
#          shape = "",
#          fill = "Season") +
#     # guides(fill = "none")+
#     facet_grid(.~season)+
#     #scale_x_discrete(drop = FALSE)+
#     #scale_y_continuous(expand = c(0, 0.1)) +
#     theme_bw() +
#     theme(strip.text = element_blank(),
#           text = element_text(size=15, family = "Avenir"),
#           panel.grid.minor = element_blank(),
#           panel.grid.major = element_blank(),
#           axis.line.y = element_blank(),
#           axis.line.x = element_blank(),
#           axis.title = element_text(),
#           axis.ticks.x = element_blank(),
#           axis.text.x = element_text(angle=45, hjust = 1),
#           plot.title = element_text(hjust=.5, face = "bold"),
#           plot.subtitle = element_text(hjust=.5),
#           legend.position = "bottom")
#   
#   print(p)
#   
#   dev.print(png, paste(analyte,"_IWDM_globelocation_2ssn_nopoints.png"), res=150, height=9, width=12, units="in")
#   
# }
# 
# GMlocation_2ssnbarplotFX(dataDF = dat,
#                        name.string = "As")
# lapply(X=Contamlist,
#        GMlocation_2ssnbarplotFX,
#        dataDF = dat)


m2 <- lmer(data = iw.dm.long,
           log(value) ~ analyte + season + community + analyte:community + (1|community:site))

anova(Pb4, m2)
summary(Pb4)
plot(allEffects(Pb4))













