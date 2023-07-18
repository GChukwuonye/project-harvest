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
setwd("/users/kunalpalawat/Documents/GitHub/ProjectHarvest/WorkingFiles")

#load data
globe <- read_xlsx("LATLOGSITE.xlsx", sheet = "globe", col_names = TRUE)
globedat <- iw.dm[iw.dm$community=="Globe/Miami",]
dat <- full_join(globedat, globe, by = c("site"))
dat <- dat[!is.na(dat$community),]
dat <- dat[!is.na(dat$location),]

dat$location <- factor(dat$location, levels = c("Claypool", "Globe District Two", "Globe District Three", "Globe District Four", "Globe District Five", "Globe District Six", "Globe District One", "Miami", "Central Heights-Midland City", "Icehouse Canyon", "Six Shooter Canyon"))

dat$location_2 <- factor(dat$location_2, levels = c("Miami/Claypool Area", "Globe Area", "Canyons Area"))

dat$nearest.dist.km <- dat$nearest.dist.rounded/1000

#As modeling ----
As0 <- lmer(data = dat,
            log(As) ~ (1|site),
            REML = T)

check_model(As0)

As1 <- lmer(data = dat,
            log(As) ~ location + (1|site))

As2 <- lmer(data = dat,
            log(As) ~ location * season + (1|site))
check_model(As2)
As3 <- lmer(data = dat,
            log(As) ~ location + season + (1|site))
As4 <- lmer(data = dat,
            log(As) ~  season + (1|site))
check_model(As4)
As5 <- lmer(data = dat,
            log(As) ~  location * season + period + (1|site))
check_model(As5)
As6 <- lmer(data = dat,
            log(As) ~  location + season + period + (1|site))
check_model(As6)
anova(As0, As1)
anova(As0, As2)
anova(As1, As2)
anova(As2, As3)
anova(As1, As3)
anova(As4, As3)
anova(As2, As5)
anova(As3, As6)
summary(As3)
plot(allEffects(As3))
lsmeans(As3,pairwise~season, adjust = "tukey")

#As modeling location_2 ----
As1 <- lmer(data = dat,
            log(As) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(As1)

As2 <- lmer(data = dat,
            log(As) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period + location_2:nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(As2)

As3 <- lmer(data = dat,
            log(As) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(As3)

As4 <- lmer(data = dat,
            log(As) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(As4)

As5 <- lmer(data = dat,
            log(As) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(As5)

As6 <- lmer(data = dat,
            log(As) ~ location_2 + season + period + nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(As6)
anova(As6)

As7 <- lmer(data = dat,
            log(As) ~ location_2 + season + period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(As7)

As8 <- lmer(data = dat,
            log(As) ~ location_2 + season + period
            + season:period + period:nearest.dist.km
            + (1|site))
vif(As8)

As9 <- lmer(data = dat,
            log(As) ~ location_2 + season + period
            + season:period
            + (1|site))
vif(As9)
anova(As9)

As10 <- lmer(data = dat,
            log(As) ~ location_2 + season
            + season:period
            + (1|site))
anova(As10)

As11 <- lmer(data = dat,
             log(As) ~ location_2 + season
             + (1|site))
anova(As11)


As13 <- lmer(data = dat,
             log(As) ~ nearest.dist.km + season
             + (1|site))
anova(As13)


As91 <- lmer(data = dat,
            log(As) ~ nearest.dist.km + season + period
            + season:period
            + (1|site))
anova(As91)

As92 <- lmer(data = dat,
              log(As) ~ nearest.dist.km + season
              + season:period
              + (1|site))
anova(As92)

As93 <- lmer(data = dat,
              log(As) ~ nearest.dist.km + season
              + (1|site),
             REML = T)
anova(As93)

As12 <- lmer(data = dat,
             log(As) ~ season
             + (1|site))
anova(As12)


anova(As9, As10)
anova(As10, As11)
anova(As11, As12)
anova(As9, As91)
anova(As91, As92)
anova(As92, As93)
anova(As93, As12)
AIC(As0)
AIC(As1)
performance(As93)

# As0 <- lmer(data = dat,
#             log(As) ~ (1|site))
# check_model(As0)
# 
# As1 <- lmer(data = dat,
#             log(As) ~ location_2 + (1|site))
# 
# As1 <- lmer(data = dat,
#             log(As) ~ location_2 * season *period * nearest.dist.km
#             + (1|site))
# check_model(As2)
# As3 <- lmer(data = dat,
#             log(As) ~ location_2 + season + (1|site))
# As4 <- lmer(data = dat,
#             log(As) ~  season + (1|site))
# check_model(As4)
# As5 <- lmer(data = dat,
#             log(As) ~  location_2 * season + period + (1|site))
# check_model(As5)
# As6 <- lmer(data = dat,
#             log(As) ~  location_2 + season + period + (1|site))
# check_model(As6)
# As7 <- lmer(data = dat,
#             log(As) ~ nearest.dist.km + (1|site))
# As8 <- lmer(data = dat,
#             log(As) ~ nearest.dist.km + location_2 + (1|site))
# As9 <- lmer(data = dat,
#             log(As) ~ nearest.dist.km * season + (1|site))
# check_model(As9)
# 
# As10 <- lmer(data = dat,
#             log(As) ~ nearest.dist.km + season + (1|site))
# As11 <- lmer(data = dat,
#             log(As) ~ nearest.dist.km + season + period + (1|site))
# 
# anova(As0, As1)
# anova(As0, As2)
# anova(As1, As2)
# anova(As2, As3)
# anova(As1, As3)
# anova(As1, As4)
# anova(As4, As3)
# anova(As2, As5)
# anova(As3, As6)
# anova(As4, As3)
# anova(As0, As7)
# anova(As7, As8)
# anova(As1, As7)
# anova(As7, As9)
# anova(As9, As10)
# anova(As11, As10)


as.sum <- summary(As93)
write.csv(as.sum$coefficients, "asglobe93coef.csv")

as.seasonmeans <- lsmeans(As93, pairwise~season, adjust = "tukey")
write.csv(as.seasonmeans$lsmeans, "asglobe93lsmeansseason.csv")
write.csv(as.seasonmeans$contrasts, "asglobe93contrastsseason.csv")


plot(allEffects(As93))
anova(As10)
lsmeans(As10,pairwise~nearest.dist.km, adjust = "tukey")


check_model(As3)
dev.print(png, "As_checkglobe9.png", res=100, height=12, width=30, units="in")

predict.dat <- ggeffect(model = As93,
                         terms = c("nearest.dist.km"),
                         back.transform = F,
                        type = "re")

# predict.datt <- ggpredict(model = As93,
#                  terms = c("nearest.dist.km"),
#                  back.transform = F) #specify both variables in terms to assess them together, this helps visualize an interaction.
ggplot(data = dat, aes(x = nearest.dist.km, y = log(As)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y =predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#00A8C6")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "As by Nearest Distance From Mine and Effect Trendline\n",
       y = "ln(As) [µg/L]\n",
       x = "\nNearest Distance From Mine (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "As_GMdisteffectln.png", res=100, height=6, width=8, units="in")



#Pb modeling ----



Pb0 <- lmer(data = dat,
            log(Pb) ~ (1|site))
check_model(Pb0)

Pb1 <- lmer(data = dat,
            log(Pb) ~ location + (1|site))

Pb2 <- lmer(data = dat,
            log(Pb) ~ location * season + (1|site))
check_model(Pb2)
Pb3 <- lmer(data = dat,
            log(Pb) ~ location + season + (1|site))
Pb4 <- lmer(data = dat,
            log(Pb) ~  season + (1|site))
check_model(Pb4)
Pb5 <- lmer(data = dat,
            log(Pb) ~  location * season + period + (1|site))
check_model(Pb5)
Pb6 <- lmer(data = dat,
            log(Pb) ~  location + season + period + (1|site))
check_model(Pb6)
anova(Pb0, Pb1)
anova(Pb0, Pb2)
anova(Pb1, Pb2)
anova(Pb2, Pb3)
anova(Pb1, Pb3)
anova(Pb3, Pb4)
anova(Pb2, Pb5)
anova(Pb3, Pb6)
summary(Pb3)
plot(allEffects(Pb3))
lsmeans(Pb3,pairwise~location, adjust = "tukey")

#Pb modeling location_2 ----
Pb0 <- lmer(data = dat,
            log(Pb) ~ (1|site),
            REML=T)

Pb1 <- lmer(data = dat,
            log(Pb) ~ location_2 * season *period * nearest.dist.km
            + (1|site),
            REML=T)
vif(Pb1)

Pb2 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period + location_2:nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Pb2)

Pb3 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Pb3)

Pb4 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Pb4)

Pb5 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Pb5)

Pb6 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period + nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Pb6)
anova(Pb6)

Pb7 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Pb7)

Pb8 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period
            + season:period + period:nearest.dist.km
            + (1|site))
vif(Pb8)

Pb9 <- lmer(data = dat,
            log(Pb) ~ location_2 + season + period
            + season:period
            + (1|site))
vif(Pb9)
anova(Pb9)

Pb10 <- lmer(data = dat,
             log(Pb) ~ location_2 + season
             + period
             + (1|site))
anova(Pb10)

Pb11 <- lmer(data = dat,
             log(Pb) ~ location_2 + season
             + (1|site))
anova(Pb11)


Pb12 <- lmer(data = dat,
             log(Pb) ~ location_2 + nearest.dist.km + season + period
             + season:period
             + (1|site))
vif(Pb12)
anova(Pb12)

Pb13 <- lmer(data = dat,
             log(Pb) ~  nearest.dist.km + season + period
             + season:period
             + (1|site))
anova(Pb13, Pb9)
anova(Pb13)

Pb92 <- lmer(data = dat,
             log(Pb) ~ nearest.dist.km + season
             + period
             + (1|site))
anova(Pb92)

Pb93 <- lmer(data = dat,
             log(Pb) ~ nearest.dist.km + season
             + (1|site),
             REML = T)
anova(Pb93)

Pb94 <- lmer(data = dat,
             log(Pb) ~ season
             + (1|site))
anova(Pb94)

Pb95 <- lmer(data = dat,
             log(Pb) ~ nearest.dist.km
             + (1|site))
anova(Pb95)


anova(Pb13, Pb92)
anova(Pb92, Pb93)
anova(Pb93, Pb94)
anova(Pb93, Pb95)
compare_performance(Pb92, Pb93)

AIC(Pb0)
AIC(Pb1)
performance(Pb93)
# Pb0 <- lmer(data = dat,
#             log(Pb) ~ (1|site))
# check_model(Pb0)
# 
# Pb1 <- lmer(data = dat,
#             log(Pb) ~ location_2 + (1|site))
# 
# Pb2 <- lmer(data = dat,
#             log(Pb) ~ location_2 * season + (1|site))
# check_model(Pb2)
# Pb3 <- lmer(data = dat,
#             log(Pb) ~ location_2 + season + (1|site))
# Pb4 <- lmer(data = dat,
#             log(Pb) ~  season + (1|site))
# check_model(Pb4)
# Pb5 <- lmer(data = dat,
#             log(Pb) ~  location_2 * season + period + (1|site))
# check_model(Pb5)
# Pb6 <- lmer(data = dat,
#             log(Pb) ~  location_2 + season + period + (1|site))
# check_model(Pb6)
# Pb7 <- lmer(data = dat,
#             log(Pb) ~ nearest.dist.km + (1|site))
# Pb8 <- lmer(data = dat,
#             log(Pb) ~ nearest.dist.km + location_2 + (1|site))
# Pb9 <- lmer(data = dat,
#             log(Pb) ~ nearest.dist.km * season + (1|site))
# check_model(Pb9)
# 
# Pb10 <- lmer(data = dat,
#              log(Pb) ~ nearest.dist.km + season + (1|site))
# Pb11 <- lmer(data = dat,
#              log(Pb) ~ nearest.dist.km + nearest.dist.km:season + (1|site))
# 
# anova(Pb0, Pb1)
# anova(Pb0, Pb2)
# anova(Pb1, Pb2)
# anova(Pb2, Pb3)
# anova(Pb1, Pb3)
# anova(Pb1, Pb4)
# anova(Pb2, Pb4)
# anova(Pb4, Pb3)
# anova(Pb2, Pb5)
# anova(Pb3, Pb6)
# anova(Pb4, Pb3)
# anova(Pb0, Pb7)
# anova(Pb7, Pb8)
# anova(Pb1, Pb7)
# anova(Pb7, Pb9)
# anova(Pb2, Pb9)
# anova(Pb9, Pb10)
# anova(Pb3, Pb10)
# anova(Pb9, Pb10)
# anova(Pb11, Pb10)
# anova(Pb11, Pb9)


pb.sum <- summary(Pb93)
write.csv(pb.sum$coefficients, "pbglobe93coef.csv")

pb.seasonmeans <- lsmeans(Pb93, pairwise~season, adjust = "tukey")
write.csv(pb.seasonmeans$lsmeans, "pbglobe93lsmeansseason.csv")
write.csv(pb.seasonmeans$contrasts, "pbglobe93contrastsseason.csv")

summary(Pb10)
vif(Pb10)
plot(allEffects(Pb10))
lsmeans(Pb10,pairwise~season, adjust = "tukey")



check_model(Pb10)
dev.print(png, "Pb_checkglobe10.png", res=100, height=12, width=30, units="in")



predict.dat <- ggeffect(model = Pb93,
                         terms = c("nearest.dist.km"),
                         back.transform = F,
                        type = "re") #specify both variables in terms to assess them together, this helps visualize an interaction.

ggplot(data = dat, aes(x = nearest.dist.km, y = log(Pb)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#00A8C6")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Pb by Nearest Distance From Mine and Effect Trendline\n",
       y = "ln(Pb) [µg/L]\n",
       x = "\nNearest Distance From Mine (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Pb_GMdisteffectln.png", res=100, height=6, width=8, units="in")

# ggplot(data = dat, aes(x = nearest.dist.km, y = Pb))+
#   geom_point(aes(fill = season), shape = 21)+
#   geom_ribbon(data = predict.dat, mapping = aes(x=x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = .2)+ #adds shading for error
#   geom_line(data = predict.dat, mapping = aes(x=x, y = exp(predicted), color = group))+
#   scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
#   scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
#   labs(title = "Pb by Nearest Distance From Mine and Effect Trendline\n",
#        y = "Pb Concentration (µg/L)\n",
#        x = "\nNearest Distance From Mine (km)",
#        fill = "Season",
#        color = "Season")+
#   theme_classic()+
#   theme(text = element_text(family = "Avenir"),
#         plot.title = element_text(hjust = 0.5, face = "bold")
#   )
# dev.print(png, "Pb_GMdistssneffect.png", res=100, height=6, width=8, units="in")



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













