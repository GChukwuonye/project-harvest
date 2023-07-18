#Kunal Palawat | kpalawat@email.arizona.edu
#Date Created: March 21th, 2021
#Description: Code to analyze dewey-humboldt specific concentrations
#Notes
#


#load libraries
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

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")
#Dewey-Humboldt=======
#load data
dewey <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "dewey", col_names = TRUE)
deweydat <- iw.dm[iw.dm$community=="Dewey-Humboldt",]
dat <- full_join(deweydat, dewey, by = c("site"))
dat <- dat[!is.na(dat$community),]
dat <- dat[!is.na(dat$location.y),]
dat$Al.ln <- log(dat$Al)
dat$Al.ln <- log(dat$Al)
dat$location.y <- factor(dat$location.y, levels = c("South", "EAlt", "North EAlt", "North", "North West"))
# dat.edist <- dat[!is.na(dat$edist.rounded),]
# dat.wdist <- dat[!is.na(dat$wdist.rounded),]
dat.long <- pivot_longer(data = dat,
                         cols = edist.rounded:wdist.rounded,
                         values_to = "dist.rounded",
                         names_to = "tailings")
dat.long[dat.long$tailings=="edist.rounded",]$tailings <- "EAltern Tailings"
dat.long[dat.long$tailings=="wdist.rounded",]$tailings <- "Western Tailings"
dat.long$tailings <- factor(dat.long$tailings, levels = c("Western Tailings", "EAltern Tailings"))

dat$edist.rounded.km <- dat$edist.rounded/1000
dat$wdist.rounded.km <- dat$wdist.rounded/1000


setwd("/users/godsgiftnkechichukwuonye/Desktop/PhD_Stuff/PH_Figures/Modelling")
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)
check_model(Al0)
Al1 <- lmer(data = dat,
            log(Al) ~ location.y + (1|site),
            REML = F)
check_model(Al1)
Al2 <- lmer(data = dat,
            log(Al) ~ location.y + edist.rounded.km + wdist.rounded.km + season + period
            + location.y:season + season:period
            +(1|site),
            REML = T)
check_model(Al2) #good model
Al3 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km + wdist.rounded.km + season + period
            + location.y:season + season:period
            +(1|site),
            REML = F)
check_model(Al3)#good model
Al4 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km + wdist.rounded.km + season + period
            + season:period
            +(1|site),
            REML = F)
check_model(Al4) #good
Al5 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km + season + period
            + season:period
            +(1|site),
            REML = F)
check_model(Al5) #good
anova(Al5)
Al6 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km + period
            + season:period
            +(1|site),
            REML = F)
check_model(Al6) #good
anova(Al6)
Al7 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km
            + season:period
            +(1|site),
            REML = F)
check_model(Al7) #good
anova(Al7)

Al8 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km
            +(1|site),
            REML = F)
anova(Al8)
check_model(Al8) #not good

Al9 <- lmer(data = dat,
            log(Al) ~ season
            +(1|site),
            REML = F)
anova(Al9)
check_model(Al9) #not good
Al10 <- lmer(data = dat,
             log(Al) ~ season+location.y
             +(1|site),
             REML = F)
anova(Al10)
check_model(Al10)#good
Al11 <- lmer(data = dat,
             log(Al) ~ location.y
             +(1|site),
             REML = T)
anova(Al11)
check_model(Al1) #not good
Al12 <- lmer(data = dat,
             log(Al) ~ edist.rounded.km
             +(1|site),
             REML = F)
check_model(Al12) #not good
anova(Al12)
Al13 <- lmer(data = dat,
             log(Al) ~ wdist.rounded.km
             +(1|site),
             REML = F)
anova(Al13)
check_model(Al13) #not good
anova(Al5, Al6)
anova(Al6, Al7)
anova(Al7, Al8)
anova(Al8, Al9)
anova(Al0, Al9)
anova(Al0, Al10)
anova(Al10, Al11)
anova(Al0, Al11)
anova(Al2, Al3)
AIC(Al0)
AIC(Al2)
performance(Al11)

plot(allEffects(Al11))
Al.sum <- summary(Al11)
write.csv(Al.sum$coefficients, "Al11coefDH.csv")
Al.locmeans <- lsmeans(Al11, pairwise~location, adjust = "tukey")#
write.csv(Al.locmeans$lsmeans, "Al11lsmeansDH.csv")
write.csv(Al.locmeans$contrAlts, "Al11contrAltsDH.csv")


#Al Modeling ----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)
Al1 <- lmer(data = dat,
            log(Al) ~ location.y  + (1|site),
            REML = F)
Al2 <- lmer(data = dat,
            log(Al) ~ location.y + edist.rounded.km + wdist.rounded.km + season + period
            + location.y:season + season:period
            +(1|site),
            REML = T)
check_model(Al2)
vif(Al2)
Al3 <- lmer(data = dat,
            log(Al) ~  edist.rounded.km + wdist.rounded.km + season + period
            + location.y:season + season:period
            + (1|site),
            REML = F)
Al4 <- lmer(data = dat,
            log(Al) ~  edist.rounded.km + wdist.rounded.km + season + period
            + season:period
            + (1|site),
            REML = F)
Al5 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km + season + period
            + season:period
            + (1|site),
            REML = F)
anova(Al5)
Al6 <- lmer(data = dat,
            log(Al) ~ edist.rounded.km + season
            + season:period
            + (1|site),
            REML = F)
anova(Al6)
Al7 <- lmer(data = dat,
            log(Al) ~  season
            + season:period
            + (1|site),
            REML = F)
anova(Al7)

Al8 <- lmer(data = dat,
            log(Al) ~  season
            + (1|site),
            REML = T)
anova(Al8)

Al9 <- lmer(data = dat,
            log(Al) ~  season + location.y
            + (1|site),
            REML = F)
anova(Al9)

Al10 <- lmer(data = dat,
             log(Al) ~   location.y
             + (1|site),
             REML = F)
Al11 <- lmer(data = dat,
            log(Al) ~  season+ season:period
            + (1|site),
            REML = T)
anova(Al11)
anova(Al10)

anova(Al5, Al6)
anova(Al6,Al7)
anova(Al7, Al8)
anova(Al8, Al0)
anova(Al10, Al0)
AIC(Al0)
AIC(Al2)
performance(Al8)

plot(allEffects(Al8))
Al.sum <- summary(Al8)
write.csv(Al.sum$coefficients, "Al8coefDH.csv")
Al.ssnmeans <- lsmeans(Al8, pairwise~season, adjust = "tukey")#
write.csv(Al.ssnmeans$lsmeans, "Al8lsmeansDH.csv")
write.csv(Al.ssnmeans$contrAlts, "Al8contrAltsDH.csv")







check_model(Al8)
dev.print(png, "Al_checkdewey13.png", res=100, height=12, width=30, units="in")

plot(allEffects(Al9))
plot(allEffects(Al10))

summary(Al8)
lsmeans(Al8,pairwise~season, adjust = "tukey")


aaggregate(dat$Al,
           by = list(dat$location.y),
           FUN = geoMean)
DHssnbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Al,
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
    labs(title = paste("Dewey-Humboldt Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nseason",
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
  
  dev.print(png, paste(analyte,"_IWDM_DHssn.png"), res=100, height=6, width=8, units="in")
  
}

DHssnbarplotFX(dataDF = dat,
               name.string = "Al")
 


#globe======
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")
globe <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "globe", col_names = TRUE)
globedat <- iw.dm[iw.dm$community=="Globe/Miami",]
dat <- full_join(globedat, globe, by = c("site"))
dat <- dat[!is.na(dat$community),]
dat <- dat[!is.na(dat$location.y),]

dat$location.y <- factor(dat$location.y, levels = c("Claypool", "Globe District Two", "Globe District Three", "Globe District Four", "Globe District Five", "Globe District Six", "Globe District One", "Miami", "Central Heights-Midland City", "Icehouse Canyon", "Six Shooter Canyon"))

dat$location_2 <- factor(dat$location_2, levels = c("Miami/Claypool Area", "Globe Area", "Canyons Area"))

dat$nearest.dist.km <- dat$nearest.dist.rounded/1000

#Al modeling ----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)

check_model(Al0)

Al1 <- lmer(data = dat,
            log(Al) ~ location.y + (1|site))

Al2 <- lmer(data = dat,
            log(Al) ~ location.y * season + (1|site))
check_model(Al2)
Al3 <- lmer(data = dat,
            log(Al) ~ location.y + season + (1|site))
Al4 <- lmer(data = dat,
            log(Al) ~  season + (1|site))
check_model(Al4)
Al5 <- lmer(data = dat,
            log(Al) ~  location.y * season + period + (1|site))
check_model(Al5)
Al6 <- lmer(data = dat,
            log(Al) ~  location.y + season + period + (1|site))
check_model(Al6)
anova(Al0, Al1)
anova(Al0, Al2)
anova(Al1, Al2)
anova(Al2, Al3)
anova(Al1, Al3)
anova(Al4, Al3)
anova(Al2, Al5)
anova(Al3, Al6)
summary(Al3)
plot(allEffects(Al3))
lsmeans(Al3,pairwise~season, adjust = "tukey")

#Al modeling location_2 ----
Al1 <- lmer(data = dat,
            log(Al) ~ location_2 * season *period * nearest.dist.km
            + (1|site), REML=T)
vif(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period + location_2:nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al4)

Al5 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Al5)

Al6 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Al6)
anova(Al6)

Al7 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Al7)

Al8 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period
            + season:period + period:nearest.dist.km
            + (1|site))
vif(Al8)

Al9 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period
            + season:period
            + (1|site))
vif(Al9)
anova(Al9)

Al10 <- lmer(data = dat,
             log(Al) ~ location_2 + season
             + season:period
             + (1|site))
anova(Al10)

Al11 <- lmer(data = dat,
             log(Al) ~ location_2 + season
             + (1|site))
anova(Al11)


Al13 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km + season
             + (1|site))
anova(Al13)


Al91 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km + season + period
             + season:period
             + (1|site))
anova(Al91)

Al92 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km + season
             + season:period
             + (1|site))
anova(Al92)

Al93 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km + season
             + (1|site),
             REML = T)
anova(Al93)

Al12 <- lmer(data = dat,
             log(Al) ~ season
             + (1|site))
anova(Al12)


anova(Al9, Al10)
anova(Al10, Al11)
anova(Al11, Al12)
anova(Al9, Al91)
anova(Al91, Al92)
anova(Al92, Al93)
anova(Al93, Al12)
AIC(Al0)
AIC(Al1)
performance(Al93)

# Al0 <- lmer(data = dat,
#             log(Al) ~ (1|site))
# check_model(Al0)
# 
# Al1 <- lmer(data = dat,
#             log(Al) ~ location_2 + (1|site))
# 
# Al1 <- lmer(data = dat,
#             log(Al) ~ location_2 * season *period * nearest.dist.km
#             + (1|site))
# check_model(Al2)
# Al3 <- lmer(data = dat,
#             log(Al) ~ location_2 + season + (1|site))
# Al4 <- lmer(data = dat,
#             log(Al) ~  season + (1|site))
# check_model(Al4)
# Al5 <- lmer(data = dat,
#             log(Al) ~  location_2 * season + period + (1|site))
# check_model(Al5)
# Al6 <- lmer(data = dat,
#             log(Al) ~  location_2 + season + period + (1|site))
# check_model(Al6)
# Al7 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km + (1|site))
# Al8 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km + location_2 + (1|site))
# Al9 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km * season + (1|site))
# check_model(Al9)
# 
# Al10 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km + season + (1|site))
# Al11 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km + season + period + (1|site))
# 
# anova(Al0, Al1)
# anova(Al0, Al2)
# anova(Al1, Al2)
# anova(Al2, Al3)
# anova(Al1, Al3)
# anova(Al1, Al4)
# anova(Al4, Al3)
# anova(Al2, Al5)
# anova(Al3, Al6)
# anova(Al4, Al3)
# anova(Al0, Al7)
# anova(Al7, Al8)
# anova(Al1, Al7)
# anova(Al7, Al9)
# anova(Al9, Al10)
# anova(Al11, Al10)


Al.sum <- summary(Al93)
write.csv(Al.sum$coefficients, "Alglobe93coef.csv")

Al.seasonmeans <- lsmeans(Al93, pairwise~season, adjust = "tukey")
write.csv(Al.seasonmeans$lsmeans, "Alglobe93lsmeansseason.csv")
write.csv(Al.seasonmeans$contrAlts, "Alglobe93contrAltsseason.csv")


plot(allEffects(Al93))
anova(Al10)
lsmeans(Al10,pairwise~nearest.dist.km, adjust = "tukey")


check_model(Al3)
dev.print(png, "Al_checkglobe9.png", res=100, height=12, width=30, units="in")

predict.dat <- ggeffect(model = Al93,
                        terms = c("nearest.dist.km"),
                        back.transform = F,
                        type = "re")

# predict.datt <- ggpredict(model = Al93,
#                  terms = c("nearest.dist.km"),
#                  back.transform = F) #specify both variables in terms to Alsess them together, this helps visualize an interaction.
ggplot(data = dat, aes(x = nearest.dist.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y =predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#00A8C6")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Al by Nearest Distance From Mine and Effect Trendline\n",
       y = "ln(Al) [µg/L]\n",
       x = "\nNearest Distance From Mine (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_GMdisteffectln.png", res=100, height=6, width=8, units="in")



#Al modeling ----



Al0 <- lmer(data = dat,
            log(Al) ~ (1|site))
check_model(Al0)

Al1 <- lmer(data = dat,
            log(Al) ~ location.y + (1|site))

Al2 <- lmer(data = dat,
            log(Al) ~ location.y * season + (1|site))
check_model(Al2)
Al3 <- lmer(data = dat,
            log(Al) ~ location.y + season + (1|site))
Al4 <- lmer(data = dat,
            log(Al) ~  season + (1|site))
check_model(Al4)
Al5 <- lmer(data = dat,
            log(Al) ~  location.y * season + period + (1|site))
check_model(Al5)
Al6 <- lmer(data = dat,
            log(Al) ~  location.y + season + period + (1|site))
check_model(Al6)
anova(Al0, Al1)
anova(Al0, Al2)
anova(Al1, Al2)
anova(Al2, Al3)
anova(Al1, Al3)
anova(Al3, Al4)
anova(Al2, Al5)
anova(Al3, Al6)
summary(Al3)
plot(allEffects(Al3))
lsmeans(Al3,pairwise~location, adjust = "tukey")

#Al modeling location_2 ----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML=T)

Al1 <- lmer(data = dat,
            log(Al) ~ location_2 * season *period * nearest.dist.km
            + (1|site),
            REML=T)
vif(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period + location_2:nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:season + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + location_2:season:period
            + (1|site))
vif(Al4)

Al5 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + location_2:period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Al5)

Al6 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period + nearest.dist.km
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Al6)
anova(Al6)

Al7 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period
            + season:period + season:nearest.dist.km + period:nearest.dist.km
            + (1|site))
vif(Al7)

Al8 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period
            + season:period + period:nearest.dist.km
            + (1|site))
vif(Al8)

Al9 <- lmer(data = dat,
            log(Al) ~ location_2 + season + period
            + season:period
            + (1|site))
vif(Al9)
anova(Al9)

Al10 <- lmer(data = dat,
             log(Al) ~ location_2 + season
             + period
             + (1|site))
anova(Al10)

Al11 <- lmer(data = dat,
             log(Al) ~ location_2 + season
             + (1|site))
anova(Al11)


Al12 <- lmer(data = dat,
             log(Al) ~ location_2 + nearest.dist.km + season + period
             + season:period
             + (1|site))
vif(Al12)
anova(Al12)

Al13 <- lmer(data = dat,
             log(Al) ~  nearest.dist.km + season + period
             + season:period
             + (1|site))
anova(Al13, Al9)
anova(Al13)

Al92 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km + season
             + period
             + (1|site))
anova(Al92)

Al93 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km + season
             + (1|site),
             REML = T)
anova(Al93)

Al94 <- lmer(data = dat,
             log(Al) ~ season
             + (1|site))
anova(Al94)

Al95 <- lmer(data = dat,
             log(Al) ~ nearest.dist.km
             + (1|site))
anova(Al95)


anova(Al13, Al92)
anova(Al92, Al93)
anova(Al93, Al94)
anova(Al93, Al95)
compare_performance(Al92, Al93)

AIC(Al0)
AIC(Al1)
performance(Al93)
# Al0 <- lmer(data = dat,
#             log(Al) ~ (1|site))
# check_model(Al0)
# 
# Al1 <- lmer(data = dat,
#             log(Al) ~ location_2 + (1|site))
# 
# Al2 <- lmer(data = dat,
#             log(Al) ~ location_2 * season + (1|site))
# check_model(Al2)
# Al3 <- lmer(data = dat,
#             log(Al) ~ location_2 + season + (1|site))
# Al4 <- lmer(data = dat,
#             log(Al) ~  season + (1|site))
# check_model(Al4)
# Al5 <- lmer(data = dat,
#             log(Al) ~  location_2 * season + period + (1|site))
# check_model(Al5)
# Al6 <- lmer(data = dat,
#             log(Al) ~  location_2 + season + period + (1|site))
# check_model(Al6)
# Al7 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km + (1|site))
# Al8 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km + location_2 + (1|site))
# Al9 <- lmer(data = dat,
#             log(Al) ~ nearest.dist.km * season + (1|site))
# check_model(Al9)
# 
# Al10 <- lmer(data = dat,
#              log(Al) ~ nearest.dist.km + season + (1|site))
# Al11 <- lmer(data = dat,
#              log(Al) ~ nearest.dist.km + nearest.dist.km:season + (1|site))
# 
# anova(Al0, Al1)
# anova(Al0, Al2)
# anova(Al1, Al2)
# anova(Al2, Al3)
# anova(Al1, Al3)
# anova(Al1, Al4)
# anova(Al2, Al4)
# anova(Al4, Al3)
# anova(Al2, Al5)
# anova(Al3, Al6)
# anova(Al4, Al3)
# anova(Al0, Al7)
# anova(Al7, Al8)
# anova(Al1, Al7)
# anova(Al7, Al9)
# anova(Al2, Al9)
# anova(Al9, Al10)
# anova(Al3, Al10)
# anova(Al9, Al10)
# anova(Al11, Al10)
# anova(Al11, Al9)


Al.sum <- summary(Al93)
write.csv(Al.sum$coefficients, "Alglobe93coef.csv")

Al.seasonmeans <- lsmeans(Al93, pairwise~season, adjust = "tukey")
write.csv(Al.seasonmeans$lsmeans, "Alglobe93lsmeansseason.csv")
write.csv(Al.seasonmeans$contrAlts, "Alglobe93contrAltsseason.csv")

summary(Al10)
vif(Al10)
plot(allEffects(Al10))
plot(allEffects(Al8))
lsmeans(Al10,pairwise~season, adjust = "tukey")



check_model(Al10)
dev.print(png, "Al_checkglobe10.png", res=100, height=12, width=30, units="in")



predict.dat <- ggeffect(model = Al93,
                        terms = c("nearest.dist.km"),
                        back.transform = F,
                        type = "re") #specify both variables in terms to Alsess them together, this helps visualize an interaction.

ggplot(data = dat, aes(x = nearest.dist.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#00A8C6")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Al by Nearest Distance From Mine and Effect Trendline\n",
       y = "ln(Al) [µg/L]\n",
       x = "\nNearest Distance From Mine (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_GMdisteffectln.png", res=100, height=6, width=8, units="in")

# ggplot(data = dat, aes(x = nearest.dist.km, y = Al))+
#   geom_point(aes(fill = season), shape = 21)+
#   geom_ribbon(data = predict.dat, mapping = aes(x=x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high), fill = group), alpha = .2)+ #adds shading for error
#   geom_line(data = predict.dat, mapping = aes(x=x, y = exp(predicted), color = group))+
#   scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
#   scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
#   labs(title = "Al by Nearest Distance From Mine and Effect Trendline\n",
#        y = "Al Concentration (µg/L)\n",
#        x = "\nNearest Distance From Mine (km)",
#        fill = "season",
#        color = "season")+
#   theme_clAlsic()+
#   theme(text = element_text(family = "Avenir"),
#         plot.title = element_text(hjust = 0.5, face = "bold")
#   )
# dev.print(png, "Al_GMdistssneffect.png", res=100, height=6, width=8, units="in")



#Barplot season ----
GMssnbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Al,
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
         x = "\nseason",
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
               name.string = "Al")

GMlocationbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Al,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$location.y),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$location.y),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$location.y),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("location", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <-as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$location.y),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$location.y),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$location.y),
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
              mapping = aes(fill=location.y, y=gmean, x=location)) +
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
                    name.string = "Al")
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
                           cols = Be:Al,
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
    labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations by Location and season", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nLocation",
         shape = "",
         fill = "season") +
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
                       name.string = "Al")
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
                           cols = Be:Al,
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
              mapping = aes(fill=season, y=gmean, x=Al.factor(nearest.dist.rounded))) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = "black", mapping = aes(shape=range, y=value, x=Al.factor(nearest.dist.rounded)))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste("Globe/Miami Geometric Mean", analyte, "Concentrations"),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep=""),
         x = "\nNearest Distance To Mine (m)",
         fill = "season",
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
                analyte.string = "Al")

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
            analyte.string = "Al",
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
                  analyte.string = "Al",
                  x.string = "nearest.dist.rounded",
                  subset.string = "season",
                  subset.title.string = "season",
                  test.string = "spearman",
                  xlabel.string = "Nearest Distance To Mine (m)")

lapply(X=Contamlist,
       GMsubsetScatterFX,
       dataDF = dat,
       x.string = "nearest.dist.rounded",
       subset.string = "season",
       subset.title.string = "season",
       test.string = "spearman",
       xlabel.string = "Nearest Distance To Mine (m)")

GMsummaryStatFX <- function(dataDF, analyte.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- analyte.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Al,
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
              mapping = aes(fill=season, y=gmean, x=Al.factor(nearest.dist.rounded))) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = "black", mapping = aes(shape=range, y=value, x=Al.factor(nearest.dist.rounded)))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste("Globe/Miami Geometric Mean", analyte, "Concentrations"),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep=""),
         x = "\nNearest Distance To Mine (m)",
         fill = "season",
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
                analyte.string = "Al")

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
            analyte.string = "Al",
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
                  analyte.string = "Al",
                  x.string = "nearest.dist.rounded",
                  subset.string = "season",
                  subset.title.string = "season",
                  test.string = "spearman",
                  xlabel.string = "Nearest Distance To Mine (m)")

lapply(X=Contamlist,
       GMsubsetScatterFX,
       dataDF = dat,
       x.string = "nearest.dist.rounded",
       subset.string = "season",
       subset.title.string = "season",
       test.string = "spearman",
       xlabel.string = "Nearest Distance To Mine (m)")


GMlocation_2barplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Al,
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
                      name.string = "Al")
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
#                            cols = Be:Al,
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
#   gdat$gmean <- Al.numeric(gdat$gmean)
#   gdat$gsd <- Al.numeric(gdat$gsd)
#   gdat$count <- Al.numeric(gdat$count)
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
#     labs(title = paste("Globe/Miami Geometric Mean ", analyte, " Concentrations by Location and season", sep = ""),
#          subtitle = paste("Rooftop Harvested Rainwater - DM"),
#          y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
#          x = "\nLocation",
#          shape = "",
#          fill = "season") +
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
#                        name.string = "Al")
# lapply(X=Contamlist,
#        GMlocation_2ssnbarplotFX,
#        dataDF = dat)


m2 <- lmer(data = iw.dm.long,
           log(value) ~ analyte + season + community + analyte:community + (1|community:site))

anova(Al8, m2)
summary(Al8)
plot(allEffects(Al8))


#Tucson=====

#load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(lmerTest)
library(performance)
library(effects)
library(EnvStats)

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data
tucson <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "tucson", col_names = TRUE)
tucsondat <- iw.dm[iw.dm$community=="Tucson",]
dat <- full_join(tucsondat, tucson, by = c("site"))
dat <- dat[!is.na(dat[c("community")]),]
dat <- dat[!is.na(dat[c("ward")]),]
dat <- dat[!is.na(dat[c("location.y")]),]

dat$ward <- factor(dat$ward, levels = c("One", "Two", "Three", "Four", "Five", "Six"))

#Set up for Kriging ----
Al.dat <- aggregate(dat$Al,
                    by = list(dat$site, dat$season),
                    FUN = geoMean)
colnames(Al.dat) <- c("site", "Sampling season","GeoMean")
Al.dat.gmean <- full_join(Al.dat, tucson, by = c("site"))
Al.dat.gmean <- Al.dat.gmean[!is.na(Al.dat.gmean[c("GeoMean")]),]
write.csv(Al.dat.gmean, "tucson_ssn_Al_gmean.csv")

#BAlic TRI Analysis ----
cor.test(dat$Al, dat$dist.jensen.km, method = "spearman")
cor.test(dat$Al, dat$dist.fci.km, method = "spearman")
cor.test(dat$Al, dat$dist.afb.km, method = "spearman")
cor.test(dat$Al, dat$dist.tia.km, method = "spearman")

plot(dat$dist.jensen.km,dat$Al)
plot(dat$dist.fci.km,dat$Al)
plot(dat$dist.afb.km,log(dat$Al))
plot(dat$dist.tia.km,log(dat$Al))


afb <- lm(data=dat,
          log(dat$Al)~dist.afb.km)
summary(afb)

jensen <- lm(data=dat,
             log(dat$Al)~dist.jensen.km)
summary(jensen)
hist(jensen$residuals)
jen0 <- lmer(data = dat,
             log(Al) ~
               + (1|site),
             REML = F)
jen1 <- lmer(data = dat,
             log(Al) ~ dist.jensen.km
             + (1|site),
             REML = F)
anova(jen1, jen0)
summary(jen1)
jen2 <- lmer(data = dat,
             log(Al) ~ dist.jensen.km + season
             + (1|site),
             REML = F)
summary(jen2)

jen3 <- lmer(data = dat,
             log(Al) ~ season
             + (1|site),
             REML = F)
anova(jen2, jen1)
anova(jen2, jen3)
anova(jen1, jen3)
plot(allEffects(jen2))
fci <- lm(data=dat,
          log(dat$Al)~dist.fci.km)
summary(fci)
hist(fci$residuals)
fci0 <- lmer(data = dat,
             log(Al) ~
               + (1|site),
             REML = F)
fci1 <- lmer(data = dat,
             log(Al) ~ dist.fci.km
             + (1|site),
             REML = F)
anova(fci1, fci0)
summary(fci1)
fci2 <- lmer(data = dat,
             log(Al) ~ dist.fci.km + season
             + (1|site),
             REML = F)
summary(fci2)

fci3 <- lmer(data = dat,
             log(Al) ~ season
             + (1|site),
             REML = F)
anova(fci0, fci1)
anova(fci2, fci1)
anova(fci2, fci3)
anova(fci1, fci3)

#Al Modeling take two (with airport)----
Al1 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al1)
vif(Al1)
anova(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al2)
vif(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al3)
vif(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al4)
vif(Al4)

Al5 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + season:period
            + (1|site),
            REML=T)
summary(Al5)
vif(Al5)
anova(Al5)

Al6 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km
            + season:period
            + (1|site),
            REML=T)
summary(Al6)
anova(Al6)

Al6.5 <- lmer(data = dat,
              log(Al) ~ season+period+dist.afb.km
              + (1|site),
              REML=T)
summary(Al6.5)
anova(Al6.5)

Al7 <- lmer(data = dat,
            log(Al) ~ season+dist.afb.km
            + (1|site),
            REML=T)
summary(Al7)
anova(Al7)

Al9 <- lmer(data = dat,
            log(Al) ~ season+dist.afb.km+dist.tia.km
            + (1|site),
            REML=T)
summary(Al9)
anova(Al9)
vif(Al9)

anova(Al5, Al6)
anova(Al6, Al7)
anova(Al7, Al8)
anova(Al8, Al9)


#Al Modeling ----
ggplot(data = dat, mapping = aes(x = dist.afb.km, y = log(Al)))+
  geom_point(shape =21)+
  geom_smooth()
plot(summary(lm(log(Al)~dist.jensen.km, dat)))

Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML=T)

Al1 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al1)
vif(Al1)
anova(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km
            + ward:period + season:period + ward:season:period
            + (1|site),
            REML=F)
summary(Al2)
vif(Al2)
anova(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km
            + ward:period + season:period
            + (1|site),
            REML=F)
summary(Al3)
vif(Al3)
anova(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km
            + ward + season:period
            + (1|site),
            REML=F)
summary(Al4)
vif(Al4)
anova(Al4)

Al5 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km
            + ward
            + (1|site),
            REML=F)
summary(Al5)
vif(Al5)
anova(Al5)

Al6 <- lmer(data = dat,
            log(Al) ~ season+ward+dist.afb.km
            + (1|site),
            REML=F)
summary(Al6)
anova(Al6)

Al7 <- lmer(data = dat,
            log(Al) ~ season+dist.afb.km
            + (1|site),
            REML=T)
summary(Al7)
anova(Al7)

Al7.5 <- lmer(data = dat,
              log(Al) ~ season+dist.afb.km+dist.afb.km:ward
              + (1|site),
              REML=T)
vif(Al7.5)

Al7.6 <- lmer(data = dat,
              log(Al) ~ season+dist.afb.km:ward
              + (1|site),
              REML=T)
vif(Al7.6)
anova(Al7,Al7.5)
anova(Al7.6,Al7.5)
anova(Al7,Al7.6)
anova(Al7)
Al8 <- lmer(data = dat,
            log(Al) ~ season
            + (1|site),
            REML=F)
summary(Al8)
anova(Al8)

Al9 <- lmer(data = dat,
            log(Al) ~ season*dist.afb.km
            + (1|site),
            REML=F)
summary(Al9)
anova(Al9)
vif(Al9)
anova(Al4, Al5)
anova(Al5, Al6)
anova(Al6, Al7)
anova(Al7, Al8)
anova(Al8, Al9)
anova(Al7, Al9)
AIC(Al0)
AIC(Al1)
performance(Al7)

plot(allEffects(Al7))


predict.dat <- ggeffect(model = Al7,
                        terms = c("dist.afb.km"),
                        back.transform = F,
                        type = "re")
Al.sum <- summary(Al7)
write.csv(Al.sum$coefficients, "Al7coefT.csv")
Al.ssnmeans <- lsmeans(Al7, pairwise~season, adjust = "tukey")#
write.csv(Al.ssnmeans$lsmeans, "Al7lsmeansT.csv")
write.csv(Al.ssnmeans$contrAlts, "Al7contrAltsT.csv")



# predict.datt <- ggpredict(model = Al93,
#                  terms = c("nearest.dist.km"),
#                  back.transform = F) #specify both variables in terms to Alsess them together, this helps visualize an interaction.
ggplot(data = dat, aes(x = dist.jensen.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y =predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#4068B2")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Al by Distance From Davis-Monthan Air Force BAle\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Distance From Davis-Monthan Air Force BAle (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_Tafbeffectln.png", res=100, height=6, width=8, units="in")



# Al1 <- lmer(data = dat.ward,
#             log(Al) ~ ward + (1|site))
# 
# Al2 <- lmer(data = dat.ward,
#             log(Al) ~ ward * season + (1|site))
# summary(Al2)
# anova(Al2)
# 
# AIC(Al0)
# AIC(Al1)
# AIC(Al2)
# 
# Al1 <- lmer(data = dat.ns,
#             log(Al) ~ NS + (1|site))
# summary(Al1)
# anova(Al1)

#Al Modeling take two (with airport)----
Al1 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al1)
vif(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al2)
vif(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al3)
vif(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al4)
vif(Al4)

Al5 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
            + season:period
            + (1|site),
            REML=T)
summary(Al5)
vif(Al5)
anova(Al5)



#Al Modeling ----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML=T)

Al1 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km+
              + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al1)
vif(Al1)
anova(Al1)

Al2 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km+
              + ward:period + season:period + ward:season:period
            + (1|site),
            REML=F)
summary(Al2)
vif(Al2)
anova(Al2)

Al3 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km
            + ward:period + season:period
            + (1|site),
            REML=F)
summary(Al3)
vif(Al3)
anova(Al3)

Al4 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km
            + ward + season:period
            + (1|site),
            REML=F)
summary(Al4)
vif(Al4)
anova(Al4)

Al5 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km
            + ward
            + (1|site),
            REML=F)
summary(Al5)
anova(Al5)

Al6 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km
            + (1|site),
            REML=F)
summary(Al6)
anova(Al6)

Al7 <- lmer(data = dat,
            log(Al) ~ season+period
            + (1|site),
            REML=T)
summary(Al7)
anova(Al7)

Al7.5 <- lmer(data = dat,
              log(Al) ~ samplings
              + (1|site),
              REML=F)
summary(Al7.5)
anova(Al7.5)

Al8 <- lmer(data = dat,
            log(Al) ~ season
            + (1|site),
            REML=F)
summary(Al8)
anova(Al8)

Al9 <- lmer(data = dat,
            log(Al) ~ season*period
            + (1|site),
            REML=F)
summary(Al9)
anova(Al9)
vif(Al9)

Al11 <- lmer(data = dat,
             log(Al) ~ period
             + (1|site),
             REML=F)
summary(Al11)
anova(Al11)

anova(Al4, Al5)
anova(Al5, Al6)
anova(Al6, Al7)
anova(Al7, Al8)
anova(Al7, Al11)
anova(Al7, Al9)
AIC(Al0)
AIC(Al1)
performance(Al7)

plot(allEffects(Al7))
summary(Al7)
Al.sum <- summary(Al7)
write.csv(Al.sum$coefficients, "Al7coefT.csv")
Al.ssnmeans <- lsmeans(Al7, pairwise~season, adjust = "tukey")#
write.csv(Al.ssnmeans$lsmeans, "Al7lsmeansTssn.csv")
write.csv(Al.ssnmeans$contrAlts, "Al7contrAltsTssn.csv")

Al.permeans <- lsmeans(Al7, pairwise~period, adjust = "tukey")#
write.csv(Al.permeans$lsmeans, "Al7lsmeansTper.csv")
write.csv(Al.permeans$contrAlts, "Al7contrAltsTper.csv")

#Al.ssnmeans <- lsmeans(Al7.5, pairwise~samplings, adjust = "tukey")


# predict.datt <- ggpredict(model = Al93,
#                  terms = c("nearest.dist.km"),
#                  back.transform = F) #specify both variables in terms to Alsess them together, this helps visualize an interaction.
ggplot(data = dat, aes(x = dist.jensen.km, y = log(Al)))+
  geom_point()+
  # geom_ribbon(data = predict.dat, mapping = aes(x=x, y =predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#4068B2")+ #adds shading for error
  # geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Al by Distance From Davis-Monthan Air Force BAle\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Distance From Davis-Monthan Air Force BAle (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_Tafbeffectln.png", res=100, height=6, width=8, units="in")


# 
# 
# Al0 <- lmer(data = dat.ward,
#             log(Al) ~ (1|site))
# 
# Al1 <- lmer(data = dat.ward,
#             log(Al) ~ ward + (1|site))
# 
# Al2 <- lmer(data = dat.ward,
#             log(Al) ~ ward * season + (1|site))
# summary(Al1)
# anova(Al1)
# 
# AIC(Al0)
# AIC(Al1)
# AIC(Al2)
# 
# Al1 <- lmer(data = dat.ns,
#             log(Al) ~ NS + (1|site))
# summary(Al1)
# anova(Al1)

TwardbarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$ward),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$ward),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$ward),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("ward", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$ward),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$ward),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$ward),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("ward", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=ward, y=gmean, x=ward)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    geom_point(data = rdat.long, fill = 'black', mapping = aes(shape=range, y=value, x=ward))+
    scale_fill_manual(values = c("#d1f8be", "#f4bde7", "#f07979", "#9ebbd7", "#f4a17a", "#fdf734", "red", "black"))+
    geom_text(aes(label = paste("n =", count), x = ward), y = 0, vjust = 1.5,
              position = "identity",
              family="Avenir") +
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations by Ward", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nWard",
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
  
  dev.print(png, paste(analyte,"_IWDM_tucsonward.png"), res=150, height=7, width=8, units="in")
  
}
dat.ward<- select(dat, Be:Pb, ward)
TwardbarplotFX(dataDF = dat.ward,
               name.string = "Al")
lapply(X = Contamlist, FUN = TwardbarplotFX, dataDF=dat.ward)

TwardssnbarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$ward, dat.long$season),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$ward, dat.long$season),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$ward, dat.long$season),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
  
  names(gdat) <- c("ward", "season", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$ward, dat.long$season),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$ward, dat.long$season),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$ward, dat.long$season),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  
  names(rdat) <- c("ward", "season", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=ward)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    #geom_point(data = rdat.long, fill = 'black', mapping = aes(shape=range, y=value, x=ward))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    geom_text(aes(label = paste("n =", count), x = ward), y = 0, vjust = 1.5,
              position = "identity",
              family="Avenir") +
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations by Ward", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nWard",
         shape = "",
         fill = "season") +
    #guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    facet_grid(.~season)+
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
  
  dev.print(png, paste(analyte,"_IWDM_tucsonwardssn_nopoints.png"), res=150, height=7, width=9, units="in")
  
}
TwardssnbarplotFX(dataDF = dat,
                  name.string = "Al")
lapply(X = Contamlist, FUN = TwardssnbarplotFX, dataDF=dat.ward)

TnsbarplotFX <- function(dataDF, name.string, units.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  units <- units.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat$NS),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat$NS),
                    FUN = geoSD)
  
  gdat <- full_join(gmeans, gsds, by = c("Group.1"))
  names(gdat) <- c("NS", "gmean", "gsd")
  
  p <- ggplot(gdat,
              mapping = aes(fill=NS, y=gmean, x=NS)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    scale_fill_manual(values = c("#4068B2", "#AF5597"))+
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations by Location", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater - DM"),
         y = paste(analyte, " Concentration (",units,")\n", sep=""),
         x = "\nNorth vs. South") +
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "none",
          legend.title = element_blank())
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_tucsonNS.png"), res=150, height=6, width=7, units="in")
  
}
dat.ns<- select(dat, Be:Pb, location.y)
dat.ns$NS<- dat.ns$location.y

TnsbarplotFX(dataDF = dat.ns,
             name.string = "Al",
             units.string = "µg/L")
lapply(X = Contamlist, FUN = TnsbarplotFX, dataDF=dat.ns, units.string = "µg/L")

TssnbarplotFX <- function(dataDF, name.string){
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
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nseason",
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
  
  dev.print(png, paste(analyte,"_IWDM_Tssn.png"), res=100, height=6, width=8, units="in")
  
}
TssnbarplotFXnomax <- function(dataDF, name.string){
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
  
  rdatt <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdatt, rmed, by = c("Group.1"))
  
  names(rdat) <- c("season","min", "max","median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "max","median"))
  
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
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nseason",
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
  
  dev.print(png, paste(analyte,"_IWDM_Tssnnomax.png"), res=100, height=6, width=8, units="in")
  
}

TssnbarplotFXnomax(dataDF = dat,
                   name.string = "Al")
lapply(X=Contamlist,
       DHssnbarplotFX,
       dataDF = dat)

TperbarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$period),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$period),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$period),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("period","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$period),
                    FUN = min)
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$period),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$period),
                    FUN = median)
  rangedat <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rangedat, rmed, by = c("Group.1"))
  
  names(rdat) <- c("period","min", "max","median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "max","median"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=period, y=gmean, x=period)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+    
    scale_fill_manual(values = c("#C5D7D2","#F5D1C4"))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = period), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = period, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nSampling Period",
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
  
  dev.print(png, paste(analyte,"_IWDM_Tper.png"), res=100, height=6, width=8, units="in")
  
}

TperbarplotFX(dataDF = dat,
              name.string = "Al")

TsampbarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$samplings),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$samplings),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$samplings),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("samplings","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$samplings),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$samplings),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$samplings),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("samplings","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=samplings, y=gmean, x=samplings)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+    
    scale_fill_viridis_b()+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = samplings), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = samplings, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    labs(title = paste("Tucson Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nSamplings",
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
  
  dev.print(png, paste(analyte,"_IWDM_Tsamp.png"), res=100, height=6, width=8, units="in")
  
}

TsampbarplotFX(dataDF = dat,
               name.string = "Al")



#Hayden/Winklemann============

#load libraries
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(effects)

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load water and soil data from contaminant descriptions file, subset hayden/winkelman data only
hayden <- iw.dm[iw.dm$community=="Hayden/Winkelman",]


#Alsign water samples pre and post strike
hayden$collection_date <- substr(hayden$sample.name, 11, 18) #subset sample name to create collection date
hayden$strike <- as.numeric(hayden$collection_date) #make numeric
hayden[hayden$strike<20191013,]$strike <- 0 #Alsign pre strike values 0
hayden[hayden$strike>=20191013,]$strike <- 1 #Alsign post strike values 1
hayden$strike <- as.factor(hayden$strike) #make factor
hayden$strike <- ifelse(hayden$strike==0, "Operating", "Not Operating") #label with words
hayden$strike <- factor(hayden$strike, levels = c("Operating", "Not Operating")) #order the labels

#load hayden distance data
distance <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "hayden", col_names = TRUE)
distance <- subset(distance, select = c("site", "siteID", "distance.rounded", "location", "distance.to.tailings.round", "dist.tailings.km"))

#merge with water and soil data by site
dat <- full_join(hayden, distance, by = c("site"))


#remove rows without data
dat <- dat[!is.na(dat$community),]


dat$direction <- factor(dat$location.y, levels = c("Hayden", "Winkelman"))


dat$distance.rounded.km <- dat$distance.rounded/1000
dat <- dat[dat$site!="H222",]
#subset winter data because only winter data exists post strike
dat.winter <- dat[dat$season=="Winter",]

#correlation between tailings and smelter
stest <- rcorr(as.matrix(dat[,c(40,42)]), type = "spearman")
stest$r
stest$P
# means <- aggregate(iw.dm.long[iw.dm.long$site=="H221",]$value,
#           by=list(iw.dm.long[iw.dm.long$site=="H221",]$samplings,iw.dm.long[iw.dm.long$site=="H221",]$analyte),
#           FUN=mean)
# write.csv(means, "h221meansSoil.csv")
#Barplot Direction Strike ----
HWdirstrikebarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$direction, dat.long$strike),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$direction, dat.long$strike),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
  
  names(gdat) <- c("direction", "strike","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  
  names(rdat) <- c("direction", "strike","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=strike, y=gmean, x=direction)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = strike), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = strike, y =0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    scale_fill_manual(values = c("#AF5597", "#4068B2"))+
    labs(title = paste("Winter Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nLocation",
         shape = "",
         fill = "Smelter Operation Status") +
    #guides(fill = "none")+
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
  
  dev.print(png, paste(analyte,"_IWDM_HWdirectionstrike.png"), res=100, height=6.5, width=10, units="in")
  
}
HWdirstrikebarplotFX(dataDF = dat.winter,
                     name.string = "Al")
lapply(X=Contamlist,
       HWstrikebarplotFX,
       dataDF = hayden.dist[hayden.dist$season=="Winter",])

#Boxplot Strike ----
HWstrikeboxplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  dat <- dat.winter
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  p <- ggplot(dat.long,
              mapping = aes_string(fill="strike", y="value", x="location.y")) +
    geom_boxplot() +
    stat_boxplot(geom = "errorbar")+
    scale_fill_manual(values = c("#AF5597", "#4068B2"))+
    labs(title = paste("Winter Hayden/Winkelman ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nLocation",
         fill = "Smelter Operation Status") +
    #guides(fill = "none")+
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
  
  dev.print(png, paste(analyte,"_IWDM_HWlocationstrike.png"), res=100, height=6, width=8, units="in")
  
}
HWstrikeboxplotFX(dataDF = dat.winter,
                  name.string = "Al")

lapply(X=Contamlist,
       HWstrikebarplotFX,
       dataDF = hayden.dist[hayden.dist$season=="Winter",])


#Barplot Direction  ----
HWdirectionbarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$direction),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$direction),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("direction", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("direction", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=direction, y=gmean, x=direction)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = direction), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = direction, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    scale_fill_manual(values = c("#77B180", "#A2A9D6"))+
    labs(title = paste("Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nLocation",
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
  
  dev.print(png, paste(analyte,"_IWDM_HWlocation.png"), res=100, height=6, width=8, units="in")
  
}
HWdirectionbarplotFX(dataDF = dat,
                     name.string = "Al")
lapply(X=Contamlist,
       HWdirectionbarplotFX,
       dataDF = hayden.is.dist)


#Barplot Direction & season ----
HWdirectionssnbarplotFX <- function(dataDF, name.string){
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
                      by = list(dat.long$direction, dat.long$season),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$direction, dat.long$season),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
  
  names(gdat) <- c("direction", "season","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  
  names(rdat) <- c("direction", "season","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=direction)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+    
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    #geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = season), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = season, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    labs(title = paste("Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nLocation",
         fill = "season",         
         shape = "") +
    #guides(fill = "none")+
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
  
  dev.print(png, paste(analyte,"_IWDM_HWlocationssn_nopoints.png"), res=100, height=6, width=8, units="in")
  
}
HWdirectionssnbarplotFX(dataDF = dat,
                        name.string = "Al")

lapply(X=Contamlist,
       HWdirectionssnbarplotFX,
       dataDF = dat)

#Barplot season ----
HWssnbarplotFX <- function(dataDF, name.string){
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
    labs(title = paste("Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nseason",
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
  
  dev.print(png, paste(analyte,"_IWDM_HWssn.png"), res=100, height=6, width=8, units="in")
  
}
HWssnbarplotFX(dataDF = dat,
               name.string = "Al")
lapply(X=Contamlist,
       HWssnbarplotFX,
       dataDF = dat)


#Scatterplot West & South ----
HWsubsetScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
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
                 ylab = paste(analyte, "Concentration (mg/kg)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = direction),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#77B180", "#A2A9D6"))+
    scale_fill_manual(values = c("#77B180", "#A2A9D6"))+
    labs(title = paste(analyte, "Concentrations in Residential Soils\nby Distance From Smelter"),
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
  dev.print(png, paste(analyte, "_IS_HWscatter.jpeg", sep = ""), res=100, height=6, width=7, units="in")
  print("Great Job!")
}


HWsubsetScatterFX(dataDF = dat[dat$distance.rounded<2500,],
                  analyte.string = "Pb",
                  x.string = "distance.rounded",
                  subset.string = "direction",
                  subset.title.string = "Direction From Smelter",
                  test.string = "spearman",
                  xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = dat[dat$distance.rounded<2500,],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")




#Water=====


#Scatterplot West & South & season ----
HWsubsetssnScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
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
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = season),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste(analyte, "Concentrations in Harvested Rainwater\nby Distance From Smelter"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    facet_grid(.~direction, scales = "free_x")+
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
  dev.print(png, paste(analyte, "_IWDM_HWdirssnscatter.jpeg", sep = ""), res=100, height=6, width=12, units="in")
  print("Great Job!")
}

HWsubsetssnScatterFX(dataDF = hayden.dist[dat$distance.rounded<2500&dat$direction=="West",],
                     analyte.string = "Al",
                     x.string = "distance.rounded",
                     subset.string = "season",
                     subset.title.string = "season",
                     test.string = "spearman",
                     xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = dat[dat$distance.rounded<2500,],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")

#Scatterplot West & season ----
HWWestsubsetssnScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
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
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = season),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste(analyte, "Concentrations in Harvested Rainwater\nby Distance From Smelter"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    facet_grid(.~direction, scales = "free_x")+
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
  dev.print(png, paste(analyte, "_IWDM_HWWestdirssnscatter.jpeg", sep = ""), res=100, height=6, width=7, units="in")
  print("Great Job!")
}

HWWestsubsetssnScatterFX(dataDF = hayden.dist[hayden.dist$distance.rounded<2500&hayden.dist$direction=="West",],
                         analyte.string = "Pb",
                         x.string = "distance.rounded",
                         subset.string = "season",
                         subset.title.string = "season",
                         test.string = "spearman",
                         xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = hayden.dist[hayden.dist$distance.rounded<2500,],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")

#Pb----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)
Al1 <- lmer(data = dat,
            log(Al) ~ location.y + (1|site),
            REML = F)
Al1.5 <- lmer(data = dat,
              log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
vif(Al1.5)
Al2 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season
            + (1|site),
            REML = F)
Al3 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:dist.tailings.km + location.y:season
            + (1|site),
            REML = F)
vif(Al3)
Al4 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:season
            + (1|site),
            REML = F)
vif(Al4)
summary(Al4)
Al5 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + location.y:season + season
            + (1|site),
            REML = F)
summary(Al5)
Al5.1 <- lmer(data = dat,
              log(Al) ~ distance.rounded.km + location.y:season + season
              + (1|site),
              REML = F)
summary(Al5.1)

Al6 <- lmer(data = dat,
            log(Al) ~ location.y + distance.rounded.km + season
            + (1|site),
            REML = F)
summary(Al6)
anova(Al6, Al5.1)
compare_performance(Al6, Al5.1)
Al7 <- lmer(data = dat,
            log(Al) ~ distance.rounded.km + season
            + (1|site),
            REML = T)
Al8 <- lmer(data = dat,
            log(Al) ~ season
            + (1|site),
            REML = F)
Al9 <- lmer(data = dat,
            log(Al) ~ distance.rounded.km
            + (1|site),
            REML = F)
Al10 <- lmer(data = dat,
             log(Al) ~ dist.tailings.km
             + (1|site),
             REML = F)
Al11 <- lmer(data = dat,
             log(Al) ~ season+location.y
             + (1|site),
             REML = F)
Al12 <- lmer(data = dat,
             log(Al) ~ distance.rounded.km + season + distance.rounded.km:location.y
             + (1|site),
             REML = F)
anova(Al11, Al1)
anova(Al1, Al8)
anova(Al11, Al8)
anova(Al3, Al4)
anova(Al4, Al5)
anova(Al5, Al6)
anova(Al6, Al7)
anova(Al6, Al11)
anova(Al7, Al8)
anova(Al7, Al9)
anova(Al7, Al11)
anova(Al6, Al12)
vif(Al6)
anova(Al6)
anova(Al7)
compare_performance(Al6, Al7)

AIC(Al0)
AIC(Al1.5)
performance(Al7)

ggplot(data = dat, aes(x = dist.tailings.km, y = Al))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")

check_model(Al7)
dev.print(png, "Al_checkhayden7.png", res=100, height=12, width=30, units="in")

plot(allEffects(Al7))
Al.sum <- summary(Al7)
write.csv(Al.sum$coefficients, "Al7coefHW.csv")
Al.ssnmeans <- lsmeans(Al7, pairwise~season, adjust = "tukey")#
write.csv(Al.ssnmeans$lsmeans, "Al7lsmeansssnHW.csv")
write.csv(Al.ssnmeans$contrAlts, "Al7contrAltsssnHW.csv")

aggregate(dat$Al,
          by = list(dat$period),
          FUN = geoMean)

predict.dat <- ggeffect(model = Al7,
                        terms = c("distance.rounded.km"),
                        back.transform = F,
                        type = "re")

ggplot(data = dat, aes(x = distance.rounded.km, y = log(Al)))+
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
dev.print(png, "Al_HWdisteffectln.png", res=100, height=6, width=8, units="in")


#Al----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)
Al1 <- lmer(data = dat,
            log(Al) ~ location.y + (1|site),
            REML = F)
Al1.5 <- lmer(data = dat,
              log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
Al2 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season
            + (1|site),
            REML = F)
Al3 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:dist.tailings.km + location.y:season
            + (1|site),
            REML = F)
Al4 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:season
            + (1|site),
            REML = F)
Al5lme <- lmer(data = dat,
               log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
               + (1|site),
               REML = F)
Al6 <- lmer(data = dat,
            log(Al) ~ location.y+distance.rounded.km + season
            + (1|site),
            REML = F)
Al7 <- lmer(data = dat,
            log(Al) ~ distance.rounded.km + season
            + (1|site),
            REML = F)
Al8 <- lmer(data = dat,
            log(Al) ~ season
            + (1|site),
            REML = F)
Al9 <- lmer(data = dat,
            log(Al) ~ distance.rounded.km
            + (1|site),
            REML = F)
Al10 <- lmer(data = dat,
             log(Al) ~ dist.tailings.km
             + (1|site),
             REML = F)
Al11 <- lmer(data = dat,
             log(Al) ~ season+location.y
             + (1|site),
             REML = F)
Al12 <- lmer(data = dat,
             log(Al) ~ distance.rounded.km+dist.tailings.km
             + (1|site),
             REML = F)
check_model(Al2)
vif(Al4)

anova(Al11, Al1)
anova(Al1, Al8)
anova(Al11, Al8)
anova(Al3, Al4)
anova(Al4, Al5)
anova(Al5, Al6)
anova(Al6, Al7)
anova(Al5, Al8)
anova(Al5, Al3)
anova(Al5, Al7)
anova(Al6, Al8)
anova(Al6, Al3)
anova(Al6, Al7)
anova(Al6, Al11)
anova(Al6)
anova(Al1, Al0)
anova(Al8, Al0)
anova(Al9, Al0)
anova(Al10, Al0)

Al0 <- lm(data = dat,
          log(Al) ~ 1)

Al1.5 <- lm(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
            + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km)
vif(Al1.5)

Al2 <- lm(data = dat,
          log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
          + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season)
vif(Al2)

Al3 <- lm(data = dat,
          log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
          + location.y:dist.tailings.km + location.y:season)
vif(Al3)
Al4 <- lm(data = dat,
          log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season
          + location.y:season)
vif(Al4)
summary(Al4)

Al5.1 <- lm(data = dat,
            log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season + dist.tailings.km:location.y)
vif(Al5.1)
summary(Al5.1)


Al5 <- lm(data = dat,
          log(Al) ~ location.y+distance.rounded.km + dist.tailings.km + season)
summary(Al5)
Al6 <- lm(data = dat,
          log(Al) ~ location.y+distance.rounded.km + season)
summary(Al6)

compare_performance(Al5, Al6)
anova(Al5, Al6)

Al7 <- lm(data = dat,
          log(Al) ~ distance.rounded.km + season)
summary(Al7)
compare_performance(Al6, Al7)

Al8 <- lm(data = dat,
          log(Al) ~ location.y + dist.tailings.km + season)
summary(Al8)
compare_performance(Al5, Al7)

performance(Al0)
performance(Al1.5)
performance(Al6)

ggplot(data = dat, aes(x = dist.tailings.km, y = Al))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")

check_model(Al6)
dev.print(png, "Al_checkhayden6.png", res=100, height=12, width=30, units="in")


plot(allEffects(Al6))
Al.sum <- summary(Al6)
write.csv(Al.sum$coefficients, "Al6coefHW.csv")
Al.ssnmeans <- lsmeans(Al6, pairwise~season, adjust = "tukey")#
write.csv(Al.ssnmeans$lsmeans, "Al6lsmeansssnHW.csv")
write.csv(Al.ssnmeans$contrAlts, "Al6contrAltsssnHW.csv")

Al.locmeans <- lsmeans(Al6, pairwise~location, adjust = "tukey")#
write.csv(Al.locmeans$lsmeans, "Al6lsmeanslocHW.csv")
write.csv(Al.locmeans$contrAlts, "Al6contrAltslocHW.csv")

predict.dat <- ggeffect(model = Al6,
                        terms = c("distance.rounded.km"),
                        back.transform = F)

ggplot(data = dat, aes(x = distance.rounded.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
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
dev.print(png, "Al_HWdisteffectln.png", res=100, height=6, width=8, units="in")


summary(lm(Al ~ dist.tailings.km, data = dat))
anova()
plot(dat$dist.tailings.km, dat$Al)

#Strike Al ----
dat.winter.long <- pivot_longer(data = dat.winter,
                                cols = Be:Pb,
                                values_to = "value",
                                names_to = "analyte")
aggregate(dat.winter.long$value,
          by = list(dat.winter.long$strike,dat.winter.long$analyte),
          FUN = geoMean)

dat$Alln <- log(dat$Al)
aggregate(dat$Alln,
          by = list(dat$location.y, dat$strike),
          FUN = mean)



Als1 <- lmer(data = dat.winter,
             log(Al) ~ strike*location.y*distance.rounded.km
             + (1|site),
             REML = F)
summary(Als1)
vif(Als1)


strikeAl1 <- lm(data = dat.winter,
                log(Al) ~ strike*location.y*distance.rounded.km)
summary(strikeAl1)
vif(strikeAl1)
anova(strikeAl1)

strikeAl2 <- lm(data = dat.winter,
                log(Al) ~ strike+location.y+distance.rounded.km+strike:location.y+strike:distance.rounded.km+location.y:distance.rounded.km)
summary(strikeAl2)
vif(strikeAl2)
anova(strikeAl2)

strikeAl3 <- lm(data = dat.winter,
                log(Al) ~ strike+location.y+distance.rounded.km+strike:location.y+strike:distance.rounded.km)
summary(strikeAl3)
vif(strikeAl3)
anova(strikeAl3)

strikeAl4 <- lm(data = dat.winter,
                log(Al) ~ strike+location.y+distance.rounded.km+strike:location.y)
summary(strikeAl4)
vif(strikeAl4)
anova(strikeAl4)

strikeAl5 <- lm(data = dat.winter,
                log(Al) ~ strike+location.y+distance.rounded.km)
summary(strikeAl5)
vif(strikeAl5)
anova(strikeAl5)

strikeAl6 <- lm(data = dat.winter,
                log(Al) ~ location.y+distance.rounded.km)
summary(strikeAl6)
vif(strikeAl6)
anova(strikeAl6)

anova(strikeAl1, strikeAl2)
anova(strikeAl2, strikeAl3)
anova(strikeAl3, strikeAl4)
anova(strikeAl4, strikeAl5)
anova(strikeAl6, strikeAl5)

t.test(log(dat$Al)~dat$strike)
t.test(log(dat[dat$location.y=="Hayden",]$Al)~dat[dat$location.y=="Hayden",]$strike)
t.test(log(dat[dat$location.y=="Winkelman",]$Al)~dat[dat$location.y=="Winkelman",]$strike)

#Strike Al ----
dat$Alln <- log(dat$Al)
aggregate(dat$Al,
          by = list(dat$location, dat$strike),
          FUN = geoMean)
Als1 <- lmer(data = dat.winter,
             log(Al) ~ strike*distance.rounded.km
             + (1|site),
             REML = F)
vif(Als1)
anova(Als1)

Als2 <- lmer(data = dat.winter,
             log(Al) ~ strike+distance.rounded.km
             + (1|site),
             REML = F)
summary(Als2)
anova(Als2)

Als3 <- lmer(data = dat.winter,
             log(Al) ~ distance.rounded.km
             + (1|site),
             REML = F)

anova(Als2, Als3)

t.test(log(dat$Al)~dat$strike)
t.test(log(dat[dat$location.y=="Hayden",]$Al)~dat[dat$location.y=="Hayden",]$strike)
t.test(log(dat[dat$location.y=="Winkelman",]$Al)~dat[dat$location.y=="Winkelman",]$strike)

Allm <- lm(data = dat.winter,
           log(Al)~strike)
summary(Allm)



Als2 <- lmer(data = dat.winter,
             log(Al) ~ strike+distance.rounded.km+location.y
             + strike:distance.rounded.km + strike:location.y + distance.rounded.km:location.y
             + (1|site),
             REML = F)
Als3 <- lmer(data = dat.winter,
             log(Al) ~ strike+distance.rounded.km+location.y
             + strike:distance.rounded.km + strike:location.y
             + (1|site),
             REML = F)
Als4 <- lmer(data = dat.winter,
             log(Al) ~ strike+distance.rounded.km+location.y
             + strike:location.y
             + (1|site),
             REML = F)
Als5 <- lmer(data = dat.winter,
             log(Al) ~ strike+distance.rounded.km+location.y
             + (1|site),
             REML = F)
Als6 <- lmer(data = dat.winter,
             log(Al) ~ strike+location.y
             + (1|site),
             REML = F)
Als7 <- lmer(data = dat.winter,
             log(Al) ~ strike
             + (1|site),
             REML = F)
Als8 <- lmer(data = dat.winter,
             log(Al) ~
               + (1|site),
             REML = F)
Als9 <- lmer(data = dat.winter,
             log(Al) ~ location.y
             + (1|site),
             REML = F)

anova(Als3, Als4)
anova(Als4, Als5)
anova(Als5, Als6)
anova(Als6, Als7)
anova(Als7, Als8)
anova(Als6, Als8)
anova(Als6, Als9)

summary(Als7)
test <- lm(log(Al)~location.y+strike, data = dat.winter)
summary(test)
t.test(log(dat$Al)~dat$strike)


m1 <- lmer(data = dat.winter.long,
           log(value) ~ analyte*strike*location.y
           + (1|site),
           REML = F)
m2 <- lmer(data = dat.winter.long,
           log(value) ~ analyte+strike+location.y
           + analyte:strike + analyte:location.y + strike:location.y
           + (1|site),
           REML = F)
m3 <- lmer(data = dat.winter.long,
           log(value) ~ strike+location.y
           + analyte:strike + analyte:location.y + strike:location.y
           + (1|site),
           REML = F)
m4 <- lmer(data = dat.winter.long,
           log(value) ~ strike+location.y
           + analyte:location.y + strike:location.y
           + (1|site),
           REML = F)
m5 <- lmer(data = dat.winter.long,
           log(value) ~ strike
           + analyte:location.y + strike:location.y
           + (1|site),
           REML = F)

m5.5 <- lmer(data = dat.winter.long,
             log(value) ~ analyte:location.y + strike:location.y
             + (1|site),
             REML = F)

m6 <- lmer(data = dat.winter.long,
           log(value) ~ strike
           + analyte:location.y
           + (1|site),
           REML = F)

m7 <- lmer(data = dat.winter.long,
           log(value) ~ analyte:location.y
           + (1|site),
           REML = F)

m8 <- lmer(data = dat.winter.long,
           log(value) ~ strike
           + (1|site),
           REML = F)

step1 <- lmer(data = dat.winter.long,
              log(value) ~ analyte + strike + location.y + (1 | site) + analyte:strike + 
                analyte:location.y + strike:location.y,
              REML = F)
vif(step1)

anova(m1,m2)
anova(m2,m3)
anova(m3,m4)
anova(m4,m5)
anova(m5,m6)
anova(m5, m5.5)
compare_performance(m5, m5.5)
anova(m5,m6)
anova(m6,m7)
anova(m5,m7)


step(m4,reduce.random = F, )

plot(allEffects(m5))
summary(m5)
# 
# 
# 
# Al0 <- lmer(data = dat,
#             log(Al) ~ (1|site),
#             REML = F)
# Al1 <- lmer(data = dat,
#             log(Al) ~ location + (1|site),
#             REML = F)
# Al2 <- lmer(data = dat,
#             log(Al) ~ location + edist.rounded.km + wdist.rounded.km + (1|site),
#             REML = F)
# check_model(Al2)
# vif(Al2)
# Al3 <- lmer(data = dat,
#             log(Al) ~ location + edist.rounded.km + (1|site),
#             REML = F)
# check_model(Al3)
# 
# 
# 
# #viz data distributions and natural log transformations
# hist(iw.winter$Pb) #nonnormal
# hist(log(iw.winter$Pb)) #normal
# 
# #viz relationships
# boxplot(iw.winter$Pb~iw.winter$strike+iw.winter$direction) #not what I expected to see, higher medians post strike
# 
# #calculate means
# aggregate(iw.winter$Pb,
#           by = list(iw.winter$strike, iw.winter$direction),
#           FUN = mean) #means in the south increAle
# 
# aggregate(iw.winter[iw.winter$sample.name!="H203IWA31-20191203",]$Pb,
#           by = list(iw.winter[iw.winter$sample.name!="H203IWA31-20191203",]$strike, iw.winter[iw.winter$sample.name!="H203IWA31-20191203",]$direction),
#           FUN = mean) #means in the south increAle due to one sample from H203
# 
# Pb1 <- lmer(data = iw.winter,
#             Pb ~ strike*direction + (1|site))
# summary(Pb1)
# anova(Pb1)
# 
# Pb2 <- lmer(data = iw.winter,
#             log(Pb) ~ strike*direction + (1|site))
# summary(Pb2)
# anova(Pb2)
# Pb3 <- lmer(data = iw.winter,
#             log(Pb) ~ strike+direction + (1|site))
# summary(Pb3)
# 
# Pb4 <- lmer(data = iw.winter,
#             log(Pb) ~ direction + (1|site))
# summary(Pb4)
# 
# Pb5 <- lmer(data = iw.winter,
#             log(Pb) ~ strike + (1|site))
# summary(Pb5)
# 
# AIC(Pb1)
# AIC(Pb2)
# AIC(Pb3)
# AIC(Pb4)
# AIC(Pb5)
# performance(Pb2)
# check_model(Pb2)
# 
# plot(allEffects(Pb2))
# 
# #viz data distributions and natural log transformations
# hist(iw.dist$Pb) #nonnormal
# hist(log(iw.dist$Pb)) #normal
# 
# #viz relationships
# boxplot(iw.dist$Pb~iw.dist$direction) #not what I expected to see, higher medians post strike
# 
# #calculate means
# aggregate(iw.dist$Pb,
#           by = list(iw.dist$direction),
#           FUN = mean) #means in the south increAle
# 
# Pb1 <- lmer(data = iw.dist,
#             log(Pb) ~ (1|site))
# summary(Pb1)
# Pb2 <- lmer(data = iw.dist,
#             log(Pb) ~ direction*season + (1|site))
# summary(Pb2)
# Pb3 <- lmer(data = iw.dist,
#             log(Pb) ~ direction+season + (1|site))
# summary(Pb3)
# Pb4 <- lmer(data = iw.dist,
#             log(Pb) ~ direction + (1|site))
# summary(Pb4)
# Pb5 <- lmer(data = iw.dist,
#             log(Pb) ~ season + (1|site))
# summary(Pb5)
# AIC(Pb1)
# AIC(Pb2)
# AIC(Pb3)
# AIC(Pb4)
# AIC(Pb5)
# 
# 
# anova(Pb2, Pb3)
# lrt(Pb1, Pb2)
# plot(allEffects(Pb3))
# 
# 
# 
# Pb1 <- lmer(data = iw.dist,
#             log(Pb) ~ direction + season + (1|site))
# summary(Pb1)
# Pb2 <- lmer(data = iw.dist,
#             log(Pb) ~ direction + (1|site))
# summary(Pb2)
# Pb3 <- lmer(data = iw.dist,
#             log(Pb) ~ season + (1|site))
# summary(Pb3)
# Pb4 <- lmer(data = iw.dist,
#             log(Pb) ~ direction + samplings + (1|site))
# summary(Pb4)
# AIC(Pb1)
# AIC(Pb2)
# AIC(Pb3)
# AIC(Pb4)
# 
# 
# ggplot(data=iw.dist,
#        mapping=aes(x=direction, y = Pb)) +
#   geom_boxplot(fill = "#95CACA") +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Pb Concentration (µg/L)\n",
#        x = "\nDirection From Smelter",
#        #fill = "Relation to \nStart of Strike",
#        title = "Harvested Rainwater Pb Concentrations \nin Relation to the AlARCO Smelter") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   #scale_fill_manual(values = c("#AF5597","#4670B4"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         #legend.position = "right",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = 0, face = "italic"))
# dev.print(png, "HW_Pb_direction.png", res=200, height=7, width=7, units="in")
# 
# ggscatter(iw.dist[iw.dist$direction=="West",], x = "distance.rounded", y = "Pb",
#                   add = "reg.line",
#                   conf.int = TRUE,
#                   cor.coef = FALSE,
#                   cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#                   ylab = "Pb Concentration (µg/L)",
#                   xlab = "Distance From Smelter (m)",
#                   cor.coef.size = 6, point = FALSE) +
#   stat_cor(method="spearman",
#            aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(size = 2.5, fill = "blue", color = "black", shape = 21) +
#   labs(title = "Pb Concentrations in Harvested Rainwater\nby Distance From AlARCO Smelter\nFor Sites West of Smelter") +
#   theme_bw() +
#   theme(text = element_text(size=10, family = "Avenir"),
#         axis.line.y = element_line(),
#         axis.line.x = element_line(),
#         panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         axis.ticks = element_blank())
# ggscatter(iw.dist, x = "distance.rounded", y = "Pb", color = "direction",
#           add = "reg.line",
#           conf.int = TRUE,
#           #cor.coef = FALSE,
#           cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#           ylab = "Pb Concentration (µg/L)",
#           xlab = "Distance From Smelter (m)",
#           cor.coef.size = 6, point = FALSE) +
#   stat_cor(method="spearman",
#            aes(color = direction, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(mapping = aes(fill = direction), size = 2.5, color = "black", shape = 21) +
#   labs(title = "Pb Concentrations in Harvested Rainwater\nby Distance From AlARCO Smelter\nFor Sites West of Smelter",
#        color = "direction",
#        fill = "direction") +
#   theme_bw() +
#   theme(text = element_text(size=10, family = "Avenir"),
#         axis.line.y = element_line(),
#         axis.line.x = element_line(),
#         panel.grid = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         axis.ticks = element_blank())
# dev.print(png, "rainwater-Pb.jpeg", res=200, height=5, width=6.5, units="in")
# 
# ggplot(data=iw.dist,
#        mapping=aes(x=season, y = Pb, fill = direction)) +
#   geom_boxplot() +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Pb Concentration (µg/L)\n",
#        x = "\nseason",
#        fill = "Direction",
#        title = "Rooftop Harvested Rainwater Pb Concentrations \nin Relation to the AlARCO Smelter") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   scale_fill_manual(values = c("#735746","#95CACA"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = 0, face = "italic"))
# dev.print(png, "HW_Pb_directionssn.png", res=100, height=7, width=7, units="in")
# 
# 
# ggplot(data=iw.winter,
#        mapping=aes(x=direction, y = Pb, fill=strike)) +
#   geom_boxplot() +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Pb Concentration (µg/L)\n",
#        x = "\nDirection From Smelter",
#        fill = "Relation to Start of Strike",
#        title = "Winter Harvested Rainwater Pb Concentrations \nin Relation to the AlARCO Smelter and Strike",
#        caption = "The strike began October 13, 2019") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   scale_fill_manual(values = c("#AF5597","#4670B4"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = -.2, face = "italic"))
# dev.print(png, "HW_Pb_winterstrike_direction.png", res=100, height=7, width=7, units="in")
# 
# 
# 
# ggscatter(iw.dist[iw.dist$distance.rounded<2000,], x = "distance.rounded", y = "Pb", color = "direction",
#           add = "reg.line",
#           conf.int = TRUE,
#           #cor.coef = FALSE,
#           cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#           ylab = "Pb Concentration (µg/L)\n",
#           xlab = "\nDistance From Smelter (m)",
#           cor.coef.size = 6, point = FALSE) +
#   stat_cor(method="spearman",
#            aes(color = direction, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(mapping = aes(fill = direction), size = 2.5, color = "black", shape = 21) +
#   labs(title = "Pb Concentrations in Harvested Rainwater\nby Distance From AlARCO Smelter\nFor Sites West of Smelter",
#        color = "direction",
#        fill = "direction") +
#   theme_bw() +
#   theme(text = element_text(size=13, family = "Avenir"),
#         axis.line.y = element_line(),
#         axis.line.x = element_line(),
#         panel.grid = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         axis.ticks = element_blank())+
#   facet_grid(.~season)
# dev.print(png, "rainwater-Pbssn.jpeg", res=150, height=6, width=12, units="in")



# #viz data distributions and natural log transformations
# hist(iw.winter$Al) #nonnormal
# hist(log(iw.winter$Al)) #normal
# 
# #viz relationships
# boxplot(iw.winter$Al~iw.winter$strike+iw.winter$direction)
# 
# #calculate means
# aggregate(iw.winter$Al,
#           by = list(iw.winter$strike, iw.winter$direction),
#           FUN = mean)
# 
# Al1 <- lmer(data = iw.winter,
#      Al ~ strike*direction + (1|site))
# summary(Al1)
# 
# Al2 <- lmer(data = iw.winter,
#             log(Al) ~ strike*direction + (1|site))
# summary(Al2)
# anova(Al2)
# Al3 <- lmer(data = iw.winter,
#             log(Al) ~ strike+direction + (1|site))
# summary(Al3)
# 
# Al4 <- lmer(data = iw.winter,
#             log(Al) ~ direction + (1|site))
# summary(Al4)
# 
# Al5 <- lmer(data = iw.winter,
#             log(Al) ~ strike + (1|site))
# summary(Al5)
# 
# AIC(Al1)
# AIC(Al2)
# AIC(Al3)
# AIC(Al4)
# AIC(Al5)
# performance(Al2)
# check_model(Al2)
# 
# plot(allEffects(Al2))
# 
# Al1 <- lmer(data = iw.dist,
#             log(Al) ~ direction + season + (1|site))
# summary(Al1)
# Al2 <- lmer(data = iw.dist,
#             log(Al) ~ direction + (1|site))
# summary(Al2)
# Al3 <- lmer(data = iw.dist,
#             log(Al) ~ season + (1|site))
# summary(Al3)
# Al4 <- lmer(data = iw.dist,
#             log(Al) ~ direction + samplings + (1|site))
# summary(Al4)
# AIC(Al1)
# AIC(Al2)
# AIC(Al3)
# AIC(Al4)
# 
# plot(allEffects(Al1))
# 
# 
# ggplot(data=iw.winter,
#        mapping=aes(x=direction, y = Al, fill=strike)) +
#   geom_boxplot() +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Al Concentration (µg/L)\n",
#        x = "\nDirection From Smelter",
#        fill = "Relation to Start of Strike",
#        title = "Winter Harvested Rainwater Al Concentrations \nin Relation to the AlARCO Smelter and Strike",
#        caption = "The strike began October 13, 2019") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   scale_fill_manual(values = c("#AF5597","#4670B4"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = -.2, face = "italic"))
# dev.print(png, "HW_Al_winterstrike_direction.png", res=200, height=7, width=7, units="in")
# 
# ggplot(data=iw.dist,
#        mapping=aes(x=season, y = Al, fill = direction)) +
#   geom_boxplot() +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Al Concentration (µg/L)\n",
#        x = "\nseason",
#        fill = "Direction",
#        title = "Rooftop Harvested Rainwater Al Concentrations \nin Relation to the AlARCO Smelter") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   scale_fill_manual(values = c("#735746","#95CACA"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = 0, face = "italic"))
# dev.print(png, "HW_Al_directionssn.png", res=100, height=7, width=7, units="in")
# 
# ggscatter(iw.dist[iw.dist$direction=="West",], x = "distance.rounded", y = "Al",
#           add = "reg.line",
#           conf.int = TRUE,
#           cor.coef = FALSE,
#           cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#           ylab = "Al Concentration (µg/L)\n",
#           xlab = "\nDistance From Smelter (m)",
#           cor.coef.size = 6, point = FALSE) +
#   stat_cor(method="spearman",
#            aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(size = 2.5, fill = "blue", color = "black", shape = 21) +
#   labs(title = "Al Concentrations in Harvested Rainwater\nby Distance From AlARCO Smelter\nFor Sites West of Smelter") +
#   theme_bw() +
#   theme(text = element_text(size=10, family = "Avenir"),
#         axis.line.y = element_line(),
#         axis.line.x = element_line(),
#         panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         axis.ticks = element_blank())
# dev.print(png, "rainwater-Al.jpeg", res=200, height=5, width=6.5, units="in")
# 
# ggscatter(iw.dist[iw.dist$distance.rounded<2000,], x = "distance.rounded", y = "Al", color = "direction",
#           add = "reg.line",
#           conf.int = TRUE,
#           #cor.coef = FALSE,
#           cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#           ylab = "Al Concentration (µg/L)\n",
#           xlab = "\nDistance From Smelter (m)",
#           cor.coef.size = 6, point = FALSE) +
#   stat_cor(method="spearman",
#            aes(color = direction, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(mapping = aes(fill = direction), size = 2.5, color = "black", shape = 21) +
#   labs(title = "Al Concentrations in Harvested Rainwater\nby Distance From AlARCO Smelter\nFor Sites West of Smelter",
#        color = "direction",
#        fill = "direction") +
#   theme_bw() +
#   theme(text = element_text(size=13, family = "Avenir"),
#         axis.line.y = element_line(),
#         axis.line.x = element_line(),
#         panel.grid = element_blank(),
#         legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         axis.ticks = element_blank())+
#   facet_grid(.~season)
# dev.print(png, "rainwater-Alssn.jpeg", res=150, height=6, width=12, units="in")
# 
# 
# 
# 
# 
# 
# #Soil
# ggscatter(is.dist[is.dist$direction=="West",], x = "distance.rounded", y = "Pb",
#           add = "reg.line",
#           conf.int = TRUE,
#           cor.coef = FALSE,
#           cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#           ylab = "Pb Concentration (mg/kg)",
#           xlab = "Distance From Smelter (m)",
#           cor.coef.size = 6) +
#   stat_cor(method="spearman",
#            aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(size = 2.5, fill = "brown", color = "black", shape = 21) +
#   labs(title = "Pb Concentrations in Residential Soil\nand Distance From AlARCO Smelter\nFor Sites West of Smelter") +
#   theme_bw() +
#   theme(text = element_text(size=10, family = "Avenir"),
#         axis.line.y = element_line(),
#         axis.line.x = element_line(),
#         panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         legend.position = "top",
#         axis.ticks = element_blank())
# dev.print(jpeg, "Pbsoil_corr.jpeg", res=200, height=5, width=6.5, units="in")
# 
# ggscatter(is.dist[is.dist$direction=="West",], x = "distance.rounded", y = "Al",
#           add = "reg.line",
#           conf.int = TRUE,
#           cor.coef = FALSE,
#           cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
#           ylab = "Al Concentration (mg/kg)\n",
#           xlab = "\nDistance From Smelter (m)",
#           cor.coef.size = 6) +
#   stat_cor(method="spearman",
#            aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#            label.x.npc = "middle",
#            family = "Avenir",
#            size = 6) +
#   geom_point(size = 2.5, fill = "brown", color = "black", shape = 21) +
#   labs(title = "Al Concentrations in Residential Soil\nand Distance From AlARCO Smelter\nFor Sites West of Smelter") +
#   theme_bw() +
#   theme(text = element_text(size=10, family = "Avenir"),
#         axis.line = element_blank(),
#         panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 18),
#         plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
#         plot.caption = element_text(size = 13, face="italic", hjust = 0),
#         axis.text = element_text(size=15, color = "black", face = "bold"),
#         axis.title = element_text(size=15, color = "black", face = "bold"),
#         legend.position = "top")
# dev.print(jpeg, "Alsoil_corr.jpeg", res=200, height=5, width=6.5, units="in")
# 
# ggplot(data=is.dist,
#        mapping=aes(x=direction, y = Al)) +
#   geom_boxplot(fill = "#735746") +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Al Concentration (mg/kg)\n",
#        x = "\nDirection From Smelter",
#        #fill = "Relation to \nStart of Strike",
#        title = "Residential Soil Al Concentrations \nin Relation to the AlARCO Smelter") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   #scale_fill_manual(values = c("#AF5597","#4670B4"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         #legend.position = "right",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = 0, face = "italic"))
# dev.print(png, "RS_Al_direction.png", res=200, height=7, width=7, units="in")
# 
# ggplot(data=is.dist,
#        mapping=aes(x=direction, y = Pb)) +
#   geom_boxplot(fill = "#735746") +
#   stat_boxplot(geom = 'errorbar') +
#   labs(y = "Pb Concentration (mg/kg)\n",
#        x = "\nDirection From Smelter",
#        #fill = "Relation to \nStart of Strike",
#        title = "Residential Soil Pb Concentrations \nin Relation to the AlARCO Smelter") +
#   #scale_y_continuous(limits = c(0, 12), expand = c(0, 0))+
#   # coord_cartesian(ylim = 10)+
#   #scale_fill_manual(values = c("#AF5597","#4670B4"))+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         text = element_text(size=15, family = "Avenir"),
#         axis.ticks.x = element_blank(),
#         #legend.position = "right",
#         plot.title = element_text(size = 17, hjust = .5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5, face = "italic"),
#         plot.caption = element_text(hjust = 0, face = "italic"))
# dev.print(png, "RS_Pb_direction.png", res=200, height=7, width=7, units="in")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




#ctrl====
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

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data
ic.dm <- read_xlsx("data/data_processing/IC_DMTM_Y23.xlsx", sheet = "Corrected - DM", col_names = TRUE)
ic.dm$community <- "AZ Background"

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
dat <- bind_rows(iw.dm, ic.dm)

#define factors
dat$community <- factor(dat$community, levels = c("AZ Background", "Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))
#dat$community <- factor(dat$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson", "Chiricahua", "Grand Canyon", "Oliver Knoll", "Organ Pipe", "Petrified Forest"))
dat$samplings <- factor(dat$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
dat$period <- factor(dat$period, levels = c("First", "Last"))
dat$season <- factor(dat$season, levels = c("Winter", "Monsoon"))

dat.long <- pivot_longer(data = dat,
                         cols = Be:Pb,
                         names_to = "analyte",
                         values_to = "value")

#summary stats community ----
dat %>% count(community)

gmean <- aggregate(dat$Pb,
                   by = list(dat$community),
                   FUN = geoMean)
colnames(gmean) <- c("community", "gmean")
gmean$gmean <- formatC(signif(gmean$gmean,digits=4), digits=4,format="fg", flag="#")

gsd <- aggregate(dat$Pb,
                 by = list(dat$community),
                 FUN = geoSD)
colnames(gsd) <- c("community", "gsd")
gsd$gsd <- formatC(signif(gsd$gsd,digits=2), digits=2,format="fg", flag="#")

median <- aggregate(dat$Pb,
                    by = list(dat$community),
                    FUN = median)
colnames(median) <- c("community", "median")

min <- aggregate(dat$Pb,
                 by = list(dat$community),
                 FUN = min)
colnames(min) <- c("community", "min")

max <- aggregate(dat$Pb,
                 by = list(dat$community),
                 FUN = max)
colnames(max) <- c("community", "max")

a <- full_join(gmean, gsd, by = c("community"))
b <- full_join(a, median, by = c("community"))
c <- full_join(b, min, by = c("community"))
d <- full_join(c, max, by = c("community"))

write.csv(d, "PbStats.csv")

count <- dat %>% count(samplings, community)

gmean <- aggregate(dat$Pb,
                   by = list(dat$community, dat$samplings),
                   FUN = geoMean)
colnames(gmean) <- c("community", "samplings", "gmean")
gmean$gmean <- formatC(signif(gmean$gmean,digits=4), digits=4,format="fg", flag="#")

gsd <- aggregate(dat$Pb,
                 by = list(dat$community, dat$samplings),
                 FUN = geoSD)
colnames(gsd) <- c("community", "samplings", "gsd")
gsd$gsd <- formatC(signif(gsd$gsd,digits=2), digits=2,format="fg", flag="#")

median <- aggregate(dat$Pb,
                    by = list(dat$community, dat$samplings),
                    FUN = median)
colnames(median) <- c("community", "samplings", "median")

min <- aggregate(dat$Pb,
                 by = list(dat$community, dat$samplings),
                 FUN = min)
colnames(min) <- c("community", "samplings", "min")

max <- aggregate(dat$Pb,
                 by = list(dat$community, dat$samplings),
                 FUN = max)
colnames(max) <- c("community", "samplings", "max")

a <- full_join(gmean, gsd, by = c("community", "samplings"))
b <- full_join(a, median, by = c("community", "samplings"))
c <- full_join(b, min, by = c("community", "samplings"))
d <- full_join(c, max, by = c("community", "samplings"))
e <- full_join(d, count, by = c("community", "samplings"))
write.csv(e, "PbStatsSamp.csv")


#graphing
dat$community_short <- as.character(dat$community)

dat$community_short <- as.character(dat$community)
dat[dat$community_short=="Dewey-Humboldt",]$community_short <- "DH"
dat[dat$community_short=="Globe/Miami",]$community_short <- "GM"
dat[dat$community_short=="Hayden/Winkelman",]$community_short <- "HW"
dat[dat$community_short=="Tucson",]$community_short <- "TU"
dat[dat$community_short=="AZ-Background",]$community_short <- "AZ"

dat$community_short <- factor(dat$community_short, levels = c("DH", "GM", "HW", "TU", "CM", "GC", "OK", "OP", "PF"))
summary(dat$community_short)

ggplot(data = dat,
       mapping = aes(x=community_short,
                     y=Pb))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
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
                     y=Pb))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  facet_wrap(.~samplings)+
  labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  coord_cartesian(ylim = c(0, 40)) +
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(jpeg, "Fig4.jpeg", res=500, height=140, width=140, units="mm")



#detection
ic.dm.detects <- read_xlsx("IC_DMTM_Y23.xlsx", sheet = "Detection - DM", col_names = TRUE)
ic.dm.detects$community <- "AZ Background"

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

