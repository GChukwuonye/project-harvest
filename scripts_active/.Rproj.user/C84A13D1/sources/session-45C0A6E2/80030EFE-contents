#Kunal Palawat | kpalawat@email.arizona.edu
#Date Created: March 19th, 2021
#Description: Code to analyze tucson specific concentrations
#Notes
#Finish up NvS plots

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
al.dat <- aggregate(dat$Al,
                    by = list(dat$site, dat$season),
                    FUN = geoMean)
colnames(al.dat) <- c("site", "Sampling Season","GeoMean")
al.dat.gmean <- full_join(al.dat, tucson, by = c("site"))
al.dat.gmean <- al.dat.gmean[!is.na(al.dat.gmean[c("GeoMean")]),]
write.csv(al.dat.gmean, "tucson_ssn_al_gmean.csv")

#Basic TRI Analysis ----
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
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)
Al1 <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km+dist.tia.km+dist.jensen.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
Al1b <- lmer(data = dat,
            log(Al) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Al1b)
car::vif(Al1b)
anova(Al1b)
Al1.5 <- lmer(data = dat,
            log(Al) ~ season+period+dist.afb.km+dist.tia.km
              + season:period 
            + (1|site),
            REML=T)
summary(Al1.5)
car::vif(Al1.5)
anova(Al1.5)
step(Al1, scope=list(lower=Al0), direction="backward")


predict.datt <- ggpredict(model = Al1,
                 terms = c("dist.afb.km"),
                 back.transform = F) #specify both variables in terms to assess them together, this helps visualize an interaction.
ggplot(data = dat, aes(x = dist.afb.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y =predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#4068B2")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Pb by Distance From Davis-Monthan Air Force Base\n",
       y = "ln(Pb) [µg/L]\n",
       x = "\n Distance From Davis-Monthan Air Force Base (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Pb_Tafbeffectln.png", res=100, height=6, width=8, units="in")



#Set up for Kriging ----
cd.dat <- aggregate(dat$Cd,
                    by = list(dat$site, dat$season),
                    FUN = geoMean)
colnames(cd.dat) <- c("site", "Sampling Season","GeoMean")
cd.dat.gmean <- full_join(cd.dat, tucson, by = c("site"))
cd.dat.gmean <- cd.dat.gmean[!is.na(cd.dat.gmean[c("GeoMean")]),]
write.csv(cd.dat.gmean, "tucson_ssn_cd_gmean.csv")

#Basic TRI Analysis ----
cor.test(dat$Cd, dat$dist.jensen.km, method = "spearman")
cor.test(dat$Cd, dat$dist.fci.km, method = "spearman")
cor.test(dat$Cd, dat$dist.afb.km, method = "spearman")
cor.test(dat$Cd, dat$dist.tia.km, method = "spearman")

plot(dat$dist.jensen.km,dat$Cd)
plot(dat$dist.fci.km,dat$Cd)
plot(dat$dist.afb.km,log(dat$Cd))
plot(dat$dist.tia.km,log(dat$Cd))


afb <- lm(data=dat,
          log(dat$Cd)~dist.afb.km)
summary(afb)

jensen <- lm(data=dat,
             log(dat$Cd)~dist.jensen.km)
summary(jensen)
hist(jensen$residuals)
jen0 <- lmer(data = dat,
             log(Cd) ~
               + (1|site),
             REML = F)
jen1 <- lmer(data = dat,
             log(Cd) ~ dist.jensen.km
             + (1|site),
             REML = F)
anova(jen1, jen0)
summary(jen1)
jen2 <- lmer(data = dat,
             log(Cd) ~ dist.jensen.km + season
             + (1|site),
             REML = F)
summary(jen2)

jen3 <- lmer(data = dat,
             log(Cd) ~ season
             + (1|site),
             REML = F)
anova(jen2, jen1)
anova(jen2, jen3)
anova(jen1, jen3)
plot(allEffects(jen2))
fci <- lm(data=dat,
          log(dat$Cd)~dist.fci.km)
summary(fci)
hist(fci$residuals)
fci0 <- lmer(data = dat,
             log(Cd) ~
               + (1|site),
             REML = F)
fci1 <- lmer(data = dat,
             log(Cd) ~ dist.fci.km
             + (1|site),
             REML = F)
anova(fci1, fci0)
summary(fci1)
fci2 <- lmer(data = dat,
             log(Cd) ~ dist.fci.km + season
             + (1|site),
             REML = F)
summary(fci2)

fci3 <- lmer(data = dat,
             log(Cd) ~ season
             + (1|site),
             REML = F)
summary(fci3)
anova(fci0, fci1)
anova(fci2, fci1)
anova(fci2, fci3)
anova(fci1, fci3)



#Cd=====
Cd0 <- lmer(data = dat,
            log(Cd) ~ (1|site),
            REML = T)
Cd1 <- lmer(data = dat,
            log(Cd) ~ ward+season+period+dist.afb.km+dist.tia.km+dist.jensen.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Cd1)
car::vif(Cd1)

Cd1.5 <- lmer(data = dat,
            log(Cd) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
car::vif(Cd1.5)
anova(Cd1.5)
summary(Cd1.5)
step(Cd1.5, scope=list(lower=Cd0), direction="backward")
Cd2 <- lmer(data = dat,
            log(Cd) ~ ward + (1 | site) + ward:season + season:period,
            REML=T)
vif(Cd2)
anova(Cd2)
summary(Cd2)
AIC(Cd2)


library(report)
r <- report(Cd2)
summary(r)
vif(Cd2)
anova(Cd2)
summary(Cd2)
model.effects <- allEffects(Cd2)
plot(model.effects)
dev.print(png, "Cd_checktucson2.png", res=300, height=10, width=20, units="in")
cd.sum <- summary(Cd2)
write.csv(cd.sum$coefficients, "Cd1coefTu.csv")
cd.ssnmeans <- lsmeans(Cd2, pairwise~ward:season,
                       adjust = "tukey")#
write.csv(cd.ssnmeans$contrasts, "Cd71contrastsssnHW.csv")




#Zn=====
Zn0 <- lmer(data = dat,
            log(Zn) ~ (1|site),
            REML = T)
Zn1 <- lmer(data = dat,
            log(Zn) ~ ward+season+period+dist.afb.km+dist.tia.km+dist.jensen.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Zn1)
car::vif(Zn1)
anova(Zn1)
Zn1.5 <- lmer(data = dat,
            log(Zn) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Zn1.5)
car::vif(Zn1.5)
anova(Zn1.5)
step(Zn1.5, scope=list(lower=Zn0), direction="backward")
Zn2 <- lmer(data = dat,
              log(Zn) ~ dist.tia.km
              + (1|site),
              REML=T)
            
summary(Zn2)
anova(Zn2)

model.effects <- allEffects(Zn2)
plot(model.effects)
dev.print(png, "Zn_checktucson2.png", res=300, height=10, width=20, units="in")
zn.sum <- summary(Zn2)
write.csv(zn.sum$coefficients, "Zn2coefTu.csv")



            
#Cu=====
Cu0 <- lmer(data = dat,
            log(Cu) ~ (1|site),
            REML = T)
Cu1 <- lmer(data = dat,
            log(Cu) ~ ward+season+period+dist.afb.km+dist.tia.km+dist.jensen.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Cu1)
vif(Cu1)
Cu2<- lmer(data = dat,
            log(Cu) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
vif(Cu2)
anova(Cu2)
summary(Cu2)
step(Cu2, scope=list(lower=Cu0), direction="backward")
Cu3<- lmer(data = dat,
           log(Cu) ~ season 
           + (1|site),
           REML=T)
anova(Cu3)
summary(Cu3)
model.effects <- allEffects(Cu3)
plot(model.effects)
dev.print(png, "Cu_checktucson3.png", res=300, height=5, width=5, units="in")
cu.sum <- summary(Cu3)
write.csv(cu.sum$coefficients, "Cu3coefTu.csv")
cu.ssnmeans <- lsmeans(Cu3, pairwise~season,
                       adjust = "tukey")#
write.csv(cu.ssnmeans$contrasts, "Cu71contrastsssnHW.csv")


#Mn======
Mn0 <- lmer(data = dat,
            log(Mn) ~ (1|site),
            REML = T)
Mn1 <- lmer(data = dat,
            log(Mn) ~ ward+season+period+dist.afb.km+dist.tia.km+dist.jensen.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(Mn1)
vif(Mn1)
Mn2 <- lmer(data = dat,
            log(Mn) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
anova(Mn2)
vif(Mn2)
step(Mn2, scope=list(lower=Mn0), direction="backward")
Mn3 <- lmer(data = dat,
     log(Mn) ~ ward + season + period + (1 | site) + ward:period + season:period)
anova(Mn3)
summary(Mn3)
vif(Mn3)

Mn4 <- lmer(data = dat,
            log(Mn) ~ ward + season + period + (1 | site) +  season:period)
anova(Mn4)
summary(Mn4)
vif(Mn4)

Mn5 <- lmer(data = dat,
            log(Mn) ~  season + period + (1 | site)  + samplings)
anova(Mn5)
summary(Mn5)
vif(Mn5)
Mn6 <- lmer(data = dat,
            log(Mn) ~  (1 | site) + ward:period + samplings)
anova(Mn6)
summary(Mn6)
vif(Mn6)
anova(Mn0, Mn1)
anova(Mn1, Mn2)
anova(Mn2, Mn3)
anova(Mn3, Mn5)
anova(Mn5, Mn6)
model.effects <- allEffects(Mn5)
plot(model.effects)
dev.print(png, "Mn_checktucson5.png", res=300, height=10, width=10, units="in")
mn.sum <- summary(Mn5)
write.csv(mn.sum$coefficients, "Mn5coefTu.csv")

mn.ssnmeans <- lsmeans(Mn5, pairwise~samplings,
                       adjust = "tukey")#
write.csv(mn.ssnmeans$contrasts, "Mn71contrastsssnHW.csv")

#As Modeling take two (with airport)----
As1 <- lmer(data = dat,
            log(As) ~ ward+season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(As1)
vif(As1)


As2 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km+dist.tia.km
            + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(As2)
vif(As2)

As3 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km+dist.tia.km
            + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(As3)
vif(As3)

As4 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km+dist.tia.km
            + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(As4)
vif(As4)

As5 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km+dist.tia.km
            + season:period
            + (1|site),
            REML=T)
summary(As5)
vif(As5)
anova(As5)



#As Modeling ----
As0 <- lmer(data = dat,
            log(As) ~ (1|site),
            REML=T)

As1 <- lmer(data = dat,
            log(As) ~ ward+season+period+dist.afb.km+
              + ward:season + ward:period + season:period + ward:season:period
            + (1|site),
            REML=T)
summary(As1)
vif(As1)
anova(As1)

As2 <- lmer(data = dat,
            log(As) ~ ward+season+period+dist.afb.km+
              + ward:period + season:period + ward:season:period
            + (1|site),
            REML=F)
summary(As2)
vif(As2)
anova(As2)

As3 <- lmer(data = dat,
            log(As) ~ ward+season+period+dist.afb.km
            + ward:period + season:period
            + (1|site),
            REML=F)
summary(As3)
vif(As3)
anova(As3)

As4 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km
            + ward + season:period
            + (1|site),
            REML=F)
summary(As4)
vif(As4)
anova(As4)

As5 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km
            + ward
            + (1|site),
            REML=F)
summary(As5)
anova(As5)

As6 <- lmer(data = dat,
            log(As) ~ season+period+dist.afb.km
            + (1|site),
            REML=F)
summary(As6)
anova(As6)

As7 <- lmer(data = dat,
            log(As) ~ season+period
            + (1|site),
            REML=T)
summary(As7)
anova(As7)

As7.5 <- lmer(data = dat,
              log(As) ~ samplings
              + (1|site),
              REML=F)
summary(As7.5)
anova(As7.5)

As8 <- lmer(data = dat,
            log(As) ~ season
            + (1|site),
            REML=F)
summary(As8)
anova(As8)

As9 <- lmer(data = dat,
            log(As) ~ season*period
            + (1|site),
            REML=F)
summary(As9)
anova(As9)
vif(As9)

As11 <- lmer(data = dat,
             log(As) ~ period
             + (1|site),
             REML=F)
summary(As11)
anova(As11)

anova(As4, As5)
anova(As5, As6)
anova(As6, As7)
anova(As7, As8)
anova(As7, As11)
anova(As7, As9)
AIC(As0)
AIC(As1)
performance(As7)

plot(allEffects(As7))
summary(As7)
as.sum <- summary(As7)
write.csv(as.sum$coefficients, "as7coefT.csv")
as.ssnmeans <- lsmeans(As7, pairwise~season, adjust = "tukey")#
write.csv(as.ssnmeans$lsmeans, "as7lsmeansTssn.csv")
write.csv(as.ssnmeans$contrasts, "as7contrastsTssn.csv")

as.permeans <- lsmeans(As7, pairwise~period, adjust = "tukey")#
write.csv(as.permeans$lsmeans, "as7lsmeansTper.csv")
write.csv(as.permeans$contrasts, "as7contrastsTper.csv")

#as.ssnmeans <- lsmeans(As7.5, pairwise~samplings, adjust = "tukey")


# predict.datt <- ggpredict(model = As93,
#                  terms = c("nearest.dist.km"),
#                  back.transform = F) #specify both variables in terms to assess them together, this helps visualize an interaction.
ggplot(data = dat, aes(x = dist.jensen.km, y = log(As)))+
  geom_point()+
  # geom_ribbon(data = predict.dat, mapping = aes(x=x, y =predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#4068B2")+ #adds shading for error
  # geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "As by Distance From Davis-Monthan Air Force Base\n",
       y = "ln(As) [µg/L]\n",
       x = "\n Distance From Davis-Monthan Air Force Base (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "As_Tafbeffectln.png", res=100, height=6, width=8, units="in")


# 
# 
# As0 <- lmer(data = dat.ward,
#             log(As) ~ (1|site))
# 
# As1 <- lmer(data = dat.ward,
#             log(As) ~ ward + (1|site))
# 
# As2 <- lmer(data = dat.ward,
#             log(As) ~ ward * season + (1|site))
# summary(As1)
# anova(As1)
# 
# AIC(As0)
# AIC(As1)
# AIC(As2)
# 
# As1 <- lmer(data = dat.ns,
#             log(As) ~ NS + (1|site))
# summary(As1)
# anova(As1)

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
TwardbarplotFX(dataDF = dat,
               name.string = "Al")
lapply(X = Contamlist, FUN = TwardbarplotFX, dataDF=dat)

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
         fill = "Season") +
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

TnsbarplotFX(dataDF = dat.ns,
             name.string = "Pb",
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
  
  dev.print(png, paste(analyte,"_IWDM_Tssnnomax.png"), res=100, height=6, width=8, units="in")
  
}

TssnbarplotFXnomax(dataDF = dat,
                   name.string = "As")
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
              name.string = "Pb")

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
               name.string = "As")

