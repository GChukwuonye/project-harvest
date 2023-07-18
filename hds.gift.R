#Kunal Palawat | kpalawat@email.arizona.edu
#Date Created: March 21th, 2021
#Description: Code to analyze dewey-humboldt specific concentrations
#Notes
#for every question, I use full_join() to combine the HDS data with the concentration data. Change out the object I have called iw.dm with the object your data is stored in
#you may need to change some of the column titles in this code/excel files because there are some differences between our datasets


#load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(lmerTest)
library(effects)
library(performance)
library(car)
library(ggeffects)
library(DHARMa)

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data
hds <- read_xlsx("data/data_processing/IO_HDS.xlsx", sheet = "stats", col_names = TRUE)

#Al Q1 What is your roof made of? ----
q1 <- hds
q1 <- q1[!is.na(q1$Q1),]
top5 <- c("Flat BUR (Reflective)", "Asphalt Shingle", "Flat BUR (Tar/Gravel)", "Wood Shakes/Shingles", "Metal Panel")
q1 <- filter(q1, Q1 %in% top5)
q1dat <- full_join(iw.dm, q1, by = c("site"))
q1dat <- q1dat[!is.na(q1dat$Q1),]
q1dat <- q1dat[!is.na(q1dat$community),]

q1dat$Q1 <- factor(q1dat$Q1, levels = c("Flat BUR (Tar/Gravel)","Flat BUR (Reflective)", "Asphalt Shingle", "Wood Shakes/Shingles", "Metal Panel"))

# boxplot(q1dat$As ~ q1dat$Q1+q1dat$community)
#As1 <- aov(log(q1dat$As) ~ q1dat$Q1 + q1dat$community)
#TukeyHSD(As1)

ggplot(data = q1dat, mapping = aes(y = Al, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Roof Materials", 
       title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_rooftypecomm.png", res=100, height=6, width=8, units="in")

ggplot(data = q1dat, mapping = aes(y = Al, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Roof Materials",
      title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_rooftype.png", res=100, height=6, width=8, units="in")

kruskal.test(q1dat$Al~q1dat$Q1) #significant
pairwise.wilcox.test(q1dat$Cd, q1dat$Q1)

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q1 + community + period + season
            + Q1:community + Q1:period + Q1:season 
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Al1)
anova(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")

Al2 <- lmer(log(Al)~ Q1 + period + season
            + Q1:period + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
Al2b <- lmer(log(Al)~ Q1:Q11:season:community
            + (1|community:site),
            data = q1dat,
            REML = F)
step(Al2b, scope=list(lower=Al0), direction="backward")
anova(Al2b)
summary(Al2b)
anova(Al2b)
check_model(Al2b)
simulateResiduals(Al2, plot=T)
vif(Al2)
step(Al2, scope=list(lower=Al0), direction="backward")
Al3 <- lmer(log(Al)~ Q1 + community + period + season
            + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Al3)
vif(Al3)

Al4 <- lmer(log(Al)~ Q1 + community + period + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Al4)
vif(Al4)
anova(Al4)

Al5 <- lmer(log(Al)~ community + period + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Al5)
anova(Al4, Al5)



#write.csv(q1dat, "q1dat.csv")


#Mn-Q1=====

ggplot(data = q1dat, mapping = aes(y = Mn, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Mangaenese Concentration (ug/L)",
       x= "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Mn_rooftypecomm.png", res=100, height=6, width=8, units="in")
 kruskal.test(q1dat$Mn~q1dat$Q1) #significant
 pairwise.wilcox.test(q1dat$Mn, q1dat$Q1)

 ggplot(data = q1dat, mapping = aes(y = Mn, x = Q1))+
   geom_boxplot() + 
   stat_boxplot(geom = 'errorbar') +
   #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
   scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
   labs(y= "Manganese Concentration (ug/L)",
        x= "Roof Materials",
        title = "What is your roof made of?")+
   theme_bw() +   theme(legend.position = "bottom")
 dev.print(png, "Mn_rooftype.png", res=100, height=6, width=8, units="in")

Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q1 + community + period + season
            + Q1:community + Q1:period + Q1:season 
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Al0), direction="backward")

Mn2 <- lmer(log(Mn)~ Q1 + community + period + season
            + Q1:period + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Mn2)

Mn3 <- lmer(log(Mn)~
            + Q1:period + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
vif(Mn3)
anova(Mn3)
summary(Mn3)
mn.sum <- summary(Mn3)
write.csv(mn.sum$coefficients, "MncoefHDS.csv")
mn.hdsmeans <- lsmeans(Mn3, pairwise~Q1:season,
                       adjust = "tukey")#
write.csv(mn.hdsmeans$contrasts, "MnHDScontrastsssn.csv")

#Cd=====
ggplot(data = q1dat, mapping = aes(y = Cd, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y = "Cadmium Concentration (ug/L)",
       x = "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_rooftypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q1dat$Cd~q1dat$Q1) #significant
pairwise.wilcox.test(q1dat$Cd, q1dat$Q1)

ggplot(data = q1dat, mapping = aes(y = Cd, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_rooftype.png", res=100, height=6, width=8, units="in")

#write.csv(q1dat, "q1dat.csv")
Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q1 + community + period + season
            + Q1:community + Q1:period + Q1:season 
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")

Cd2 <- lmer(log(Cd)~ Q1 + community + season
             + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
anova(Cd2)
summary(Cd2)

Cd3 <- lmer(log(Cd)~ Q1  + season
            + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
anova(Cd3)
summary(Cd3)
cd.sum <- summary(Cd3)
write.csv(cd.sum$coefficients, "CdcoefHDS.csv")
cd.hdsmeans <- lsmeans(Cd3, pairwise~Q1:season,
                       adjust = "tukey")#
write.csv(cd.hdsmeans$contrasts, "CdHDScontrastsssn.csv")

#Zn=====
ggplot(data = q1dat, mapping = aes(y = Zn, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_rooftypecomm.png", res=100, height=6, width=8, units="in")

kruskal.test(q1dat$Zn~q1dat$Q1) #significant
pairwise.wilcox.test(q1dat$Zn, q1dat$Q1)

ggplot(data = q1dat, mapping = aes(y = Zn, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x = "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_rooftype.png", res=100, height=6, width=8, units="in")

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q1 + community + period + season
            + Q1:community + Q1:period + Q1:season 
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")

Zn2 <- lmer(log(Zn)~ Q1 + community + period + season
            + Q1:period + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
vif(Zn2)
summary(Zn2)
Zn3 <- lmer(log(Zn)~ Q1 + period + season
            + (1|community:site),
            data = q1dat,
            REML = F)
anova(Zn3)
vif(Zn3)
summary(Zn3)
zn.sum <- summary(Zn3)
write.csv(zn.sum$coefficients, "ZncoefHDS.csv")
zn.hdsmeans <- lsmeans(Zn3, pairwise~Q1,
                       adjust = "tukey")#
write.csv(zn.hdsmeans$contrasts, "ZnHDScontrastsssn.csv")
#Cu=====
ggplot(data = q1dat, mapping = aes(y = Cu, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_rooftypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q1dat$Cu~q1dat$Q1) #significant
pairwise.wilcox.test(q1dat$Cu, q1dat$Q1)

ggplot(data = q1dat, mapping = aes(y = Cu, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Roof Materials",
    title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_rooftype.png", res=100, height=6, width=8, units="in")
Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q1 + community + period + season
            + Q1:community + Q1:period + Q1:season 
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

Cu2 <- lmer(log(Al)~ Q1 + community + season
            + (1|community:site),
            data = q1dat,
            REML = F)
vif(Cu2)
anova(Cu2)
Cu3 <- lmer(log(Al)~ Q1 + season
            + (1|community:site),
            data = q1dat,
            REML = F)
vif(Cu3)
anova(Cu3)
summary(Cu3)
cu.sum <- summary(Cu3)
write.csv(cu.sum$coefficients, "CucoefHDS.csv")
cu.hdsmeans <- lsmeans(Cu3, pairwise~Q1,
                       adjust = "tukey")#
write.csv(cu.hdsmeans$contrasts, "CuHDScontrastsssn.csv")
cu.hdsmeans <- lsmeans(Cu3, pairwise~season,
                       adjust = "tukey")#
write.csv(cu.hdsmeans$contrasts, "CuHDS2contrastsssn.csv")











#As Q9 Approximately, when was your home built? ---- 
q9 <- hds
q9$Q9 <- as.character(q9$Q9)
q9 <- q9[q9$Q9!="0",]
q9 <- q9[!is.na(q9$Q9),]
q9$Q9b <- q9$Q9
q9[q9$Q9=="1",]$Q9 <- "Pre 1940"
q9[q9$Q9=="2",]$Q9 <- "1941-1949"
q9[q9$Q9=="3",]$Q9 <- "1950-1959"
q9[q9$Q9=="4",]$Q9 <- "1960-1969"
q9[q9$Q9=="5",]$Q9 <- "1970-1979"
q9[q9$Q9=="6",]$Q9 <- "1980-1989"
q9[q9$Q9=="7",]$Q9 <- "1990-1999"
q9[q9$Q9=="8",]$Q9 <- "2000-2009"
q9[q9$Q9=="9",]$Q9 <- "2010-2018"



q9[q9$Q9b=="1",]$Q9b <- "1940"
q9[q9$Q9b=="2",]$Q9b <- "1945"
q9[q9$Q9b=="3",]$Q9b <- "1955"
q9[q9$Q9b=="4",]$Q9b <- "1965"
q9[q9$Q9b=="5",]$Q9b <- "1975"
q9[q9$Q9b=="6",]$Q9b <- "1985"
q9[q9$Q9b=="7",]$Q9b <- "1995"
q9[q9$Q9b=="8",]$Q9b <- "2005"
q9[q9$Q9b=="9",]$Q9b <- "2015"

q9$Q9b <- as.numeric(q9$Q9b)
q9$age2021 <- 2021 - q9$Q9b
mean(q9$age2021)
summary(q9$age2021)


q9$Q9 <- factor(q9$Q9, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))

q9dat <- full_join(iw.dm, q9, by = c("site"))
q9dat <- q9dat[!is.na(q9dat$Q9),]
q9dat <- q9dat[!is.na(q9dat$community),]

q9dat$eighties <- c("Pre 1980")
# q9dat[q9dat$Q9=="1980-1989",]$eighties <- "Post 1980"
# q9dat[q9dat$Q9=="1990-1999",]$eighties <- "Post 1980"
# q9dat[q9dat$Q9=="2000-2009",]$eighties <- "Post 1980"
# q9dat[q9dat$Q9=="2010-2018",]$eighties <- "Post 1980"

q9dat[q9dat$Q9=="1980-1989",]$eighties <- "1980-1989"
q9dat[q9dat$Q9=="1990-1999",]$eighties <- "1990-1999"
q9dat[q9dat$Q9=="2000-2009",]$eighties <- "2000-2009"
q9dat[q9dat$Q9=="2010-2018",]$eighties <- "2010-2018"

summary(as.factor(q9dat$eighties))

ggplot(data = q9dat, mapping = aes(y = Al, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
    title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Al, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_houseage2.png", res=100, height=6, width=8, units="in")
kruskal.test(q9dat$Al ~ q9dat$Q9)

ggplot(data = q9dat, mapping = aes(y = Al, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_houseagecommssn.png", res=100, height=6, width=12, units="in")

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q9dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q9 + community + period + season
            + Q9:community + Q9:period + Q9:season 
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")
Al3 <- lmer(log(Al)~
           Q9 + period + season + (1 | community:site),
           data = q9dat,
           REML = F)
vif(Al3)
anova(Al3)
summary(Al3)
al.sum <- summary(Al3)
write.csv(al.sum$coefficients, "AlcoefHDS2.csv")
al.hdsmeans <- lsmeans(Al3, pairwise~Q9,
                       adjust = "tukey")#
write.csv(al.hdsmeans$contrasts, "AlHDScontrastsHDS9.csv")


#Q9-Cd=====
ggplot(data = q9dat, mapping = aes(y = Cd, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Cadmium Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Cd, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Cadmium Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
kruskal.test(q9dat$Cd ~ q9dat$Q9)

ggplot(data = q9dat, mapping = aes(y = Cd, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Cadmium Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_houseagecommssn.png", res=100, height=6, width=12, units="in")


Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q9dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q9 + community + period + season
            + Q9:community + Q9:period + Q9:season 
            + (1|community:site),
            data = q9dat,
            REML = F)
anova(Cd1)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")
Cd2 <- lmer(log(Cd)~ Q9 + community + season
            + Q9:community 
            + (1|community:site),
            data = q9dat,
            REML = F)

anova(Cd2)
summary(Cd2)
cd.sum <- summary(Cd2)
write.csv(cd.sum$coefficients, "CdcoefHDS2.csv")
cd.hdsmeans <- lsmeans(Cd2, pairwise~Q9,
                       adjust = "tukey")#
write.csv(cd.hdsmeans$contrasts, "CdHDScontrastsHDS9.csv")
cd.hdsmeans <- lsmeans(Cd2, pairwise~season,
                       adjust = "tukey")
write.csv(cd.hdsmeans$contrasts, "CdHDS2contrastsHDS9.csv")
cd.hdsmeans <- lsmeans(Cd2, pairwise~Q9+community,
                       adjust = "tukey")#
write.csv(al.hdsmeans$contrasts, "CdHDS3contrastsHDS9.csv")

#Q9-Zn=====
ggplot(data = q9dat, mapping = aes(y = Zn, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Zinc Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Zn, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Zinc Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
kruskal.test(q9dat$Zn ~ q9dat$Q9)

ggplot(data = q9dat, mapping = aes(y = Zn, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Zinc Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_houseagecommssn.png", res=100, height=6, width=12, units="in")

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q9dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q9 + community + period + season
            + Q9:community + Q9:period + Q9:season 
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")
Zn2 <- lmer(log(Zn)~ Q9  + period + season
            + (1|community:site),
            data = q9dat,
          REML = F)
anova(Zn2) 
summary(Zn2)
zn.sum <- summary(Zn2)
write.csv(zn.sum$coefficients, "ZncoefHDS2.csv")
zn.hdsmeans <- lsmeans(Zn2, pairwise~Q9,
                       adjust = "tukey")#
write.csv(zn.hdsmeans$contrasts, "ZnHDScontrastsHDS9.csv")
zn.hdsmeans <- lsmeans(Zn2, pairwise~season,
                       adjust = "tukey")
write.csv(zn.hdsmeans$contrasts, "ZnHDS2contrastsHDS9.csv")
zn.hdsmeans <- lsmeans(Zn2, pairwise~period,
                       adjust = "tukey")#
write.csv(zn.hdsmeans$contrasts, "ZnHDS3contrastsHDS9.csv")


#Q9-Cu=======
ggplot(data = q9dat, mapping = aes(y = Cu, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Copper Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Cu, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Copper Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
kruskal.test(q9dat$Cu ~ q9dat$Q9)

ggplot(data = q9dat, mapping = aes(y = Al, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_houseagecommssn.png", res=100, height=6, width=12, units="in")

Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q9dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q9 + community + period + season
            + Q9:community + Q9:period + Q9:season 
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

Cu2 <- lmer(log(Cu)~ Q9 + community + period + season
            + (1|community:site),
            data = q9dat,
            REML = F)
anova(Cu2)
summary(Cu2)
vif(Cu2)

cu.sum <- summary(Cu2)
write.csv(cu.sum$coefficients, "CucoefHDS2.csv")
cu.hdsmeans <- lsmeans(Cu2, pairwise~Q9,
                       adjust = "tukey")#
write.csv(cu.hdsmeans$contrasts, "CuHDScontrastsHDS9.csv")
zn.hdsmeans <- lsmeans(Zn2, pairwise~season,
                       adjust = "tukey")

#Q9-Mn======
ggplot(data = q9dat, mapping = aes(y = Mn, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Manganese Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Mn_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Mn, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Manganese Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom")
kruskal.test(q9dat$Mn ~ q9dat$Q9)

ggplot(data = q9dat, mapping = aes(y = Al, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year home was built")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_houseagecommssn.png", res=100, height=6, width=12, units="in")




Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q9dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q9 + community + period + season
            + Q9:community + Q9:period + Q9:season 
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")


Mn2 <- lmer(log(Mn)~  Q9 +period + season 
            + (1|community:site),
            data = q9dat,
            REML = F)
anova(Mn2)
summary(Mn2)
vif(Mn2)
mn.sum <- summary(Mn2)
write.csv(mn.sum$coefficients, "MncoefHDS2.csv")
mn.hdsmeans <- lsmeans(Mn2, pairwise~Q9,
                       adjust = "tukey")#
write.csv(mn.hdsmeans$contrasts, "MnHDScontrastsHDS9.csv")




model.effects <- allEffects(Al3)
plot(model.effects)



predict.dat <- ggeffect(model = Al3,
                        terms = c("Q9"),
                        back.transform = F,
                        type = "re")
predict.dat$Q9 <- predict.dat$x

plot <- ggplot(data= q9dat,
               mapping = aes(x=Q9,y=log(Al))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~Q9, scales = "free")+
  geom_pointrange(data = predict.dat, aes(x=Q9, y = predicted, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nHome Age",
       y = expression(paste("ln[As] (µg ",L^-1, ")")))+
  #scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
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
plot
dev.print(png, "Al_rhihomeageln.png", res=300, height=4, width=7, units="in")




ggplot(data = q9dat, aes(x = Q9, y = log(Al)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Al by Approximate Home Age\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_rhihomeage2ln.png", res=300, height=4, width=10, units="in")


ggplot(data = q9dat, aes(x = Q9, y = log(Mn)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Mn by Approximate Home Age\n",
       y = "ln(Mn) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Mn_rhihomeage2ln.png", res=300, height=4, width=10, units="in")

ggplot(data = q9dat, aes(x = Q9, y = log(Cd)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Cd by Approximate Home Age\n",
       y = "ln(Cd) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cd_rhihomeage2ln.png", res=300, height=4, width=10, units="in")

ggplot(data = q9dat, aes(x = Q9, y = log(Cu)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Cu by Approximate Home Age\n",
       y = "ln(Cu) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cu_rhihomeage2ln.png", res=300, height=4, width=10, units="in")

ggplot(data = q9dat, aes(x = Q9, y = log(Zn)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Zn by Approximate Home Age\n",
       y = "ln(Zn) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Zn_rhihomeage2ln.png", res=300, height=4, width=10, units="in")




#As Q65 How old is your cistern? ----
q65 <- hds
q65$Q65b <- q65$Q65
q65$Q65 <- as.character(q65$Q65)
q65 <- q65[q65$Q65!="0",]
q65 <- q65[q65$Q65!="1",]
q65 <- q65[!is.na(q65$Q65),]
q65[q65$Q65=="2",]$Q65 <- "<1 year"
q65[q65$Q65=="3",]$Q65 <- "<1 year"
q65[q65$Q65=="4",]$Q65 <- "1-2 years"
q65[q65$Q65=="5",]$Q65 <- "2-3 years"
q65[q65$Q65=="6",]$Q65 <- "3-4 years"
q65[q65$Q65=="7",]$Q65 <- "5+ years"

q65$Q65 <- factor(q65$Q65, levels = c("<1 year", "1-2 years", "2-3 years", "3-4 years", "5+ years"))

q65$Q65b <- as.character(q65$Q65b)
q65[q65$Q65b=="2",]$Q65b <- "0-2 years"
q65[q65$Q65b=="3",]$Q65b <- "0-2 years"
q65[q65$Q65b=="4",]$Q65b <- "0-2 years"
q65[q65$Q65b=="5",]$Q65b <- "2-4 years"
q65[q65$Q65b=="6",]$Q65b <- "2-4 years"
q65[q65$Q65b=="7",]$Q65b <- "5+ years"




q65$Q65b <- factor(q65$Q65b, levels = c("0-2 years", "2-4 years", "5+ years"))
summary(q65$Q65b)
q65dat <- full_join(iw.dm, q65, by = c("site"))
q65dat <- q65dat[!is.na(q65dat$Q65b),]
q65dat <- q65dat[!is.na(q65dat$community),]

summary(as.factor(q65dat$Q65b))

#Al=====
ggplot(data = q65dat, mapping = aes(y = Al, x = Q65b))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
        x= "Years",
        title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Al, x = Q65b, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Years",
    title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom") 
dev.print(png, "Al_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

kruskal.test(q65dat$Al, q65dat$Q65b)

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q65 + community + period + season
            + Q65:community + Q65:period + Q65:season 
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")
Al2 <- lmer(log(Al)~ Q65 + community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)
anova(Al2)
vif(Al2)
 
al.sum <- summary(Al2)
write.csv(al.sum$coefficients, "Al2coefhomeage.csv")
al.means <- lsmeans(Al2, pairwise~Q65, adjust = "tukey")#
write.csv(al.means$contrasts, "Al2contrastsssnHW.csv")

predict.dat <- ggeffect(model = Al2,
                        terms = c("Q65"),
                        back.transform = F,
                        type = "re")

ggplot(data = q65dat, aes(x = Q65, y = log(Al)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Al by Approximate Cistern Age\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Cistern Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_rhihomeageln.png", res=100, height=6, width=10, units="in")
#Cd=====
ggplot(data = q65dat, mapping = aes(y = Cd, x = Q65b))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Cd, x = Q65b, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom") 
dev.print(png, "Cd_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

kruskal.test(q65dat$Cd, q65dat$Q65b)

Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q65 + community + period + season
            + Q65:community + Q65:period + Q65:season 
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")
Cd2 <- lmer(log(Cd)~ Q65 + community  + season
          + (1|community:site),
            data = q65dat,
            REML = F)
summary(Cd2)
vif(Cd2)
anova(Cd2)
cd.sum <- summary(Cd2)
write.csv(cd.sum$coefficients, "Cd2coefhomeage.csv")
cd.means <- lsmeans(Cd2, pairwise~Q65, adjust = "tukey")#
write.csv(cd.means$contrasts, "Cd2contrastsssnHW.csv")

predict.dat <- ggeffect(model = Cd2,
                        terms = c("Q65"),
                        back.transform = F,
                        type = "re")

ggplot(data = q65dat, aes(x = Q65, y = log(Cd)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Cd by Approximate Cistern Age\n",
       y = "ln(Cd) [µg/L]\n",
       x = "\n Cistern Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cd_rhihomeageln.png", res=100, height=6, width=10, units="in")


#Cu=====
ggplot(data = q65dat, mapping = aes(y = Cu, x = Q65b))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Cu, x = Q65b, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom") 
dev.print(png, "Cu_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

kruskal.test(q65dat$Cu, q65dat$Q65b)
Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q65 + community + period + season
            + Q65:community + Q65:period + Q65:season 
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

Cu2 <- lmer(log(Cu)~ community + period + season
            + Q65
            + (1|community:site),
            data = q65dat,
            REML = F)
anova(Cu2)
vif(Cu2)

cu.sum <- summary(Cu2)
write.csv(cu.sum$coefficients, "Cu2coefhomeage.csv")
cu.means <- lsmeans(Cu2, pairwise~Q65, adjust = "tukey")#
write.csv(cu.means$contrasts, "Cu2contrastsssnHW.csv")

predict.dat <- ggeffect(model = Cu2,
                        terms = c("Q65"),
                        back.transform = F,
                        type = "re")

ggplot(data = q65dat, aes(x = Q65, y = log(Cu)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Cu by Approximate Cistern Age\n",
       y = "ln(Cu) [µg/L]\n",
       x = "\n Cistern Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cu_rhihomeageln.png", res=100, height=6, width=10, units="in")


#Mn=====
ggplot(data = q65dat, mapping = aes(y = Mn, x = Q65b))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Mn_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Mn, x = Q65b, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom") 
dev.print(png, "Mn_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

kruskal.test(q65dat$Mn, q65dat$Q65b)

Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q65 + community + period + season
            + Q65:community + Q65:period + Q65:season 
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")

#Zn======
ggplot(data = q65dat, mapping = aes(y = Zn, x = Q65b))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Zn, x = Q65b, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Years",
       title = "Effect of Cistern Age in Analytes Concentration?")+
  theme_bw() +   theme(legend.position = "bottom") 
dev.print(png, "Al_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

kruskal.test(q65dat$Zn, q65dat$Q65b)
Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q65 + community + period + season
            + Q65:community + Q65:period + Q65:season 
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")


Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q65b + community + period + season
            +Q65b:community + Q65b:period + Q65b:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Al1)
vif(Al1)

Al2 <- lmer(log(Al)~ Q65b:community + community + period + season
            + Q65b:period + Q65b:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Al2)
vif(Al2)

Al3 <- lmer(log(Al)~ Q65b:period  + community + period + season
            + Q65b:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Al3)
vif(Al3)
anova(Al3)
Al4 <- lmer(log(As)~ Q65b:season + community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Al4)
anova(Al4, Al3)
anova(Al4)

Al5 <- lmer(log(Al)~ community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)

Al6 <- lmer(log(Al)~ Q65b+community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Al6)
anova(Al6)

anova(Al4, Al5)
anova(Al4, Al6)
anova(Al5, Al6)

compare_performance(Al4, Al5)
check_model(Al4)
model.effects <- allEffects(Al6)
plot(model.effects)

as.sum <- summary(Al6)
write.csv(as.sum$coefficients, "As4coefhomeage.csv")
as.means <- lsmeans(Al6, pairwise~Q65b, adjust = "tukey")#
write.csv(as.means$lsmeans, "ALlsmeansssnHW.csv")
write.csv(as.means$contrasts, "Al7contrastsssnHW.csv")

predict.dat <- ggeffect(model = Al4,
                        terms = c("Q65b"),
                        back.transform = F,
                        type = "re")

ggplot(data = q65dat, aes(x = Q65b, y = log(Al)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "As by Approximate Home Age\n",
       y = "ln(As) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "As_rhihomeageln.png", res=100, height=6, width=8, units="in")


ggplot(data= hdseffect.df,
       mapping =(aes(x=Q65b,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q65") + 
  ylab("Estimated ln([As]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Cistern Age", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "As_cisternageEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q65dat, "q65dat.csv")

#As Q60 What is your cistern made out of? ----
q60 <- hds
q60$Q60 <- as.character(q60$Q60)
q60 <- q60[!is.na(q60$Q60),]

q60$Q60 <- factor(q60$Q60, levels = c("Plastic","Metal", "Concrete", "Fiberglass", "Other"))

q60 <- q60[q60$Q60!="Other",]
summary(q60$Q60)
q60dat <- full_join(iw.dm, q60, by = c("site"))
q60dat <- q60dat[!is.na(q60dat$Q60),]
q60dat <- q60dat[!is.na(q60dat$community),]

summary(as.factor(q60dat$Q60))


ggplot(data = q60dat, mapping = aes(y = Al, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "ALuminum Concentration (ug/L)",
       x= "Cistern Material",
    title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Al, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q60dat$Al, q60dat$Q60)

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q60dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q60 + community + period + season
            + Q60:community + Q60:period + Q60:season 
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")

q60 <- q60[!is.na(q60$Q60),]
#Cd=====
ggplot(data = q60dat, mapping = aes(y = Cd, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Cd, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_cisterntypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q60dat$Cd, q60dat$Q60)

Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q60dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q60 + community + period + season
            + Q60:community + Q60:period + Q60:season 
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")


#Cu======

ggplot(data = q60dat, mapping = aes(y = Cu, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Cu, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q60dat$Cu, q60dat$Q60)

Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q60dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q60 + community + period + season
            + Q60:community + Q60:period + Q60:season 
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")


Cu2 <- lmer(log(Cu)~  community + period + season
            + Q60:community + Q60:period + Q60:season 
            + (1|community:site),
            data = q60dat,
            REML = F)
anova(Cu2)
summary(Cu2)
vif(Cu1)

#Mn=======

ggplot(data = q60dat, mapping = aes(y = Mn, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Mn_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Mn, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q60dat$Mn, q60dat$Q60)

Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q60dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q60 + community + period + season
            + Q60:community + Q60:period + Q60:season 
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")
#Zn=====

ggplot(data = q60dat, mapping = aes(y = Zn, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Zn, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntypecomm.png", res=100, height=6, width=8, units="in")
kruskal.test(q60dat$Zn~q60dat$Q60)

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q60dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q60 + community + period + season
            + Q60:community + Q60:period + Q60:season 
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")

Zn2 <- lmer(log(Zn)~ Q60 + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)
anova(Zn2)
summary(Zn2)


zn.sum <- summary(Zn2)
write.csv(zn.sum$coefficients, "Zn2coefhomeage.csv")
zn.means <- lsmeans(Zn2, pairwise~Q60, adjust = "tukey")#
write.csv(zn.means$contrasts, "Zn2contrastsssnHW.csv")

predict.dat <- ggeffect(model = Zn2,
                        terms = c("Q60"),
                        back.transform = F,
                        type = "re")

ggplot(data = q65dat, aes(x = Q60, y = log(Zn)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Zn by Approximate Cistern Material\n",
       y = "ln(Zn) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Zn_rhihomeageln.png", res=100, height=6, width=8, units="in")

#Al=====
ggplot(data = q60dat, mapping = aes(y = Al, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Al Concentration (ug/L)",
       x= "Cistern Material",
       title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_cisterntype.png", res=100, height=6, width=8, units="in")

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q60dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q60 + community + period + season
            +Q60:community + Q60:period + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Al1)
vif(Al1)

Al2 <- lmer(log(Al)~ Q60:community + community + period + season
            + Q60:period + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Al2)
vif(Al2)

Al3 <- lmer(log(Al)~ Q60:period  + community + period + season
            + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Al3)
vif(Al3)
anova(Al3)
Al4 <- lmer(log(Al)~ Q60:period + community + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Al4)
anova(Al4, Al3)
anova(Al4)

Al5 <- lmer(log(Al)~ community + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)

Al6 <- lmer(log(Al)~ Q60+community + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Al6)
anova(Al6)

anova(Al4, Al5)
anova(Al4, Al6)



#As Q76 Does your cistern have first flush? ----
q76 <- hds
q76$Q76 <- as.character(q76$Q76)
q76 <- q76[q76$Q76!="0",]
q76 <- q76[q76$Q76!="3",]
q76 <- q76[!is.na(q76$Q76),]
q76[q76$Q76=="1",]$Q76 <- "Yes"
q76[q76$Q76=="2",]$Q76 <- "No"

q76$Q76 <- factor(q76$Q76, levels = c("Yes", "No"))
summary(q76$Q76)
q76dat <- full_join(iw.dm, q76, by = c("site"))
q76dat <- q76dat[!is.na(q76dat$Q76),]
q76dat <- q76dat[!is.na(q76dat$community),]

summary(as.factor(q76dat$Q76))

ggplot(data = q76dat, mapping = aes(y = Al, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
    title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Al, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_firstflushcomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q76dat$Al, q76dat$Q76)

#Cd=====

ggplot(data = q76dat, mapping = aes(y = Cd, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Cd, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_firstflushcomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q76dat$Cd, q76dat$Q76)

Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q76 + community + period + season
            + Q76:community + Q76:period + Q76:season 
            + (1|community:site),
            data = q76dat,
            REML = F)
anova(Cd1)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")
Cd2 <- lmer(log(Cd)~ season
            + Q76:community 
            + (1|community:site),
            data = q76dat,
            REML = F)
vif(Cd2)
anova(Cd2)
summary(Cd2)
cd.sum <- summary(Cd2)
write.csv(cd.sum$coefficients, "Cd2coefcisternscreen.csv")
cd.means <- lsmeans(Cd2, pairwise~Q76:community, adjust = "tukey")#
write.csv(cd.means$contrasts, "Cd2contrastsscreen.csv")
#Cu=====

ggplot(data = q76dat, mapping = aes(y = Cu, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Cu, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflushcomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q76dat$Cu, q76dat$Q76)

Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q76 + community + period + season
            + Q76:community + Q76:period + Q76:season 
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")
Cu2 <- lmer(log(Cu)~ 
            Q76:period + season + community 
            + (1|community:site),
            data = q76dat,
            REML = F)
anova(Cu2)
anova (Cu0, Cu2)
anova (Cu1, Cu2)
summary(Cu2)
cu.sum <- summary(Cu2)
write.csv(cu.sum$coefficients, "Cu2coefcisternscreen.csv")
cu.means <- lsmeans(Cu2, pairwise~Q76:period, adjust = "tukey")#
write.csv(cu.means$contrasts, "Cu2contrastsscreen.csv")
#Mn======
ggplot(data = q76dat, mapping = aes(y = Mn, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Mn, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflushcomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q76dat$Mn, q76dat$Q76)

Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q76 + community + period + season
            + Q76:community + Q76:period + Q76:season 
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")

#Zn=====

ggplot(data = q76dat, mapping = aes(y = Zn, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Zn, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflushcomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q76dat$Zn, q76dat$Q76)
Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q76 + community + period + season
            + Q76:community + Q76:period + Q76:season 
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")

#Al=====
Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q76 + community + period + season
            + Q76:community + Q76:period + Q76:season 
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q76 + community + period + season
            +Q76:community + Q76:period + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Al1)
vif(Al1)

Al2 <- lmer(log(Al)~ Q76 + community + period + season
            + Q76:period + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Al2)
vif(Al2)

Al3 <- lmer(log(Al)~ Q76 + community + period + season
            + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Al3)
vif(Al3)
anova(Al3)
Al4 <- lmer(log(Al)~ Q76 + community + period + season
            + (1|community:site),
            data = q76dat,
            REML = F)
vif(Al4)
summary(Al4)
anova(Al4, Al3)
anova(Al4)

Al5 <- lmer(log(Al)~ community + period + season
            + (1|community:site),
            data = q76dat,
            REML = F)

anova(Al4, Al5)


ggplot(data= hdseffect.df,
       mapping =(aes(x=Q76,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q76") + 
  ylab("Estimated ln([As]) (ln(ug/L))") + 
  labs(title="Modeled Effect of First Flush", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "As_firstflushEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q76dat, "q76dat.csv")


#As Q77 Does your cistern have a screen? ----
q77 <- hds
q77$Q77 <- as.character(q77$Q77)
q77 <- q77[q77$Q77!="0",]
q77 <- q77[q77$Q77!="3",]
q77 <- q77[!is.na(q77$Q77),]
q77[q77$Q77=="1",]$Q77 <- "Yes"
q77[q77$Q77=="2",]$Q77 <- "No"

q77$Q77 <- factor(q77$Q77, levels = c("Yes", "No"))
summary(q77$Q77)
q77dat <- full_join(iw.dm, q77, by = c("site"))
q77dat <- q77dat[!is.na(q77dat$Q77),]
q77dat <- q77dat[!is.na(q77dat$community),]

summary(as.factor(q77dat$Q77))

ggplot(data = q77dat, mapping = aes(y = Al, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
    title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Al, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screencomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q77dat$Al~q77dat$Q77)
#Al=====
Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q77dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q77 + community + period + season
            + Q77:community + Q77:period + Q77:season 
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")
#Cd=====
ggplot(data = q77dat, mapping = aes(y = Cd, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Cd, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screencomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q77dat$Cd~q77dat$Q77)


Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q77dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q77 + community + period + season
            + Q77:community + Q77:period + Q77:season 
            + (1|community:site),
            data = q77dat,
            REML = F)
anova(Cd1)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")


#Cu=====
ggplot(data = q77dat, mapping = aes(y = Cu, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Cu, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screencomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q77dat$Cu~q77dat$Q77)


Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q77dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q77 + community + period + season
            + Q77:community + Q77:period + Q77:season 
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

#Mn======
ggplot(data = q77dat, mapping = aes(y = Mn, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Mn, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screencomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q77dat$Mn~q77dat$Q77)


Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q77dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q77 + community + period + season
            + Q77:community + Q77:period + Q77:season 
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")


#Zn========
ggplot(data = q77dat, mapping = aes(y = Zn, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Zn, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screencomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q77dat$Zn~q77dat$Q77)

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q77dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q77 + community + period + season
            + Q77:community + Q77:period + Q77:season 
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")







#write.csv(q77dat, "q77dat.csv")

#As Q44 Is the home/community garden located within two blocks of a major roadway, freeway, elevated highway, or other transportation structures? ----
q44 <- hds
q44$Q44 <- as.character(q44$Q44)
q44 <- q44[q44$Q44!="0",]
q44 <- q44[!is.na(q44$Q44),]
q44[q44$Q44=="1",]$Q44 <- "Yes"
q44[q44$Q44=="2",]$Q44 <- "No"

q44$Q44 <- factor(q44$Q44, levels = c("Yes", "No"))
summary(q44$Q44)
q44dat <- full_join(iw.dm, q44, by = c("site"))
q44dat <- q44dat[!is.na(q44dat$Q44),]
q44dat <- q44dat[!is.na(q44dat$community),]

summary(as.factor(q44dat$Q44))

ggplot(data = q44dat, mapping = aes(y = Al, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum COncentration (ug/L)",
       x= "Responses",
    title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Al_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Al, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Al_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Al, q44dat$Q44)
#Al=====
Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q44dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q44 + community + period + season
            + Q44:community + Q44:period + Q44:season 
            + (1|community:site),
            data = q44dat,
            REML = F)
anova(Al1)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")

Al2 <- lmer(log(Al)~ community + period + season
            + Q44:community
            + (1|community:site),
            data = q44dat,
            REML = F)
anova(Al2)
al.sum <- summary(Al2)
write.csv(al.sum$coefficients, "Al2coefroad.csv")
al.means <- lsmeans(Al2, pairwise~Q44, adjust = "tukey")#
write.csv(al.means$contrasts, "Al2contrastsssnHW.csv")


#Cd=====

ggplot(data = q44dat, mapping = aes(y = Cd, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cd_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Cd, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cd_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Cd, q44dat$Q44)

Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q44dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q44 + community + period + season
            + Q44:community + Q44:period + Q44:season 
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")

#Cu======
ggplot(data = q44dat, mapping = aes(y = Cu, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cu_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Cu, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cu_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Cu, q44dat$Q44)

Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q44dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q44 + community + period + season
            + Q44:community + Q44:period + Q44:season 
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

#Mn======
ggplot(data = q44dat, mapping = aes(y = Mn, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese COncentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Mn_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Mn, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Mn_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Mn, q44dat$Q44)


Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q44dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q44 + community + period + season
            + Q44:community + Q44:period + Q44:season 
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")

#Zn=====
ggplot(data = q44dat, mapping = aes(y = Zn, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cd_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Zn, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Zn_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Zn, q44dat$Q44)

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q44dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q44 + community + period + season
            + Q44:community + Q44:period + Q44:season 
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")



#Q11:Approximately, when was the current roof installed?====
q11 <- hds
q11$Q11 <- as.character(q11$Q11)
q11 <- q11[q11$Q11!="0",]
q11 <- q11[!is.na(q11$Q11),]
q11$Q11 <- q11$Q11
q11[q11$Q11=="1",]$Q11 <- "Pre 1940"
q11[q11$Q11=="2",]$Q11 <- "1941-1949"
q11[q11$Q11=="3",]$Q11 <- "1950-1959"
q11[q11$Q11=="4",]$Q11 <- "1960-1969"
q11[q11$Q11=="5",]$Q11 <- "1970-1979"
q11[q11$Q11=="6",]$Q11 <- "1980-1989"
q11[q11$Q11=="7",]$Q11 <- "1990-1999"
q11[q11$Q11=="8",]$Q11 <- "2000-2009"
q11[q11$Q11=="9",]$Q11 <- "2010-2018"

q11[q11$Q11=="1",]$Q11 <- "1945"
q11[q11$Q11=="2",]$Q11 <- "1950"
q11[q11$Q11=="3",]$Q11 <- "1955"
q11[q11$Q11=="4",]$Q11 <- "1960"
q11[q11$Q11=="5",]$Q11 <- "1975"
q11[q11$Q11=="6",]$Q11 <- "1985"
q11[q11$Q11=="7",]$Q11 <- "1995"
q11[q11$Q11=="8",]$Q11 <- "2005"
q11[q11$Q11=="9",]$Q11 <- "2015"



q11$Q11 <- factor(q11$Q11, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))

q11dat <- full_join(iw.dm, q11, by = c("site"))
q11dat <- q11dat[!is.na(q11dat$Q11),]
q11dat <- q11dat[!is.na(q11dat$community),]

q11dat$eighties <- c("Pre 1980")
# q9dat[q9dat$Q9=="1980-1989",]$eighties <- "Post 1980"
# q9dat[q9dat$Q9=="1990-1999",]$eighties <- "Post 1980"
# q9dat[q9dat$Q9=="2000-2009",]$eighties <- "Post 1980"
# q9dat[q9dat$Q9=="2010-2018",]$eighties <- "Post 1980"

q11dat[q11dat$Q11=="1980-1989",]$eighties <- "1980-1989"
q11dat[q11dat$Q11=="1990-1999",]$eighties <- "1990-1999"
q11dat[q11dat$Q11=="2000-2009",]$eighties <- "2000-2009"
q11dat[q11dat$Q11=="2010-2018",]$eighties <- "2010-2018"

summary(as.factor(q11dat$eighties))

ggplot(data = q11dat, mapping = aes(y = Al, x = Q11))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_roofage.png", res=100, height=6, width=8, units="in")

ggplot(data = q11dat, mapping = aes(y = Al, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_roofage2.png", res=100, height=6, width=8, units="in")
kruskal.test(q9dat$Al ~ q9dat$Q9)

ggplot(data = q11dat, mapping = aes(y = Al, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Aluminum Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_roofagecommssn.png", res=100, height=6, width=12, units="in")

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q11dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q11 + community + period + season
            + Q11:community + Q11:period + Q11:season 
            + (1|community:site),
            data = q11dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")



#Q11-Cd=====
ggplot(data = q11dat, mapping = aes(y = Cd, x = Q11))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Cadmium Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_roofage.png", res=100, height=6, width=8, units="in")

ggplot(data = q11dat, mapping = aes(y = Cd, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Cadmium Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_roofage2.png", res=100, height=6, width=8, units="in")

ggplot(data = q11dat, mapping = aes(y = Cd, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Cadmium Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Cd_houseagecommssn.png", res=100, height=6, width=12, units="in")


Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q11dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q11 + community + period + season
            + Q11:community + Q11:period + Q11:season 
            + (1|community:site),
            data = q11dat,
            REML = F)
anova(Cd1)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")
Cd2 <- lmer(log(Cd)~ Q11 + community + season
            + Q11:community 
            + (1|community:site),
            data = q11dat,
            REML = F)

anova(Cd2)
summary(Cd2)
cd.sum <- summary(Cd2)


#Q11-Zn=====
ggplot(data = q11dat, mapping = aes(y = Zn, x = Q11))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Zinc Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_roofage.png", res=100, height=6, width=8, units="in")

ggplot(data = q11dat, mapping = aes(y = Zn, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Zinc Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Zn_roofage2.png", res=100, height=6, width=8, units="in")
kruskal.test(q9dat$Zn ~ q9dat$Q9)

ggplot(data = q11dat, mapping = aes(y = Zn, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Zinc Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Al_houseagecommssn.png", res=100, height=6, width=12, units="in")

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q11dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q11 + community + period + season
            + Q11:community + Q11:period + Q11:season 
            + (1|community:site),
            data = q11dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")


#Q11-Cu=======
ggplot(data = q11dat, mapping = aes(y = Cu, x = Q11))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Copper Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q11dat, mapping = aes(y = Cu, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Copper Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cu_houseage2.png", res=100, height=6, width=8, units="in")
kruskal.test(q9dat$Cu ~ q9dat$Q9)



Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q11dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q11 + community + period + season
            + Q11:community + Q11:period + Q11:season 
            + (1|community:site),
            data = q11dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")


#Q11-Mn======
ggplot(data = q11dat, mapping = aes(y = Mn, x = Q11))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Manganese Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Mn_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q11dat, mapping = aes(y = Mn, x = Q11, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(x= "Year",
       y= "Manganese Concentration (ug/L)",
       title = "Year roof was installed")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Mn_houseage2.png", res=100, height=6, width=8, units="in")
kruskal.test(q9dat$Mn ~ q9dat$Q9)



Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q11dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q11 + community + period + season
            + Q11:community + Q11:period + Q11:season 
            + (1|community:site),
            data = q11dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")


Mn2 <- lmer(log(Mn)~  Q11 +period + season 
            + (1|community:site),
            data = q11dat,
            REML = F)
anova(Mn2)
summary(Mn2)
vif(Mn2)
mn.sum <- summary(Mn2)
write.csv(mn.sum$coefficients, "MncoefHDS2.csv")
mn.hdsmeans <- lsmeans(Mn2, pairwise~Q9,
                       adjust = "tukey")#
write.csv(mn.hdsmeans$contrasts, "MnHDScontrastsHDS9.csv")




model.effects <- allEffects(Al3)
plot(model.effects)



predict.dat <- ggeffect(model = Al3,
                        terms = c("Q9"),
                        back.transform = F,
                        type = "re")
predict.dat$Q9 <- predict.dat$x

plot <- ggplot(data= q9dat,
               mapping = aes(x=Q9,y=log(Al))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~Q9, scales = "free")+
  geom_pointrange(data = predict.dat, aes(x=Q9, y = predicted, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nHome Age",
       y = expression(paste("ln[As] (µg ",L^-1, ")")))+
  #scale_color_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
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
plot
dev.print(png, "Al_rhihomeageln.png", res=300, height=4, width=7, units="in")




ggplot(data = q9dat, aes(x = Q9, y = log(Al)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Al by Approximate Home Age\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_rhihomeage2ln.png", res=300, height=4, width=10, units="in")


ggplot(data = q9dat, aes(x = Q9, y = log(Mn)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Mn by Approximate Home Age\n",
       y = "ln(Mn) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Mn_rhihomeage2ln.png", res=300, height=4, width=10, units="in")

ggplot(data = q9dat, aes(x = Q9, y = log(Cd)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Cd by Approximate Home Age\n",
       y = "ln(Cd) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cd_rhihomeage2ln.png", res=300, height=4, width=10, units="in")

ggplot(data = q9dat, aes(x = Q9, y = log(Cu)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Cu by Approximate Home Age\n",
       y = "ln(Cu) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cu_rhihomeage2ln.png", res=300, height=4, width=10, units="in")

ggplot(data = q9dat, aes(x = Q9, y = log(Zn)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Zn by Approximate Home Age\n",
       y = "ln(Zn) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Zn_rhihomeage2ln.png", res=300, height=4, width=10, units="in")


#Q28:Is this home/community garden located near (less than 10 miles) an active mining site?=====
q28 <- hds
q44$Q44 <- as.character(q44$Q44)
q44 <- q44[q44$Q44!="0",]
q44 <- q44[!is.na(q44$Q44),]
q44[q44$Q44=="1",]$Q44 <- "Yes"
q44[q44$Q44=="2",]$Q44 <- "No"

q44$Q44 <- factor(q44$Q44, levels = c("Yes", "No"))
summary(q44$Q44)
q44dat <- full_join(iw.dm, q44, by = c("site"))
q44dat <- q44dat[!is.na(q44dat$Q44),]
q44dat <- q44dat[!is.na(q44dat$community),]

summary(as.factor(q44dat$Q44))

ggplot(data = q44dat, mapping = aes(y = Al, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum COncentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Al, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum COncentration (ug/L)",
       x= "Responses",
       title = "Home within two blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Al, q44dat$Q44)
q28 <- hds
q28$Q28 <- as.character(q30$Q30)
q28 <- q28[q28$Q28!="0",]
q28 <- q28[!is.na(q28$Q28),]
q28[q28$Q28=="1",]$Q28 <- "Yes"
q28[q28$Q28=="2",]$Q28 <- "No"

q28$Q28 <- factor(q28$Q28, levels = c("Yes", "No"))
summary(q28$Q28)
q28dat <- full_join(iw.dm, q28, by = c("site"))
q28dat <- q28dat[!is.na(q28dat$Q28),]
q28dat <- q28dat[!is.na(q28dat$community),]

summary(as.factor(q28dat$Q28))

ggplot(data = q28dat, mapping = aes(y = Al, x = Q28))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Al_10miles.png", res=100, height=6, width=6, units="in")

ggplot(data = q28dat, mapping = aes(y = Al, x = Q28, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum COncentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Al_10blocksactivecomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q28dat$Al, q28dat$Q28)
#Al=====
Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q28dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q28 + community + period + season
            + Q28:community + Q28:period + Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")

#Cd=====

ggplot(data = q28dat, mapping = aes(y = Cd, x = Q28))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cd_tenblocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q28dat, mapping = aes(y = Cd, x = Q28, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cd_2blockactivescomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q30dat$Cd, q30dat$Q30)

Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q28dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q28 + community + period + season
            + Q28:community + Q28:period + Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")

Cd2 <- lmer(log(Cd)~ Q28 + community + season
            +  Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)


#Cu======
ggplot(data = q28dat, mapping = aes(y = Cu, x = Q28))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cu_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q28dat, mapping = aes(y = Cu, x = Q28, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cu_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Cu, q44dat$Q44)

Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q28dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q28 + community + period + season
            + Q28:community + Q28:period + Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

Cu2 <- lmer(log(Cu)~ Q28 + community + period + season
            + Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)
anova(Cu2)
summary(Cu2)
vif(Cu2)
cu.sum <- summary(Cu2)
write.csv(cu.sum$coefficients, "Cucoefactive.csv")
cu.hdsmeans <- lsmeans(Cu2, pairwise~Q28:season,
                       adjust = "tukey")#
write.csv(cu.hdsmeans$contrasts, "Cuactivecontrasts.csv")

#Mn======
ggplot(data = q28dat, mapping = aes(y = Mn, x = Q28))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Mn_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q28dat, mapping = aes(y = Mn, x = Q28, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Mn_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q28dat$Mn, q28dat$Q28)


Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q28dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q28 + community + period + season
            + Q28:community + Q28:period + Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")

#Zn=====
ggplot(data = q28dat, mapping = aes(y = Zn, x = Q28))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active  mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Zn_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q28dat, mapping = aes(y = Zn, x = Q28, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of an active mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Zn_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Zn, q44dat$Q44)

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q28dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q28 + community + period + season
            + Q28:community + Q28:period + Q28:season 
            + (1|community:site),
            data = q28dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")
anova(Zn1)

Zn2 <- lmer(log(Zn)~ Q28 + community +  period + season
            + Q28:community +Q28:season
            + (1|community:site),
            data = q28dat,
            REML = F)
summary(Zn2)
vif(Zn2)
anova(Zn2)
zn.sum <- summary(Zn2)
write.csv(zn.sum$coefficients, "Zncoefactive.csv")
zn.means <- lsmeans(Zn2, pairwise~Q28:season, adjust = "tukey")#
write.csv(zn.means$contrasts, "Zn2contrastactive.csv")





#Q30: Is this home/community garden located near (less than 10 miles) from an inactive/legacy mining site?====
q30 <- hds
q30$Q30 <- as.character(q30$Q30)
q30 <- q30[q30$Q30!="0",]
q30 <- q30[!is.na(q30$Q30),]
q30[q30$Q30=="1",]$Q30 <- "Yes"
q30[q30$Q30=="2",]$Q30 <- "No"

q30$Q30 <- factor(q30$Q30, levels = c("Yes", "No"))
summary(q30$Q30)
q30dat <- full_join(iw.dm, q30, by = c("site"))
q30dat <- q30dat[!is.na(q30dat$Q30),]
q30dat <- q30dat[!is.na(q30dat$community),]

summary(as.factor(q30dat$Q30))

ggplot(data = q30dat, mapping = aes(y = Al, x = Q30))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Aluminum Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Al_10miles.png", res=100, height=6, width=6, units="in")

ggplot(data = q30dat, mapping = aes(y = Al, x = Q30, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Aluminum COncentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Al_10blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Al, q44dat$Q44)
#Al=====
Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q30dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q30 + community + period + season
            + Q30:community + Q30:period + Q30:season 
            + (1|community:site),
            data = q30dat,
            REML = F)
summary(Al1)
vif(Al1)
step(Al1, scope=list(lower=Al0), direction="backward")

#Cd=====

ggplot(data = q30dat, mapping = aes(y = Cd, x = Q30))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cd_tenblocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q30dat, mapping = aes(y = Cd, x = Q30, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Cadmium Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q30dat$Cd, q30dat$Q30)

Cd0 <- lmer(log(Cd)~
              + (1|community:site),
            data = q30dat,
            REML = F)
Cd1 <- lmer(log(Cd)~ Q30 + community + period + season
            + Q30:community + Q30:period + Q30:season 
            + (1|community:site),
            data = q30dat,
            REML = F)
summary(Cd1)
vif(Cd1)
step(Cd1, scope=list(lower=Cd0), direction="backward")

#Cu======
ggplot(data = q30dat, mapping = aes(y = Cu, x = Q30))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cu_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q30dat, mapping = aes(y = Cu, x = Q30, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Copper Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Cu_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Cu, q44dat$Q44)

Cu0 <- lmer(log(Cu)~
              + (1|community:site),
            data = q30dat,
            REML = F)
Cu1 <- lmer(log(Cu)~ Q30 + community + period + season
            + Q30:community + Q30:period + Q30:season 
            + (1|community:site),
            data = q30dat,
            REML = F)
summary(Cu1)
vif(Cu1)
step(Cu1, scope=list(lower=Cu0), direction="backward")

#Mn======
ggplot(data = q30dat, mapping = aes(y = Mn, x = Q30))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Manganese COncentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Mn_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q30dat, mapping = aes(y = Mn, x = Q30, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Manganese Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Mn_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q30dat$Mn, q30dat$Q30)


Mn0 <- lmer(log(Mn)~
              + (1|community:site),
            data = q30dat,
            REML = F)
Mn1 <- lmer(log(Mn)~ Q30 + community + period + season
            + Q30:community + Q30:period + Q30:season 
            + (1|community:site),
            data = q30dat,
            REML = F)
summary(Mn1)
vif(Mn1)
step(Mn1, scope=list(lower=Mn0), direction="backward")

#Zn=====
ggplot(data = q30dat, mapping = aes(y = Zn, x = Q30))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Zn_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q30dat, mapping = aes(y = Zn, x = Q30, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(y= "Zinc Concentration (ug/L)",
       x= "Responses",
       title = "Home within ten miles of a legacy mine?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Zn_2blockscomm.png", res=100, height=6, width=6, units="in")
kruskal.test(q44dat$Zn, q44dat$Q44)

Zn0 <- lmer(log(Zn)~
              + (1|community:site),
            data = q30dat,
            REML = F)
Zn1 <- lmer(log(Zn)~ Q30 + community + period + season
            + Q30:community + Q30:period + Q30:season 
            + (1|community:site),
            data = q30dat,
            REML = F)
summary(Zn1)
vif(Zn1)
step(Zn1, scope=list(lower=Zn0), direction="backward")
anova(Zn1)

Zn2 <- lmer(log(Zn)~ Q30 +  period + season
            + Q30:community
            + (1|community:site),
            data = q30dat,
            REML = F)
summary(Zn2)
vif(Zn2)
anova(Zn2)
zn.sum <- summary(Zn2)
write.csv(zn.sum$coefficients, "Zncoeflegacy.csv")
zn.means <- lsmeans(Zn2, pairwise~Q30:community, adjust = "tukey")#
write.csv(zn.means$contrasts, "Zn2contrastlegacy.csv")


#74: When was the last time you washed/treated the inside of your cistern?=====
q74 <- hds
q74$Q74 <- q74$Q74
q74$Q74 <- as.character(q74$Q74)
q74 <- q74[q74$Q74!="0",]
q74 <- q74[!is.na(q74$Q74),]
q74[q74$Q74=="1",]$Q74 <- "1 month ago"
q74[q74$Q74=="2",]$Q74<- "2-3 months ago"
q74[q74$Q74=="3",]$Q74 <- "4-6 months ago"
q74[q74$Q74=="4",]$Q74 <- "6-12 months ago"
q74[q74$Q74=="5",]$Q74 <- ">1 year ago"


q74$Q74 <- factor(q74$Q74, levels = c("1 month ago", "2-3 months ago", "4-6 months ago", "6-12 months ago", ">1 year ago"))

q74$Q74 <- as.character(q74$Q74)
q74[q74$Q74=="1",]$Q74 <- "1 month ago"
q74[q74$Q74=="2",]$Q74<- "2-3 months ago"
q74[q74$Q74=="3",]$Q74 <- "4-6 months ago"
q74[q74$Q74=="4",]$Q74 <- "6-12 months ago"
q74[q74$Q74=="5",]$Q74 <- ">1 year ago"

q74$Q74 <- factor(q74$Q74, levels = c("1 month ago", "2-3 months ago", "4-6 months ago", "6-12 months ago", ">1 year ago"))
summary(q74$Q74)
q74dat <- full_join(iw.dm, q74, by = c("site"))
q74dat <- q74dat[!is.na(q74dat$Q74),]
q74dat <- q74dat[!is.na(q74dat$community),]

summary(as.factor(q74dat$Q74))
