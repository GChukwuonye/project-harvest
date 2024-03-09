#Kunal Palawat, Gift Chukwuonye
#Description: modeling analytes overall
#

#load libraries----
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
library(wesanderson)
library(palettetown)
library(pals)

#load data ----
iws.c <- iw.dm %>%
  drop_na(pH) %>%
  drop_na(prox.normal) %>%
  group_by(community) %>%
  group_split()

iws.dh <- iws.c[[1]]
iws.gm <- iws.c[[2]]
iws.hw <- iws.c[[3]]
iws.tu <- iws.c[[4]]

#add ward and sub location to Tucson data
tuc <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "tucson", col_names = TRUE)
iws.tu <- full_join(iws.tu, tuc, by = c("site"))
iws.tu <- iws.tu %>%
  drop_na(ward) %>%
  drop_na(community) %>%
  drop_na(location)
iws.tu$ward <- factor(iws.tu$ward, levels = c("One", "Two", "Three", "Four", "Five", "Six"))

#add sub location to globe data
glo <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "globe", col_names = TRUE)
iws.gm <- full_join(iws.gm, glo, by = c("site"))
iws.gm <- iws.gm %>%
  drop_na(community) %>%
  drop_na(location_2)
iws.gm$location_2 <- factor(iws.gm$location_2, levels = c("Miami/Claypool Area", "Globe Area", "Canyons Area"))

## dh ----
###Ag ----
Ag.dh.0 <- lmer(data = iws.dh,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.dh.0))

Ag.dh.1 <- lmer(data = iws.dh,
                log(Ag) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Ag.dh.1))

Ag.dh.2.step <- step(Ag.dh.1)
Ag.dh.2.step
Ag.dh.2 <- get_model(Ag.dh.2.step)
print(summary(Ag.dh.2))
check_model(Ag.dh.2)
anova(Ag.dh.1)
print(anova(Ag.dh.2))
#nothing

###Al ----
al.dh.0 <- lmer(data = iws.dh,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.dh.0))

al.dh.1 <- lmer(data = iws.dh,
                log(Al) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(al.dh.1))

al.dh.2.step <- step(al.dh.1)
al.dh.2.step
al.dh.2 <- get_model(al.dh.2.step)
print(summary(al.dh.2))
check_model(al.dh.2)
anova(al.dh.1)
print(anova(al.dh.2))
#nothing signif

###As ----
As.dh.0 <- lmer(data = iws.dh,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.dh.0))

As.dh.1 <- lmer(data = iws.dh,
                log(As) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(As.dh.1))

As.dh.2.step <- step(As.dh.1)
As.dh.2.step
As.dh.2 <- get_model(As.dh.2.step)
As.dh.2 <- lmer(data = iws.dh,
                log(As) ~ season+
                  (1|site),
                REML = T)
print(summary(As.dh.2))
check_model(As.dh.2)
anova(As.dh.1)
print(anova(As.dh.2))
perf <- performance(As.dh.2)
perf
write.csv(perf, "asdh_diag.csv")

#season only


###Ba ----
ba.dh.0 <- lmer(data = iws.dh,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.dh.0))

ba.dh.1 <- lmer(data = iws.dh,
                log(Ba) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(ba.dh.1))

ba.dh.2.step <- step(ba.dh.1)
ba.dh.2.step
ba.dh.2 <- get_model(ba.dh.2.step)
print(summary(ba.dh.2))
ba.dh.2 <- lm(data = iws.dh,
                log(Ba) ~ season  +pH)
print(summary(ba.dh.2))
anova(ba.dh.1)
print(anova(ba.dh.2))
check_model(ba.dh.2)
perf <- performance(ba.dh.2)
perf
write.csv(perf, "badh_diag.csv")

#season  and pH

###Be ----
Be.dh.0 <- lmer(data = iws.dh,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.dh.0))

Be.dh.1 <- lmer(data = iws.dh,
                log(Be) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Be.dh.1))

Be.dh.2.step <- step(Be.dh.1)
Be.dh.2.step
Be.dh.2 <- get_model(Be.dh.2.step)
print(summary(Be.dh.2))
check_model(Be.dh.2)
anova(Be.dh.1)
print(anova(Be.dh.2))
performance(Be.dh.2)
#nothing

###Cd ----
Cd.dh.0 <- lmer(data = iws.dh,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.dh.0))

Cd.dh.1 <- lmer(data = iws.dh,
                log(Cd) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Cd.dh.1))

Cd.dh.2.step <- step(Cd.dh.1)
Cd.dh.2.step
Cd.dh.2 <- get_model(Cd.dh.2.step)
print(summary(Cd.dh.2))
Cd.dh.2 <- lm(data = iws.dh,
                log(Cd) ~ season)
print(summary(Cd.dh.2))
check_model(Cd.dh.2)
anova(Cd.dh.1)
print(anova(Cd.dh.2))
perf <- performance(Cd.dh.2)
perf
write.csv(perf, "cddh_diag.csv")
#season only

###Co ----
Co.dh.0 <- lmer(data = iws.dh,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.dh.0))

Co.dh.1 <- lmer(data = iws.dh,
                log(Co) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Co.dh.1))

Co.dh.2.step <- step(Co.dh.1)
Co.dh.2.step
Co.dh.2 <- get_model(Co.dh.2.step)
print(summary(Co.dh.2))
Co.dh.2 <- lm(data = iws.dh,
                log(Co) ~ season)
print(summary(Co.dh.2))
check_model(Co.dh.2)
anova(Co.dh.1)
print(anova(Co.dh.2))
perf <- performance(Co.dh.2)
perf
write.csv(perf, "codh_diag.csv")
#season only

###Cr ----
Cr.dh.0 <- lmer(data = iws.dh,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.dh.0))

Cr.dh.1 <- lmer(data = iws.dh,
                log(Cr) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Cr.dh.1))

Cr.dh.2.step <- step(Cr.dh.1)
Cr.dh.2.step
Cr.dh.2 <- get_model(Cr.dh.2.step)
print(summary(Cr.dh.2))
check_model(Cr.dh.2)
anova(Cr.dh.1)
print(anova(Cr.dh.2))
performance(Cr.dh.2)
#nothing

###Cu ----
Cu.dh.0 <- lmer(data = iws.dh,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.dh.0))

Cu.dh.1 <- lmer(data = iws.dh,
                log(Cu) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Cu.dh.1))

Cu.dh.2.step <- step(Cu.dh.1)
Cu.dh.2.step
Cu.dh.2 <- get_model(Cu.dh.2.step)
print(summary(Cu.dh.2))
check_model(Cu.dh.2)
anova(Cu.dh.1)
print(anova(Cu.dh.2))
performance(Cu.dh.2)
#nothing

###Fe ----
Fe.dh.0 <- lmer(data = iws.dh,
                log(Fe) ~
                  (1|site),
                REML = F)
print(summary(Fe.dh.0))

Fe.dh.1 <- lmer(data = iws.dh,
                log(Fe) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Fe.dh.1))


Fe.dh.2.step <- step(Fe.dh.1)
Fe.dh.2.step
Fe.dh.2 <- get_model(Fe.dh.2.step)
print(summary(Fe.dh.2))
Fe.dh.2 <- lmer(data = iws.dh,
                log(Fe) ~ season+
                  (1|site),
                REML = T)
print(summary(Fe.dh.2))
check_model(Fe.dh.2)
anova(Fe.dh.1)
print(anova(Fe.dh.2))
perf <- performance(Fe.dh.2)
perf
write.csv(perf, "fedh_diag.csv")
#season

###Mn ----
Mn.dh.0 <- lmer(data = iws.dh,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(Mn.dh.0))

Mn.dh.1 <- lmer(data = iws.dh,
                log(Mn) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Mn.dh.1))

Mn.dh.2.step <- step(Mn.dh.1)
Mn.dh.2.step
Mn.dh.2 <- get_model(Mn.dh.2.step)
print(summary(Mn.dh.2))
Mn.dh.2 <- lm(data = iws.dh,
                log(Mn) ~ season)
print(summary(Mn.dh.2))
check_model(Mn.dh.2)
anova(Mn.dh.1)
print(anova(Mn.dh.2))
perf <- performance(Mn.dh.2)
perf
write.csv(perf, "mndh_diag.csv")
#season only


###Mo ----
mo.dh.0 <- lmer(data = iws.dh,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.dh.0))

mo.dh.1 <- lmer(data = iws.dh,
                log(Mo) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(mo.dh.1))

mo.dh.2.step <- step(mo.dh.1)
mo.dh.2.step
mo.dh.2 <- get_model(mo.dh.2.step)
print(summary(mo.dh.2))
mo.dh.2 <- lm(data = iws.dh,
                log(Mo) ~ pH)
print(summary(mo.dh.2))
anova(mo.dh.1)
print(anova(mo.dh.2))
check_model(mo.dh.2)
perf <- performance(mo.dh.2)
perf
write.csv(perf, "modh_diag.csv")
#pH

###Ni ----
ni.dh.0 <- lmer(data = iws.dh,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.dh.0))

ni.dh.1 <- lmer(data = iws.dh,
                log(Ni) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(ni.dh.1))

ni.dh.2.step <- step(ni.dh.1)
ni.dh.2.step
ni.dh.2 <- get_model(ni.dh.2.step)
print(summary(ni.dh.2))
ni.dh.2 <- lm(data = iws.dh,
                log(Ni) ~ season)
print(summary(ni.dh.2))
anova(ni.dh.1)
print(anova(ni.dh.2))
check_model(ni.dh.2)
perf <- performance(ni.dh.2)
perf
write.csv(perf, "nidh_diag.csv")
#season only

###Pb ----
Pb.dh.0 <- lmer(data = iws.dh,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.dh.0))

Pb.dh.1 <- lmer(data = iws.dh,
                log(Pb) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Pb.dh.1))

Pb.dh.2.step <- step(Pb.dh.1)
Pb.dh.2.step
Pb.dh.2 <- get_model(Pb.dh.2.step)
print(summary(Pb.dh.2))
check_model(Pb.dh.2)
anova(Pb.dh.1)
print(anova(Pb.dh.2))
performance(Pb.dh.2)
plot(allEffects(Pb.dh.2))
#score only

###Sb ----
Sb.dh.0 <- lmer(data = iws.dh,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.dh.0))

Sb.dh.1 <- lmer(data = iws.dh,
                log(Sb) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Sb.dh.1))

Sb.dh.2.step <- step(Sb.dh.1)
Sb.dh.2.step
Sb.dh.2 <- get_model(Sb.dh.2.step)
print(summary(Sb.dh.2))
check_model(Sb.dh.2)
anova(Sb.dh.1)
print(anova(Sb.dh.2))
performance(Sb.dh.2)
#nothing

###Se ----
Se.dh.0 <- lmer(data = iws.dh,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.dh.0))

Se.dh.1 <- lmer(data = iws.dh,
                log(Se) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Se.dh.1))

Se.dh.2.step <- step(Se.dh.1)
Se.dh.2.step
Se.dh.2 <- get_model(Se.dh.2.step)
print(summary(Se.dh.2))
Se.dh.2 <- lm(data = iws.dh,
                log(Se) ~ pH)
print(summary(Se.dh.2))
check_model(Se.dh.2)
anova(Se.dh.1)
print(anova(Se.dh.2))
perf <- performance(Se.dh.2)
perf
write.csv(perf, "sedh_diag.csv")
#pH only

###Sn ----
Sn.dh.0 <- lmer(data = iws.dh,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.dh.0))

Sn.dh.1 <- lmer(data = iws.dh,
                log(Sn) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Sn.dh.1))

Sn.dh.2.step <- step(Sn.dh.1)
Sn.dh.2.step
Sn.dh.2 <- get_model(Sn.dh.2.step)
print(summary(Sn.dh.2))
check_model(Sn.dh.2)
anova(Sn.dh.1)
print(anova(Sn.dh.2))
performance(Sn.dh.2)
#nothing

###V ----
v.dh.0 <- lmer(data = iws.dh,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.dh.0))

v.dh.1 <- lmer(data = iws.dh,
               log(V) ~ season + prox.normal +  pH +
                 (1|site),
               REML = F)
print(summary(v.dh.1))

v.dh.2.step <- step(v.dh.1)
v.dh.2.step
v.dh.2 <- get_model(v.dh.2.step)
print(summary(v.dh.2))
v.dh.2 <- lm(data = iws.dh,
               log(V) ~ season +  pH)
print(summary(v.dh.2))
anova(v.dh.1)
print(anova(v.dh.2))
check_model(v.dh.2)
perf <- performance(v.dh.2)
perf
write.csv(perf, "vdh_diag.csv")
#pH and season

###Zn ----
Zn.dh.0 <- lmer(data = iws.dh,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.dh.0))

Zn.dh.1 <- lmer(data = iws.dh,
                log(Zn) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Zn.dh.1))

Zn.dh.2.step <- step(Zn.dh.1)
Zn.dh.2.step
Zn.dh.2 <- get_model(Zn.dh.2.step)
print(summary(Zn.dh.2))
check_model(Zn.dh.2)
anova(Zn.dh.1)
print(anova(Zn.dh.2))
performance(Zn.dh.2)
#nothing


# 
# 
# lapply(X = contam_list,
#        FUN = hdslmerstepFX,
#        dataDF = iw.score.long,
#        dfname.string = ".dh")
# 
# hdslmerstepFX(datalongDF = iws.dh[iws.dh$analyte == "Mn",],
#               analyte.string = "Mn",
#               dfname.string = ".dh")


## gm ----
###Ag ----
Ag.gm.0 <- lmer(data = iws.gm,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.gm.0))
check_model(Ag.gm.0)

Ag.gm.1 <- lmer(data = iws.gm,
                log(Ag) ~ season + prox.normal+location_2 + pH +
                  (1|site),
                REML = F)
print(summary(Ag.gm.1))
check_model(Ag.gm.1)
vif(Ag.gm.1)
Ag.gm.2.step <- step(Ag.gm.1)
Ag.gm.2.step
Ag.gm.2 <- get_model(Ag.gm.2.step)
print(summary(Ag.gm.2))
Ag.gm.2 <- lm(data = iws.gm,
                log(Ag) ~ location_2 +  pH)
print(summary(Ag.gm.2))
check_model(Ag.gm.2)
anova(Ag.gm.1)
print(anova(Ag.gm.2))
plot(allEffects(Ag.gm.2))
perf <- performance(Ag.gm.2)
perf
write.csv(perf, "aggm_diag.csv")
#location and pH

# ggplot(data = iws.gm, mapping = aes(color = location_2, x = prox.normal, y = Al))+
#   geom_point()

###Al ----
al.gm.0 <- lmer(data = iws.gm,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.gm.0))

al.gm.1 <- lmer(data = iws.gm,
                log(Al) ~ season + prox.normal+location_2 +  pH +
                  (1|site),
                REML = F)
print(summary(al.gm.1))
check_model(al.gm.1)
al.gm.2.step <- step(al.gm.1)
al.gm.2.step
al.gm.2 <- get_model(al.gm.2.step)
print(summary(al.gm.2))
al.gm.2 <- lmer(data = iws.gm,
                log(Al) ~ prox.normal+
                  (1|site),
                REML = T)
print(summary(al.gm.2))
check_model(al.gm.2)
anova(al.gm.1)
print(anova(al.gm.2))
perf <- performance(al.gm.2)
perf
write.csv(perf, "algm_diag.csv")
plot(allEffects(al.gm.2))
#prox and score

###As ----
As.gm.0 <- lmer(data = iws.gm,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.gm.0))

As.gm.1 <- lmer(data = iws.gm,
                log(As) ~ season + prox.normal+location_2 +  pH +
                  (1|site),
                REML = F)
print(summary(As.gm.1))

As.gm.2.step <- step(As.gm.1)
As.gm.2.step
As.gm.2 <- get_model(As.gm.2.step)
print(summary(As.gm.2))
As.gm.2 <- lm(data = iws.gm,
                log(As) ~ season +  pH + prox.normal)
print(summary(As.gm.2))
check_model(As.gm.2)
anova(As.gm.1)
print(anova(As.gm.2))
perf <- performance(As.gm.2)
perf
write.csv(perf, "asgm_diag.csv")
plot(allEffects(As.gm.2))
#prox pH season

###Ba ----
ba.gm.0 <- lmer(data = iws.gm,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.gm.0))

ba.gm.1 <- lmer(data = iws.gm,
                log(Ba) ~ season + prox.normal + location_2+  pH +
                  (1|site),
                REML = F)
print(summary(ba.gm.1))

ba.gm.2.step <- step(ba.gm.1)
ba.gm.2.step
ba.gm.2 <- get_model(ba.gm.2.step)
print(summary(ba.gm.2))
ba.gm.2 <- lmer(data = iws.gm,
                log(Ba) ~ season +  pH+prox.normal +
                  (1|site),
                REML = T)
print(summary(ba.gm.2))
anova(ba.gm.1)
print(anova(ba.gm.2))
check_model(ba.gm.2)
perf <- performance(ba.gm.2)
perf
write.csv(perf, "bagm_diag.csv")
plot(allEffects(ba.gm.2))
#prox pH season

###Be ----
Be.gm.0 <- lmer(data = iws.gm,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.gm.0))

Be.gm.1 <- lmer(data = iws.gm,
                log(Be) ~ season + prox.normal +  location_2+pH +
                  (1|site),
                REML = F)
print(summary(Be.gm.1))

Be.gm.2.step <- step(Be.gm.1)
Be.gm.2.step
Be.gm.2 <- get_model(Be.gm.2.step)
print(summary(Be.gm.2))
Be.gm.2 <- lmer(data = iws.gm,
                log(Be) ~ season + prox.normal +
                  (1|site),
                REML = T)
print(summary(Be.gm.2))
check_model(Be.gm.2)
anova(Be.gm.1)
print(anova(Be.gm.2))
perf <- performance(Be.gm.2)
perf
write.csv(perf, "begm_diag.csv")
plot(allEffects(Be.gm.2))
#prox season score

###Cd ----
Cd.gm.0 <- lmer(data = iws.gm,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.gm.0))

Cd.gm.1 <- lmer(data = iws.gm,
                log(Cd) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Cd.gm.1))

Cd.gm.2.step <- step(Cd.gm.1)
Cd.gm.2.step
Cd.gm.2 <- get_model(Cd.gm.2.step)
print(summary(Cd.gm.2))
Cd.gm.2 <- lmer(data = iws.gm,
                log(Cd) ~ season + prox.normal +
                  (1|site),
                REML = T)
print(summary(Cd.gm.2))
check_model(Cd.gm.2)
anova(Cd.gm.1)
print(anova(Cd.gm.2))
perf <- performance(Cd.gm.2)
perf
write.csv(perf, "cdgm_diag.csv")
plot(allEffects(Cd.gm.2))
#season and prox

###Co ----
Co.gm.0 <- lmer(data = iws.gm,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.gm.0))

Co.gm.1 <- lmer(data = iws.gm,
                log(Co) ~ season + prox.normal +  location_2+pH +
                  (1|site),
                REML = F)
print(summary(Co.gm.1))

Co.gm.2.step <- step(Co.gm.1)
Co.gm.2.step
Co.gm.2 <- get_model(Co.gm.2.step)
print(summary(Co.gm.2))
Co.gm.2 <- lmer(data = iws.gm,
                log(Co) ~ season+
                  (1|site),
                REML = T)
print(summary(Co.gm.2))
check_model(Co.gm.2)
anova(Co.gm.1)
print(anova(Co.gm.2))
perf <- performance(Co.gm.2)
perf
write.csv(perf, "cogm_diag.csv")
#season only

###Cr ----
Cr.gm.0 <- lmer(data = iws.gm,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.gm.0))

Cr.gm.1 <- lmer(data = iws.gm,
                log(Cr) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Cr.gm.1))

Cr.gm.2.step <- step(Cr.gm.1)
Cr.gm.2.step
Cr.gm.2 <- get_model(Cr.gm.2.step)
print(summary(Cr.gm.2))
Cr.gm.2 <- lmer(data = iws.gm,
                log(Cr) ~ season +
                  (1|site),
                REML = T)
print(summary(Cr.gm.2))
check_model(Cr.gm.2)
anova(Cr.gm.1)
print(anova(Cr.gm.2))
perf <- performance(Cr.gm.2)
perf
write.csv(perf, "crgm_diag.csv")
#season

###Cu ----
Cu.gm.0 <- lmer(data = iws.gm,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.gm.0))

Cu.gm.1 <- lmer(data = iws.gm,
                log(Cu) ~ season + prox.normal +  location_2+pH +
                  (1|site),
                REML = F)
print(summary(Cu.gm.1))

Cu.gm.2.step <- step(Cu.gm.1)
Cu.gm.2.step
Cu.gm.2 <- get_model(Cu.gm.2.step)
print(summary(Cu.gm.2))
Cu.gm.2 <- lmer(data = iws.gm,
                log(Cu) ~ season +
                  (1|site),
                REML = T)
print(summary(Cu.gm.2))
check_model(Cu.gm.2)
anova(Cu.gm.1)
print(anova(Cu.gm.2))
perf <- performance(Cu.gm.2)
perf
write.csv(perf, "cugm_diag.csv")
Cu.gm.3 <- lmer(data = iws.gm,
                log(Cu) ~ season + prox.normal +
                  (1|site),
                REML = F)
print(summary(Cu.gm.3))

#season

###Fe ----
Fe.gm.0 <- lmer(data = iws.gm,
                log(Fe) ~
                  (1|site),
                REML = F)
print(summary(Fe.gm.0))

Fe.gm.1 <- lmer(data = iws.gm,
                log(Fe) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Fe.gm.1))

Fe.gm.2.step <- step(Fe.gm.1)
Fe.gm.2.step
Fe.gm.2 <- get_model(Fe.gm.2.step)
print(summary(Fe.gm.2))
check_model(Fe.gm.2)
anova(Fe.gm.1)
print(anova(Fe.gm.2))
performance(Fe.gm.2)
#nothing

###Mn ----
Mn.gm.0 <- lmer(data = iws.gm,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(Mn.gm.0))

Mn.gm.1 <- lmer(data = iws.gm,
                log(Mn) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Mn.gm.1))

Mn.gm.2.step <- step(Mn.gm.1)
Mn.gm.2.step
Mn.gm.2 <- get_model(Mn.gm.2.step)
print(summary(Mn.gm.2))
Mn.gm.2 <- lmer(data = iws.gm,
                log(Mn) ~ season +
                  (1|site),
                REML = T)
print(summary(Mn.gm.2))
check_model(Mn.gm.2)
anova(Mn.gm.1)
print(anova(Mn.gm.2))
perf <- performance(Mn.gm.2)
perf
write.csv(perf, "mngm_diag.csv")
plot(allEffects(Mn.gm.2))
#season only


###Mo ----
mo.gm.0 <- lmer(data = iws.gm,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.gm.0))

mo.gm.1 <- lmer(data = iws.gm,
                log(Mo) ~ season + prox.normal +  location_2+pH +
                  (1|site),
                REML = F)
print(summary(mo.gm.1))

mo.gm.2.step <- step(mo.gm.1)
mo.gm.2.step
mo.gm.2 <- get_model(mo.gm.2.step)
print(summary(mo.gm.2))
mo.gm.2 <- lmer(data = iws.gm,
                log(Mo) ~ season +  pH + prox.normal+
                  (1|site),
                REML = T)
print(summary(mo.gm.2))
anova(mo.gm.1)
print(anova(mo.gm.2))
check_model(mo.gm.2)
perf <- performance(mo.gm.2)
perf
write.csv(perf, "mogm_diag.csv")
plot(allEffects(mo.gm.2))
#pH prox season

###Ni ----
ni.gm.0 <- lmer(data = iws.gm,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.gm.0))

ni.gm.1 <- lmer(data = iws.gm,
                log(Ni) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(ni.gm.1))

ni.gm.2.step <- step(ni.gm.1)
ni.gm.2.step
ni.gm.2 <- get_model(ni.gm.2.step)
print(summary(ni.gm.2))
ni.gm.2 <- lmer(data = iws.gm,
                log(Ni) ~ season+
                  (1|site),
                REML = T)
print(summary(ni.gm.2))
anova(ni.gm.1)
print(anova(ni.gm.2))
check_model(ni.gm.2)
perf <- performance(ni.gm.2)
perf
write.csv(perf, "nigm_diag.csv")
#season only, low marg r2

###Pb ----
Pb.gm.0 <- lmer(data = iws.gm,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.gm.0))

Pb.gm.1 <- lmer(data = iws.gm,
                log(Pb) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Pb.gm.1))

Pb.gm.2.step <- step(Pb.gm.1)
Pb.gm.2.step
Pb.gm.2 <- get_model(Pb.gm.2.step)
print(summary(Pb.gm.2))
Pb.gm.2 <- lmer(data = iws.gm,
                log(Pb) ~ season + prox.normal  +
                  (1|site),
                REML = T)
print(summary(Pb.gm.2))
check_model(Pb.gm.2)
anova(Pb.gm.1)
print(anova(Pb.gm.2))
perf <- performance(Pb.gm.2)
perf
write.csv(perf, "pbdm_diag.csv")
plot(allEffects(Pb.gm.2))
#season prox only

###Sb ----
Sb.gm.0 <- lmer(data = iws.gm,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.gm.0))

Sb.gm.1 <- lmer(data = iws.gm,
                log(Sb) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Sb.gm.1))

Sb.gm.2.step <- step(Sb.gm.1)
Sb.gm.2.step
Sb.gm.2 <- get_model(Sb.gm.2.step)
print(summary(Sb.gm.2))

check_model(Sb.gm.2)
anova(Sb.gm.1)
print(anova(Sb.gm.2))
performance(Sb.gm.2)
#nothing

###Se ----
Se.gm.0 <- lmer(data = iws.gm,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.gm.0))

Se.gm.1 <- lmer(data = iws.gm,
                log(Se) ~ season + prox.normal + location_2+ pH +
                  (1|site),
                REML = F)
print(summary(Se.gm.1))

Se.gm.2.step <- step(Se.gm.1)
Se.gm.2.step
Se.gm.2 <- get_model(Se.gm.2.step)
print(summary(Se.gm.2))
check_model(Se.gm.2)
anova(Se.gm.1)
print(anova(Se.gm.2))
performance(Se.gm.2)
#nothing

###Sn ----
Sn.gm.0 <- lmer(data = iws.gm,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.gm.0))

Sn.gm.1 <- lmer(data = iws.gm,
                log(Sn) ~ season + prox.normal +location_2+pH +
                  (1|site),
                REML = F)
print(summary(Sn.gm.1))

Sn.gm.2.step <- step(Sn.gm.1)
Sn.gm.2.step
Sn.gm.2 <- get_model(Sn.gm.2.step)
print(summary(Sn.gm.2))
check_model(Sn.gm.2) #location is highly colinear, remove from model, keep proximity because it has a lower VIF

Sn.gm.1 <- lmer(data = iws.gm,
                log(Sn) ~ season + prox.normal ++pH +
                  (1|site),
                REML = F)
Sn.gm.2.step <- step(Sn.gm.1)
Sn.gm.2.step
Sn.gm.2 <- get_model(Sn.gm.2.step)
print(summary(Sn.gm.2))
check_model(Sn.gm.2)
print(summary(Sn.gm.2))

check_model(Sn.gm.2)
anova(Sn.gm.1)
print(anova(Sn.gm.2))
perf<- performance(Sn.gm.2)
perf
write.csv(perf, "sngm_diag.csv")
plot(allEffects(Sn.gm.2))
#season

###V ----
v.gm.0 <- lmer(data = iws.gm,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.gm.0))

v.gm.1 <- lmer(data = iws.gm,
               log(V) ~ season + prox.normal + location_2+ pH +
                 (1|site),
               REML = F)
print(summary(v.gm.1))

v.gm.2.step <- step(v.gm.1)
v.gm.2.step
v.gm.2 <- get_model(v.gm.2.step)
print(summary(v.gm.2))
v.gm.2 <- lmer(data = iws.gm,
               log(V) ~ season +
                 (1|site),
               REML = T)
print(summary(v.gm.2))
anova(v.gm.1)
print(anova(v.gm.2))
check_model(v.gm.2)
perf <- performance(v.gm.2)
perf
write.csv(perf, "vgm_diag.csv")
#season only score almost signif

###Zn ----
Zn.gm.0 <- lmer(data = iws.gm,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.gm.0))

Zn.gm.1 <- lmer(data = iws.gm,
                log(Zn) ~ season + prox.normal +  location_2+pH +
                  (1|site),
                REML = F)
print(summary(Zn.gm.1))

Zn.gm.2.step <- step(Zn.gm.1)
Zn.gm.2.step
Zn.gm.2 <- get_model(Zn.gm.2.step)
print(summary(Zn.gm.2))
Zn.gm.2 <- lmer(data = iws.gm,
                log(Zn) ~ season+
                  (1|site),
                REML = T)
print(summary(Zn.gm.2))
check_model(Zn.gm.2)
anova(Zn.gm.1)
print(anova(Zn.gm.2))
perf <- performance(Zn.gm.2)
perf
write.csv(perf, "zngm_diag.csv")
#season

## hw ----
###Ag ----
Ag.hw.0 <- lmer(data = iws.hw,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.hw.0))

Ag.hw.1 <- lmer(data = iws.hw,
                log(Ag) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Ag.hw.1))
vif(Ag.hw.1)
Ag.hw.2.step <- step(Ag.hw.1)
Ag.hw.2.step
Ag.hw.2 <- get_model(Ag.hw.2.step)
print(summary(Ag.hw.2))
check_model(Ag.hw.2)
anova(Ag.hw.1)
print(anova(Ag.hw.2))
plot(allEffects(Ag.hw.2))
#nothing

###Al ----
al.hw.0 <- lmer(data = iws.hw,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.hw.0))

al.hw.1 <- lmer(data = iws.hw,
                log(Al) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(al.hw.1))

al.hw.2.step <- step(al.hw.1)
al.hw.2.step
al.hw.2 <- get_model(al.hw.2.step)
print(summary(al.hw.2))
al.hw.2 <- lm(data = iws.hw,
                log(Al) ~ season + prox.normal +
                  (1|site),
                REML = T)
print(summary(al.hw.2))
check_model(al.hw.2)
anova(al.hw.1)
print(anova(al.hw.2))
perf <- performance(al.hw.2)
perf
write.csv(perf, "alhw_diag.csv")
plot(allEffects(al.hw.2))
#prox and score

###As ----
As.hw.0 <- lmer(data = iws.hw,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.hw.0))

As.hw.1 <- lmer(data = iws.hw,
                log(As) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(As.hw.1))

As.hw.2.step <- step(As.hw.1)
As.hw.2.step
As.hw.2 <- get_model(As.hw.2.step)
print(summary(As.hw.2))
As.hw.2 <- lm(data = iws.hw,
                log(As) ~ season + prox.normal +  pH)
As.hw.2.1 <- lm(data = iws.hw,
              log(As) ~ season*prox.normal +  pH)
check_model(As.hw.2)
anova(As.hw.1)
print(anova(As.hw.2))
perf <- performance(As.hw.2)
perf
write.csv(perf, "ashw_diag.csv")
plot(allEffects(As.hw.2))
#prox pH season

###Ba ----
ba.hw.0 <- lmer(data = iws.hw,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.hw.0))

ba.hw.1 <- lmer(data = iws.hw,
                log(Ba) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(ba.hw.1))

ba.hw.2.step <- step(ba.hw.1)
ba.hw.2.step
ba.hw.2 <- get_model(ba.hw.2.step)
print(summary(ba.hw.2))
ba.hw.2 <- lmer(data = iws.hw,
                log(Ba) ~ season + prox.normal +
                  (1|site),
                REML = T)
print(summary(ba.hw.2))
anova(ba.hw.1)
print(anova(ba.hw.2))
check_model(ba.hw.2)
perf <- performance(ba.hw.2)
perf
write.csv(perf, "bahw_diag.csv")
plot(allEffects(ba.hw.2))
#prox season

###Be ----
Be.hw.0 <- lmer(data = iws.hw,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.hw.0))

Be.hw.1 <- lmer(data = iws.hw,
                log(Be) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Be.hw.1))

Be.hw.2.step <- step(Be.hw.1)
Be.hw.2.step
Be.hw.2 <- get_model(Be.hw.2.step)
print(summary(Be.hw.2))
check_model(Be.hw.2)
anova(Be.hw.1)
print(anova(Be.hw.2))
performance(Be.hw.2)
plot(allEffects(Be.hw.2))
#nothing

###Cd ----
Cd.hw.0 <- lmer(data = iws.hw,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.hw.0))

Cd.hw.1 <- lmer(data = iws.hw,
                log(Cd) ~ season + prox.normal +  pH  + 
                  (1|site),
                REML = F)
print(summary(Cd.hw.1))

Cd.hw.2.step <- step(Cd.hw.1)
Cd.hw.2.step
Cd.hw.2 <- get_model(Cd.hw.2.step)
print(summary(Cd.hw.2))
Cd.hw.2 <- lmer(data = iws.hw,
                log(Cd) ~ season + prox.normal +  pH +
                  (1|site),
                REML = T)
print(summary(Cd.hw.2))
check_model(Cd.hw.2)
anova(Cd.hw.1)
print(anova(Cd.hw.2))
perf <- performance(Cd.hw.2)
perf
write.csv(perf, "cdhw_diag.csv")
plot(allEffects(Cd.hw.2))
#season and prox and score

###Co ----
Co.hw.0 <- lmer(data = iws.hw,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.hw.0))

Co.hw.1 <- lmer(data = iws.hw,
                log(Co) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Co.hw.1))

Co.hw.2.step <- step(Co.hw.1)
Co.hw.2.step
Co.hw.2 <- get_model(Co.hw.2.step)
print(summary(Co.hw.2))
check_model(Co.hw.2)
anova(Co.hw.1)
print(anova(Co.hw.2))
perf <- performance(Co.hw.2)
perf
write.csv(perf, "cohw_diag.csv")
#season prox

###Cr ----
Cr.hw.0 <- lmer(data = iws.hw,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.hw.0))

Cr.hw.1 <- lmer(data = iws.hw,
                log(Cr) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Cr.hw.1))

Cr.hw.2.step <- step(Cr.hw.1)
Cr.hw.2.step
Cr.hw.2 <- get_model(Cr.hw.2.step)
print(summary(Cr.hw.2))
Cr.hw.2 <- lmer(data = iws.hw,
                log(Cr) ~ season +
                  (1|site),
                REML = T)
print(summary(Cr.hw.2))
check_model(Cr.hw.2)
anova(Cr.hw.1)
print(anova(Cr.hw.2))
perf <- performance(Cr.hw.2)
perf
write.csv(perf, "crhw_diag.csv")
#season

###Cu ----
Cu.hw.0 <- lmer(data = iws.hw,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.hw.0))

Cu.hw.1 <- lmer(data = iws.hw,
                log(Cu) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Cu.hw.1))

Cu.hw.2.step <- step(Cu.hw.1)
Cu.hw.2.step
Cu.hw.2 <- get_model(Cu.hw.2.step)
print(summary(Cu.hw.2))
check_model(Cu.hw.2)
anova(Cu.hw.1)
print(anova(Cu.hw.2))
perf <- performance(Cu.hw.2)
perf
write.csv(perf, "cuhw_diag.csv")
#season prox

###Fe ----
Fe.hw.0 <- lmer(data = iws.hw,
                log(Fe) ~
                  (1|site),
                REML = F)
print(summary(Fe.hw.0))

Fe.hw.1 <- lmer(data = iws.hw,
                log(Fe) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Fe.hw.1))

Fe.hw.2.step <- step(Fe.hw.1)
Fe.hw.2.step
Fe.hw.2 <- get_model(Fe.hw.2.step)
print(summary(Fe.hw.2))
Fe.hw.2 <- lmer(data = iws.hw,
                log(Fe) ~ season +  pH +
                  (1|site),
                REML = T)
print(summary(Fe.hw.2))
check_model(Fe.hw.2)
anova(Fe.hw.1)
print(anova(Fe.hw.2))
perf <- performance(Fe.hw.2)
perf
write.csv(perf, "fehw_diag.csv")
plot(allEffects(Fe.hw.2))
#pH season

###Mn ----
Mn.hw.0 <- lmer(data = iws.hw,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(Mn.hw.0))

Mn.hw.1 <- lmer(data = iws.hw,
                log(Mn) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Mn.hw.1))

Mn.hw.2.step <- step(Mn.hw.1)
Mn.hw.2.step
Mn.hw.2 <- get_model(Mn.hw.2.step)
print(summary(Mn.hw.2))
check_model(Mn.hw.2)
anova(Mn.hw.1)
print(anova(Mn.hw.2))
perf <- performance(Mn.hw.2)
perf
write.csv(perf, "mnhw_diag.csv")
#season prox only


###Mo ----
mo.hw.0 <- lmer(data = iws.hw,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.hw.0))

mo.hw.1 <- lmer(data = iws.hw,
                log(Mo) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(mo.hw.1))

mo.hw.2.step <- step(mo.hw.1)
mo.hw.2.step
mo.hw.2 <- get_model(mo.hw.2.step)
print(summary(mo.hw.2))
anova(mo.hw.1)
print(anova(mo.hw.2))
check_model(mo.hw.2)
perf <- performance(mo.hw.2)
perf
write.csv(perf, "mohw_diag.csv")
plot(allEffects(mo.hw.2))
#prox season

###Ni ----
ni.hw.0 <- lmer(data = iws.hw,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.hw.0))

ni.hw.1 <- lmer(data = iws.hw,
                log(Ni) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(ni.hw.1))

ni.hw.2.step <- step(ni.hw.1)
ni.hw.2.step
ni.hw.2 <- get_model(ni.hw.2.step)
print(summary(ni.hw.2))
anova(ni.hw.1)
print(anova(ni.hw.2))
check_model(ni.hw.2)
perf <- performance(ni.hw.2)
perf
write.csv(perf, "nihw_diag.csv")
#season only, 

###Pb ----
Pb.hw.0 <- lmer(data = iws.hw,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.hw.0))

Pb.hw.1 <- lmer(data = iws.hw,
                log(Pb) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Pb.hw.1))

Pb.hw.2.step <- step(Pb.hw.1)
Pb.hw.2.step
Pb.hw.2 <- get_model(Pb.hw.2.step)
print(summary(Pb.hw.2))
Pb.hw.2 <- lmer(data = iws.hw,
                log(Pb) ~ season + prox.normal +
                  (1|site),
                REML = T)
print(summary(Pb.hw.2))
check_model(Pb.hw.2)
anova(Pb.hw.1)
print(anova(Pb.hw.2))
perf <- performance(Pb.hw.2)
perf
write.csv(perf, "pbhw_diag.csv")
plot(allEffects(Pb.hw.2))
#season prox only

###Sb ----
Sb.hw.0 <- lmer(data = iws.hw,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.hw.0))

Sb.hw.1 <- lmer(data = iws.hw,
                log(Sb) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Sb.hw.1))

Sb.hw.2.step <- step(Sb.hw.1)
Sb.hw.2.step
Sb.hw.2 <- get_model(Sb.hw.2.step)
print(summary(Sb.hw.2))
check_model(Sb.hw.2)
anova(Sb.hw.1)
print(anova(Sb.hw.2))
perf <- performance(Sb.hw.2)
perf
write.csv(perf, "sbhw_diag.csv")
#season prox

###Se ----
Se.hw.0 <- lmer(data = iws.hw,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.hw.0))

Se.hw.1 <- lmer(data = iws.hw,
                log(Se) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Se.hw.1))

Se.hw.2.step <- step(Se.hw.1)
Se.hw.2.step
Se.hw.2 <- get_model(Se.hw.2.step)
print(summary(Se.hw.2))
check_model(Se.hw.2)
anova(Se.hw.1)
print(anova(Se.hw.2))
perf <- performance(Se.hw.2)
perf
write.csv(perf, "sehw_diag.csv")
#season

###Sn ----
Sn.hw.0 <- lmer(data = iws.hw,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.hw.0))

Sn.hw.1 <- lmer(data = iws.hw,
                log(Sn) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Sn.hw.1))

Sn.hw.2.step <- step(Sn.hw.1)
Sn.hw.2.step
Sn.hw.2 <- get_model(Sn.hw.2.step)
print(summary(Sn.hw.2))
check_model(Sn.hw.2)
anova(Sn.hw.1)
print(anova(Sn.hw.2))
perf <- performance(Sn.hw.2)
perf
write.csv(perf, "snhw_diag.csv")
#season

###V ----
v.hw.0 <- lmer(data = iws.hw,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.hw.0))

v.hw.1 <- lmer(data = iws.hw,
               log(V) ~ season + prox.normal +  pH +
                 (1|site),
               REML = F)
print(summary(v.hw.1))

v.hw.2.step <- step(v.hw.1)
v.hw.2.step
v.hw.2 <- get_model(v.hw.2.step)
print(summary(v.hw.2))
v.hw.2 <- lmer(data = iws.hw,
               log(V) ~ season +
                 (1|site),
               REML = T)
print(summary(v.hw.2))
anova(v.hw.1)
print(anova(v.hw.2))
check_model(v.hw.2)
perf <- performance(v.hw.2)
perf
write.csv(perf, "vhw_diag.csv")
#season

###Zn ----
Zn.hw.0 <- lmer(data = iws.hw,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.hw.0))

Zn.hw.1 <- lmer(data = iws.hw,
                log(Zn) ~ season + prox.normal +  pH +
                  (1|site),
                REML = F)
print(summary(Zn.hw.1))

Zn.hw.2.step <- step(Zn.hw.1)
Zn.hw.2.step
Zn.hw.2 <- get_model(Zn.hw.2.step)
print(summary(Zn.hw.2))
Zn.hw.2 <- lmer(data = iws.hw,
                log(Zn) ~ season +
                  (1|site),
                REML = T)
print(summary(Zn.hw.2))
check_model(Zn.hw.2)
anova(Zn.hw.1)
print(anova(Zn.hw.2))
perf <- performance(Zn.hw.2)
perf
write.csv(perf, "znhw_diag.csv")
#season

## tu ----
###Ag ----
Ag.tu.0 <- lmer(data = iws.tu,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.tu.0))

Ag.tu.1 <- lmer(data = iws.tu,
                log(Ag) ~ season + prox.normal +  pH + ward + pH:prox.normal + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Ag.tu.1))
vif(Ag.tu.1)
Ag.tu.2.step <- step(Ag.tu.1)
Ag.tu.2.step
Ag.tu.2 <- get_model(Ag.tu.2.step)
print(summary(Ag.tu.2))
check_model(Ag.tu.2)
anova(Ag.tu.1)
print(anova(Ag.tu.2))
plot(allEffects(Ag.tu.2))
perf <- performance(Ag.tu.2)
perf
write.csv(perf, "agtu_diag.csv")
#season

###Al ----
al.tu.0 <- lmer(data = iws.tu,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.tu.0))

al.tu.1 <- lmer(data = iws.tu,
                log(Al) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(al.tu.1))

al.tu.2.step <- step(al.tu.1)
al.tu.2.step
al.tu.2 <- get_model(al.tu.2.step)
print(summary(al.tu.2))
al.tu.2 <- lmer(data = iws.tu,
                log(Al) ~  pH +
                  (1|site),
                REML = T)
al.tu.2.1 <- lmer(data = iws.tu,
                log(Al) ~  pH + season*prox.normal+
                  (1|site),
                REML = F)
al.tu.2.2 <- lmer(data = iws.tu,
                log(Al) ~  pH + prox.normal+
                  (1|site),
                REML = F)
print(summary(al.tu.2.2))
plot(allEffects(al.tu.2.1))
plot(allEffects(al.tu.2.2))
print(summary(al.tu.2))
check_model(al.tu.2)
anova(al.tu.1)
print(anova(al.tu.2))
print(anova(al.tu.2.1))
print(anova(al.tu.2.2))
anova(al.tu.2, al.tu.2.1)
anova(al.tu.2.2, al.tu.2.1)
anova(al.tu.2.2, al.tu.2)
#stick to pH
perf <- performance(al.tu.2)
perf
write.csv(perf, "altu_diag.csv")
plot(allEffects(al.tu.2))
#pH

###As ----
As.tu.0 <- lmer(data = iws.tu,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.tu.0))

As.tu.1 <- lmer(data = iws.tu,
                log(As) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(As.tu.1))

As.tu.2.step <- step(As.tu.1)
As.tu.2.step
As.tu.2 <- get_model(As.tu.2.step)
print(summary(As.tu.2))
As.tu.2 <- lmer(data = iws.tu,
                log(As) ~ season+pH+
                  (1|site),
                REML = T)
As.tu.2.1 <- lmer(data = iws.tu,
                log(As) ~ season+pH+prox.normal+
                  (1|site),
                REML = F)
anova(As.tu.2, As.tu.2.1)
print(summary(As.tu.2))
check_model(As.tu.2)
anova(As.tu.1)
print(anova(As.tu.2))
perf <- performance(As.tu.2)
perf
write.csv(perf, "astu_diag.csv")
plot(allEffects(As.tu.2))
#prox pH season

###Ba ----
ba.tu.0 <- lmer(data = iws.tu,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.tu.0))

ba.tu.1 <- lmer(data = iws.tu,
                log(Ba) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(ba.tu.1))

ba.tu.2.step <- step(ba.tu.1)
ba.tu.2.step
ba.tu.2 <- get_model(ba.tu.2.step)
print(summary(ba.tu.2))
ba.tu.2 <- lmer(data = iws.tu,
                log(Ba) ~ season + pH+
                  (1|site),
                REML = T)
print(summary(ba.tu.2))
anova(ba.tu.1)
print(anova(ba.tu.2))
check_model(ba.tu.2)
perf <- performance(ba.tu.2)
perf
write.csv(perf, "batu_diag.csv")
plot(allEffects(ba.tu.2))
#prox season

###Be ----
Be.tu.0 <- lmer(data = iws.tu,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.tu.0))

Be.tu.1 <- lmer(data = iws.tu,
                log(Be) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Be.tu.1))

Be.tu.2.step <- step(Be.tu.1)
Be.tu.2.step
Be.tu.2 <- get_model(Be.tu.2.step)
print(summary(Be.tu.2))
Be.tu.2 <- lm(data = iws.tu,
                log(Be) ~ pH+prox.normal)
summary(Be.tu.2)
check_model(Be.tu.2)
anova(Be.tu.1)
print(anova(Be.tu.2))
perf <- performance(Be.tu.2)
perf
write.csv(perf, "betu_diag.csv")
plot(allEffects(Be.tu.2))
#nothing

###Cd ----
Cd.tu.0 <- lmer(data = iws.tu,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.tu.0))
check_model(Cd.tu.0)

Cd.tu.1 <- lmer(data = iws.tu,
                log(Cd) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Cd.tu.1))

Cd.tu.2.step <- step(Cd.tu.1)
Cd.tu.2.step
Cd.tu.2 <- get_model(Cd.tu.2.step)
print(summary(Cd.tu.2))
Cd.tu.2 <- lmer(data = iws.tu,
                log(Cd) ~ prox.normal*pH +
                  (1|site),
                REML = T)
Cd.tu.2.1 <- lmer(data = iws.tu,
                log(Cd) ~ pH+prox.normal +
                  (1|site),
                REML = T)
Cd.tu.2.2 <- lmer(data = iws.tu,
                log(Cd) ~ pH+prox.normal:pH +
                  (1|site),
                REML = T)
Cd.tu.2.3 <- lmer(data = iws.tu,
                log(Cd) ~ prox.normal*pH + ward+
                  (1|site),
                REML = T)
Cd.tu.2.4 <- lmer(data = iws.tu,
                  log(Cd) ~ prox.normal:pH +pH + ward+
                    (1|site),
                  REML = T)
print(summary(Cd.tu.2.2))
check_model(Cd.tu.2)
check_model(Cd.tu.2.3)
vif(Cd.tu.2.2)
anova(Cd.tu.1)
print(anova(Cd.tu.2))
anova(Cd.tu.2.2)
print(anova(Cd.tu.2, Cd.tu.2.1))
print(anova(Cd.tu.2, Cd.tu.2.2))
print(anova(Cd.tu.2.1, Cd.tu.2.2))
print(anova(Cd.tu.2, Cd.tu.2.3))
print(anova(Cd.tu.2.1, Cd.tu.2.3))
print(anova(Cd.tu.2.2, Cd.tu.2.3))
print(anova(Cd.tu.2.4, Cd.tu.2.3))
print(anova(Cd.tu.2.2, Cd.tu.2.4))
compare_performance(Cd.tu.2, Cd.tu.2.2)
compare_performance(Cd.tu.2.4, Cd.tu.2.2)
perf <- performance(Cd.tu.2.2)
perf
write.csv(perf, "cdtu_diag.csv")
plot(allEffects(Cd.tu.2.2))
#pH and prox significant, ward almost signif
model.effects <- ggeffect(model = Cd.tu.2.2,
                          type = "re",
                          terms = c("pH", "prox.normal"))
ggplot(model.effects, aes(x = x, y = exp(predicted), color = group))+
  #geom_point(data = iw.dm[iw.dm$community!="Dewey-Humboldt",], aes(x = prox.normal, y=pli, color = landuse), alpha = .3)+
  geom_line(linetype = "longdash")+
  #geom_ribbon(aes(ymin=exp(conf.low), ymax=exp(conf.high), fill = group),alpha=0.25, color = NA) +
  scale_fill_poke(pokemon = "tentacruel")+
  scale_color_poke(pokemon = "tentacruel")+
  labs(title = "Tucson - Effect of pH and proximity on [Cd]",
       x = "\nNormalized Proximity to Point Source (km)",
       y = "Predicted [Cd] (ug/L)\n",
       color = "pH",
       fill = "pH")+
  #coord_cartesian(ylim = c(0,10))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text())
dev.print(png, "tu_cd_effect.png", res=300, height=6, width=8, units="in")



###Co ----
Co.tu.0 <- lmer(data = iws.tu,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.tu.0))

Co.tu.1 <- lmer(data = iws.tu,
                log(Co) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Co.tu.1))

Co.tu.2.step <- step(Co.tu.1)
Co.tu.2.step
Co.tu.2 <- get_model(Co.tu.2.step)
print(summary(Co.tu.2))
Co.tu.2 <- lmer(data = iws.tu,
                log(Co) ~ season +pH+
                  (1|site),
                REML = T)
Co.tu.2.1 <- lmer(data = iws.tu,
                log(Co) ~ season +pH + prox.normal+
                  (1|site),
                REML = F)
print(summary(Co.tu.2))
check_model(Co.tu.2)
anova(Co.tu.1)
print(anova(Co.tu.2))
print(anova(Co.tu.2.1, Co.tu.2))
performance(Co.tu.2.1)
perf <- performance(Co.tu.2)
perf
write.csv(perf, "cotu_diag.csv")
plot(allEffects(Co.tu.2))
#season pH

###Cr ----
Cr.tu.0 <- lmer(data = iws.tu,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.tu.0))

Cr.tu.1 <- lmer(data = iws.tu,
                log(Cr) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Cr.tu.1))

Cr.tu.2.step <- step(Cr.tu.1)
Cr.tu.2.step
Cr.tu.2 <- get_model(Cr.tu.2.step)
print(summary(Cr.tu.2))
Cr.tu.2 <- lmer(data = iws.tu,
                log(Cr) ~ season + prox.normal*pH  +
                  (1|site),
                REML = T)
Cr.tu.2.1 <- lmer(data = iws.tu,
                log(Cr) ~ season + pH+ pH:prox.normal  +
                  (1|site),
                REML = T)
Cr.tu.2.2 <- lmer(data = iws.tu,
                  log(Cr) ~ season + pH+ prox.normal  +
                    (1|site),
                  REML = T)
print(summary(Cr.tu.2))
print(summary(Cr.tu.2.2))
check_model(Cr.tu.2)
check_model(Cr.tu.2.2)
vif(Cr.tu.2)
vif(Cr.tu.2.1)
vif(Cr.tu.2.2)
anova(Cr.tu.1)
print(anova(Cr.tu.2))
print(anova(Cr.tu.2.1))
print(anova(Cr.tu.2, Cr.tu.2.1))
print(anova(Cr.tu.2.1, Cr.tu.2.2))
compare_performance(Cr.tu.2, Cr.tu.2.1)
perf <- performance(Cr.tu.2.2)
perf
write.csv(perf, "crtu_diag.csv")
plot(allEffects(Cr.tu.2.2))
#

###Cu ----
Cu.tu.0 <- lmer(data = iws.tu,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.tu.0))

Cu.tu.1 <- lmer(data = iws.tu,
                log(Cu) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Cu.tu.1))

Cu.tu.2.step <- step(Cu.tu.1)
Cu.tu.2.step
Cu.tu.2 <- get_model(Cu.tu.2.step)
print(summary(Cu.tu.2))
Cu.tu.2 <- lmer(data = iws.tu,
                log(Cu) ~ season + prox.normal + pH + (1 | site) + prox.normal:pH + season:prox.normal,
                REML = T)
Cu.tu.2.1 <- lmer(data = iws.tu,
                log(Cu) ~ season + prox.normal + pH + (1 | site) + season:prox.normal,
                REML = T)
print(summary(Cu.tu.2))
print(summary(Cu.tu.2.1))
check_model(Cu.tu.2)
vif(Cu.tu.2)
vif(Cu.tu.2.1)
anova(Cu.tu.1)
print(anova(Cu.tu.2))
print(anova(Cu.tu.2.1))
print(anova(Cu.tu.2, Cu.tu.2.1))
compare_performance(Cu.tu.2, Cu.tu.2.1)
perf <- performance(Cu.tu.2.1)
perf
write.csv(perf, "cutu_diag.csv")
plot(allEffects(Cu.tu.2.1))
#

###Fe ----
Fe.tu.0 <- lmer(data = iws.tu,
                log(Fe) ~
                  (1|site),
                REML = F)
print(summary(Fe.tu.0))

Fe.tu.1 <- lmer(data = iws.tu,
                log(Fe) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Fe.tu.1))

Fe.tu.2.step <- step(Fe.tu.1)
Fe.tu.2.step
Fe.tu.2 <- get_model(Fe.tu.2.step)
print(summary(Fe.tu.2))
Fe.tu.2 <- lmer(data = iws.tu,
                log(Fe) ~ season +
                  (1|site),
                REML = T)
print(summary(Fe.tu.2))
check_model(Fe.tu.2)
anova(Fe.tu.1)
print(anova(Fe.tu.2))
perf <- performance(Fe.tu.2)
perf
write.csv(perf, "fetu_diag.csv")
#prox pH season

###Mn ----
Mn.tu.0 <- lmer(data = iws.tu,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(Mn.tu.0))

Mn.tu.1 <- lmer(data = iws.tu,
                log(Mn) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Mn.tu.1))

Mn.tu.2.step <- step(Mn.tu.1)
Mn.tu.2.step
Mn.tu.2 <- get_model(Mn.tu.2.step)
print(summary(Mn.tu.2))
Mn.tu.2 <- lmer(data = iws.tu,
                log(Mn) ~ season + pH+ 
                  (1|site),
                REML = T)
print(summary(Mn.tu.2))
check_model(Mn.tu.2)
anova(Mn.tu.1)
print(anova(Mn.tu.2))
perf <- performance(Mn.tu.2)
perf
write.csv(perf, "mntu_diag.csv")
plot(allEffects(Mn.tu.2))
#season prox only


###Mo ----
mo.tu.0 <- lmer(data = iws.tu,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.tu.0))

mo.tu.1 <- lmer(data = iws.tu,
                log(Mo) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(mo.tu.1))

mo.tu.2.step <- step(mo.tu.1)
mo.tu.2.step
mo.tu.2 <- get_model(mo.tu.2.step)
print(summary(mo.tu.2))
mo.tu.2 <- lmer(data = iws.tu,
                log(Mo) ~ season +  pH+ ward +
                  (1|site),
                REML = T)
print(summary(mo.tu.2))
anova(mo.tu.1)
print(anova(mo.tu.2))
check_model(mo.tu.2)
perf <- performance(mo.tu.2)
perf
write.csv(perf, "motu_diag.csv")
plot(allEffects(mo.tu.2))


###Ni ----
ni.tu.0 <- lmer(data = iws.tu,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.tu.0))

ni.tu.1 <- lmer(data = iws.tu,
                log(Ni) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(ni.tu.1))

ni.tu.2.step <- step(ni.tu.1)
ni.tu.2.step
ni.tu.2 <- get_model(ni.tu.2.step)
print(summary(ni.tu.2))
ni.tu.2 <- lmer(data = iws.tu,
                log(Ni) ~ season +  pH +
                  (1|site),
                REML = T)
print(summary(ni.tu.2))
anova(ni.tu.1)
print(anova(ni.tu.2))
check_model(ni.tu.2)
perf <- performance(ni.tu.2)
perf
write.csv(perf, "nitu_diag.csv")
plot(allEffects(ni.tu.2))
#season only, 

###Pb ----
Pb.tu.0 <- lmer(data = iws.tu,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.tu.0))

Pb.tu.1 <- lmer(data = iws.tu,
                log(Pb) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Pb.tu.1))

Pb.tu.2.step <- step(Pb.tu.1)
Pb.tu.2.step
Pb.tu.2 <- get_model(Pb.tu.2.step)
print(summary(Pb.tu.2))
Pb.tu.2 <- lmer(data = iws.tu,
                log(Pb) ~ season +  pH+prox.normal +
                  (1|site),
                REML = T)
print(summary(Pb.tu.2))
check_model(Pb.tu.2)
anova(Pb.tu.1)
print(anova(Pb.tu.2))
perf <- performance(Pb.tu.2)
perf
write.csv(perf, "pbtu_diag.csv")
plot(allEffects(Pb.tu.2))

#

###Sb ----
Sb.tu.0 <- lmer(data = iws.tu,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.tu.0))

Sb.tu.1 <- lmer(data = iws.tu,
                log(Sb) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Sb.tu.1))

Sb.tu.2.step <- step(Sb.tu.1)
Sb.tu.2.step
Sb.tu.2 <- get_model(Sb.tu.2.step)
print(summary(Sb.tu.2))
Sb.tu.2 <- lmer(data = iws.tu,
                log(Sb) ~ season +  pH + ward +
                  (1|site),
                REML = T)
print(summary(Sb.tu.2))
check_model(Sb.tu.2)
anova(Sb.tu.1)
print(anova(Sb.tu.2))
perf <- performance(Sb.tu.2)
perf
write.csv(perf, "sbtu_diag.csv")
plot(allEffects(Sb.tu.2))
#season prox

###Se ----
Se.tu.0 <- lmer(data = iws.tu,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.tu.0))

Se.tu.1 <- lmer(data = iws.tu,
                log(Se) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Se.tu.1))

Se.tu.2.step <- step(Se.tu.1)
Se.tu.2.step
Se.tu.2 <- get_model(Se.tu.2.step)
print(summary(Se.tu.2))
check_model(Se.tu.2)
anova(Se.tu.1)
print(anova(Se.tu.2))
perf <- performance(Se.tu.2)
perf
write.csv(perf, "setu_diag.csv")

#season

###Sn ----
Sn.tu.0 <- lmer(data = iws.tu,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.tu.0))

Sn.tu.1 <- lmer(data = iws.tu,
                log(Sn) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Sn.tu.1))

Sn.tu.2.step <- step(Sn.tu.1)
Sn.tu.2.step
Sn.tu.2 <- get_model(Sn.tu.2.step)
print(summary(Sn.tu.2))
Sn.tu.2 <- lmer(data = iws.tu,
                log(Sn) ~ season +
                  (1|site),
                REML = T)
Sn.tu.2.1 <- lmer(data = iws.tu,
                log(Sn) ~ season + prox.normal+
                  (1|site),
                REML = T)
print(summary(Sn.tu.2.1))
check_model(Sn.tu.2.1)
anova(Sn.tu.2)
anova(Sn.tu.2.1)
print(anova(Sn.tu.2, Sn.tu.2.1))
compare_performance(Sn.tu.2, Sn.tu.2.1)
perf <- performance(Sn.tu.2.1)
perf
write.csv(perf, "sntu_diag.csv")
#season prox almost significant but kept in the model because it greatly improved teh r2 and model assumptions

###V ----
v.tu.0 <- lmer(data = iws.tu,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.tu.0))

v.tu.1 <- lmer(data = iws.tu,
               log(V) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                 (1|site),
               REML = F)
print(summary(v.tu.1))

v.tu.2.step <- step(v.tu.1)
v.tu.2.step
v.tu.2 <- get_model(v.tu.2.step)
print(summary(v.tu.2))
v.tu.2 <- lmer(data = iws.tu,
               log(V) ~ season +  pH+prox.normal +
                 (1|site),
               REML = T)
print(summary(v.tu.2))
anova(v.tu.1)
print(anova(v.tu.2))
check_model(v.tu.2)
perf <- performance(v.tu.2)
perf
write.csv(perf, "vtu_diag.csv")
plot(allEffects(v.tu.2))
#season

###Zn ----
Zn.tu.0 <- lmer(data = iws.tu,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.tu.0))

Zn.tu.1 <- lmer(data = iws.tu,
                log(Zn) ~ season + prox.normal +  pH + ward + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = F)
print(summary(Zn.tu.1))

Zn.tu.2.step <- step(Zn.tu.1)
Zn.tu.2.step
Zn.tu.2 <- get_model(Zn.tu.2.step)
print(summary(Zn.tu.2))
Zn.tu.2 <- lmer(data = iws.tu,
                log(Zn) ~  season + prox.normal +  pH + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = T)
Zn.tu.2.2 <- lmer(data = iws.tu,
                log(Zn) ~  prox.normal +  pH + prox.normal:pH + prox.normal:season +
                  (1|site),
                REML = T)
Zn.tu.2.3 <- lmer(data = iws.tu,
                  log(Zn) ~  prox.normal +  pH + prox.normal:pH +
                    (1|site),
                  REML = T)
Zn.tu.2.4 <- lmer(data = iws.tu,
                log(Zn) ~  season + prox.normal +  pH + prox.normal:season +
                  (1|site),
                REML = T)
Zn.tu.2.1 <- lm(data = iws.tu,
                log(Zn) ~  season + prox.normal +  pH + prox.normal:season)
print(summary(Zn.tu.2.4))
check_model(Zn.tu.2)
check_model(Zn.tu.2.1)
check_model(Zn.tu.2.2)
check_model(Zn.tu.2.3)
check_model(Zn.tu.2.4)
vif(Zn.tu.2)
anova(Zn.tu.1)
print(anova(Zn.tu.2))
print(anova(Zn.tu.2.1))
print(anova(Zn.tu.2.2))
print(anova(Zn.tu.2.1, Zn.tu.2))
print(anova(Zn.tu.2.2, Zn.tu.2))
print(anova(Zn.tu.2.2, Zn.tu.2.3))
compare_performance(Zn.tu.2.1, Zn.tu.2)
compare_performance(Zn.tu.2.2, Zn.tu.2)
compare_performance(Zn.tu.2.2, Zn.tu.2.3)
perf <- performance(Zn.tu.2.4)
perf
write.csv(perf, "zntu_diag.csv")
plot(allEffects(Zn.tu.2.4))
#season

#plotting ----
#quick example plots
ggplot(iws.dh, mapping = aes(y = log(Mo), x = pH)) +
  geom_point(shape = 2, size = 4, color = "#F9A785") +
  labs(x = "\npH",
       y = "[Mo] (ln(ug/L))\n",
       title = "Relationship between [Mo] and pH",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwdh_mo_pH.png", res=300, height=8, width=8, units="in")

ggplot(iws.dh, mapping = aes(y = log(Mn), x = season, shape = season)) +
  geom_point(position = "jitter", size = 4, color = "#F9A785") +
  scale_shape_manual(values = c(8, 5))+
  labs(x = "\nSeason",
       y = "[Mn] (ln(ug/L))\n",
       title = "Relationship between [Mn] and Season",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwdh_mn_ssn.png", res=300, height=8, width=8, units="in")

ggplot(iws.gm, mapping = aes(y = log(Ag), x = pH)) +
  geom_point(shape = 2, size = 4, color = "#00A8C6") +
  labs(x = "\npH",
       y = "[Ag] (ln(ug/L))\n",
       title = "Relationship between [Ag] and pH",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwgm_ag_pH.png", res=300, height=8, width=8, units="in")

ggplot(iws.gm, mapping = aes(y = log(Pb), x = proximity.km)) +
  geom_point(shape = 3, size = 4, color = "#00A8C6") +
  labs(x = "\nProximity to Mine (km)",
       y = "[Pb] (ln(ug/L))\n",
       title = "Relationship between [Pb] and Proximity to Mine",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwgm_pb_prox.png", res=300, height=8, width=8, units="in")


ggplot(iws.hw, mapping = aes(y = log(Cd), x = pH)) +
  geom_point(shape = 2, size = 4, color = "#95CACA") +
  labs(x = "\npH",
       y = "[Cd] (ln(ug/L))\n",
       title = "Relationship between [Cd] and pH",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwhw_cd_pH.png", res=300, height=8, width=8, units="in")

ggplot(iws.hw, mapping = aes(y = log(Pb), x = proximity.km)) +
  geom_point(shape = 3, size = 4, color = "#95CACA") +
  labs(x = "\nProximity to Smelter (km)",
       y = "[Pb] (ln(ug/L))\n",
       title = "Relationship between [Pb] and Proximity to Smelter",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwhw_pb_prox.png", res=300, height=8, width=8, units="in")

ggplot(iws.tu, mapping = aes(y = log(Cd), x = location, shape = location)) +
  geom_point(position = "jitter", size = 4, color = "#4068B2") +
  scale_shape_manual(values = c(21,22))+
  labs(x = "\nLocation",
       y = "[Cd] (ln(ug/L))\n",
       title = "Relationship between [Cd] and Location",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwtu_cd_loc.png", res=300, height=8, width=8, units="in")

ggplot(iws.tu, mapping = aes(y = log(Cd), x = ward, shape = ward)) +
  geom_point(position = "jitter", size = 4, color = "#4068B2") +
  scale_shape_manual(values = c(1, 3, 2, 4, 6, 5))+
  labs(x = "\nWard",
       y = "[Cd] (ln(ug/L))\n",
       title = "Relationship between [Cd] and Ward",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "iwtu_cd_ward.png", res=300, height=8, width=12, units="in")
