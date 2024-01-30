#Kunal Palawat, Gift Chukwuonye
#Description: modeling analytes with rhrw infrastructure maintenance score====
#load libraries----
#base
library(readxl) #read excel files
library(tidyverse)
library(ggplot2)
#models====
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

#data formatting----
#iw.score contains samples with homes that had at least one response to the home description survey. if not all 5 questions were answered, a value of 0 was automatically assigned as a conservative assumption of behavior
#view(iw.score)
iw.score.long <- pivot_longer(data = iw.dm,
                              cols = Be:Pb,
                              values_to = "concentration",
                              names_to = "analyte")

#split dataframe into different ones for each community
iws.c <- iw.dm79 %>%
  drop_na(pH) %>%
  group_by(community) %>%
  group_split()

iws.dh <- iws.c[[1]]
iws.gm <- iws.c[[2]]
iws.hw <- iws.c[[3]]
iws.tu <- iws.c[[4]]

#globe specific
glo <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "globe", col_names = TRUE)
iws.gm <- full_join(iws.gm, glo, by = c("site"))
iws.gm <- iws.gm %>%
  drop_na(community) %>%
  drop_na(location_2)
iws.gm$location_2 <- factor(iws.gm$location_2, levels = c("Miami/Claypool Area", "Globe Area", "Canyons Area"))

iws.gm$Q79 <- as.factor(iws.gm$Q79)

#tucson specific
iws.tu <- iws.tu %>%
  drop_na(prox.normal)

iws.tu$Q67 <- as.factor(iws.tu$Q67)

tuc <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "tucson", col_names = TRUE)
iws.tu <- full_join(iws.tu, tuc, by = c("site"))
iws.tu <- iws.tu %>%
  drop_na(ward) %>%
  drop_na(community) %>%
  drop_na(location)
iws.tu$ward <- factor(iws.tu$ward, levels = c("One", "Two", "Three", "Four", "Five", "Six"))



#initial viz----
ggplot(iw.score.long, mapping = aes(y = log(concentration), x = score_bin, fill = community)) +
  geom_boxplot() +
  #scale_fill_manual(values = wes_palette(name = "Darjeeling2", n = 5)) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #scale_fill_viridis_d() +
  facet_grid(.~analyte, scales = "free") +
  labs(x = "\nMaintenance Score",
       y = "[analyte] (ln(ug/L))\n",
       title = "Boxplots of maintenance score by analyte",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "maintenancescore_analyte_com.png", res=300, height=8, width=18, units="in")

#summaries ----
sumFX(datalongDF = iw.score.long,
      subset.vector.string = c("analyte", "score_bin","season", "community"),
      value.string = "concentration",
      dfname.string = "sum.score.ssn.com",
      filename.string = "score_ssncom")

#zinc, aluminum, copper, iron, manganese have highest standard deviations
#manganese, lead, copper, cadmium, aluminum, zinc, nickel have highest geometric standard deviations

#analytes with a sd > 1
#DH
#Al,As,Ba,Cr,Cu,Fe,Mn,Mo,Ni,Pb,Se,V,Zn
#GM
#Al,As,Ba,Cd,Co,Cr,Cu,Fe,Mn,Mo,Ni,Pb,Se,Sn,V,Zn
#
#HW
#Al,As,Ba,Cr,Cu,Fe,Mn,Mo,Pb,Sb,Se,Sn,V,Zn
#
#TU
#Al,As,Ba,Cr,Cu,Fe,Mn,Mo,Ni,Pb,Sb,Se,V,Zn

#initial modeling ----
#anovas

zn.0 <- lm(data = iw.score,
           log(Zn) ~ score_bin+community + season + prox.normal)
summary(zn.0)
check_model(zn.0)
performance(zn.0) #very low r2
anova(zn.0) #score almost signif
plot(allEffects(zn.0))

al.0 <- lm(data = iw.score,
           log(Al) ~ score_bin + community + season + prox.normal)
summary(al.0)
check_model(al.0)
performance(al.0) #low r2
anova(al.0) #score not signif
plot(allEffects(al.0))

cu.0 <- lm(data = iw.score,
           log(Cu) ~ score_bin + community + season + prox.normal)
summary(cu.0)
check_model(cu.0)
performance(cu.0)
anova(cu.0) #score signif
plot(allEffects(cu.0))

pb.0 <- lm(data = iw.score,
           log(Pb) ~ score_bin + community + season + prox.normal)
summary(pb.0)
check_model(pb.0)
performance(pb.0) #low r2
anova(pb.0) #score signif
plot(allEffects(pb.0))

cd.0 <- lm(data = iw.score,
           log(Cd) ~ score_bin + community + season + prox.normal)
summary(cd.0)
check_model(cd.0)
performance(cd.0)
anova(cd.0) #score signif
plot(allEffects(cd.0))


mn.0 <- lm(data = iw.score,
           log(Mn) ~ score_bin + community + season + prox.normal)
summary(mn.0)
check_model(mn.0)
performance(mn.0)
anova(mn.0) #score not signif
plot(allEffects(mn.0))

#stepwise
#stepwise community modeling ----
contam_list <- list("Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")


## tu Q67 ----
###Ag ----
Ag.tu.0 <- lmer(data = iws.tu,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.tu.0))

Ag.tu.1 <- lmer(data = iws.tu,
                log(Ag) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
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
#nothing

###Al ----
al.tu.0 <- lmer(data = iws.tu,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.tu.0))

al.tu.1 <- lmer(data = iws.tu,
                log(Al) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(al.tu.1))

al.tu.2.step <- step(al.tu.1)
al.tu.2.step
al.tu.2 <- get_model(al.tu.2.step)
print(summary(al.tu.2))
check_model(al.tu.2)
anova(al.tu.1)
print(anova(al.tu.2))
performance(al.tu.2)
plot(allEffects(al.tu.2))
#prox and score

###As ----
As.tu.0 <- lmer(data = iws.tu,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.tu.0))

As.tu.1 <- lmer(data = iws.tu,
                log(As) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(As.tu.1))

As.tu.2.step <- step(As.tu.1)
As.tu.2.step
As.tu.2 <- get_model(As.tu.2.step)
print(summary(As.tu.2))
check_model(As.tu.2)
anova(As.tu.1)
print(anova(As.tu.2))
performance(As.tu.2)
plot(allEffects(As.tu.2))
#prox pH season

###Ba ----
ba.tu.0 <- lmer(data = iws.tu,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.tu.0))

ba.tu.1 <- lmer(data = iws.tu,
                log(Ba) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(ba.tu.1))

ba.tu.2.step <- step(ba.tu.1)
ba.tu.2.step
ba.tu.2 <- get_model(ba.tu.2.step)
print(summary(ba.tu.2))
anova(ba.tu.1)
print(anova(ba.tu.2))
check_model(ba.tu.2)
performance(ba.tu.2)
plot(allEffects(ba.tu.2))
#prox season

###Be ----
Be.tu.0 <- lmer(data = iws.tu,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.tu.0))

Be.tu.1 <- lmer(data = iws.tu,
                log(Be) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Be.tu.1))

Be.tu.2.step <- step(Be.tu.1)
Be.tu.2.step
Be.tu.2 <- get_model(Be.tu.2.step)
print(summary(Be.tu.2))
check_model(Be.tu.2)
anova(Be.tu.1)
print(anova(Be.tu.2))
performance(Be.tu.2)
plot(allEffects(Be.tu.2))
#nothing

###Cd ----
Cd.tu.0 <- lmer(data = iws.tu,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.tu.0))

Cd.tu.1 <- lmer(data = iws.tu,
                log(Cd) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Cd.tu.1))

Cd.tu.2.step <- step(Cd.tu.1)
Cd.tu.2.step
Cd.tu.2 <- get_model(Cd.tu.2.step)
print(summary(Cd.tu.2))
check_model(Cd.tu.2)
anova(Cd.tu.1)
print(anova(Cd.tu.2))
performance(Cd.tu.2)
plot(allEffects(Cd.tu.2))
#season and prox and score

###Co ----
Co.tu.0 <- lmer(data = iws.tu,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.tu.0))

Co.tu.1 <- lmer(data = iws.tu,
                log(Co) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Co.tu.1))

Co.tu.2.step <- step(Co.tu.1)
Co.tu.2.step
Co.tu.2 <- get_model(Co.tu.2.step)
print(summary(Co.tu.2))
check_model(Co.tu.2)
anova(Co.tu.1)
print(anova(Co.tu.2))
performance(Co.tu.2)
#season prox

###Cr ----
Cr.tu.0 <- lmer(data = iws.tu,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.tu.0))

Cr.tu.1 <- lmer(data = iws.tu,
                log(Cr) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Cr.tu.1))

Cr.tu.2.step <- step(Cr.tu.1)
Cr.tu.2.step
Cr.tu.2 <- get_model(Cr.tu.2.step)
print(summary(Cr.tu.2))
check_model(Cr.tu.2)
anova(Cr.tu.1)
print(anova(Cr.tu.2))
performance(Cr.tu.2)
#season

###Cu ----
Cu.tu.0 <- lmer(data = iws.tu,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.tu.0))

Cu.tu.1 <- lmer(data = iws.tu,
                log(Cu) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Cu.tu.1))

Cu.tu.2.step <- step(Cu.tu.1)
Cu.tu.2.step
Cu.tu.2 <- get_model(Cu.tu.2.step)
print(summary(Cu.tu.2))
check_model(Cu.tu.2)
anova(Cu.tu.1)
print(anova(Cu.tu.2))
performance(Cu.tu.2)
#season prox

###Fe ----
Fe.tu.0 <- lmer(data = iws.tu,
                log(Fe) ~
                  (1|site),
                REML = F)
print(summary(Fe.tu.0))

Fe.tu.1 <- lmer(data = iws.tu,
                log(Fe) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Fe.tu.1))

Fe.tu.2.step <- step(Fe.tu.1)
Fe.tu.2.step
Fe.tu.2 <- get_model(Fe.tu.2.step)
print(summary(Fe.tu.2))
check_model(Fe.tu.2)
anova(Fe.tu.1)
print(anova(Fe.tu.2))
performance(Fe.tu.2)
#prox pH season

###Mn ----
Mn.tu.0 <- lmer(data = iws.tu,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(Mn.tu.0))

Mn.tu.1 <- lmer(data = iws.tu,
                log(Mn) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Mn.tu.1))

Mn.tu.2.step <- step(Mn.tu.1)
Mn.tu.2.step
Mn.tu.2 <- get_model(Mn.tu.2.step)
print(summary(Mn.tu.2))
check_model(Mn.tu.2)
anova(Mn.tu.1)
print(anova(Mn.tu.2))
performance(Mn.tu.2)
#season prox only


###Mo ----
mo.tu.0 <- lmer(data = iws.tu,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.tu.0))

mo.tu.1 <- lmer(data = iws.tu,
                log(Mo) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(mo.tu.1))

mo.tu.2.step <- step(mo.tu.1)
mo.tu.2.step
mo.tu.2 <- get_model(mo.tu.2.step)
print(summary(mo.tu.2))
anova(mo.tu.1)
print(anova(mo.tu.2))
check_model(mo.tu.2)
performance(mo.tu.2)
plot(allEffects(mo.tu.2))
#prox season

###Ni ----
ni.tu.0 <- lmer(data = iws.tu,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.tu.0))

ni.tu.1 <- lmer(data = iws.tu,
                log(Ni) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(ni.tu.1))

ni.tu.2.step <- step(ni.tu.1)
ni.tu.2.step
ni.tu.2 <- get_model(ni.tu.2.step)
print(summary(ni.tu.2))
vif(ni.tu.2)
ni.tu.3 <- lmer(data = iws.tu,
                log(Ni) ~ season + prox.normal + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = T)
ni.tu.4 <- lmer(data = iws.tu,
                log(Ni) ~ season + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = T)
ni.tu.5 <- lmer(data = iws.tu,
                log(Ni) ~ season + prox.normal + Q67 + pH +
                  (1|site),
                REML = T)
check_model(ni.tu.3)
anova(ni.tu.3)
anova(ni.tu.3, ni.tu.4)
anova(ni.tu.3, ni.tu.5)
anova(ni.tu.4, ni.tu.5)
print(summary(ni.tu.4))
perf <- performance(ni.tu.4)
perf
write.csv(perf, "nitu67_diag.csv")
plot(allEffects(ni.tu.4))
#season only, 

###Pb ----
Pb.tu.0 <- lmer(data = iws.tu,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.tu.0))

Pb.tu.1 <- lmer(data = iws.tu,
                log(Pb) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Pb.tu.1))

Pb.tu.2.step <- step(Pb.tu.1)
Pb.tu.2.step
Pb.tu.2 <- get_model(Pb.tu.2.step)
print(summary(Pb.tu.2))
check_model(Pb.tu.2)
anova(Pb.tu.1)
print(anova(Pb.tu.2))
performance(Pb.tu.2)
plot(allEffects(Pb.tu.2))
#season prox only

###Sb ----
Sb.tu.0 <- lmer(data = iws.tu,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.tu.0))

Sb.tu.1 <- lmer(data = iws.tu,
                log(Sb) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Sb.tu.1))

Sb.tu.2.step <- step(Sb.tu.1)
Sb.tu.2.step
Sb.tu.2 <- get_model(Sb.tu.2.step)
print(summary(Sb.tu.2))
check_model(Sb.tu.2)
anova(Sb.tu.1)
print(anova(Sb.tu.2))
performance(Sb.tu.2)
#season prox

###Se ----
Se.tu.0 <- lmer(data = iws.tu,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.tu.0))

Se.tu.1 <- lmer(data = iws.tu,
                log(Se) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
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
performance(Se.tu.2)
#season

###Sn ----
Sn.tu.0 <- lmer(data = iws.tu,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.tu.0))

Sn.tu.1 <- lmer(data = iws.tu,
                log(Sn) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Sn.tu.1))

Sn.tu.2.step <- step(Sn.tu.1)
Sn.tu.2.step
Sn.tu.2 <- get_model(Sn.tu.2.step)
print(summary(Sn.tu.2))
check_model(Sn.tu.2)
anova(Sn.tu.1)
print(anova(Sn.tu.2))
performance(Sn.tu.2)
#season

###V ----
v.tu.0 <- lmer(data = iws.tu,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.tu.0))

v.tu.1 <- lmer(data = iws.tu,
               log(V) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                 (1|site),
               REML = F)
print(summary(v.tu.1))

v.tu.2.step <- step(v.tu.1)
v.tu.2.step
v.tu.2 <- get_model(v.tu.2.step)
print(summary(v.tu.2))
anova(v.tu.1)
print(anova(v.tu.2))
check_model(v.tu.2)
performance(v.tu.2)
#season

###Zn ----
Zn.tu.0 <- lmer(data = iws.tu,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.tu.0))

Zn.tu.1 <- lmer(data = iws.tu,
                log(Zn) ~ season + prox.normal + ward + prox.normal:pH + prox.normal:season + Q67 + pH +
                  (1|site),
                REML = F)
print(summary(Zn.tu.1))

Zn.tu.2.step <- step(Zn.tu.1)
Zn.tu.2.step
Zn.tu.2 <- get_model(Zn.tu.2.step)
print(summary(Zn.tu.2))
check_model(Zn.tu.2)
anova(Zn.tu.1)
print(anova(Zn.tu.2))
performance(Zn.tu.2)
#season

## gm 77 ----
###Ag ----
Ag.gm.0 <- lmer(data = iws.gm,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.gm.0))

Ag.gm.1 <- lmer(data = iws.gm,
                log(Ag) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Ag.gm.1))
vif(Ag.gm.1)
Ag.gm.2.step <- step(Ag.gm.1)
Ag.gm.2.step
Ag.gm.2 <- get_model(Ag.gm.2.step)
print(summary(Ag.gm.2))
check_model(Ag.gm.2)
anova(Ag.gm.1)
print(anova(Ag.gm.2))
plot(allEffects(Ag.gm.2))
#score and pH

###Al ----
al.gm.0 <- lmer(data = iws.gm,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.gm.0))

al.gm.1 <- lmer(data = iws.gm,
                log(Al) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(al.gm.1))

al.gm.2.step <- step(al.gm.1)
al.gm.2.step
al.gm.2 <- get_model(al.gm.2.step)
print(summary(al.gm.2))
check_model(al.gm.2)
anova(al.gm.1)
print(anova(al.gm.2))
performance(al.gm.2)
plot(allEffects(al.gm.2))
#prox and score

###As ----
As.gm.0 <- lmer(data = iws.gm,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.gm.0))

As.gm.1 <- lmer(data = iws.gm,
                log(As) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(As.gm.1))

As.gm.2.step <- step(As.gm.1)
As.gm.2.step
As.gm.2 <- get_model(As.gm.2.step)
print(summary(As.gm.2))
check_model(As.gm.2)
anova(As.gm.1)
print(anova(As.gm.2))
performance(As.gm.2)
plot(allEffects(As.gm.2))
#prox pH season

###Ba ----
ba.gm.0 <- lmer(data = iws.gm,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.gm.0))

ba.gm.1 <- lmer(data = iws.gm,
                log(Ba) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(ba.gm.1))

ba.gm.2.step <- step(ba.gm.1)
ba.gm.2.step
ba.gm.2 <- get_model(ba.gm.2.step)
ba.gm.2 <- lmer(data = iws.gm,
                log(Ba) ~ season + prox.normal + Q77 +
                  (1|site),
                REML = T)
print(summary(ba.gm.2))
print(anova(ba.gm.2))
check_model(ba.gm.2)
perf <- performance(ba.gm.2)
perf
write.csv(perf, "bagm77_diag.csv")
plot(allEffects(ba.gm.2))
#prox q77 season

###Be ----
Be.gm.0 <- lmer(data = iws.gm,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.gm.0))

Be.gm.1 <- lmer(data = iws.gm,
                log(Be) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Be.gm.1))

Be.gm.2.step <- step(Be.gm.1)
Be.gm.2.step
Be.gm.2 <- get_model(Be.gm.2.step)
print(summary(Be.gm.2))
check_model(Be.gm.2)
anova(Be.gm.1)
print(anova(Be.gm.2))
performance(Be.gm.2)
plot(allEffects(Be.gm.2))
#prox season score

###Cd ----
Cd.gm.0 <- lmer(data = iws.gm,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.gm.0))

Cd.gm.1 <- lmer(data = iws.gm,
                log(Cd) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Cd.gm.1))

Cd.gm.2.step <- step(Cd.gm.1)
Cd.gm.2.step
Cd.gm.2 <- get_model(Cd.gm.2.step)
print(summary(Cd.gm.2))
check_model(Cd.gm.2)
anova(Cd.gm.1)
print(anova(Cd.gm.2))
performance(Cd.gm.2)
plot(allEffects(Cd.gm.2))
#season and prox

###Co ----
Co.gm.0 <- lmer(data = iws.gm,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.gm.0))

Co.gm.1 <- lmer(data = iws.gm,
                log(Co) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Co.gm.1))

Co.gm.2.step <- step(Co.gm.1)
Co.gm.2.step
Co.gm.2 <- get_model(Co.gm.2.step)
print(summary(Co.gm.2))
check_model(Co.gm.2)
anova(Co.gm.1)
print(anova(Co.gm.2))
performance(Co.gm.2)
#season only

###Cr ----
Cr.gm.0 <- lmer(data = iws.gm,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.gm.0))

Cr.gm.1 <- lmer(data = iws.gm,
                log(Cr) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Cr.gm.1))

Cr.gm.2.step <- step(Cr.gm.1)
Cr.gm.2.step
Cr.gm.2 <- get_model(Cr.gm.2.step)
print(summary(Cr.gm.2))
check_model(Cr.gm.2)
anova(Cr.gm.1)
print(anova(Cr.gm.2))
performance(Cr.gm.2)
#season

###Cu ----
Cu.gm.0 <- lmer(data = iws.gm,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.gm.0))

Cu.gm.1 <- lmer(data = iws.gm,
                log(Cu) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Cu.gm.1))

Cu.gm.2.step <- step(Cu.gm.1)
Cu.gm.2.step
Cu.gm.2 <- get_model(Cu.gm.2.step)
print(summary(Cu.gm.2))
check_model(Cu.gm.2)
anova(Cu.gm.1)
print(anova(Cu.gm.2))
performance(Cu.gm.2)

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
                log(Fe) ~ season + prox.normal + location_2 + Q77 + pH +
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
                log(Mn) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Mn.gm.1))

Mn.gm.2.step <- step(Mn.gm.1)
Mn.gm.2.step
Mn.gm.2 <- get_model(Mn.gm.2.step)
print(summary(Mn.gm.2))
check_model(Mn.gm.2)
anova(Mn.gm.1)
print(anova(Mn.gm.2))
performance(Mn.gm.2)
#season only


###Mo ----
mo.gm.0 <- lmer(data = iws.gm,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.gm.0))

mo.gm.1 <- lmer(data = iws.gm,
                log(Mo) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(mo.gm.1))

mo.gm.2.step <- step(mo.gm.1)
mo.gm.2.step
mo.gm.2 <- get_model(mo.gm.2.step)
print(summary(mo.gm.2))
anova(mo.gm.1)
print(anova(mo.gm.2))
check_model(mo.gm.2)
performance(mo.gm.2)
plot(allEffects(mo.gm.2))
#pH prox season

###Ni ----
ni.gm.0 <- lmer(data = iws.gm,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.gm.0))

ni.gm.1 <- lmer(data = iws.gm,
                log(Ni) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(ni.gm.1))

ni.gm.2.step <- step(ni.gm.1)
ni.gm.2.step
ni.gm.2 <- get_model(ni.gm.2.step)
print(summary(ni.gm.2))
anova(ni.gm.1)
print(anova(ni.gm.2))
check_model(ni.gm.2)
performance(ni.gm.2)
#season only, low marg r2

###Pb ----
Pb.gm.0 <- lmer(data = iws.gm,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.gm.0))

Pb.gm.1 <- lmer(data = iws.gm,
                log(Pb) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Pb.gm.1))

Pb.gm.2.step <- step(Pb.gm.1)
Pb.gm.2.step
Pb.gm.2 <- get_model(Pb.gm.2.step)
print(summary(Pb.gm.2))
check_model(Pb.gm.2)
anova(Pb.gm.1)
print(anova(Pb.gm.2))
performance(Pb.gm.2)
plot(allEffects(Pb.gm.2))
#season prox only

###Sb ----
Sb.gm.0 <- lmer(data = iws.gm,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.gm.0))

Sb.gm.1 <- lmer(data = iws.gm,
                log(Sb) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Sb.gm.1))

Sb.gm.2.step <- step(Sb.gm.1)
Sb.gm.2.step
Sb.gm.2 <- get_model(Sb.gm.2.step)

Sb.gm.2 <- lm(data = iws.gm,
                log(Sb) ~ Q77)
print(summary(Sb.gm.2))
check_model(Sb.gm.2)
print(anova(Sb.gm.2))
perf <- performance(Sb.gm.2)
perf
write.csv(perf, "sbgm77_diag.csv")
plot(allEffects(Sb.gm.2))


###Se ----
Se.gm.0 <- lmer(data = iws.gm,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.gm.0))

Se.gm.1 <- lmer(data = iws.gm,
                log(Se) ~ season + prox.normal + location_2 + Q77 + pH +
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
                log(Sn) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Sn.gm.1))

Sn.gm.2.step <- step(Sn.gm.1)
Sn.gm.2.step
Sn.gm.2 <- get_model(Sn.gm.2.step)
print(summary(Sn.gm.2))
check_model(Sn.gm.2)
anova(Sn.gm.1)
print(anova(Sn.gm.2))
performance(Sn.gm.2)
#season

###V ----
v.gm.0 <- lmer(data = iws.gm,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.gm.0))

v.gm.1 <- lmer(data = iws.gm,
               log(V) ~ season + prox.normal + location_2 + Q77 + pH +
                 (1|site),
               REML = F)
print(summary(v.gm.1))

v.gm.2.step <- step(v.gm.1)
v.gm.2.step
v.gm.2 <- get_model(v.gm.2.step)
print(summary(v.gm.2))
anova(v.gm.1)
print(anova(v.gm.2))
check_model(v.gm.2)
performance(v.gm.2)
#season only score almost signif

###Zn ----
Zn.gm.0 <- lmer(data = iws.gm,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.gm.0))

Zn.gm.1 <- lmer(data = iws.gm,
                log(Zn) ~ season + prox.normal + location_2 + Q77 + pH +
                  (1|site),
                REML = F)
print(summary(Zn.gm.1))

Zn.gm.2.step <- step(Zn.gm.1)
Zn.gm.2.step
Zn.gm.2 <- get_model(Zn.gm.2.step)
print(summary(Zn.gm.2))
check_model(Zn.gm.2)
anova(Zn.gm.1)
print(anova(Zn.gm.2))
performance(Zn.gm.2)
#season

## gm 79 ----
###Ag ----
Ag.gm.0 <- lmer(data = iws.gm,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.gm.0))

Ag.gm.1 <- lmer(data = iws.gm,
                log(Ag) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Ag.gm.1))
vif(Ag.gm.1)
Ag.gm.2.step <- step(Ag.gm.1)
Ag.gm.2.step
Ag.gm.2 <- get_model(Ag.gm.2.step)
print(summary(Ag.gm.2))
check_model(Ag.gm.2)
anova(Ag.gm.1)
print(anova(Ag.gm.2))
plot(allEffects(Ag.gm.2))
#score and pH

###Al ----
al.gm.0 <- lmer(data = iws.gm,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.gm.0))

al.gm.1 <- lmer(data = iws.gm,
                log(Al) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(al.gm.1))

al.gm.2.step <- step(al.gm.1)
al.gm.2.step
al.gm.2 <- get_model(al.gm.2.step)
print(summary(al.gm.2))
check_model(al.gm.2)
anova(al.gm.1)
print(anova(al.gm.2))
performance(al.gm.2)
plot(allEffects(al.gm.2))
#prox and score

###As ----
As.gm.0 <- lmer(data = iws.gm,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.gm.0))

As.gm.1 <- lmer(data = iws.gm,
                log(As) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(As.gm.1))

As.gm.2.step <- step(As.gm.1)
As.gm.2.step
As.gm.2 <- get_model(As.gm.2.step)
print(summary(As.gm.2))
check_model(As.gm.2)
anova(As.gm.1)
print(anova(As.gm.2))
performance(As.gm.2)
plot(allEffects(As.gm.2))
#prox pH season

###Ba ----
ba.gm.0 <- lmer(data = iws.gm,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.gm.0))

ba.gm.1 <- lmer(data = iws.gm,
                log(Ba) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(ba.gm.1))

ba.gm.2.step <- step(ba.gm.1)
ba.gm.2.step
ba.gm.2 <- get_model(ba.gm.2.step)
ba.gm.2 <- lmer(data = iws.gm,
                log(Ba) ~ season + prox.normal + Q79 +
                  (1|site),
                REML = T)
print(summary(ba.gm.2))
print(anova(ba.gm.2))
check_model(ba.gm.2)
plot(allEffects(ba.gm.2))
#prox q79 season

###Be ----
Be.gm.0 <- lmer(data = iws.gm,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.gm.0))

Be.gm.1 <- lmer(data = iws.gm,
                log(Be) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Be.gm.1))

Be.gm.2.step <- step(Be.gm.1)
Be.gm.2.step
Be.gm.2 <- get_model(Be.gm.2.step)
print(summary(Be.gm.2))
check_model(Be.gm.2)
anova(Be.gm.1)
print(anova(Be.gm.2))
perf <- performance(Be.gm.2)
perf
write.csv(perf, "Begm79_diag.csv")
plot(allEffects(Be.gm.2))

#prox season score

###Cd ----
Cd.gm.0 <- lmer(data = iws.gm,
                log(Cd) ~
                  (1|site),
                REML = F)
print(summary(Cd.gm.0))

Cd.gm.1 <- lmer(data = iws.gm,
                log(Cd) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Cd.gm.1))

Cd.gm.2.step <- step(Cd.gm.1)
Cd.gm.2.step
Cd.gm.2 <- get_model(Cd.gm.2.step)
print(summary(Cd.gm.2))
check_model(Cd.gm.2)
anova(Cd.gm.1)
print(anova(Cd.gm.2))
performance(Cd.gm.2)
plot(allEffects(Cd.gm.2))
#season and prox

###Co ----
Co.gm.0 <- lmer(data = iws.gm,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.gm.0))

Co.gm.1 <- lmer(data = iws.gm,
                log(Co) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Co.gm.1))

Co.gm.2.step <- step(Co.gm.1)
Co.gm.2.step
Co.gm.2 <- get_model(Co.gm.2.step)
print(summary(Co.gm.2))
check_model(Co.gm.2)
anova(Co.gm.1)
print(anova(Co.gm.2))
performance(Co.gm.2)
#season only

###Cr ----
Cr.gm.0 <- lmer(data = iws.gm,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.gm.0))

Cr.gm.1 <- lmer(data = iws.gm,
                log(Cr) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Cr.gm.1))

Cr.gm.2.step <- step(Cr.gm.1)
Cr.gm.2.step
Cr.gm.2 <- get_model(Cr.gm.2.step)
print(summary(Cr.gm.2))
check_model(Cr.gm.2)
anova(Cr.gm.1)
print(anova(Cr.gm.2))
performance(Cr.gm.2)
#season

###Cu ----
Cu.gm.0 <- lmer(data = iws.gm,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.gm.0))

Cu.gm.1 <- lmer(data = iws.gm,
                log(Cu) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Cu.gm.1))

Cu.gm.2.step <- step(Cu.gm.1)
Cu.gm.2.step
Cu.gm.2 <- get_model(Cu.gm.2.step)
print(summary(Cu.gm.2))
check_model(Cu.gm.2)
anova(Cu.gm.1)
print(anova(Cu.gm.2))
performance(Cu.gm.2)

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
                log(Fe) ~ season + prox.normal + location_2 + Q79 + pH +
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
                log(Mn) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Mn.gm.1))

Mn.gm.2.step <- step(Mn.gm.1)
Mn.gm.2.step
Mn.gm.2 <- get_model(Mn.gm.2.step)
print(summary(Mn.gm.2))
check_model(Mn.gm.2)
anova(Mn.gm.1)
print(anova(Mn.gm.2))
performance(Mn.gm.2)
#season only


###Mo ----
mo.gm.0 <- lmer(data = iws.gm,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.gm.0))

mo.gm.1 <- lmer(data = iws.gm,
                log(Mo) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(mo.gm.1))

mo.gm.2.step <- step(mo.gm.1)
mo.gm.2.step
mo.gm.2 <- get_model(mo.gm.2.step)
print(summary(mo.gm.2))
anova(mo.gm.1)
print(anova(mo.gm.2))
check_model(mo.gm.2)
performance(mo.gm.2)
plot(allEffects(mo.gm.2))
#pH prox season

###Ni ----
ni.gm.0 <- lmer(data = iws.gm,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.gm.0))

ni.gm.1 <- lmer(data = iws.gm,
                log(Ni) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(ni.gm.1))

ni.gm.2.step <- step(ni.gm.1)
ni.gm.2.step
ni.gm.2 <- get_model(ni.gm.2.step)
print(summary(ni.gm.2))
anova(ni.gm.1)
print(anova(ni.gm.2))
check_model(ni.gm.2)
performance(ni.gm.2)
#season only, low marg r2

###Pb ----
Pb.gm.0 <- lmer(data = iws.gm,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.gm.0))

Pb.gm.1 <- lmer(data = iws.gm,
                log(Pb) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Pb.gm.1))

Pb.gm.2.step <- step(Pb.gm.1)
Pb.gm.2.step
Pb.gm.2 <- get_model(Pb.gm.2.step)
print(summary(Pb.gm.2))
check_model(Pb.gm.2)
anova(Pb.gm.1)
print(anova(Pb.gm.2))
performance(Pb.gm.2)
plot(allEffects(Pb.gm.2))
#season prox only

###Sb ----
Sb.gm.0 <- lmer(data = iws.gm,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.gm.0))

Sb.gm.1 <- lmer(data = iws.gm,
                log(Sb) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Sb.gm.1))

Sb.gm.2.step <- step(Sb.gm.1)
Sb.gm.2.step
Sb.gm.2 <- get_model(Sb.gm.2.step)

Sb.gm.2 <- lm(data = iws.gm,
              log(Sb) ~ Q79)
print(summary(Sb.gm.2))
check_model(Sb.gm.2)
print(anova(Sb.gm.2))
perf <- performance(Sb.gm.2)
perf
write.csv(perf, "sbgm79_diag.csv")
plot(allEffects(Sb.gm.2))


###Se ----
Se.gm.0 <- lmer(data = iws.gm,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.gm.0))

Se.gm.1 <- lmer(data = iws.gm,
                log(Se) ~ season + prox.normal + location_2 + Q79 + pH +
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
                log(Sn) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Sn.gm.1))

Sn.gm.2.step <- step(Sn.gm.1)
Sn.gm.2.step
Sn.gm.2 <- get_model(Sn.gm.2.step)
print(summary(Sn.gm.2))
check_model(Sn.gm.2)
anova(Sn.gm.1)
print(anova(Sn.gm.2))
performance(Sn.gm.2)
#season

###V ----
v.gm.0 <- lmer(data = iws.gm,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.gm.0))

v.gm.1 <- lmer(data = iws.gm,
               log(V) ~ season + prox.normal + location_2 + Q79 + pH +
                 (1|site),
               REML = F)
print(summary(v.gm.1))

v.gm.2.step <- step(v.gm.1)
v.gm.2.step
v.gm.2 <- get_model(v.gm.2.step)
print(summary(v.gm.2))
anova(v.gm.1)
print(anova(v.gm.2))
check_model(v.gm.2)
performance(v.gm.2)
#season only score almost signif

###Zn ----
Zn.gm.0 <- lmer(data = iws.gm,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.gm.0))

Zn.gm.1 <- lmer(data = iws.gm,
                log(Zn) ~ season + prox.normal + location_2 + Q79 + pH +
                  (1|site),
                REML = F)
print(summary(Zn.gm.1))

Zn.gm.2.step <- step(Zn.gm.1)
Zn.gm.2.step
Zn.gm.2 <- get_model(Zn.gm.2.step)
print(summary(Zn.gm.2))
check_model(Zn.gm.2)
anova(Zn.gm.1)
print(anova(Zn.gm.2))
performance(Zn.gm.2)
#season
## dh ----
###Ag ----
Ag.dh.0 <- lmer(data = iws.dh,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.dh.0))

Ag.dh.1 <- lmer(data = iws.dh,
                log(Ag) ~ season + prox.normal + score_bin + pH +
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
                log(Al) ~ season + prox.normal + score_bin + pH +
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
                log(As) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(As.dh.1))

As.dh.2.step <- step(As.dh.1)
As.dh.2.step
As.dh.2 <- get_model(As.dh.2.step)
print(summary(As.dh.2))
check_model(As.dh.2)
anova(As.dh.1)
print(anova(As.dh.2))
performance(As.dh.2)
#season only

###Ba ----
ba.dh.0 <- lmer(data = iws.dh,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.dh.0))

ba.dh.1 <- lmer(data = iws.dh,
                log(Ba) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(ba.dh.1))

ba.dh.2.step <- step(ba.dh.1)
ba.dh.2.step
ba.dh.2 <- get_model(ba.dh.2.step)
print(summary(ba.dh.2))
anova(ba.dh.1)
print(anova(ba.dh.2))
check_model(ba.dh.2)
performance(ba.dh.2)
#season only and pH

###Be ----
Be.dh.0 <- lmer(data = iws.dh,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.dh.0))

Be.dh.1 <- lmer(data = iws.dh,
                log(Be) ~ season + prox.normal + score_bin + pH +
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
                log(Cd) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Cd.dh.1))

Cd.dh.2.step <- step(Cd.dh.1)
Cd.dh.2.step
Cd.dh.2 <- get_model(Cd.dh.2.step)
print(summary(Cd.dh.2))
check_model(Cd.dh.2)
anova(Cd.dh.1)
print(anova(Cd.dh.2))
performance(Cd.dh.2)
#season only

###Co ----
Co.dh.0 <- lmer(data = iws.dh,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.dh.0))

Co.dh.1 <- lmer(data = iws.dh,
                log(Co) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Co.dh.1))

Co.dh.2.step <- step(Co.dh.1)
Co.dh.2.step
Co.dh.2 <- get_model(Co.dh.2.step)
print(summary(Co.dh.2))
check_model(Co.dh.2)
anova(Co.dh.1)
print(anova(Co.dh.2))
performance(Co.dh.2)
#season only

###Cr ----
Cr.dh.0 <- lmer(data = iws.dh,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.dh.0))

Cr.dh.1 <- lmer(data = iws.dh,
                log(Cr) ~ season + prox.normal + score_bin + pH +
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
                log(Cu) ~ season + prox.normal + score_bin + pH +
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
                log(Fe) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Fe.dh.1))

Fe.dh.2.step <- step(Fe.dh.1)
Fe.dh.2.step
Fe.dh.2 <- get_model(Fe.dh.2.step)
print(summary(Fe.dh.2))
check_model(Fe.dh.2)
anova(Fe.dh.1)
print(anova(Fe.dh.2))
performance(Fe.dh.2)
#nothing

###Mn ----
Mn.dh.0 <- lmer(data = iws.dh,
             log(Mn) ~
               (1|site),
             REML = F)
print(summary(Mn.dh.0))

Mn.dh.1 <- lmer(data = iws.dh,
             log(Mn) ~ season + prox.normal + score_bin + pH +
               (1|site),
             REML = F)
print(summary(Mn.dh.1))

Mn.dh.2.step <- step(Mn.dh.1)
Mn.dh.2.step
Mn.dh.2 <- get_model(Mn.dh.2.step)
print(summary(Mn.dh.2))
check_model(Mn.dh.2)
anova(Mn.dh.1)
print(anova(Mn.dh.2))
performance(Mn.dh.2)
#season only


###Mo ----
mo.dh.0 <- lmer(data = iws.dh,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.dh.0))

mo.dh.1 <- lmer(data = iws.dh,
                log(Mo) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(mo.dh.1))

mo.dh.2.step <- step(mo.dh.1)
mo.dh.2.step
mo.dh.2 <- get_model(mo.dh.2.step)
print(summary(mo.dh.2))
anova(mo.dh.1)
print(anova(mo.dh.2))
check_model(mo.dh.2)
performance(mo.dh.2)
#pH

###Ni ----
ni.dh.0 <- lmer(data = iws.dh,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.dh.0))

ni.dh.1 <- lmer(data = iws.dh,
                log(Ni) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(ni.dh.1))

ni.dh.2.step <- step(ni.dh.1)
ni.dh.2.step
ni.dh.2 <- get_model(ni.dh.2.step)
print(summary(ni.dh.2))
anova(ni.dh.1)
print(anova(ni.dh.2))
check_model(ni.dh.2)
performance(ni.dh.2)
#season only

###Pb ----
Pb.dh.0 <- lmer(data = iws.dh,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.dh.0))

Pb.dh.1 <- lmer(data = iws.dh,
                log(Pb) ~ season + prox.normal + score_bin + pH +
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
                log(Sb) ~ season + prox.normal + score_bin + pH +
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
                log(Se) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Se.dh.1))

Se.dh.2.step <- step(Se.dh.1)
Se.dh.2.step
Se.dh.2 <- get_model(Se.dh.2.step)
print(summary(Se.dh.2))
check_model(Se.dh.2)
anova(Se.dh.1)
print(anova(Se.dh.2))
performance(Se.dh.2)
#season only

###Sn ----
Sn.dh.0 <- lmer(data = iws.dh,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.dh.0))

Sn.dh.1 <- lmer(data = iws.dh,
                log(Sn) ~ season + prox.normal + score_bin + pH +
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
                log(V) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(v.dh.1))

v.dh.2.step <- step(v.dh.1)
v.dh.2.step
v.dh.2 <- get_model(v.dh.2.step)
print(summary(v.dh.2))
anova(v.dh.1)
print(anova(v.dh.2))
check_model(v.dh.2)
performance(v.dh.2)
#season only

###Zn ----
Zn.dh.0 <- lmer(data = iws.dh,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.dh.0))

Zn.dh.1 <- lmer(data = iws.dh,
                log(Zn) ~ season + prox.normal + score_bin + pH +
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




## hw ----
###Ag ----
Ag.hw.0 <- lmer(data = iws.hw,
                log(Ag) ~
                  (1|site),
                REML = F)
print(summary(Ag.hw.0))

Ag.hw.1 <- lmer(data = iws.hw,
                log(Ag) ~ season + prox.normal + score_bin + pH +
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
                log(Al) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(al.hw.1))

al.hw.2.step <- step(al.hw.1)
al.hw.2.step
al.hw.2 <- get_model(al.hw.2.step)
print(summary(al.hw.2))
check_model(al.hw.2)
anova(al.hw.1)
print(anova(al.hw.2))
performance(al.hw.2)
plot(allEffects(al.hw.2))
#prox and score

###As ----
As.hw.0 <- lmer(data = iws.hw,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(As.hw.0))

As.hw.1 <- lmer(data = iws.hw,
                log(As) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(As.hw.1))

As.hw.2.step <- step(As.hw.1)
As.hw.2.step
As.hw.2 <- get_model(As.hw.2.step)
print(summary(As.hw.2))
check_model(As.hw.2)
anova(As.hw.1)
print(anova(As.hw.2))
performance(As.hw.2)
plot(allEffects(As.hw.2))
#prox pH season

###Ba ----
ba.hw.0 <- lmer(data = iws.hw,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.hw.0))

ba.hw.1 <- lmer(data = iws.hw,
                log(Ba) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(ba.hw.1))

ba.hw.2.step <- step(ba.hw.1)
ba.hw.2.step
ba.hw.2 <- get_model(ba.hw.2.step)
print(summary(ba.hw.2))
anova(ba.hw.1)
print(anova(ba.hw.2))
check_model(ba.hw.2)
performance(ba.hw.2)
plot(allEffects(ba.hw.2))
#prox season

###Be ----
Be.hw.0 <- lmer(data = iws.hw,
                log(Be) ~
                  (1|site),
                REML = F)
print(summary(Be.hw.0))

Be.hw.1 <- lmer(data = iws.hw,
                log(Be) ~ season + prox.normal + score_bin + pH +
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
                log(Cd) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Cd.hw.1))

Cd.hw.2.step <- step(Cd.hw.1)
Cd.hw.2.step
Cd.hw.2 <- get_model(Cd.hw.2.step)
print(summary(Cd.hw.2))
check_model(Cd.hw.2)
anova(Cd.hw.1)
print(anova(Cd.hw.2))
performance(Cd.hw.2)
plot(allEffects(Cd.hw.2))
#season and prox and score

###Co ----
Co.hw.0 <- lmer(data = iws.hw,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(Co.hw.0))

Co.hw.1 <- lmer(data = iws.hw,
                log(Co) ~ season + prox.normal + score_bin + pH +
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
performance(Co.hw.2)
#season prox

###Cr ----
Cr.hw.0 <- lmer(data = iws.hw,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(Cr.hw.0))

Cr.hw.1 <- lmer(data = iws.hw,
                log(Cr) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Cr.hw.1))

Cr.hw.2.step <- step(Cr.hw.1)
Cr.hw.2.step
Cr.hw.2 <- get_model(Cr.hw.2.step)
print(summary(Cr.hw.2))
check_model(Cr.hw.2)
anova(Cr.hw.1)
print(anova(Cr.hw.2))
performance(Cr.hw.2)
#season

###Cu ----
Cu.hw.0 <- lmer(data = iws.hw,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(Cu.hw.0))

Cu.hw.1 <- lmer(data = iws.hw,
                log(Cu) ~ season + prox.normal + score_bin + pH +
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
performance(Cu.hw.2)
#season prox

###Fe ----
Fe.hw.0 <- lmer(data = iws.hw,
                log(Fe) ~
                  (1|site),
                REML = F)
print(summary(Fe.hw.0))

Fe.hw.1 <- lmer(data = iws.hw,
                log(Fe) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Fe.hw.1))

Fe.hw.2.step <- step(Fe.hw.1)
Fe.hw.2.step
Fe.hw.2 <- get_model(Fe.hw.2.step)
print(summary(Fe.hw.2))
check_model(Fe.hw.2)
anova(Fe.hw.1)
print(anova(Fe.hw.2))
performance(Fe.hw.2)
#prox pH season

###Mn ----
Mn.hw.0 <- lmer(data = iws.hw,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(Mn.hw.0))

Mn.hw.1 <- lmer(data = iws.hw,
                log(Mn) ~ season + prox.normal + score_bin + pH +
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
performance(Mn.hw.2)
#season prox only


###Mo ----
mo.hw.0 <- lmer(data = iws.hw,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.hw.0))

mo.hw.1 <- lmer(data = iws.hw,
                log(Mo) ~ season + prox.normal + score_bin + pH +
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
performance(mo.hw.2)
plot(allEffects(mo.hw.2))
#prox season

###Ni ----
ni.hw.0 <- lmer(data = iws.hw,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.hw.0))

ni.hw.1 <- lmer(data = iws.hw,
                log(Ni) ~ season + prox.normal + score_bin + pH +
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
performance(ni.hw.2)
#season only, 

###Pb ----
Pb.hw.0 <- lmer(data = iws.hw,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(Pb.hw.0))

Pb.hw.1 <- lmer(data = iws.hw,
                log(Pb) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Pb.hw.1))

Pb.hw.2.step <- step(Pb.hw.1)
Pb.hw.2.step
Pb.hw.2 <- get_model(Pb.hw.2.step)
print(summary(Pb.hw.2))
check_model(Pb.hw.2)
anova(Pb.hw.1)
print(anova(Pb.hw.2))
performance(Pb.hw.2)
plot(allEffects(Pb.hw.2))
#season prox only

###Sb ----
Sb.hw.0 <- lmer(data = iws.hw,
                log(Sb) ~
                  (1|site),
                REML = F)
print(summary(Sb.hw.0))

Sb.hw.1 <- lmer(data = iws.hw,
                log(Sb) ~ season + prox.normal + score_bin + pH +
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
performance(Sb.hw.2)
#season prox

###Se ----
Se.hw.0 <- lmer(data = iws.hw,
                log(Se) ~
                  (1|site),
                REML = F)
print(summary(Se.hw.0))

Se.hw.1 <- lmer(data = iws.hw,
                log(Se) ~ season + prox.normal + score_bin + pH +
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
performance(Se.hw.2)
#season

###Sn ----
Sn.hw.0 <- lmer(data = iws.hw,
                log(Sn) ~
                  (1|site),
                REML = F)
print(summary(Sn.hw.0))

Sn.hw.1 <- lmer(data = iws.hw,
                log(Sn) ~ season + prox.normal + score_bin + pH +
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
performance(Sn.hw.2)
#season

###V ----
v.hw.0 <- lmer(data = iws.hw,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.hw.0))

v.hw.1 <- lmer(data = iws.hw,
               log(V) ~ season + prox.normal + score_bin + pH +
                 (1|site),
               REML = F)
print(summary(v.hw.1))

v.hw.2.step <- step(v.hw.1)
v.hw.2.step
v.hw.2 <- get_model(v.hw.2.step)
print(summary(v.hw.2))
anova(v.hw.1)
print(anova(v.hw.2))
check_model(v.hw.2)
performance(v.hw.2)
#season

###Zn ----
Zn.hw.0 <- lmer(data = iws.hw,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(Zn.hw.0))

Zn.hw.1 <- lmer(data = iws.hw,
                log(Zn) ~ season + prox.normal + score_bin + pH +
                  (1|site),
                REML = F)
print(summary(Zn.hw.1))

Zn.hw.2.step <- step(Zn.hw.1)
Zn.hw.2.step
Zn.hw.2 <- get_model(Zn.hw.2.step)
print(summary(Zn.hw.2))
check_model(Zn.hw.2)
anova(Zn.hw.1)
print(anova(Zn.hw.2))
performance(Zn.hw.2)
#season



#functions ----
sumFX <- function(datalongDF, subset.vector.string, value.string, dfname.string, filename.string){
  
  #load libraries
  library(tidyverse)
  library(EnvStats)
  
  #assign data
  dat.long <- datalongDF
  cols <- subset.vector.string
  value <- value.string
  dfname <- dfname.string
  filename <- filename.string
  
  #calculate summary stats
  sumtable <- dat.long %>%
    group_by(across(all_of(cols))) %>%
    summarize(n = n(),
              min = min(.data[[value]]),
              max = max(.data[[value]]),
              median = median(.data[[value]]),
              mean = mean(.data[[value]]),
              sd = sd(.data[[value]])
              ,
              gmean = geoMean(.data[[value]]),
              gsd = geoSD(.data[[value]])
    )
  
  #make longer
  sum.long <- pivot_longer(data = sumtable,
                           cols = n:gsd,
                           values_to = "value",
                           names_to = "stat",
                           values_drop_na = T)
  
  #sig figs
  sum.long$value <- formatC(signif(as.numeric(sum.long$value),digits=3), digits=3,format="fg", flag="#")
  
  sum.long$value <- as.numeric(sum.long$value)
  
  #widen
  sum.wide <- pivot_wider(data = sum.long,
                          names_from = stat, #change out as needed
                          values_from = value)
  
  #sig figs messes up count for some reason, add from original
  sum.wide$n <- sumtable$n
  
  #save as csv file in your working directory
  write.csv(sum.wide, paste(filename,"_sum.csv", sep = ""))
  
  #copy to new dataframe with a unique name and place in global environment
  assign(paste(dfname), sum.wide, envir=.GlobalEnv)
  
  # #return the dataframe by character string
  # return(get(dfname))
  
}

hdslmerstepFX <- function(datalongDF, analyte.string, dfname.string){
  
  dat <- datalongDF
  analyte.s <- analyte.string
  dfname <- dfname.string
  
  #dat <- datlong[datlong$analyte == analyte.s,]
  
  mm.0 <- lmer(data = dat,
               log(concentration) ~
                 (1|site),
               REML = F)
  print(summary(mm.0))
  assign(paste(analyte.s, dfname, ".0", sep = ""), mm.0, envir=.GlobalEnv)
  
  mm.1 <- lmer(data = dat,
               log(concentration) ~ season + prox.normal + score_bin +
                 (1|site),
               REML = F)
  print(summary(mm.1))
  assign(paste(analyte.s, dfname, ".1", sep = ""), mm.1, envir=.GlobalEnv)
  
  mm.2.step <- step(mm.1)
  #mm.2.step
  #mm.2 <- get_model(mm.2.step)
  #print(summary(mm.2))
  #print(anova(mm.2))
  assign(paste(analyte.s, dfname, ".2", sep = ""), mm.2, envir=.GlobalEnv)
  
}

hdslmerstepFX(datalongDF = iw.score.long[is.score.long$analyte == "Mn",],
              analyte.string = "Mn",
              dfname.string = ".all")

ForwardStep <- function(df,yName, Xs, XsMin) {
  Data <- df[, c(yName,Xs)]
  fit <- glm(formula = paste(yName, " ~ ", paste0(XsMin, collapse = " + ")),
             data = Data, family = binomial(link = "logit") )
  ScopeFormula <- list(lower = paste(yName, " ~ ", paste0(XsMin, collapse = " + ")), 
                       upper = paste(yName, " ~ ", paste0(Xs, collapse = " + ")))
  result <- step(fit, direction = "forward", scope = ScopeFormula, trace = 1 )
  
  return(result)
}


ForwardStep <- function(df,Yname, Xs, XsMin) {
  Data <- df[, c(Yname,Xs)]
  f1 <- as.formula(paste(Yname, " ~ ", paste0(XsMin, collapse = " + ")))
  
  fit <- glm(formula = f1,
             data = Data, family = binomial(link = "logit") )
  f2 <- as.formula(paste(Yname, " ~ ", paste0(XsMin, collapse = " + ")))
  f3 <- as.formula(paste(Yname, " ~ ", paste0(Xs, collapse = " + ")))
  
  ScopeFormula <- list(lower = f2, 
                       upper = f3)
  step(fit, direction = "forward", scope = ScopeFormula, trace = 1)
}






