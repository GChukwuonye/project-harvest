library(car)
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





#pli full modeling ----
pli0 <- lmer(data = iw.score,
             pli ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(pli0)

#including relevant variables - period not included based on MFA and previous modeling. proximity.km:season interaction not included because proximity to pollutant does not change by season
pli2 <- lmer(data = iw.score,
             pli ~  season + prox.normal + score_bin+
               season:score_bin+
               prox.normal:score_bin
             + (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(pli2)
plot(pli2) #not heteroscedastic when untransformed
model.effects <- allEffects(pli2)
plot(model.effects)
vif(pli2)
anova(pli2)
step(pli2, direction="backward")

pli3 <- lmer(data = iw.score,
             pli ~  season + prox.normal + score_bin +
               prox.normal:score_bin
             + (1|community:site),
             REML = F) #ML for comparison, REML for final
anova(pli3)
summary(pli3) 
plot(pli3) 
model.effects <- allEffects(pli3)
plot(model.effects)
vif(pli3)

AIC(pli2, pli3) #pli3 is a better model


#pli tucson modeling ----
pli_tucson<- iw.score[iw.score$community=="Tucson",]
plt0 <- lmer(data = pli_tucson,
             pli ~ (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(plt0)

plt <- lmer(data = pli_tucson,
            pli ~  season + prox.normal + score_bin +
              season:score_bin+
              prox.normal:score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plt)
plot(plt) #not heteroscedastic when untransformed
model.effects <- allEffects(plt)
plot(model.effects)
vif(plt)
anova(plt) #only season is relevant for Tucson
step(plt, direction="backward")


#pli dewey modeling ----

pli_dewey<- iw.score[iw.score$community=="Dewey-Humboldt",]

pld0 <- lmer(data =pli_dewey,
             pli ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(pld0)

pld <- lmer(data = pli_dewey,
            pli ~  season + prox.normal + score_bin +
              season:score_bin+
              prox.normal:score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(pld)
plot(pld) #not heteroscedastic when untransformed
model.effects <- allEffects(pld)
plot(model.effects)
vif(pld)
anova(pld) #only season is relevant
step(pld, direction="backward")

#pli globe modeling ----
pli_globe<- iw.score[iw.score$community=="Globe/Miami",]
plg0 <- lmer(data =pli_globe,
             pli ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plg0)


plg <- lmer(data = pli_globe,
         pli ~  season + prox.normal + score_bin+
              season:score_bin+
              prox.normal:score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plg)
plot(plg) #not heteroscedastic when untransformed
model.effects <- allEffects(plg)
plot(model.effects)
vif(plg)
anova(plg)
step(plg, direction="backward")

plg2 <- lmer(data = pli_globe,
            pli ~  season + prox.normal +  season:score_bin + score_bin+
              prox.normal:score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plg2)
plot(plg2) #not heteroscedastic when untransformed
model.effects <- allEffects(plg2)
plot(model.effects)
vif(plg2)
anova(plg2) #season, prooximity, hds, season:hds and proximity:hds are all relevant


#pli hayden modeling ----
pli_hayden<- iw.score[iw.score$community=="Hayden/Winkelman",]
plh0 <- lmer(data =pli_hayden,
             pli ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plh0)
plh <- lmer(data = pli_hayden,
           pli ~  season + prox.normal + score_bin +
              season:score_bin+
              prox.normal:score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plh)
plot(plh) 
model.effects <- allEffects(plh)
plot(model.effects)
vif(plh)
anova(plh)
step(plh, direction="backward")

plh2 <- lmer(data = pli_hayden,
            pli ~  season + score_bin+ season:score_bin+
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plh2)
plot(plh2)
model.effects <- allEffects(plh2)
plot(model.effects)
vif(plh2)
anova(plh2)

