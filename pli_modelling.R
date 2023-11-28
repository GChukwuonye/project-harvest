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
               season :score_bin
             + (1|community:site),
             REML = F) #ML for comparison, REML for final
anova(pli3)
summary(pli3) 
plot(pli3) 
model.effects <- allEffects(pli3)
plot(model.effects)
vif(pli3)

AIC(pli2, pli3) #pli3 is a better model

pli4 <- lmer(data = iw.score,
             pli ~  season + prox.normal + score_bin 
             + (1|community:site),
             REML = F) #ML for comparison, REML for final
anova(pli4)
summary(pli4) 
plot(pli4) 
model.effects <- allEffects(pli4)
plot(model.effects)
vif(pli4)


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

plt2 <- lmer(data = pli_tucson,
            pli ~  season + prox.normal + score_bin +
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plt2)
plot(plt2) #not heteroscedastic when untransformed
model.effects <- allEffects(plt2)
plot(model.effects)
vif(plt2)
anova(plt2) #only season is relevant for Tucson



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

pld2 <- lmer(data = pli_dewey,
            pli ~  season + prox.normal + score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(pld2)

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
vif(plg2)
anova(plg2) #season, prooximity, hds, season:hds and proximity:hds are all relevant


plg3 <- lmer(data = pli_globe,
            pli ~  season + prox.normal + score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plg3)
plot(plg3) #not heteroscedastic when untransformed
vif(plg3)
anova(plg3) #season, prooximity, hds, season:hds and proximity:hds are all relevant


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



plh3 <- lmer(data = pli_hayden,
             pli ~  season + score_bin+ prox.normal
               + (1|community:site),
             REML = F) #ML for comparison, REML for final
summary(plh3)
plot(plh3)
model.effects <- allEffects(plh3)
plot(model.effects)
vif(plh3)
anova(plh3)


#individual model====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
model1 <- lmer(data = iw.score,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= iw.score,
              pli~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q67 is significant
model.effects <- allEffects(model2)
plot(model.effects) #pollution load increased for people who clean their roof?



#Q71====
#Do you treat or wash your cistern with anything?
model1 <- lmer(data = iw.score,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= iw.score,
              pli~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q71 is not significant
model.effects <- allEffects(model2)
plot(model.effects) 



  #Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
model1 <- lmer(data = iw.score,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= iw.score,
              pli~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q79 is not significant

#Q76====
#Does your cistern have a first flush?
model1 <- lmer(data = iw.score,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= iw.score,
              pli~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q76 is  significant
model.effects <- allEffects(model2)
plot(model.effects) #pollution load increased for people who have a first flush?


#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
model1 <- lmer(data = iw.score,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= iw.score,
              pli~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant


#tucson individual model=====
#pli_tucson<- iw.score[iw.score$community=="Tucson",]

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
              pli~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q67 is not significant

#Q71====
#Do you treat or wash your cistern with anything?
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
              pli~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q71 is not significant



#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
              pli~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q79 is not significant

#Q76====
#Does your cistern have a first flush?
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
              pli~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q76 is not significant



#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
              pli~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant


#dewey individual model=====
  #pli_tucson<- iw.score[iw.score$community=="Tucson",]
  
  #Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
model1 <- lmer(data = pli_dewey,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_dewey,
              pli~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q67 is not significant

#Q71====
#Do you treat or wash your cistern with anything?
model1 <- lmer(data = pli_dewey,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_dewey,
              pli~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q71 is not significant



#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
model1 <- lmer(data = pli_dewey,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data=pli_dewey,
              pli~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q79 is not significant

#Q76====
#Does your cistern have a first flush?
model1 <- lmer(data = pli_dewey,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_dewey,
              pli~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q76 is not significant



#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
model1 <- lmer(data = pli_dewey,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_dewey,
              pli~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant

#hayden individual model=====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
model1 <- lmer(data = pli_hayden,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data=  pli_hayden,
              pli~ season+ prox.normal+ Q67 + 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q67 is not significant

#Q71====
#Do you treat or wash your cistern with anything?
model1 <- lmer(data =  pli_hayden,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data=  pli_hayden,
              pli~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q71 is not significant



#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
model1 <- lmer(data =  pli_hayden,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_hayden,
              pli~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q79 is not significant

#Q76====
#Does your cistern have a first flush?
model1 <- lmer(data =  pli_hayden,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_hayden,
              pli~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q76 is  significantly increasing pollution load



#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
model1 <- lmer(data =  pli_hayden,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_hayden,
              pli~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant


#globe individual model=====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe,
              pli~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q67 is not significant

#Q71====
#Do you treat or wash your cistern with anything?
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe,
              pli~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q71 is not significant



#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data=pli_globe,
              pli~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q79 is almost significant

#Q76====
#Does your cistern have a first flush?
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe,
              pli~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q76 is  significantly increasing pollution load



#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe,
              pli~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant