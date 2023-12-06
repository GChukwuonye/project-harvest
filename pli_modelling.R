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


#pli tucson modeling ----
pli_tucson<- iw.dm[iw.dm$community=="Tucson",]
pli_tucson$pli.ln<- na.omit(pli_tucson$pli.ln)
pli_tucson$score_bin<- na.omit(pli_tucson$score_bin)
pli_tucson$prox.normal<- na.omit(pli_tucson$season)
pli_tucson$season<- na.omit(pli_tucson$season)

plt0 <- lmer(data = pli_tucson,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final

plt2 <- lmer(data = pli_tucson,
            pli.ln ~  season + prox.normal + score_bin +
              season:score_bin+
              prox.normal:score_bin+
            (1|site),
            REML = F) 
plt.step <- step(plt2)
plt.step
plt3 <- get_model(plt.step)
print(summary(plt3))
check_model(plt3)
anova(plt3)
print(anova(plt3))
performance(plt3)




#pli dewey modeling ----

pli_dewey<- iw.dm[iw.dm$community=="Dewey-Humboldt",]

pld0 <- lmer(data =pli_dewey,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(pld0)

pld <- lmer(data = pli_dewey,
            pli.ln ~  season + prox.normal + score_bin +
              season:score_bin+
              prox.normal:score_bin
            + (1|site),
            REML = F) #ML for comparison, REML for final
summary(pld)

pld.step <- step(pld)
pld.step
pld2 <- get_model(pld.step)

print(summary(pld2))
check_model(pld2)
anova(pld2)
print(anova(pld2))
performance(pld2)



#pli globe modeling ----
pli_globe<- iw.dm[iw.dm$community=="Globe/Miami",]
plg0 <- lmer(data =pli_globe,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plg0)


plg <- lmer(data = pli_globe,
         pli.ln ~  season + prox.normal + score_bin+
              season:score_bin+
              prox.normal:score_bin
            + (1|community:site),
            REML = F) #ML for comparison, REML for final
summary(plg)
plg.step <- step(plg)
plg.step
plg2 <- get_model(plg.step)

print(summary(plg2))
check_model(plg2)
anova(plg2)
print(anova(plg2))
performance(plg2)


#pli hayden modeling ----
pli_hayden<- iw.dm[iw.dm$community=="Hayden/Winkelman",]
plh0 <- lmer(data =pli_hayden,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plh0)
plh <- lmer(data = pli_hayden,
           pli.ln ~  season + prox.normal + score_bin +
              season:score_bin+
              prox.normal:score_bin
            + (1|site),
            REML = F) #ML for comparison, REML for final
summary(plh)
plh.step <- step(plh)
plh.step
plh2 <- get_model(plh.step)

print(summary(plh2))
check_model(plh2)
vif(plh2)
anova(plh2)
print(anova(plh2))
performance(plh2)


#individual model====
#tucson individual model=====
#pli_tucson<- iw.dm[iw.dm$community=="Tucson",]

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
tuc0<- lmer(data = pli_tucson,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(tuc0)

tuc1<- lmer(data= pli_tucson,
              pli.ln~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(tuc1)  

tuc.step <- step(tuc1, direction= "both")
tuc.step
tuc2 <- get_model(tuc.step)

print(summary(tuc2))
check_model(tuc2)
vif(tuc2)
anova(tuc2)
print(anova(tuc2))
performance(tuc2)

#Q71====
#Do you treat or wash your cistern with anything?
tuc71 <- lmer(data = pli_tucson,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(tuc71)

tuc71b<- lmer(data= pli_tucson,
              pli.ln~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(tuc71b)                
tuc.step2 <- step(tuc71b)
tuc.step2
tuc71c <- get_model(tuc.step2)

print(summary(tuc71c))
check_model(tuc71c)
vif(tuc71c)
anova(tuc71c)
print(anova(tuc71c))
performance(tuc71c)



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
  #pli_tucson<- iw.dm[iw.dm$community=="Tucson",]
  
  #Q67====
dew <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew)

dew2<- lmer(data= pli_dewey,
              pli.ln~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew2)                
anova(dew2) 
dew.step <- step(dew2)
dew.step
dew3 <- get_model(dew.step)

print(summary(dew3))
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
performance(dew3)

#Q71====
#Do you treat or wash your cistern with anything?
dew71 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew71)

dew71b<- lmer(data= pli_dewey,
              pli.ln~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew71b)                
dew.step <- step(dew71b)
dew.step
dew3 <- get_model(dew.step)

print(summary(dew3))
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
performance(dew3)
#not relevant for dewey. All dewey participants responded with no. 




#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
dew79 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew79)

dew79b<- lmer(data=pli_dewey,
              pli.ln~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew79b)  
dew.step <- step(dew79b)
dew.step
dew3 <- get_model(dew.step)

print(summary(dew3))
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
performance(dew3)


#Q76====
dew76 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew76)

dew76b<- lmer(data= pli_dewey,
              pli.ln~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew76b)                


dew.step <- step(dew76b)
dew.step
dew3 <- get_model(dew.step) #model irrelevant. all dewey residents answerted with no. 

print(summary(dew3))
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
performance(dew3)

#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
dew77 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew77)

dew77b<- lmer(data= pli_dewey,
              pli.ln~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew77b)                
dew.step <- step(dew77b)
dew.step
dew3 <- get_model(dew.step)

print(summary(dew3))
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
performance(dew3)

#hayden individual model=====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
hay <- lmer(data = pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay)

hay67<- lmer(data=  pli_hayden,
              pli.ln~ season+ prox.normal+ Q67 + 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay67)                
hay.step <- step(hay67)
hay.step
hay2 <- get_model(hay.step)

print(summary(hay2))
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
performance(hay2)
#no response from Hayden- All were zero

#Q71====
#Do you treat or wash your cistern with anything?
hay71 <- lmer(data =  pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

hay71b<- lmer(data=  pli_hayden,
              pli.ln~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay71b)                
hay.step <- step(hay71b)
hay.step
hay2 <- get_model(hay.step)

print(summary(hay2))
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
performance(hay2)


#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
hay79<- lmer(data =  pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay79)

hay79b<- lmer(data= pli_hayden,
              pli.ln~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay79b)                
hay.step <- step(hay79b)
hay.step
hay2 <- get_model(hay.step)

print(summary(hay2))
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
performance(hay2)

#Q76====
#Does your cistern have a first flush?
hay76 <- lmer(data =  pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay76)

hay76b<- lmer(data= pli_hayden,
              pli.ln~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay76b)                

hay.step <- step(hay76b)
hay.step
hay2 <- get_model(hay.step)

print(summary(hay2))
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
performance(hay2)



#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
hay77<- lmer(data =  pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay77)

hay77b<- lmer(data= pli_hayden,
              pli.ln~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay77b)                


hay.step <- step(hay77b)
hay.step
hay2 <- get_model(hay.step)

print(summary(hay2))
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
performance(hay2)


#globe individual model=====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
globe <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe67<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q67+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe67)                

globe.step <- step(globe67)
globe.step
globe2 <- get_model(globe.step)

print(summary(globe2))
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
performance(globe2)

#Q71====
#Do you treat or wash your cistern with anything?
globe71 <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe71)

globe71b<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q71+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe71b)                

globe.step <- step(globe71)
globe.step
globe2 <- get_model(globe.step)

print(summary(globe2))
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
performance(globe2)



#Q79====
globe <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe79<- lmer(data=pli_globe,
              pli.ln~ season+ prox.normal+ Q79+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe79)                

globe.step <- step(globe79)
globe.step
globe2 <- get_model(globe.step)

print(summary(globe2))
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
performance(globe2)

#Q76====
#Does your cistern have a first flush?
globe <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe76<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q76+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe76)                

globe.step <- step(globe76)
globe.step
globe2 <- get_model(globe.step)

print(summary(globe2))
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
performance(globe2)


#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
globe <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe77<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q77+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe77)   

globe.step <- step(globe77)
globe.step
globe2 <- get_model(globe.step)

print(summary(globe2))
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
performance(globe2)


