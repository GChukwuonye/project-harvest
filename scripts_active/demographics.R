#Kunal Palawat
#January 11th, 2024
#Description: Code to analyze rainwater data by sociodemographics
#analyzing the following demographics: zipcode, primary language, race/ethnicity, BIPOC, education, household size, income level
#notes
#87 sites with values for all the demographics listed above - might be possible to analyze all of them at once with pli/specific analytes
#control by season and then list all these variables???
#community can be a proxy for race, language, income, zipcode
#for education - do pre and post high school + trade school


#load libraries ----
library(readxl)
library(tidyverse)
library(EnvStats)
library(corrplot)
library(lme4)
library(lmerTest)
library(performance)
library(effects)

#data formatting----
#make sure there are no NA/blank values and remove samples with zipcodes with less than 3 sites
iw.demo$income <- iw.demo$`Low Income`
iw.demo$lang <- iw.demo$`Primary Language`
iwm.demo <- iw.demo %>%
  filter(BIPOC!="Other")%>%
  drop_na(community_2)%>%
  drop_na(income)%>%
  drop_na(prox.normal)%>%
  drop_na(Zip)


#split dataframe into different ones for each community


iws.c <- iw.demo %>%
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

#tucson specific
iws.tu <- iws.tu %>%
  drop_na(prox.normal)

tuc <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "tucson", col_names = TRUE)
iws.tu <- full_join(iws.tu, tuc, by = c("site"))
iws.tu <- iws.tu %>%
  drop_na(ward) %>%
  drop_na(community) %>%
  drop_na(location)
iws.tu$ward <- factor(iws.tu$ward, levels = c("One", "Two", "Three", "Four", "Five", "Six"))

#summaries----
##demo summaries ----
###overall ----
facsumFX(datalongDF = demo.long,
      subset.vector.string = c("demographic", "value"),
      dfname.string = "sum.demo",
      filename.string = "demo")

###community ----
facsumFX(datalongDF = demo.long,
         subset.vector.string = c("demographic", "community", "value"),
         dfname.string = "sum.demo.com",
         filename.string = "demo_com")

###community_2 ----
facsumFX(datalongDF = demo.long,
         subset.vector.string = c("demographic", "community_2", "value"),
         dfname.string = "sum.demo.com2",
         filename.string = "demo_com2")

##concentration summaries----
###overall----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte"),
      value.string = "value",
      dfname.string = "sum.iwdemo",
      filename.string = "iwdemo")

###community----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community"),
      value.string = "value",
      dfname.string = "sum.iwdemo.com",
      filename.string = "iwdemo_com")

###community_2----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community_2"),
      value.string = "value",
      dfname.string = "sum.iwdemo.com2",
      filename.string = "iwdemo_com2")

###community, season----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community", "season"),
      value.string = "value",
      dfname.string = "sum.iwdemo.comssn",
      filename.string = "iwdemo_comssn")

###community_2, season----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community_2", "season"),
      value.string = "value",
      dfname.string = "sum.iwdemo.com2ssn",
      filename.string = "iwdemo_com2ssn")

###demos----
####zipcode----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Zip"),
      value.string = "value",
      dfname.string = "sum.iwdemo.zip",
      filename.string = "iwdemo_zip")

####primary language----
#assuming the primary language of the participant is the primary language of the household
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Primary Language"),
      value.string = "value",
      dfname.string = "sum.iwdemo.lang",
      filename.string = "iwdemo_lang")

####race ethnicity----
#assuming all members of the household are the same race as the participant
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Race Ethnicity"),
      value.string = "value",
      dfname.string = "sum.iwdemo.race",
      filename.string = "iwdemo_race")

####BIPOC----
#assuming all members of the household are the same race as the participant
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "BIPOC"),
      value.string = "value",
      dfname.string = "sum.iwdemo.bipoc",
      filename.string = "iwdemo_bipoc")

####Education level----
#assuming the participant is the one in the household with the highest education level
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Highest Levelof Education Completed"),
      value.string = "value",
      dfname.string = "sum.iwdemo.ed",
      filename.string = "iwdemo_ed")

sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Education_grouped"),
      value.string = "value",
      dfname.string = "sum.iwdemo.ed2",
      filename.string = "iwdemo_ed2")

####Household Size----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Household Size"),
      value.string = "value",
      dfname.string = "sum.iwdemo.size",
      filename.string = "iwdemo_size")

####household_size----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "household_size"),
      value.string = "value",
      dfname.string = "sum.iwdemo.size2",
      filename.string = "iwdemo_size2")

####Income Level----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Income Level"),
      value.string = "value",
      dfname.string = "sum.iwdemo.inc",
      filename.string = "iwdemo_inc")

####Low Income Level----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Low Income"),
      value.string = "value",
      dfname.string = "sum.iwdemo.inc",
      filename.string = "iwdemo_inc")


#Initial correlations ----
##community----
#community x zip are correlated, definitely dont use both in a single model
#community x language are correlated
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Primary Language",
      filename.string = "X2_comlang")

#community and race are correlated - does this mean we cannot have both those variables in the same model?
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Race Ethnicity",
      filename.string = "X2_comrace")

xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "BIPOC",
      filename.string = "X2_compoc")

#com x ed are correlated
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Education_grouped",
      filename.string = "X2_comed")

#com x household size
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "household_size",
      filename.string = "X2_comsize")

#com x income
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Low Income",
      filename.string = "X2_comsize")

##proximity ----
#prox x language not collinear?

demo$lang <- demo$`Primary Language`
demo$income <- demo$`Low Income`

plang <- lm(data = demo,
            prox.normal~lang*community)
summary(plang)
plot(allEffects(plang))
anova(plang)

ggplot(data = demo, mapping = aes(x = prox.normal, fill = `Primary Language`))+
  geom_histogram() +
  facet_wrap(.~community_2)

#prox x race only correlated for Tucson
demopoc <- demo%>%
  filter(BIPOC!="Other")
ppoc <- lm(data = demopoc,
            log(proximity.km)~BIPOC*community_2)
summary(ppoc)
plot(allEffects(ppoc))
anova(ppoc)

ppocgm <- lm(data = demopoc[demopoc$community=="Globe/Miami",],
           prox.normal~BIPOC)
summary(ppocgm)
plot(allEffects(ppocgm))
anova(ppocgm)
ppoctu <- lm(data = demopoc[demopoc$community=="Tucson",],
             prox.normal~BIPOC)
summary(ppoctu)
plot(allEffects(ppoctu))
anova(ppoctu)



ggplot(data = demopoc, mapping = aes(x = proximity.km, fill = BIPOC))+
  geom_boxplot() +
  facet_grid(.~community_2) +
  theme(legend.position = "bottom")

#correlated for globe?
ggplot(data = demopoc, mapping = aes(x = prox.normal, fill = `BIPOC`))+
  geom_boxplot() +
  facet_grid(.~community) +
  theme(legend.position = "bottom")

#prox x education only correlated for Tucson
ped <- lm(data = demo,
           prox.normal~Education_grouped*community_2)
summary(ped)
plot(allEffects(ped))
anova(ped)

ggplot(data = demo, mapping = aes(x = prox.normal, fill = `Education_grouped`))+
  geom_histogram() +
  #facet_grid(.~community) +
  theme(legend.position = "bottom")

#prox x household size no clear correlation
phs <- lm(data = demo,
          prox.normal~household_size*community_2)
summary(phs)
plot(allEffects(phs))
anova(phs)

ggplot(data = demo, mapping = aes(x = prox.normal, fill = `household_size`))+
  geom_histogram() +
  facet_grid(.~community) +
  theme(legend.position = "bottom")

#prox x income
pinc <- lm(data = demo,
          prox.normal~income*community_2)
summary(pinc)
plot(allEffects(pinc))
anova(pinc)

ggplot(data = demo, mapping = aes(x = proximity.km, fill = income))+
  geom_boxplot() +
  facet_grid(.~community) +
  theme(legend.position = "bottom")

#Models ----
##all communities ----
###pli, multivariate ----
#Language removed because spanish speakers are only in tucson

pli.demo.1 <- lmer(data = iwm.demo,
            log(pli) ~ season + community_2+ BIPOC + `Low Income`+Zip+prox.normal + pH+
              (1|site),
            REML = T)
summary(pli.demo.1)
vif(pli.demo.1)
check_model(pli.demo.1)
#as expected, zipcode, community_2, and language are highly correlated

pli.demo.2 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC + `Low Income`+ `Primary Language` + `Education_grouped`+Zip)
summary(pli.demo.2)
vif(pli.demo.2)
check_model(pli.demo.2)
#zipcode is highly colinear...but not sure with what

pli.demo.3 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC + `Low Income`+ `Primary Language` + `Education_grouped`+community_2)
summary(pli.demo.3)
vif(pli.demo.3)
check_model(pli.demo.3)
#community_2 colinear

pli.demo.4 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC + `Low Income`+ `Primary Language` + `Education_grouped`)
summary(pli.demo.4)
vif(pli.demo.4)
check_model(pli.demo.4)
anova(pli.demo.4)
performance(pli.demo.4)

####best linear model ----
stepmodel <- step(pli.demo.1)
pli.demo.a <- get_model(stepmodel)
summary(pli.demo.a)
anova(pli.demo.a)
perf <- performance(pli.demo.a)
write.csv(perf, "pli_demo_diag.csv")
model.sum <- summary(pli.demo.a)
write.csv(model.sum$coefficients, "pli_demo_sum.csv")

pli.demo.5 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC+Zip+season)
summary(pli.demo.5)
vif(pli.demo.5)
check_model(pli.demo.5)
anova(pli.demo.5)
performance(pli.demo.5)
plot(allEffects(pli.demo.5))
#season and ZIP and BIPOC significant

pli.demo.5.1 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC+Zip)
anova(pli.demo.5, pli.demo.5.1)
compare_performance(pli.demo.5, pli.demo.5.1)

pli.demo.6 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC*Zip)
summary(pli.demo.6)
vif(pli.demo.6)
check_model(pli.demo.6)
anova(pli.demo.6)

pli.demo.7 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC+community_2+season+Zip+prox.normal)
summary(pli.demo.7)
vif(pli.demo.7)
check_model(pli.demo.7)
anova(pli.demo.7) #proximity is NOT significant

pli.demo.7.1 <- lm(data = iwm.demo,
                 log(pli) ~ prox.normal)
summary(pli.demo.7.1)


###pli bivariate ----
pli.demorace.1 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC*community_2)
summary(pli.demorace.1)
vif(pli.demorace.1)
check_model(pli.demorace.1)
#all collinear...expected, not sure what to do

pli.demorace.2 <- lm(data = iwm.demo,
                     log(pli) ~ BIPOC+community_2)
summary(pli.demorace.2)
vif(pli.demorace.2)
check_model(pli.demorace.2)
anova(pli.demorace.2)
performance(pli.demorace.2)

step(pli.demorace.1)

##dewey-humboldt ----
#education, household size, income, zipcode
#note - not enough samples to focus on all of these, prioritize education, income?? sample size goes from 42 to 25, with DH may need to do univariate models controlling for season and proximity
###only demos----
####income signif ----
iw.demo.dh <- iws.dh %>%
  drop_na(`Education_grouped`)%>%
  drop_na(`Zip`)%>%
  drop_na(`Low Income`)%>%
  drop_na(`household_size`)
#this reduces sample size from 42 to 25
iw.demo.dh <- iws.dh %>%
  drop_na(`Zip`)%>%
  drop_na(`Low Income`)%>%
  drop_na(`household_size`)
#this reduces sample size from 42 to 32
iw.demo.dh$income <- iw.demo.dh$`Income Level`
pli.demodh.dems <- lmer(data = iw.demo.dh,
                       log(pli) ~ Zip + income + `household_size`+
                         (1|site),
                       REML = T)
print(summary(pli.demodh.dems))
check_model(pli.demodh.dems)
performance(pli.demodh.dems)
pli.demodh.dems.step <- step(pli.demodh.dems)
pli.demodh.dems.step
pli.demodh.dems.1 <- get_model(pli.demodh.dems.step)
summary(pli.demodh.dems.1)
check_model(pli.demodh.dems.1)
anova(pli.demodh.dems.1)
plot(allEffects(pli.demodh.dems.1))
perf <- performance(pli.demodh.dems.1)
perf
write.csv(perf, "plidemos_dh_diag.csv")

###univariate ----
#zip, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(`Zip`)
pli.demodh.zip <- lmer(data = iw.demo.dh,
                       log(pli) ~ Zip +
                         (1|site),
                       REML = T)
print(summary(pli.demodh.zip))
check_model(pli.demodh.zip)
performance(pli.demodh.zip)
anova(pli.demodh.zip)
plot(allEffects(pli.demodh.zip))
#not signif

#education, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(`Education_grouped`)
pli.demodh.ed <- lmer(data = iw.demo.dh,
                      log(pli) ~ Education_grouped +
                        (1|site),
                      REML = T)
print(summary(pli.demodh.ed))
check_model(pli.demodh.ed)
performance(pli.demodh.ed)
anova(pli.demodh.ed)

#household size, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(`household_size`)

pli.demodh.house <- lmer(data = iw.demo.dh,
                         log(pli) ~ `household_size` +
                           (1|site),
                         REML = T)
print(summary(pli.demodh.house))
check_model(pli.demodh.house)
performance(pli.demodh.house)
pli.demodh.house.step <- step(pli.demodh.house)
pli.demodh.house.step
pli.demodh.house.1 <- get_model(pli.demodh.house.step)
summary(pli.demodh.house.1)

#income, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(`Low Income`)
iw.demo.dh$low <- iw.demo.dh$`Low Income`
pli.demodh.inc <- lmer(data = iw.demo.dh,
                       log(pli) ~ low +
                         (1|site),
                       REML = T)
print(summary(pli.demodh.inc))
check_model(pli.demodh.inc)
performance(pli.demodh.inc)
pli.demodh.inc.step <- step(pli.demodh.inc)
pli.demodh.inc.step
pli.demodh.inc.1 <- get_model(pli.demodh.inc.step)
summary(pli.demodh.inc.1)
perf <- performance(pli.demodh.inc.1)
perf
write.csv(perf, "pli_demodh_inc_diag.csv")
modelsum <- summary(pli.demodh.inc.1)
write.csv(modelsum$coefficients, "pli_demodh_inc_sum.csv")
plot(allEffects(pli.demodh.inc.1))

###univariate plus env ----
#zip, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(prox.normal)%>%
  drop_na(`Zip`)
pli.demodh.zip <- lmer(data = iw.demo.dh,
                log(pli) ~ season + Zip + prox.normal+
                  (1|site),
                REML = T)
print(summary(pli.demodh.zip))
check_model(pli.demodh.zip)
performance(pli.demodh.zip)
pli.demodh.zip.step <- step(pli.demodh.zip)
pli.demodh.zip.step
pli.demodh.zip.1 <- get_model(pli.demodh.zip.step)
summary(pli.demodh.zip.1)

#education, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(prox.normal)%>%
  drop_na(`Education_grouped`)
pli.demodh.ed <- lmer(data = iw.demo.dh,
                     log(pli) ~ season + Education_grouped + prox.normal+
                       (1|site),
                     REML = T)
print(summary(pli.demodh.ed))
check_model(pli.demodh.ed)
performance(pli.demodh.ed)
pli.demodh.ed.step <- step(pli.demodh.ed)
pli.demodh.ed.step
pli.demodh.ed.1 <- get_model(pli.demodh.ed.step)
summary(pli.demodh.ed.1)

#household size, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(prox.normal)%>%
  drop_na(`household_size`)

pli.demodh.house <- lmer(data = iw.demo.dh,
                      log(pli) ~ season + `household_size` + prox.normal+
                        (1|site),
                      REML = T)
print(summary(pli.demodh.house))
check_model(pli.demodh.house)
performance(pli.demodh.house)
pli.demodh.house.step <- step(pli.demodh.house)
pli.demodh.house.step
pli.demodh.house.1 <- get_model(pli.demodh.house.step)
summary(pli.demodh.house.1)

#income, not signif
iw.demo.dh <- iws.dh %>%
  drop_na(prox.normal)%>%
  drop_na(`Low Income`)
pli.demodh.inc <- lmer(data = iw.demo.dh,
                         log(pli) ~ season + `Low Income` + prox.normal+
                           (1|site),
                         REML = T)
print(summary(pli.demodh.inc))
check_model(pli.demodh.inc)
performance(pli.demodh.inc)
pli.demodh.inc.step <- step(pli.demodh.inc)
pli.demodh.inc.step
pli.demodh.inc.1 <- get_model(pli.demodh.inc.step)
summary(pli.demodh.inc.1)

##globe/miami ----
#bipoc, education, household size, income, race?, zip?
#sample size decreases from 107 to 57 when removing NAs for all variables of interest...may need to do individual modeling controlling for season and proximity

###only demos ----
####nothing signif ----
iw.demo.gm <- iws.gm %>%
  drop_na(BIPOC)%>%
  drop_na(`Education_grouped`)%>%
  drop_na(`Low Income`)%>%
  drop_na(`household_size`)%>%
  drop_na(Zip)
#this reduces sample size from 107 to 57
iw.demo.gm <- iws.gm %>%
  drop_na(BIPOC)%>%
  drop_na(`household_size`)%>%
  drop_na(`Low Income`)%>%
  drop_na(Zip)
#this reduces sample size from 107 to 86

pli.demogm.dems <- lmer(data = iw.demo.gm,
                        log(pli) ~ BIPOC + `Low Income`+ `household_size`+ Zip+
                          (1|site),
                        REML = T)
print(summary(pli.demogm.dems))
check_model(pli.demogm.dems)
performance(pli.demogm.dems)
pli.demogm.dems.step <- step(pli.demogm.dems)
pli.demogm.dems.step
pli.demogm.dems.1 <- get_model(pli.demogm.dems.step)
summary(pli.demogm.dems.1)
plot(allEffects(pli.demogm.dems.1))

###univariate ----
#bipoc, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(`BIPOC`)
pli.demogm.poc <- lmer(data = iw.demo.gm,
                       log(pli) ~  BIPOC + 
                         (1|site),
                       REML = T)
print(summary(pli.demogm.poc))
check_model(pli.demogm.poc)
performance(pli.demogm.poc)
pli.demogm.poc.step <- step(pli.demogm.poc)
pli.demogm.poc.step
pli.demogm.poc.1 <- get_model(pli.demogm.poc.step)
summary(pli.demogm.poc.1)

#education, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(`Education_grouped`)

pli.demogm.ed <- lmer(data = iw.demo.gm,
                      log(pli) ~  Education_grouped + 
                        (1|site),
                      REML = T)
print(summary(pli.demogm.ed))
check_model(pli.demogm.ed)
performance(pli.demogm.ed)
pli.demogm.ed.step <- step(pli.demogm.ed)
pli.demogm.ed.step
pli.demogm.ed.1 <- get_model(pli.demogm.ed.step)
summary(pli.demogm.ed.1)

#household size, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(`household_size`)

pli.demogm.house <- lmer(data = iw.demo.gm,
                         log(pli) ~  `household_size` + 
                           (1|site),
                         REML = T)
print(summary(pli.demogm.house))
check_model(pli.demogm.house)
performance(pli.demogm.house)
pli.demogm.house.step <- step(pli.demogm.house)
pli.demogm.house.step
pli.demogm.house.1 <- get_model(pli.demogm.house.step)
summary(pli.demogm.house.1)

#income, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(`Low Income`)
iw.demo.gm$low <- iw.demo.gm$`Low Income`

pli.demogm.inc <- lmer(data = iw.demo.gm,
                       log(pli) ~  low + 
                         (1|site),
                       REML = T)
print(summary(pli.demogm.inc))
check_model(pli.demogm.inc)
performance(pli.demogm.inc)
pli.demogm.inc.step <- step(pli.demogm.inc)
pli.demogm.inc.step
pli.demogm.inc.1 <- get_model(pli.demogm.inc.step)
summary(pli.demogm.inc.1)

#zipcode, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(`Zip`)

pli.demogm.zip <- lmer(data = iw.demo.gm,
                       log(pli) ~  `Zip` + 
                         (1|site),
                       REML = T)
print(summary(pli.demogm.zip))
check_model(pli.demogm.zip)
performance(pli.demogm.zip)
pli.demogm.zip.step <- step(pli.demogm.zip)
pli.demogm.zip.step
pli.demogm.zip.1 <- get_model(pli.demogm.zip.step)
summary(pli.demogm.zip.1)

#race, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(`Race Ethnicity`)
iw.demo.gm$race <- iw.demo.gm$`Race Ethnicity`

pli.demogm.race <- lmer(data = iw.demo.gm,
                        log(pli) ~  race + 
                          (1|site),
                        REML = T)
print(summary(pli.demogm.race))
check_model(pli.demogm.race)
performance(pli.demogm.race)
pli.demogm.race.step <- step(pli.demogm.race)
pli.demogm.race.step
pli.demogm.race.1 <- get_model(pli.demogm.race.step)
summary(pli.demogm.race.1)

###univariate plus env ----
#bipoc, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`BIPOC`)
pli.demogm.poc <- lmer(data = iw.demo.gm,
                       log(pli) ~ season + BIPOC + prox.normal+pH+
                         (1|site),
                       REML = T)
print(summary(pli.demogm.poc))
check_model(pli.demogm.poc)
performance(pli.demogm.poc)
pli.demogm.poc.step <- step(pli.demogm.poc)
pli.demogm.poc.step
pli.demogm.poc.1 <- get_model(pli.demogm.poc.step)
summary(pli.demogm.poc.1)

#education, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Education_grouped`)

pli.demogm.ed <- lmer(data = iw.demo.gm,
                       log(pli) ~ season + Education_grouped + prox.normal+pH+
                         (1|site),
                       REML = T)
print(summary(pli.demogm.ed))
check_model(pli.demogm.ed)
performance(pli.demogm.ed)
pli.demogm.ed.step <- step(pli.demogm.ed)
pli.demogm.ed.step
pli.demogm.ed.1 <- get_model(pli.demogm.ed.step)
summary(pli.demogm.ed.1)

#household size, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`household_size`)

pli.demogm.house <- lmer(data = iw.demo.gm,
                      log(pli) ~ season + `household_size` + prox.normal+pH+
                        (1|site),
                      REML = T)
print(summary(pli.demogm.house))
check_model(pli.demogm.house)
performance(pli.demogm.house)
pli.demogm.house.step <- step(pli.demogm.house)
pli.demogm.house.step
pli.demogm.house.1 <- get_model(pli.demogm.house.step)
summary(pli.demogm.house.1)

#income, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Low Income`)

pli.demogm.inc <- lmer(data = iw.demo.gm,
                         log(pli) ~ season + `Low Income` + prox.normal+pH+
                           (1|site),
                         REML = T)
print(summary(pli.demogm.inc))
check_model(pli.demogm.inc)
performance(pli.demogm.inc)
pli.demogm.inc.step <- step(pli.demogm.inc)
pli.demogm.inc.step
pli.demogm.inc.1 <- get_model(pli.demogm.inc.step)
summary(pli.demogm.inc.1)

#zipcode, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Zip`)

pli.demogm.zip <- lmer(data = iw.demo.gm,
                       log(pli) ~ season + `Zip` + prox.normal+pH+
                         (1|site),
                       REML = T)
print(summary(pli.demogm.zip))
check_model(pli.demogm.zip)
performance(pli.demogm.zip)
pli.demogm.zip.step <- step(pli.demogm.zip)
pli.demogm.zip.step
pli.demogm.zip.1 <- get_model(pli.demogm.zip.step)
summary(pli.demogm.zip.1)

#race, not signif
iw.demo.gm <- iws.gm %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Race Ethnicity`)

pli.demogm.race <- lmer(data = iw.demo.gm,
                       log(pli) ~ season + `Race Ethnicity` + prox.normal+pH+
                         (1|site),
                       REML = T)
print(summary(pli.demogm.race))
check_model(pli.demogm.race)
performance(pli.demogm.race)
pli.demogm.race.step <- step(pli.demogm.race)
pli.demogm.race.step
pli.demogm.race.1 <- get_model(pli.demogm.race.step)
summary(pli.demogm.race.1)


##hayden/winkelman ----
#education, household size, income, zip?
###only demos----
####zip signif ----
####winkelman has lower concentrations than hayden, even more signif than proximity when they were modeled together - surprisingly, not much covariance between the two variables
iw.demo.hw <- iws.hw %>%
  drop_na(`Education_grouped`)%>%
  drop_na(`Zip`)%>%
  drop_na(`Low Income`)%>%
  drop_na(`household_size`)
#this reduces sample size from 73 to 56
iw.demo.hw <- iws.hw %>%
  drop_na(`Zip`)%>%
  drop_na(`Low Income`)%>%
  drop_na(`household_size`)
#this reduces sample size from 42 to 68

pli.demohw.dems <- lmer(data = iw.demo.hw,
                        log(pli) ~ Zip + income + `household_size`+
                          (1|site),
                        REML = T)
print(summary(pli.demohw.dems))
check_model(pli.demohw.dems)
performance(pli.demohw.dems)
pli.demohw.dems.step <- step(pli.demohw.dems)
pli.demohw.dems.step
pli.demohw.dems.1 <- get_model(pli.demohw.dems.step)
summary(pli.demohw.dems.1)
plot(allEffects(pli.demohw.dems.1))
check_model(pli.demohw.dems.1)
anova(pli.demohw.dems.1)
perf <- performance(pli.demohw.dems.1)
perf
write.csv(perf, "plidemos_hw_diag.csv")

###univariate ----
####education
iw.demo.hw <- iws.hw %>%
  drop_na(`Education_grouped`)

pli.demohw.ed <- lmer(data = iw.demo.hw,
                      log(pli) ~ Education_grouped+
                        (1|site),
                      REML = T)
print(summary(pli.demohw.ed))
check_model(pli.demohw.ed)
performance(pli.demohw.ed)
pli.demohw.ed.step <- step(pli.demohw.ed)
pli.demohw.ed.step
pli.demohw.ed.1 <- get_model(pli.demohw.ed.step)
summary(pli.demohw.ed.1)
anova(pli.demohw.ed.1)


####household size ----
iw.demo.hw <- iws.hw %>%
  drop_na(`household_size`)

pli.demohw.house <- lmer(data = iw.demo.hw,
                         log(pli) ~ `household_size` + 
                           (1|site),
                         REML = T)
print(summary(pli.demohw.house))
check_model(pli.demohw.house)
performance(pli.demohw.house)
pli.demohw.house.step <- step(pli.demohw.house)
pli.demohw.house.step
pli.demohw.house.1 <- get_model(pli.demohw.house.step)
summary(pli.demohw.house.1)
plot(allEffects(pli.demohw.house.1))
perf <- performance(pli.demohw.house.1)
perf
write.csv(perf, "pli_demohw_house_diag.csv")
modelsum <- summary(pli.demohw.house.1)
write.csv(modelsum$coefficients, "pli_demohw_house_sum.csv")
plot(allEffects(pli.demohw.house.1))

#income, not signif
iw.demo.hw <- iws.hw %>%
  drop_na(`Low Income`)
iw.demo.hw$low <- iw.demo.hw$`Low Income`

pli.demohw.inc <- lmer(data = iw.demo.hw,
                       log(pli) ~ low + 
                         (1|site),
                       REML = T)
print(summary(pli.demohw.inc))
check_model(pli.demohw.inc)
performance(pli.demohw.inc)
pli.demohw.inc.step <- step(pli.demohw.inc)
pli.demohw.inc.step
pli.demohw.inc.1 <- get_model(pli.demohw.inc.step)
summary(pli.demohw.inc.1)

#zipcode, not signif
iw.demo.hw <- iws.hw %>%
  drop_na(`Zip`)

pli.demohw.zip <- lmer(data = iw.demo.hw,
                       log(pli) ~ `Zip`+
                         (1|site),
                       REML = T)
print(summary(pli.demohw.zip))
check_model(pli.demohw.zip)
performance(pli.demohw.zip)
pli.demohw.zip.step <- step(pli.demohw.zip)
pli.demohw.zip.step
pli.demohw.zip.1 <- get_model(pli.demohw.zip.step)
summary(pli.demohw.zip.1)
plot(allEffects(pli.demohw.zip.1))
perf <- performance(pli.demohw.zip.1)
perf
write.csv(perf, "pli_demohw_zip_diag.csv")
modelsum <- summary(pli.demohw.zip.1)
write.csv(modelsum$coefficients, "pli_demohw_zip_sum.csv")
plot(allEffects(pli.demohw.zip.1))


###univariate plus env ----
####education, unclear... ----
iw.demo.hw <- iws.hw %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Education_grouped`)

pli.demohw.ed <- lmer(data = iw.demo.hw,
                      log(pli) ~ season + Education_grouped*prox.normal+pH+
                        (1|site),
                      REML = T)
print(summary(pli.demohw.ed))
check_model(pli.demohw.ed)
performance(pli.demohw.ed)
pli.demohw.ed.step <- step(pli.demohw.ed)
pli.demohw.ed.step
pli.demohw.ed.1 <- get_model(pli.demohw.ed.step)
summary(pli.demohw.ed.1)
anova(pli.demohw.ed.1)
check_model(pli.demohw.ed.1)
pli.demohw.ed.2 <- lm(data = iw.demo.hw,
                      log(pli) ~ season  + prox.normal)
anova(pli.demohw.ed.1, pli.demohw.ed.2)
compare_performance(pli.demohw.ed.1, pli.demohw.ed.2)
plot(allEffects(pli.demohw.ed.1))

#household size, almost signif
iw.demo.hw <- iws.hw %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`household_size`)

pli.demohw.house <- lmer(data = iw.demo.hw,
                         log(pli) ~ season + `household_size` + prox.normal+pH+
                           (1|site),
                         REML = T)
print(summary(pli.demohw.house))
check_model(pli.demohw.house)
performance(pli.demohw.house)
pli.demohw.house.step <- step(pli.demohw.house)
pli.demohw.house.step
pli.demohw.house.1 <- get_model(pli.demohw.house.step)
summary(pli.demohw.house.1)


#income, not signif
iw.demo.hw <- iws.hw %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Low Income`)

pli.demohw.inc <- lmer(data = iw.demo.hw,
                       log(pli) ~ season + `Low Income` + prox.normal+pH+
                         (1|site),
                       REML = T)
print(summary(pli.demohw.inc))
check_model(pli.demohw.inc)
performance(pli.demohw.inc)
pli.demohw.inc.step <- step(pli.demohw.inc)
pli.demohw.inc.step
pli.demohw.inc.1 <- get_model(pli.demohw.inc.step)
summary(pli.demohw.inc.1)

#zipcode, not signif
iw.demo.hw <- iws.hw %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Zip`)

pli.demohw.zip <- lmer(data = iw.demo.hw,
                       log(pli) ~ season + `Zip`*prox.normal+pH+
                         (1|site),
                       REML = T)
print(summary(pli.demohw.zip))
check_model(pli.demohw.zip)
performance(pli.demohw.zip)
pli.demohw.zip.step <- step(pli.demohw.zip)
pli.demohw.zip.step
pli.demohw.zip.1 <- get_model(pli.demohw.zip.step)
summary(pli.demohw.zip.1)
plot(allEffects(pli.demohw.zip.1))

##tucson english/spanish ----
#bipoc, education, household size, income, language, zip
###only demos----
####nothing signif ----
iw.demo.tu <- iws.tu %>%
  drop_na(BIPOC)%>%
  drop_na(`Education_grouped`)%>%
  drop_na(`Primary Language`)%>%
  drop_na(`Zip`)%>%
  drop_na(`Low Income`)%>%
  drop_na(`household_size`)
#this reduces sample size from 173 to 139
iw.demo.tu <- iws.tu %>%
  drop_na(BIPOC)%>%
  drop_na(`Primary Language`)%>%
  drop_na(`Zip`)%>%
  drop_na(`Education_grouped`)%>%
  drop_na(`household_size`)
#this reduces sample size from 173 to 152

pli.demotu.dems <- lmer(data = iw.demo.tu,
                        log(pli) ~ BIPOC + Zip + `Education_grouped`+`Primary Language`+`Low Income`+`household_size`+community_2+
                          (1|site),
                        REML = T)
print(summary(pli.demotu.dems))
check_model(pli.demotu.dems)
vif(pli.demotu.dems) #zip collinear to oncome level and household size and primary language?
performance(pli.demotu.dems)
pli.demotu.dems.step <- step(pli.demotu.dems)
pli.demotu.dems.step
pli.demotu.dems.1 <- get_model(pli.demotu.dems.step)
summary(pli.demotu.dems.1)
plot(allEffects(pli.demotu.dems.1))
vif(pli.demotu.dems.1)

pli.demotu.dems.2 <- lmer(data = iw.demo.tu,
                        log(pli) ~  `Education_grouped`+`Low Income`+`household_size`+
                          (1|site),
                        REML = T)
summary(pli.demotu.dems.2)
plot(allEffects(pli.demotu.dems.2))
vif(pli.demotu.dems.2)
check_model(pli.demotu.dems.2)
anova(pli.demotu.dems.2)


###univariate ----
#community, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(community_2)
pli.demotu.com <- lmer(data = iw.demo.tu,
                       log(pli) ~ community_2 + 
                         (1|site),
                       REML = T)
print(summary(pli.demotu.com))
check_model(pli.demotu.com)
performance(pli.demotu.com)
pli.demotu.com.step <- step(pli.demotu.com)
pli.demotu.com.step
pli.demotu.com.1 <- get_model(pli.demotu.com.step)
summary(pli.demotu.com.1)

#bipoc, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`BIPOC`)
pli.demotu.poc <- lmer(data = iw.demo.tu,
                       log(pli) ~ BIPOC + 
                         (1|site),
                       REML = T)
print(summary(pli.demotu.poc))
check_model(pli.demotu.poc)
performance(pli.demotu.poc)
pli.demotu.poc.step <- step(pli.demotu.poc)
pli.demotu.poc.step
pli.demotu.poc.1 <- get_model(pli.demotu.poc.step)
summary(pli.demotu.poc.1)

#education, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`Education_grouped`)

pli.demotu.ed <- lmer(data = iw.demo.tu,
                      log(pli) ~ Education_grouped + 
                        (1|site),
                      REML = T)
print(summary(pli.demotu.ed))
check_model(pli.demotu.ed) #ward and education have high VIF
performance(pli.demotu.ed)
pli.demotu.ed.step <- step(pli.demotu.ed)
pli.demotu.ed.step
pli.demotu.ed.1 <- get_model(pli.demotu.ed.step)
summary(pli.demotu.ed.1)

#household size, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`household_size`)

pli.demotu.house <- lmer(data = iw.demo.tu,
                         log(pli) ~ `household_size` + 
                           (1|site),
                         REML = T)
print(summary(pli.demotu.house))
check_model(pli.demotu.house)
performance(pli.demotu.house)
pli.demotu.house.step <- step(pli.demotu.house)
pli.demotu.house.step
pli.demotu.house.1 <- get_model(pli.demotu.house.step)
summary(pli.demotu.house.1)

#income, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`Low Income`)
iw.demo.tu$low <- iw.demo.tu$`Low Income`

pli.demotu.inc <- lmer(data = iw.demo.tu,
                       log(pli) ~ `Low Income` + 
                         (1|site),
                       REML = T)
print(summary(pli.demotu.inc))
check_model(pli.demotu.inc)
performance(pli.demotu.inc)
pli.demotu.inc.step <- step(pli.demotu.inc)
pli.demotu.inc.step
pli.demotu.inc.1 <- get_model(pli.demotu.inc.step)
summary(pli.demotu.inc.1)

#zipcode, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`Zip`)

pli.demotu.zip <- lmer(data = iw.demo.tu,
                       log(pli) ~ `Zip` + 
                         (1|site),
                       REML = T)
print(summary(pli.demotu.zip))
check_model(pli.demotu.zip)
performance(pli.demotu.zip)
pli.demotu.zip.step <- step(pli.demotu.zip)
pli.demotu.zip.step
pli.demotu.zip.1 <- get_model(pli.demotu.zip.step)
summary(pli.demotu.zip.1)

#race, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`Race Ethnicity`)
iw.demo.tu$race <- iw.demo.tu$`Race Ethnicity`

pli.demotu.race <- lmer(data = iw.demo.tu,
                        log(pli) ~ race + 
                          (1|site),
                        REML = T)
print(summary(pli.demotu.race))
check_model(pli.demotu.race)
performance(pli.demotu.race)
pli.demotu.race.step <- step(pli.demotu.race)
pli.demotu.race.step
pli.demotu.race.1 <- get_model(pli.demotu.race.step)
summary(pli.demotu.race.1)

#language, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(`Primary Language`)
iw.demo.tu$lang <- iw.demo.tu$`Primary Language`

pli.demotu.lang <- lmer(data = iw.demo.tu,
                        log(pli) ~ lang + 
                          (1|site),
                        REML = T)
print(summary(pli.demotu.lang))
check_model(pli.demotu.lang)
performance(pli.demotu.lang)
pli.demotu.lang.step <- step(pli.demotu.lang)
pli.demotu.lang.step
pli.demotu.lang.1 <- get_model(pli.demotu.lang.step)
summary(pli.demotu.lang.1)

###univariate plus env ----
#bipoc, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`BIPOC`)
pli.demotu.poc <- lmer(data = iw.demo.tu,
                       log(pli) ~ season + BIPOC + prox.normal+pH+ ward+
                         (1|site),
                       REML = T)
print(summary(pli.demotu.poc))
check_model(pli.demotu.poc)
performance(pli.demotu.poc)
pli.demotu.poc.step <- step(pli.demotu.poc)
pli.demotu.poc.step
pli.demotu.poc.1 <- get_model(pli.demotu.poc.step)
summary(pli.demotu.poc.1)

#education, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Education_grouped`)

pli.demotu.ed <- lmer(data = iw.demo.tu,
                      log(pli) ~ season + Education_grouped + prox.normal+pH+ward+
                        (1|site),
                      REML = T)
print(summary(pli.demotu.ed))
check_model(pli.demotu.ed) #ward and education have high VIF
performance(pli.demotu.ed)
pli.demotu.ed.step <- step(pli.demotu.ed)
pli.demotu.ed.step
pli.demotu.ed.1 <- get_model(pli.demotu.ed.step)
summary(pli.demotu.ed.1)

#household size, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`household_size`)

pli.demotu.house <- lmer(data = iw.demo.tu,
                         log(pli) ~ season + `household_size` + prox.normal+pH+ward+
                           (1|site),
                         REML = T)
print(summary(pli.demotu.house))
check_model(pli.demotu.house)
performance(pli.demotu.house)
pli.demotu.house.step <- step(pli.demotu.house)
pli.demotu.house.step
pli.demotu.house.1 <- get_model(pli.demotu.house.step)
summary(pli.demotu.house.1)

#income, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Low Income`)

pli.demotu.inc <- lmer(data = iw.demo.tu,
                       log(pli) ~ season + `Low Income` + prox.normal+pH+ward+
                         (1|site),
                       REML = T)
print(summary(pli.demotu.inc))
check_model(pli.demotu.inc)
performance(pli.demotu.inc)
pli.demotu.inc.step <- step(pli.demotu.inc)
pli.demotu.inc.step
pli.demotu.inc.1 <- get_model(pli.demotu.inc.step)
summary(pli.demotu.inc.1)

#zipcode, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Zip`)

pli.demotu.zip <- lmer(data = iw.demo.tu,
                       log(pli) ~ season + `Zip` + prox.normal+pH+ward+
                         (1|site),
                       REML = T)
print(summary(pli.demotu.zip))
check_model(pli.demotu.zip)
performance(pli.demotu.zip)
pli.demotu.zip.step <- step(pli.demotu.zip)
pli.demotu.zip.step
pli.demotu.zip.1 <- get_model(pli.demotu.zip.step)
summary(pli.demotu.zip.1)

#race, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Race Ethnicity`)

pli.demotu.race <- lmer(data = iw.demo.tu,
                        log(pli) ~ season + `Race Ethnicity` + prox.normal+pH+ward+
                          (1|site),
                        REML = T)
print(summary(pli.demotu.race))
check_model(pli.demotu.race)
performance(pli.demotu.race)
pli.demotu.race.step <- step(pli.demotu.race)
pli.demotu.race.step
pli.demotu.race.1 <- get_model(pli.demotu.race.step)
summary(pli.demotu.race.1)

#language, not signif
iw.demo.tu <- iws.tu %>%
  drop_na(prox.normal)%>%
  drop_na(pH)%>%
  drop_na(`Primary Language`)

pli.demotu.lang <- lmer(data = iw.demo.tu,
                        log(pli) ~ season + `Primary Language` + prox.normal+pH+ward+
                          (1|site),
                        REML = T)
print(summary(pli.demotu.lang))
check_model(pli.demotu.lang)
performance(pli.demotu.lang)
pli.demotu.lang.step <- step(pli.demotu.lang)
pli.demotu.lang.step
pli.demotu.lang.1 <- get_model(pli.demotu.lang.step)
summary(pli.demotu.lang.1)

#Functions----
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
    drop_na(all_of(cols))%>%
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

facsumFX <- function(datalongDF, subset.vector.string, dfname.string, filename.string){
  
  #load libraries
  library(tidyverse)
  library(EnvStats)
  
  #assign data
  dat.long <- datalongDF
  cols <- subset.vector.string
  dfname <- dfname.string
  filename <- filename.string
  
  #calculate summary stats
  demon <- demo.long %>%
    mutate(value = factor(value)) %>%
    mutate(demographic = factor(demographic))%>%
    group_by(across(all_of(cols))) %>%
    summarise(n=n())
  
  demoN <- demon%>%
    group_by(demographic)%>%
    summarize(N=sum(n))
  
  sumtable <- full_join(demon, demoN, by = "demographic")
  sumtable$perc <- paste(round(sumtable$n/sumtable$N*100, 1), "%", sep="")
  
  #save as csv file in your working directory
  write.csv(sumtable, paste(filename,"_sum.csv", sep = ""))
  
  #copy to new dataframe with a unique name and place in global environment
  assign(paste(dfname), sumtable, envir=.GlobalEnv)
  
}

xtest <- function(dataDF, var1.string, var2.string, filename.string){
  library(corrplot)
  library(tidyverse)
  
  dat2 <- dataDF
  var1 <- var1.string
  var2 <- var2.string
  filename <- filename.string
  
  dat <- dat2 %>%
    drop_na(all_of(var1)) %>%
    drop_na(all_of(var2))
    
  X2 <- chisq.test(table(pull(dat,var1), pull(dat,var2)))
  print(X2)
  X2$residuals
  p1 <- corrplot(X2$residuals, is.cor = FALSE)
  print(p1)
  dev.print(jpeg, paste(filename, ".jpg", sep = ""), res=300, height=7, width=7, units="in")
  
  # contrib <- 100*X2$residuals^2/X2$statistic
  # corrplot(contrib, is.cor = FALSE)
  # dev.print(jpeg, paste(filename, ".jpg", sep = ""), res=300, height=7, width=7, units="in")
  # 
  
}


#scratch ----
# 
# demon <- demo.long %>%
#   mutate(value = factor(value)) %>%
#   mutate(demographic = factor(demographic))%>%
#   group_by(demographic, value) %>%
#   summarise(n=n())
# 
# demoN <- demon%>%
#   group_by(demographic)%>%
#   summarize(N=sum(n))
# 
# democount <- full_join(demon, demoN, by = "demographic")
# democount$perc <- paste(round(democount$n/democount$N*100, 1), "%", sep="")
