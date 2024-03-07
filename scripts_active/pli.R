#Kunal Palawat
#Description: code to analyze PLI by community and with hds questions

#load libraries ----
#base
library(readxl) #read excel files
library(tidyverse)
library(ggplot2)

#models
library(EnvStats)
library(car)
library(MASS)
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


#data wrangling ----
##hds wrangling ----
#each time you run the models, you need to change out the hds question you're focused on below. And apply the correct factor ordering
iws <- iw.hds%>%
  drop_na(Q65)%>%
  mutate(Q9 = factor(Q9))%>%
  mutate(Q18 = factor(Q18))%>%
  mutate(Q44 = factor(Q44))%>%
  mutate(Q67 = factor(Q67))%>%
  mutate(Q68 = factor(Q68))%>%
  mutate(Q60 = factor(Q60))%>%
  mutate(Q65 = factor(Q65))%>%
  mutate(Q76 = factor(Q76))%>%
  mutate(Q77 = factor(Q77))%>%
  mutate(Q78 = factor(Q78))%>%
  mutate(Q1 = factor(Q1))

summary(iws$Q1)

#iws$Q9 <- factor(iws$Q9, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))

#iws$Q65 <- factor(iws$Q65, levels = c("<6 months", "6 months-1 year", "1-2 years", "2-3 years", "3-4 years", "5+ years"))

# #for Q1 only
# #overall
# summary(as.factor(iws$Q1))
# #removing the following due to low SAMPLE size (1, 2, 3, 4): Clay/Concrete Tile; Flat BUR (Tar/Gravel), Metal Panel
# #may need to remove roofs with only 5 SAMPLES: Clay/Concrete Tile, Flat BUR (Reflective)
# iws <- iws%>%
#   filter(Q1 != "Clay/Concrete Tile")%>%
#   filter(Q1 != "Flat BUR (Tar/Gravel), Metal Panel")
#   

##general wrangling----
#iws <- iw.dm #use this line if NOT doing hds modeling and ignore the data section above
iws.c <- iws %>%
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

#add sub location to hayden data
hay <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "hayden", col_names = TRUE)
iws.hw <- full_join(iws.hw, hay, by = c("site"))
iws.hw <- iws.hw %>%
  drop_na(community) %>%
  drop_na(location)
iws.hw$location <- factor(iws.hw$location, levels = c("Hayden", "Winkelman"))



# #for Q1 only
# #dh
# summary(as.factor(iws.dh$Q1)) #not enough samples to compare
# #gm
# summary(as.factor(iws.gm$Q1)) #compare, remove Flat BUR (Tar/Gravel), Flat BUR (Reflective) - n=5 AND because all those samples are coming from one site
# iws.gm <- iws.gm%>%
#   filter(Q1!="Flat BUR (Tar/Gravel), Flat BUR (Reflective)")
# 
# #hw
# summary(as.factor(iws.hw$Q1)) #compare, potentially remove wood shakes/shingles - n=6
# #tu
# summary(as.factor(iws.tu$Q1)) #compare, remove Flat BUR (Tar/Gravel), Flat BUR (Reflective) - n=1; remove Clay/Concrete Tile, Flat BUR (Reflective) - n=5 AND all 4 from 1 site; Metal Panel  - n=5 AND all from one site; Rubber Membrane - n=5 AND all from one site
# iws.tu <- iws.tu%>%
#   filter(Q1!="Flat BUR (Tar/Gravel), Flat BUR (Reflective)")%>%
#   filter(Q1!="Clay/Concrete Tile, Flat BUR (Reflective)")%>%
#   filter(Q1!="Metal Panel")%>%
#   filter(Q1!="Rubber Membrane")
#   

#for Q60 only
#iws.tu <- iws.tu[iws.tu$Q60 != "Concrete"&iws.tu$Q60 != "Fiberglass",]

#for Q78b only
# iws.tu <- iws.tu[iws.tu$Q78b!="Metal, Plastic"&iws.tu$Q78b!="Rocks"&iws.tu$Q78b!=
#                    "Cotton/Cloth",]
# summary(as.factor(iws.tu$Q78b))


#basic viz ----
ggplot(iw.dm, mapping = aes(y = log(pli), x = community, fill = community)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black", alpha=0.4) +
  geom_hline(yintercept = 0, color = "grey")+
  #scale_fill_manual(values = wes_palette(name = "Darjeeling2", n = 5)) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #scale_fill_viridis_d() +
  facet_grid(.~season, scales = "free") +
  labs(x = "",
       y = "ln(pli)\n",
       title = "Violin plots of pollution load index by community",
       fill = "Community")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="right",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "pli_comssn.png", res=300, height=8, width=10, units="in")

#modeling ----
##overall pli ----
###all communities ----
pli.0 <- lmer(data = iw.dm,
                log(pli) ~
                  (1|site),
                REML = F)
print(summary(pli.tu.0))

pli.1 <- lmer(data = iw.dm,
                log(pli) ~ season + prox.normal + pH + community+community:prox.normal+
                  (1|site),
                REML = F)
print(summary(pli.1))
vif(pli.1)
pli.2.step <- step(pli.1)
pli.2.step
pli.2 <- get_model(pli.2.step)
print(summary(pli.2))
check_model(pli.2)
anova(pli.1)
print(anova(pli.2))
plot(allEffects(pli.2))
#season, pH, proximity:community with DH having a negative proximity slope

###dewey-humboldt ----
pli.dh.0 <- lmer(data = iws.dh,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.dh.0))

pli.dh.1 <- lmer(data = iws.dh,
                 log(pli) ~ season + pH + prox.normal+
                   (1|site),
                 REML = F)
print(summary(pli.dh.1))
vif(pli.dh.1)
pli.dh.2.step <- step(pli.dh.1)
pli.dh.2.step
pli.dh.2 <- get_model(pli.dh.2.step)
print(summary(pli.dh.2))
check_model(pli.dh.2) #poor model assumptions
anova(pli.dh.1)
print(anova(pli.dh.2))
plot(allEffects(pli.dh.2))
pli.dh.sum <- summary(pli.dh.2)
write.csv(pli.dh.sum$coefficients, "pli_dh_coefs.csv")
perf <- performance(pli.dh.2)
perf
write.csv(perf, "pli_dh_diag.csv")
#season

###globe/miami ----
pli.gm.0 <- lmer(data = iws.gm,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.gm.0))

pli.gm.1 <- lmer(data = iws.gm,
                 log(pli) ~ season + prox.normal + location_2 + pH+
                   (1|site),
                 REML = T)
print(summary(pli.gm.1))
vif(pli.gm.1)
anova(pli.gm.1)
pli.gm.2.step <- step(pli.gm.1)
pli.gm.2.step
pli.gm.2 <- get_model(pli.gm.2.step)
print(summary(pli.gm.2))
check_model(pli.gm.2)
print(anova(pli.gm.2))
plot(allEffects(pli.gm.2)) #prox beats out location, they are correlated
pli.gm.sum <- summary(pli.gm.2)
write.csv(pli.gm.sum$coefficients, "pli_gm_coefs.csv")
perf <- performance(pli.gm.2)
perf
write.csv(perf, "pli_gm_diag.csv")
#season, pH

###hayden/winkelman ----
pli.hw.0 <- lmer(data = iws.hw,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.hw.0))

pli.hw.1 <- lmer(data = iws.hw,
                 log(pli) ~ season + prox.normal + location + pH+ prox.normal:location+
                   (1|site),
                 REML = F)
print(summary(pli.hw.1))
vif(pli.hw.1)
anova(pli.hw.1)
pli.hw.2.step <- step(pli.hw.1)
pli.hw.2.step
pli.hw.2 <- get_model(pli.hw.2.step)
print(summary(pli.hw.2))#prox beats out location
check_model(pli.hw.2)
print(anova(pli.hw.2))
plot(allEffects(pli.hw.2)) #prox beats out location, they are correlated
pli.hw.sum <- summary(pli.hw.2)
write.csv(pli.hw.sum$coefficients, "pli_hw_coefs.csv")
perf <- performance(pli.hw.2)
perf
write.csv(perf, "pli_hw_diag.csv")
#season, proximity

###tucson ----
pli.tu.0 <- lmer(data = iws.tu,
                log(pli) ~
                  (1|site),
                REML = F)
print(summary(pli.tu.0))

pli.tu.1 <- lmer(data = iws.tu,
                log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+
                  (1|site),
                REML = T)
print(summary(pli.tu.1))
vif(pli.tu.1)
pli.tu.2.step <- step(pli.tu.1)
pli.tu.2.step
pli.tu.2 <- get_model(pli.tu.2.step)
print(summary(pli.tu.2))
check_model(pli.tu.2)
anova(pli.tu.1)
print(anova(pli.tu.2))
plot(allEffects(pli.tu.2))

pli.tu.sum <- summary(pli.tu.2)
write.csv(pli.tu.sum$coefficients, "pli_tu_coefs.csv")
perf <- performance(pli.tu.2)
perf
write.csv(perf, "pli_tu_diag.csv")
#season, pH



##hds Q9 home age----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q9))+
  geom_histogram()+
  facet_grid(community~.)
#might be some correlation between proximity and home age in Globe and in Tucson

###all communities ----
pli.q9.0 <- lmer(data = iws,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q9.0))

pli.q9 <- lmer(data = iws,
                 log(pli) ~ Q9+
                   (1|site),
                 REML = F)
print(summary(pli.q9))
anova(pli.q9)
check_model(pli.q9)
plot(allEffects(pli.q9))

#univariate, q9 significant with a negative trend

pli.q9.1 <- lmer(data = iws,
              log(pli) ~ season + prox.normal + pH + community+community:prox.normal+Q9+
                (1|site),
              REML = F)
print(summary(pli.q9.1))
vif(pli.q9.1)
pli.q9.2.step <- step(pli.q9.1)
pli.q9.2.step
pli.q9.2 <- get_model(pli.q9.2.step)
print(summary(pli.q9.2))
check_model(pli.q9.2)
pli.q9.3 <- lmer(data = iws,
                 log(pli) ~ season + prox.normal + pH + community+Q9+
                   (1|site),
                 REML = T)
pli.q9.3.step <- step(pli.q9.3)
pli.q9.3.step
pli.q9.4 <- lmer(data = iws,
                 log(pli) ~ season + prox.normal + pH + community+
                   (1|site),
                 REML = F)
anova(pli.q9.3, pli.q9.4)
check_model(pli.q9.3)
print(anova(pli.q9.3))
plot(allEffects(pli.q9.3))
pli.q9.sum <- summary(pli.q9.3)
pli.q9.sum
write.csv(pli.q9.sum$coefficients, "pli_q9_coefs.csv")
perf <- performance(pli.q9.3)
perf
write.csv(perf, "pli_q9_diag.csv")
#home age significant with a generally negative trend
model.effects <- ggeffect(model = pli.q9.3,
                          type = "re",
                          terms = c("Q9"))
model.effects$x <- as.character(model.effects$x)
model.effects$x  <- factor(model.effects$x , levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))
ggplot(model.effects, aes(x = x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high)))+
  geom_pointrange()+
  labs(title = "When was your home built?",
       subtitle = "All Communities",
       x = "\nInfrastructure Practice",
       y = "PLI\n")+
  coord_cartesian(ylim = c(0,4))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust =1))
dev.print(png, "pli_q9_effect.png", res=300, height=6, width=10, units="in")

###dewey-humboldt ----
pli.q9.dh.0 <- lmer(data = iws.dh,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q9.dh.0))

pli.q9.dh <- lmer(data = iws.dh,
                    log(pli) ~ Q9+
                      (1|site),
                    REML = F)
print(summary(pli.q9.dh))
anova(pli.q9.dh)
check_model(pli.q9.dh)
plot(allEffects(pli.q9.dh))
#q9 not signif

pli.q9.dh.1 <- lmer(data = iws.dh,
                 log(pli) ~ season + pH + prox.normal+Q9+
                   (1|site),
                 REML = F)
print(summary(pli.q9.dh.1))
vif(pli.q9.dh.1)
pli.q9.dh.2.step <- step(pli.q9.dh.1)
pli.q9.dh.2.step
pli.q9.dh.2 <- get_model(pli.q9.dh.2.step)
print(summary(pli.q9.dh.2))
check_model(pli.q9.dh.2) #poor model assumptions
anova(pli.q9.dh.1)
print(anova(pli.q9.dh.2))
plot(allEffects(pli.q9.dh.2))
pli.q9.dh.sum <- summary(pli.q9.dh.2)
pli.q9.dh.sum
# write.csv(pli.q9.dh.sum$coefficients, "pli_q9_dh_coefs.csv")
# perf <- performance(pli.q9.dh.2)
# perf
# write.csv(perf, "pli_q9_dh_diag.csv")
#q9 not signif

###globe/miami ----
pli.q9.gm.0 <- lmer(data = iws.gm,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q9.gm.0))

pli.q9.gm <- lmer(data = iws.gm,
                  log(pli) ~ Q9+
                    (1|site),
                  REML = F)
print(summary(pli.q9.gm))
anova(pli.q9.gm)
check_model(pli.q9.gm)
plot(allEffects(pli.q9.gm))
#q9 signif, generally a negative trend, decent assumptions

pli.q9.gm.1 <- lmer(data = iws.gm,
                 log(pli) ~ season + prox.normal + location_2 + pH+Q9+
                   (1|site),
                 REML = F)
print(summary(pli.q9.gm.1))
vif(pli.q9.gm.1)
anova(pli.q9.gm.1)
pli.q9.gm.2.step <- step(pli.q9.gm.1)
pli.q9.gm.2.step
pli.q9.gm.2 <- get_model(pli.q9.gm.2.step)
print(summary(pli.q9.gm.2))
check_model(pli.q9.gm.2)
print(anova(pli.q9.gm.2))
plot(allEffects(pli.q9.gm.2)) #
pli.q9.gm.sum <- summary(pli.q9.gm.2)
pli.q9.gm.sum
# write.csv(pli.q9.gm.sum$coefficients, "pli_q9_gm_coefs.csv")
# perf <- performance(pli.q9.gm.2)
# perf
# write.csv(perf, "pli_q9_gm_diag.csv")
#q9 not signif

###hayden/winkelman ----
pli.q9.hw.0 <- lmer(data = iws.hw,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q9.hw.0))

pli.q9.hw <- lmer(data = iws.hw,
                  log(pli) ~ Q9+
                    (1|site),
                  REML = F)
print(summary(pli.q9.hw))
anova(pli.q9.hw)
check_model(pli.q9.hw)
plot(allEffects(pli.q9.hw))
#q9 signif, generally a negative trend, decent assumptions

pli.q9.hw.1 <- lmer(data = iws.hw,
                    log(pli) ~ season + prox.normal + location + pH+prox.normal:location+Q9+
                      (1|site),
                    REML = F)
print(summary(pli.q9.hw.1))
vif(pli.q9.hw.1)
anova(pli.q9.hw.1)
pli.q9.hw.2.step <- step(pli.q9.hw.1)
pli.q9.hw.2.step
pli.q9.hw.2 <- get_model(pli.q9.hw.2.step)
print(summary(pli.q9.hw.2))
check_model(pli.q9.hw.2)
print(anova(pli.q9.hw.2))
plot(allEffects(pli.q9.hw.2)) #
pli.q9.hw.sum <- summary(pli.q9.hw.2)
pli.q9.hw.sum
# write.csv(pli.q9.hw.sum$coefficients, "pli_q9_hw_coefs.csv")
# perf <- performance(pli.q9.hw.2)
# perf
# write.csv(perf, "pli_q9_hw_diag.csv")
#q9 not signif

###tucson ----
pli.q9.tu.0 <- lmer(data = iws.tu,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q9.tu.0))

pli.q9.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q9+
                    (1|site),
                  REML = F)
print(summary(pli.q9.tu))
anova(pli.q9.tu)
check_model(pli.q9.tu)
plot(allEffects(pli.q9.tu))
#q9 signif, generally a negative trend, decent assumptions

pli.q9.tu.1 <- lmer(data = iws.tu,
                 log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q9+
                   (1|site),
                 REML = F)
print(summary(pli.q9.tu.1))
vif(pli.q9.tu.1)
pli.q9.tu.2.step <- step(pli.q9.tu.1)
pli.q9.tu.2.step
pli.q9.tu.2 <- get_model(pli.q9.tu.2.step)
print(summary(pli.q9.tu.2))
check_model(pli.q9.tu.2)
anova(pli.q9.tu.1)
print(anova(pli.q9.tu.2))
plot(allEffects(pli.q9.tu.2))
pli.q9.tu.sum <- summary(pli.q9.tu.2)
pli.q9.tu.sum
# write.csv(pli.q9.tu.sum$coefficients, "pli_q9_tu_coefs.csv")
# perf <- performance(pli.q9.tu.2)
# perf
# write.csv(perf, "pli_q9_tu_diag.csv")
#q9 not signif



##hds Q18 peeling paint ----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q18))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlation

###hayden/winkelman ----
pli.q18.hw.0 <- lmer(data = iws.hw,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q18.hw.0))

pli.q18.hw <- lmer(data = iws.hw,
                  log(pli) ~ Q18+
                    (1|site),
                  REML = F)
print(summary(pli.q18.hw))
anova(pli.q18.hw)
check_model(pli.q18.hw)
plot(allEffects(pli.q18.hw))
#q18 not signif

pli.q18.hw.1 <- lmer(data = iws.hw,
                    log(pli) ~ season + prox.normal + location + pH+prox.normal:location+Q18+
                      (1|site),
                    REML = F)
print(summary(pli.q18.hw.1))
vif(pli.q18.hw.1)
anova(pli.q18.hw.1)
pli.q18.hw.2.step <- step(pli.q18.hw.1)
pli.q18.hw.2.step
pli.q18.hw.2 <- get_model(pli.q18.hw.2.step)
print(summary(pli.q18.hw.2))
check_model(pli.q18.hw.2)
print(anova(pli.q18.hw.2))
plot(allEffects(pli.q18.hw.2)) #
pli.q18.hw.sum <- summary(pli.q18.hw.2)
pli.q18.hw.sum
# write.csv(pli.q18.hw.sum$coefficients, "pli_q18_hw_coefs.csv")
# perf <- performance(pli.q18.hw.2)
# perf
# write.csv(perf, "pli_q18_hw_diag.csv")
#q18 not signif

###tucson ----
pli.q18.tu.0 <- lmer(data = iws.tu,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q18.tu.0))

pli.q18.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q18+
                    (1|site),
                  REML = F)
print(summary(pli.q18.tu))
anova(pli.q18.tu)
check_model(pli.q18.tu)
plot(allEffects(pli.q18.tu))
#q18 not signif

pli.q18.tu.1 <- lmer(data = iws.tu,
                    log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q18+
                      (1|site),
                    REML = F)
print(summary(pli.q18.tu.1))
vif(pli.q18.tu.1)
pli.q18.tu.2.step <- step(pli.q18.tu.1)
pli.q18.tu.2.step
pli.q18.tu.2 <- get_model(pli.q18.tu.2.step)
print(summary(pli.q18.tu.2))
check_model(pli.q18.tu.2)
anova(pli.q18.tu.1)
print(anova(pli.q18.tu.2))
plot(allEffects(pli.q18.tu.2))
pli.q18.tu.sum <- summary(pli.q18.tu.2)
pli.q18.tu.sum
# write.csv(pli.q18.tu.sum$coefficients, "pli_q18_tu_coefs.csv")
# perf <- performance(pli.q18.tu.2)
# perf
# write.csv(perf, "pli_q18_tu_diag.csv")
#q18 not signif



##hds Q44 prox to road ----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q44))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlation

###all communities ----
pli.q44.0 <- lmer(data = iws[iws$community!="Dewey-Humboldt",],
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q44.0))

pli.q44 <- lmer(data = iws[iws$community!="Dewey-Humboldt",],
               log(pli) ~ Q44+
                 (1|site),
               REML = F)
print(summary(pli.q44))
anova(pli.q44)
check_model(pli.q44)
plot(allEffects(pli.q44))
#univariate, q44 not significant

pli.q44.1 <- lmer(data = iws[iws$community!="Dewey-Humboldt",],
                 log(pli) ~ season + prox.normal + pH + community+community:prox.normal+Q44+
                   (1|site),
                 REML = F)
print(summary(pli.q44.1))
vif(pli.q44.1)
pli.q44.2.step <- step(pli.q44.1)
pli.q44.2.step
pli.q44.2 <- get_model(pli.q44.2.step)
print(summary(pli.q44.2))
check_model(pli.q44.2)
anova(pli.q44.1)
print(anova(pli.q44.2))
plot(allEffects(pli.q44.2))
#q44 not signif


###globe/miami ----
pli.q44.gm.0 <- lmer(data = iws.gm,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q44.gm.0))

pli.q44.gm <- lmer(data = iws.gm,
                  log(pli) ~ Q44+
                    (1|site),
                  REML = F)
print(summary(pli.q44.gm))
anova(pli.q44.gm)
check_model(pli.q44.gm)
plot(allEffects(pli.q44.gm))
#q44 not signif

pli.q44.gm.1 <- lmer(data = iws.gm,
                    log(pli) ~ season + prox.normal + location_2 + pH+Q44+
                      (1|site),
                    REML = F)
print(summary(pli.q44.gm.1))
vif(pli.q44.gm.1)
anova(pli.q44.gm.1)
pli.q44.gm.2.step <- step(pli.q44.gm.1)
pli.q44.gm.2.step
pli.q44.gm.2 <- get_model(pli.q44.gm.2.step)
print(summary(pli.q44.gm.2))
check_model(pli.q44.gm.2)
print(anova(pli.q44.gm.2))
plot(allEffects(pli.q44.gm.2)) #
pli.q44.gm.sum <- summary(pli.q44.gm.2)
pli.q44.gm.sum
# write.csv(pli.q44.gm.sum$coefficients, "pli_q44_gm_coefs.csv")
# perf <- performance(pli.q44.gm.2)
# perf
# write.csv(perf, "pli_q44_gm_diag.csv")
#q44 not signif

###hayden/winkelman ----
pli.q44.hw.0 <- lmer(data = iws.hw,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q44.hw.0))

pli.q44.hw <- lmer(data = iws.hw,
                  log(pli) ~ Q44+
                    (1|site),
                  REML = F)
print(summary(pli.q44.hw))
anova(pli.q44.hw)
check_model(pli.q44.hw)
plot(allEffects(pli.q44.hw))
#q44 not signif

pli.q44.hw.1 <- lmer(data = iws.hw,
                    log(pli) ~ season + prox.normal + location + pH+prox.normal:location+Q44+
                      (1|site),
                    REML = F)
print(summary(pli.q44.hw.1))
vif(pli.q44.hw.1)
anova(pli.q44.hw.1)
pli.q44.hw.2.step <- step(pli.q44.hw.1)
pli.q44.hw.2.step
pli.q44.hw.2 <- get_model(pli.q44.hw.2.step)
print(summary(pli.q44.hw.2))
check_model(pli.q44.hw.2)
print(anova(pli.q44.hw.2))
plot(allEffects(pli.q44.hw.2)) #
pli.q44.hw.sum <- summary(pli.q44.hw.2)
pli.q44.hw.sum
# write.csv(pli.q44.hw.sum$coefficients, "pli_q44_hw_coefs.csv")
# perf <- performance(pli.q44.hw.2)
# perf
# write.csv(perf, "pli_q44_hw_diag.csv")
#q44 not signif

###tucson ----
pli.q44.tu.0 <- lmer(data = iws.tu,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q44.tu.0))

pli.q44.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q44+
                    (1|site),
                  REML = F)
print(summary(pli.q44.tu))
anova(pli.q44.tu)
check_model(pli.q44.tu)
plot(allEffects(pli.q44.tu))
#q44 signif, generally a negative trend, decent assumptions

pli.q44.tu.1 <- lmer(data = iws.tu,
                    log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q44+
                      (1|site),
                    REML = F)
print(summary(pli.q44.tu.1))
vif(pli.q44.tu.1)
pli.q44.tu.2.step <- step(pli.q44.tu.1)
pli.q44.tu.2.step
pli.q44.tu.2 <- get_model(pli.q44.tu.2.step)
print(summary(pli.q44.tu.2))
check_model(pli.q44.tu.2)
anova(pli.q44.tu.1)
print(anova(pli.q44.tu.2))
plot(allEffects(pli.q44.tu.2))
pli.q44.tu.sum <- summary(pli.q44.tu.2)
pli.q44.tu.sum
# write.csv(pli.q44.tu.sum$coefficients, "pli_q44_tu_coefs.csv")
# perf <- performance(pli.q44.tu.2)
# perf
# write.csv(perf, "pli_q44_tu_diag.csv")
#q44 not signif



##hds Q1 roof material ----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q1))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlations

###all communities ----
#remove these roofs from all community analysis because 5 of the 6 samples are all coming from one site, G428
#when analysis was done with these samples in, Q1 was signif, largely because this roof material has very high concentrations, but we cannot accurately compare this roof type to others because no other site had this roof type except for 1 site (and 1 sample) in Tucson. So we are really seeing the effect of one site, which is the site closest to freeport mcmoran mine in Miami.
iws <- iws%>%
  filter(Q1!="Flat BUR (Tar/Gravel), Flat BUR (Reflective)")

pli.q1.0 <- lmer(data = iws,
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q1.0))

pli.q1 <- lmer(data = iws,
               log(pli) ~ Q1+
                 (1|site),
               REML = F)
print(summary(pli.q1))
anova(pli.q1) #not signif
check_model(pli.q1)
plot(allEffects(pli.q1))

#univariate, q1 not significant

pli.q1.1 <- lmer(data = iws,
                 log(pli) ~ season + prox.normal + pH + community+community:prox.normal+Q1+
                   (1|site),
                 REML = F)
print(summary(pli.q1.1))
vif(pli.q1.1)
pli.q1.2.step <- step(pli.q1.1)
pli.q1.2.step
pli.q1.2 <- get_model(pli.q1.2.step)
print(summary(pli.q1.2))
check_model(pli.q1.2)
pli.q1.3 <- lmer(data = iws,
                 log(pli) ~ season + prox.normal + pH + community+Q1+
                   (1|site),
                 REML = F)
pli.q1.4 <- lmer(data = iws,
                 log(pli) ~ season + prox.normal + pH + community+
                   (1|site),
                 REML = F)
anova(pli.q1.1)
anova(pli.q1.2)
anova(pli.q1.3)
anova(pli.q1.2, pli.q1.3)
anova(pli.q1.4, pli.q1.3)
check_model(pli.q1.3)
plot(allEffects(pli.q1.3))
#q1 not signif
# model.effects <- ggeffect(model = pli.q1.3,
#                           type = "re",
#                           terms = c("Q1"))
# model.effects$x <- as.character(model.effects$x)
# ggplot(model.effects, aes(x = x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high)))+
#   geom_pointrange()+
#   labs(title = "What is your roof made of?",
#        subtitle = "All communities",
#        x = "\nRoof type",
#        y = "PLI\n")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1))
# #dev.print(png, "Ni_tu_q67_effect.png", res=300, height=8, width=10, units="in")

###globe/miami ----
pli.q1.gm.0 <- lmer(data = iws.gm,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q1.gm.0))

pli.q1.gm <- lmer(data = iws.gm,
                  log(pli) ~ Q1+
                    (1|site),
                  REML = F)
print(summary(pli.q1.gm))
anova(pli.q1.gm)
check_model(pli.q1.gm)
plot(allEffects(pli.q1.gm))
#q1 not signif, when tar/gravel + reflective removed

pli.q1.gm.1 <- lmer(data = iws.gm,
                    log(pli) ~ season + prox.normal + location_2 + pH+Q1+
                      (1|site),
                    REML = F)
print(summary(pli.q1.gm.1))
vif(pli.q1.gm.1)
anova(pli.q1.gm.1)
pli.q1.gm.2.step <- step(pli.q1.gm.1)
pli.q1.gm.2.step
pli.q1.gm.2 <- get_model(pli.q1.gm.2.step)
print(summary(pli.q1.gm.2))
check_model(pli.q1.gm.2)
vif(pli.q1.gm.2)
print(anova(pli.q1.gm.2))
plot(allEffects(pli.q1.gm.2)) #
pli.q1.gm.sum <- summary(pli.q1.gm.2)
pli.q1.gm.sum
write.csv(pli.q1.gm.sum$coefficients, "pli_q1_gm_coefs.csv")
perf <- performance(pli.q1.gm.2)
perf
write.csv(perf, "pli_q1_gm_diag.csv")
#q1 signif, with asphalt rooves and metal panel roofs having the highest plis

model.effects <- ggeffect(model = pli.q1.gm.2,
                          type = "re",
                          terms = c("Q1"))
model.effects$x <- as.character(model.effects$x)
ggplot(model.effects, aes(x = x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high)))+
  geom_pointrange()+
  labs(title = "What is your roof made of?",
       subtitle = "Globe/Miami",
       x = "\nRoof Material",
       y = "PLI\n")+
  coord_cartesian(ylim = c(0,3))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.print(png, "pli_q1_gm_effect.png", res=300, height=6, width=8, units="in")


###hayden/winkelman ----
pli.q1.hw.0 <- lmer(data = iws.hw,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q1.hw.0))

pli.q1.hw <- lmer(data = iws.hw,
                  log(pli) ~ Q1+
                    (1|site),
                  REML = F)
print(summary(pli.q1.hw))
anova(pli.q1.hw)
check_model(pli.q1.hw)
plot(allEffects(pli.q1.hw))
#q1 not signif

pli.q1.hw.1 <- lmer(data = iws.hw,
                    log(pli) ~ season + prox.normal + location + pH+prox.normal:location+Q1+
                      (1|site),
                    REML = F)
print(summary(pli.q1.hw.1))
vif(pli.q1.hw.1)
anova(pli.q1.hw.1)
pli.q1.hw.2.step <- step(pli.q1.hw.1)
pli.q1.hw.2.step
pli.q1.hw.2 <- get_model(pli.q1.hw.2.step)
print(summary(pli.q1.hw.2))
check_model(pli.q1.hw.2)
print(anova(pli.q1.hw.2))
plot(allEffects(pli.q1.hw.2)) #
pli.q1.hw.sum <- summary(pli.q1.hw.2)
pli.q1.hw.sum
# write.csv(pli.q1.hw.sum$coefficients, "pli_q1_hw_coefs.csv")
# perf <- performance(pli.q1.hw.2)
# perf
# write.csv(perf, "pli_q1_hw_diag.csv")
#q1 not signif

###tucson ----
pli.q1.tu.0 <- lmer(data = iws.tu,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q1.tu.0))

pli.q1.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q1+
                    (1|site),
                  REML = F)
print(summary(pli.q1.tu))
anova(pli.q1.tu)
check_model(pli.q1.tu)
plot(allEffects(pli.q1.tu))
#q1 nearly signif, p = .08 with asphalt being the lowest

pli.q1.tu.1 <- lmer(data = iws.tu,
                    log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q1+
                      (1|site),
                    REML = F)
print(summary(pli.q1.tu.1))
vif(pli.q1.tu.1)
pli.q1.tu.2.step <- step(pli.q1.tu.1)
pli.q1.tu.2.step
pli.q1.tu.2 <- get_model(pli.q1.tu.2.step)
print(summary(pli.q1.tu.2))
check_model(pli.q1.tu.2)
anova(pli.q1.tu.1)
print(anova(pli.q1.tu.2))
plot(allEffects(pli.q1.tu.2))
pli.q1.tu.sum <- summary(pli.q1.tu.2)
pli.q1.tu.sum
# write.csv(pli.q1.tu.sum$coefficients, "pli_q1_tu_coefs.csv")
# perf <- performance(pli.q1.tu.2)
# perf
# write.csv(perf, "pli_q1_tu_diag.csv")
#q1 nearly signif, p=.1



##hds Q67 roof cleaning----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q67))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlation

###all communities ----
pli.q67.0 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                 log(pli) ~
                   (1|site),
                 REML = F)
print(summary(pli.q67.0))

pli.q67 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
               log(pli) ~ Q67+
                 (1|site),
               REML = F)
print(summary(pli.q67))
anova(pli.q67)
check_model(pli.q67) #lots of high leverage points
plot(allEffects(pli.q67))
#univariate, q67 significant cleaning introduces contamination, lots of high leverage points

pli.q67.1 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                 log(pli) ~ season + prox.normal + pH + community+community:prox.normal+Q67+
                   (1|site),
                 REML = F)
print(summary(pli.q67.1))
vif(pli.q67.1)
pli.q67.2.step <- step(pli.q67.1)
pli.q67.2.step
pli.q67.2 <- get_model(pli.q67.2.step)
pli.q67.2 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                  log(pli) ~ season + prox.normal + pH + community + community:prox.normal + Q67 + (1 | site),
                  REML = F)
print(summary(pli.q67.2))
check_model(pli.q67.2)
vif(pli.q67.2)
anova(pli.q67.1)
pli.q67.3 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                  log(pli) ~ season + prox.normal + pH + community + Q67 + (1 | site),
                  REML = F)
print(anova(pli.q67.2))
anova(pli.q67.3)
plot(allEffects(pli.q67.3))
check_model(pli.q67.3)
pli.q67.sum <- summary(pli.q67.3)
pli.q67.sum
write.csv(pli.q67.sum$coefficients, "pli_q67_coefs.csv")
perf <- performance(pli.q67.3)
perf
write.csv(perf, "pli_q67_diag.csv")
#q67 nearly signif, cleaning has higher pli
model.effects <- ggeffect(model = pli.q67.3,
                          type = "re",
                          terms = c("Q67"))
model.effects$x <- as.character(model.effects$x)
ggplot(model.effects, aes(x = x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high)))+
  geom_pointrange()+
  labs(title = "Do you clean your roof?",
       subtitle = "All Communities",
       x = "\nInfrastructure Practice",
       y = "PLI\n")+
  coord_cartesian(ylim = c(0,2.5))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text())
dev.print(png, "pli_q67_effect.png", res=300, height=6, width=8, units="in")


###dewey-humboldt ----
pli.q67.dh.0 <- lmer(data = iws.dh,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q67.dh.0))

pli.q67.dh <- lmer(data = iws.dh,
                  log(pli) ~ Q67+
                    (1|site),
                  REML = F)
print(summary(pli.q67.dh))
anova(pli.q67.dh)
check_model(pli.q67.dh)
plot(allEffects(pli.q67.dh))
#q67 not signif

pli.q67.dh.1 <- lmer(data = iws.dh,
                    log(pli) ~ season + pH + prox.normal+Q67+
                      (1|site),
                    REML = F)
print(summary(pli.q67.dh.1))
vif(pli.q67.dh.1)
pli.q67.dh.2.step <- step(pli.q67.dh.1)
pli.q67.dh.2.step
pli.q67.dh.2 <- get_model(pli.q67.dh.2.step)
print(summary(pli.q67.dh.2))
check_model(pli.q67.dh.2) #poor model assumptions
anova(pli.q67.dh.1)
print(anova(pli.q67.dh.2))
plot(allEffects(pli.q67.dh.2))
pli.q67.dh.sum <- summary(pli.q67.dh.2)
pli.q67.dh.sum
# write.csv(pli.q67.dh.sum$coefficients, "pli_q67_dh_coefs.csv")
# perf <- performance(pli.q67.dh.2)
# perf
# write.csv(perf, "pli_q67_dh_diag.csv")
#q67 not signif

###globe/miami ----
pli.q67.gm.0 <- lmer(data = iws.gm,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q67.gm.0))

pli.q67.gm <- lmer(data = iws.gm,
                  log(pli) ~ Q67+
                    (1|site),
                  REML = F)
print(summary(pli.q67.gm))
anova(pli.q67.gm)
check_model(pli.q67.gm)
plot(allEffects(pli.q67.gm))
#q67 signif, generally a negative trend, decent assumptions

pli.q67.gm.1 <- lmer(data = iws.gm,
                    log(pli) ~ season + prox.normal + location_2 + pH+Q67+
                      (1|site),
                    REML = F)
print(summary(pli.q67.gm.1))
vif(pli.q67.gm.1)
anova(pli.q67.gm.1)
pli.q67.gm.2.step <- step(pli.q67.gm.1)
pli.q67.gm.2.step
pli.q67.gm.2 <- get_model(pli.q67.gm.2.step)
print(summary(pli.q67.gm.2))
check_model(pli.q67.gm.2)
print(anova(pli.q67.gm.2))
plot(allEffects(pli.q67.gm.2)) #
pli.q67.gm.sum <- summary(pli.q67.gm.2)
pli.q67.gm.sum
# write.csv(pli.q67.gm.sum$coefficients, "pli_q67_gm_coefs.csv")
# perf <- performance(pli.q67.gm.2)
# perf
# write.csv(perf, "pli_q67_gm_diag.csv")
#q67 not signif

###tucson ----
pli.q67.tu.0 <- lmer(data = iws.tu,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q67.tu.0))

pli.q67.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q67+
                    (1|site),
                  REML = F)
print(summary(pli.q67.tu))
anova(pli.q67.tu)
check_model(pli.q67.tu)
plot(allEffects(pli.q67.tu))
#q67 signif, cleaning roof introduces contamination

pli.q67.tu.1 <- lmer(data = iws.tu,
                    log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q67+
                      (1|site),
                    REML = F)
print(summary(pli.q67.tu.1))
vif(pli.q67.tu.1)
pli.q67.tu.2.step <- step(pli.q67.tu.1)
pli.q67.tu.2.step
pli.q67.tu.2 <- get_model(pli.q67.tu.2.step)
print(summary(pli.q67.tu.2))
pli.q67.tu.2 <- lmer(data = iws.tu,
                     log(pli) ~ season + pH +Q67+
                       (1|site),
                     REML = T)
check_model(pli.q67.tu.2)
anova(pli.q67.tu.1)
print(anova(pli.q67.tu.2))
plot(allEffects(pli.q67.tu.2))
pli.q67.tu.sum <- summary(pli.q67.tu.2)
pli.q67.tu.sum
write.csv(pli.q67.tu.sum$coefficients, "pli_q67_tu_coefs.csv")
perf <- performance(pli.q67.tu.2)
perf
write.csv(perf, "pli_q67_tu_diag.csv")
#q67 signif
model.effects <- ggeffect(model = pli.q67.tu.2,
                          type = "re",
                          terms = c("Q67"))
model.effects$x <- as.character(model.effects$x)
ggplot(model.effects, aes(x = x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high)))+
  geom_pointrange()+
  labs(title = "Do you clean your roof?",
       subtitle = "Tucson",
       x = "\nInfrastructure Practice",
       y = "PLI\n")+
  coord_cartesian(ylim = c(0,2.2))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text())
dev.print(png, "pli_q67_tu_effect.png", res=300, height=6, width=8, units="in")



##hds Q68 roof wash frequency ----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q68))+
  geom_histogram()+
  facet_grid(community~.)
#might be some correlation between proximity and wash frequency in Globe
###globe/miami ----
pli.q68.gm.0 <- lmer(data = iws.gm,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q68.gm.0))

pli.q68.gm <- lmer(data = iws.gm,
                  log(pli) ~ Q68+
                    (1|site),
                  REML = F)
print(summary(pli.q68.gm))
anova(pli.q68.gm)
check_model(pli.q68.gm)
plot(allEffects(pli.q68.gm))
#q68 signif, generally a negative trend, decent assumptions

pli.q68.gm.1 <- lmer(data = iws.gm,
                    log(pli) ~ season + prox.normal + location_2 + pH+Q68+
                      (1|site),
                    REML = F)
print(summary(pli.q68.gm.1))
vif(pli.q68.gm.1)
anova(pli.q68.gm.1)
pli.q68.gm.2.step <- step(pli.q68.gm.1)
pli.q68.gm.2.step
pli.q68.gm.2 <- get_model(pli.q68.gm.2.step)
print(summary(pli.q68.gm.2))
check_model(pli.q68.gm.2)
print(anova(pli.q68.gm.2))
plot(allEffects(pli.q68.gm.2)) #
pli.q68.gm.sum <- summary(pli.q68.gm.2)
pli.q68.gm.sum
# write.csv(pli.q68.gm.sum$coefficients, "pli_q68_gm_coefs.csv")
# perf <- performance(pli.q68.gm.2)
# perf
# write.csv(perf, "pli_q68_gm_diag.csv")
#q68 not signif

###tucson ----
pli.q68.tu.0 <- lmer(data = iws.tu,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q68.tu.0))

pli.q68.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q68+
                    (1|site),
                  REML = F)
print(summary(pli.q68.tu))
anova(pli.q68.tu)
check_model(pli.q68.tu)
plot(allEffects(pli.q68.tu))
#q68 not signif

pli.q68.tu.1 <- lmer(data = iws.tu,
                    log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q68+
                      (1|site),
                    REML = F)
print(summary(pli.q68.tu.1))
vif(pli.q68.tu.1)
pli.q68.tu.2.step <- step(pli.q68.tu.1)
pli.q68.tu.2.step
pli.q68.tu.2 <- get_model(pli.q68.tu.2.step)
print(summary(pli.q68.tu.2))
check_model(pli.q68.tu.2)
anova(pli.q68.tu.1)
print(anova(pli.q68.tu.2))
plot(allEffects(pli.q68.tu.2))
pli.q68.tu.sum <- summary(pli.q68.tu.2)
pli.q68.tu.sum
# write.csv(pli.q68.tu.sum$coefficients, "pli_q68_tu_coefs.csv")
# perf <- performance(pli.q68.tu.2)
# perf
# write.csv(perf, "pli_q68_tu_diag.csv")
#q68 not signif






##hds Q60 cistern material ----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q60))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlation
####tucson ----
pli.q60.tu.0 <- lmer(data = iws.tu,
                    log(pli) ~
                      (1|site),
                    REML = F)
print(summary(pli.q60.tu.0))

pli.q60.tu <- lmer(data = iws.tu,
                  log(pli) ~ Q60+
                    (1|site),
                  REML = F)
print(summary(pli.q60.tu))
anova(pli.q60.tu)
check_model(pli.q60.tu)
plot(allEffects(pli.q60.tu))
#q60 not signif

pli.q60.tu.1 <- lmer(data = iws.tu,
                    log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q60+
                      (1|site),
                    REML = F)
print(summary(pli.q60.tu.1))
vif(pli.q60.tu.1)
pli.q60.tu.2.step <- step(pli.q60.tu.1)
pli.q60.tu.2.step
pli.q60.tu.2 <- get_model(pli.q60.tu.2.step)
print(summary(pli.q60.tu.2))
check_model(pli.q60.tu.2)
anova(pli.q60.tu.1)
print(anova(pli.q60.tu.2))
plot(allEffects(pli.q60.tu.2))
pli.q60.tu.sum <- summary(pli.q60.tu.2)
pli.q60.tu.sum
# write.csv(pli.q60.tu.sum$coefficients, "pli_q60_tu_coefs.csv")
# perf <- performance(pli.q60.tu.2)
# perf
# write.csv(perf, "pli_q60_tu_diag.csv")
#q60 not signif




##hds Q65 cistern age----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q65))+
  geom_histogram()+
  facet_grid(community~.)
#might be a correlation with proximity and cistern age in Globe, older cisterns closer to mine and canyons - further from town?

###all communities ----
pli.q65.0 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                  log(pli) ~
                    (1|site),
                  REML = F)
print(summary(pli.q65.0))

pli.q65 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                log(pli) ~ Q65+
                  (1|site),
                REML = F)
print(summary(pli.q65))
anova(pli.q65)
check_model(pli.q65) #pretty good model
plot(allEffects(pli.q65))
#univariate, q65 significant oldest homes have highest contamination

pli.q65.1 <- lmer(data = iws[iws$community!="Hayden/Winkelman",],
                  log(pli) ~ season + prox.normal + pH + community+community:prox.normal+Q65+
                    (1|site),
                  REML = F)
print(summary(pli.q65.1))
vif(pli.q65.1)
pli.q65.2.step <- step(pli.q65.1)
pli.q65.2.step
pli.q65.2 <- get_model(pli.q65.2.step)
print(summary(pli.q65.2))
check_model(pli.q65.2)
anova(pli.q65.1)
print(anova(pli.q65.2))
plot(allEffects(pli.q65.2))
pli.q65.sum <- summary(pli.q65.2)
# write.csv(pli.q65.sum$coefficients, "pli_q65_coefs.csv")
# perf <- performance(pli.q65.2)
# perf
# write.csv(perf, "pli_q65_diag.csv")
#q65 not signif


###dewey-humboldt ----
pli.q65.dh.0 <- lmer(data = iws.dh,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q65.dh.0))

pli.q65.dh <- lmer(data = iws.dh,
                   log(pli) ~ Q65+
                     (1|site),
                   REML = F)
print(summary(pli.q65.dh))
anova(pli.q65.dh)
check_model(pli.q65.dh)
plot(allEffects(pli.q65.dh))
#q65 not signif

pli.q65.dh.1 <- lmer(data = iws.dh,
                     log(pli) ~ season + pH + prox.normal+Q65+
                       (1|site),
                     REML = F)
print(summary(pli.q65.dh.1))
vif(pli.q65.dh.1)
pli.q65.dh.2.step <- step(pli.q65.dh.1)
pli.q65.dh.2.step
pli.q65.dh.2 <- get_model(pli.q65.dh.2.step)
print(summary(pli.q65.dh.2))
check_model(pli.q65.dh.2) #decent assumptions
anova(pli.q65.dh.1)
print(anova(pli.q65.dh.2))
plot(allEffects(pli.q65.dh.2))
pli.q65.dh.sum <- summary(pli.q65.dh.2)
pli.q65.dh.sum
write.csv(pli.q65.dh.sum$coefficients, "pli_q65_dh_coefs.csv")
perf <- performance(pli.q65.dh.2)
perf
write.csv(perf, "pli_q65_dh_diag.csv")
#q65 is signif, with cisterns <6mo - 2 years having lower contam than cisterns 3-5+ years old
model.effects <- ggeffect(model = pli.q65.dh.2,
                          type = "re",
                          terms = c("Q65"))
model.effects$x <- as.character(model.effects$x)
model.effects$x <- factor(model.effects$x, levels = c("<6 months", "6 months-1 year", "1-2 years", "2-3 years", "3-4 years", "5+ years"))
ggplot(model.effects, aes(x = x, y = exp(predicted), ymin = exp(conf.low), ymax = exp(conf.high)))+
  geom_pointrange()+
  labs(title = "How old is your cistern?",
       subtitle = "Dewey-Humboldt",
       x = "\nInfrastructure Practice",
       y = "PLI\n")+
  coord_cartesian(ylim = c(0,4))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust =1))
dev.print(png, "pli_q65_dh_effect.png", res=300, height=6, width=10, units="in")



###globe/miami ----
pli.q65.gm.0 <- lmer(data = iws.gm,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q65.gm.0))

pli.q65.gm <- lmer(data = iws.gm,
                   log(pli) ~ Q65+
                     (1|site),
                   REML = F)
print(summary(pli.q65.gm))
anova(pli.q65.gm)
check_model(pli.q65.gm)
plot(allEffects(pli.q65.gm))
#q65 nearly signif p = .1

pli.q65.gm.1 <- lmer(data = iws.gm,
                     log(pli) ~ season + prox.normal + location_2 + pH+Q65+
                       (1|site),
                     REML = F)
print(summary(pli.q65.gm.1))
vif(pli.q65.gm.1)
anova(pli.q65.gm.1)
pli.q65.gm.2.step <- step(pli.q65.gm.1)
pli.q65.gm.2.step
pli.q65.gm.2 <- get_model(pli.q65.gm.2.step)
print(summary(pli.q65.gm.2))
check_model(pli.q65.gm.2)
print(anova(pli.q65.gm.2))
plot(allEffects(pli.q65.gm.2)) #
pli.q65.gm.sum <- summary(pli.q65.gm.2)
pli.q65.gm.sum
# write.csv(pli.q65.gm.sum$coefficients, "pli_q65_gm_coefs.csv")
# perf <- performance(pli.q65.gm.2)
# perf
# write.csv(perf, "pli_q65_gm_diag.csv")
#q65 not signif

###tucson ----
pli.q65.tu.0 <- lmer(data = iws.tu,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q65.tu.0))

pli.q65.tu <- lmer(data = iws.tu,
                   log(pli) ~ Q65+
                     (1|site),
                   REML = F)
print(summary(pli.q65.tu))
anova(pli.q65.tu)
check_model(pli.q65.tu)
plot(allEffects(pli.q65.tu))
#q65 not signif

pli.q65.tu.1 <- lmer(data = iws.tu,
                     log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q65+
                       (1|site),
                     REML = F)
print(summary(pli.q65.tu.1))
vif(pli.q65.tu.1)
pli.q65.tu.2.step <- step(pli.q65.tu.1)
pli.q65.tu.2.step
pli.q65.tu.2 <- get_model(pli.q65.tu.2.step)
print(summary(pli.q65.tu.2))
check_model(pli.q65.tu.2)
anova(pli.q65.tu.1)
print(anova(pli.q65.tu.2))
plot(allEffects(pli.q65.tu.2))
pli.q65.tu.sum <- summary(pli.q65.tu.2)
pli.q65.tu.sum
# write.csv(pli.q65.tu.sum$coefficients, "pli_q65_tu_coefs.csv")
# perf <- performance(pli.q65.tu.2)
# perf
# write.csv(perf, "pli_q65_tu_diag.csv")
#q65 not signif



##hds Q71 cistern wash ----
#not enough sample spread in any community to reliably analyze

##hds Q76 first flush----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q76))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlation
####tucson ----
pli.q76.tu.0 <- lmer(data = iws.tu,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q76.tu.0))

pli.q76.tu <- lmer(data = iws.tu,
                   log(pli) ~ Q76+
                     (1|site),
                   REML = F)
print(summary(pli.q76.tu))
anova(pli.q76.tu)
check_model(pli.q76.tu)
plot(allEffects(pli.q76.tu))
#q76 not signif

pli.q76.tu.1 <- lmer(data = iws.tu,
                     log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q76+
                       (1|site),
                     REML = F)
print(summary(pli.q76.tu.1))
vif(pli.q76.tu.1)
pli.q76.tu.2.step <- step(pli.q76.tu.1)
pli.q76.tu.2.step
pli.q76.tu.2 <- get_model(pli.q76.tu.2.step)
print(summary(pli.q76.tu.2))
check_model(pli.q76.tu.2)
anova(pli.q76.tu.1)
print(anova(pli.q76.tu.2))
plot(allEffects(pli.q76.tu.2))
pli.q76.tu.sum <- summary(pli.q76.tu.2)
pli.q76.tu.sum
# write.csv(pli.q76.tu.sum$coefficients, "pli_q76_tu_coefs.csv")
# perf <- performance(pli.q76.tu.2)
# perf
# write.csv(perf, "pli_q76_tu_diag.csv")
#q76 not signif




##hds Q77 cistern screen----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q77))+
  geom_histogram()+
  facet_grid(community~.)
#no apparent correlation

###dewey-humboldt ----
pli.q77.dh.0 <- lmer(data = iws.dh,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q77.dh.0))

pli.q77.dh <- lmer(data = iws.dh,
                   log(pli) ~ Q77+
                     (1|site),
                   REML = F)
print(summary(pli.q77.dh))
anova(pli.q77.dh)
check_model(pli.q77.dh)
plot(allEffects(pli.q77.dh))
#q77 not signif

pli.q77.dh.1 <- lmer(data = iws.dh,
                     log(pli) ~ season + pH + prox.normal+Q77+
                       (1|site),
                     REML = F)
print(summary(pli.q77.dh.1))
vif(pli.q77.dh.1)
pli.q77.dh.2.step <- step(pli.q77.dh.1)
pli.q77.dh.2.step
pli.q77.dh.2 <- get_model(pli.q77.dh.2.step)
print(summary(pli.q77.dh.2))
check_model(pli.q77.dh.2) #
anova(pli.q77.dh.1)
print(anova(pli.q77.dh.2))
plot(allEffects(pli.q77.dh.2))
pli.q77.dh.sum <- summary(pli.q77.dh.2)
pli.q77.dh.sum
# write.csv(pli.q77.dh.sum$coefficients, "pli_q77_dh_coefs.csv")
# perf <- performance(pli.q77.dh.2)
# perf
# write.csv(perf, "pli_q77_dh_diag.csv")
#q77 is not signif

###globe/miami ----
pli.q77.gm.0 <- lmer(data = iws.gm,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q77.gm.0))

pli.q77.gm <- lmer(data = iws.gm,
                   log(pli) ~ Q77+
                     (1|site),
                   REML = F)
print(summary(pli.q77.gm))
anova(pli.q77.gm)
check_model(pli.q77.gm)
plot(allEffects(pli.q77.gm))
#q77 not signif

pli.q77.gm.1 <- lmer(data = iws.gm,
                     log(pli) ~ season + prox.normal + location_2 + pH+Q77+
                       (1|site),
                     REML = F)
print(summary(pli.q77.gm.1))
vif(pli.q77.gm.1)
anova(pli.q77.gm.1)
pli.q77.gm.2.step <- step(pli.q77.gm.1)
pli.q77.gm.2.step
pli.q77.gm.2 <- get_model(pli.q77.gm.2.step)
print(summary(pli.q77.gm.2))
check_model(pli.q77.gm.2)
print(anova(pli.q77.gm.2))
plot(allEffects(pli.q77.gm.2)) #
pli.q77.gm.sum <- summary(pli.q77.gm.2)
pli.q77.gm.sum
# write.csv(pli.q77.gm.sum$coefficients, "pli_q77_gm_coefs.csv")
# perf <- performance(pli.q77.gm.2)
# perf
# write.csv(perf, "pli_q77_gm_diag.csv")
#q77 not signif

##hds Q78b cistern screen material----
###viz ----
ggplot(data = iws, mapping = aes(x = prox.normal, fill = Q78b))+
  geom_histogram()+
  facet_grid(community~.)
#globe has plastic cistern screens closer to mine

####tucson ----
pli.q78b.tu.0 <- lmer(data = iws.tu,
                     log(pli) ~
                       (1|site),
                     REML = F)
print(summary(pli.q78b.tu.0))

pli.q78b.tu <- lmer(data = iws.tu,
                   log(pli) ~ Q78b+
                     (1|site),
                   REML = F)
print(summary(pli.q78b.tu))
anova(pli.q78b.tu)
check_model(pli.q78b.tu)
plot(allEffects(pli.q78b.tu))
#q78b not signif

pli.q78b.tu.1 <- lmer(data = iws.tu,
                     log(pli) ~ season + prox.normal + ward + pH + prox.normal:pH + prox.normal:season+Q78b+
                       (1|site),
                     REML = F)
print(summary(pli.q78b.tu.1))
vif(pli.q78b.tu.1)
pli.q78b.tu.2.step <- step(pli.q78b.tu.1)
pli.q78b.tu.2.step
pli.q78b.tu.2 <- get_model(pli.q78b.tu.2.step)
print(summary(pli.q78b.tu.2))
check_model(pli.q78b.tu.2)
anova(pli.q78b.tu.1)
print(anova(pli.q78b.tu.2))
plot(allEffects(pli.q78b.tu.2))
pli.q78b.tu.sum <- summary(pli.q78b.tu.2)
pli.q78b.tu.sum
# write.csv(pli.q78b.tu.sum$coefficients, "pli_q78b_tu_coefs.csv")
# perf <- performance(pli.q78b.tu.2)
# perf
# write.csv(perf, "pli_q78b_tu_diag.csv")
#q78b not signif whether rocks, cotton/cloth are included or not



