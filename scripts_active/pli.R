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

#add sub location to hayden data
hay <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "hayden", col_names = TRUE)
iws.hw <- full_join(iws.hw, hay, by = c("site"))
iws.hw <- iws.hw %>%
  drop_na(community) %>%
  drop_na(location)
iws.hw$location <- factor(iws.hw$location, levels = c("Hayden", "Winkelman"))

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
##overall ----
###all communities ----
pli.tu.0 <- lmer(data = iw.dm,
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


