#Kunal Palawat
#Description: code to summarize and analyze RHRW standard exceedances

#load libraries
library(readxl)
library(MASS)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(aod)
library(wesanderson)
library(car)
library(GGally)
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
library(boot)
library(lattice)
library(performance)
library(palettetown)
library(pals)

#load data ----
standards <- read_xlsx("/Users/gift/Documents/GitHub/WorkingFiles/data/data_processing/Standards.xlsx", sheet = "standards", col_names = TRUE)
#standards <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/Standards.xlsx", sheet = "standards", col_names = TRUE)

#join standard data to contaminant data, this repeats the standard data for each analyte for each sample
ex.dat <- full_join(iw.dm.long, standards, by = c("analyte"))

#make longer
ex.dat.long <- pivot_longer(data = ex.dat,
                            cols = PB:LDW,
                            values_to = "standard_value",
                            names_to = "standard")
#calculate exceedance
ex.dat.long$exceedance <- ifelse(ex.dat.long$value > ex.dat.long$standard_value,1,0)

#remove standard value for ease of making wider
ex.dat.long <- subset(ex.dat.long, select = -c(standard_value))

#remove NAs for analytes that do not have standards
ex.dat.long <- ex.dat.long %>%
  drop_na(exceedance)

#data for modeling
#drop samples without proximity data
ex.dat.long.prox <- ex.dat.long %>%
  drop_na(proximity.km) %>%
  drop_na(prox.normal)

#split dataframe into different ones for each community
exc <- ex.dat.long.prox %>%
  group_by(community) %>%
  group_split()

exdh <- exc[[1]]
exgm <- exc[[2]]
exhw <- exc[[3]]
extu <- exc[[4]]


#summary ----
sumFX(datalongDF = ex.dat.long,
      subset.vector.string = c("standard", "analyte", "season", "community"),
      value.string = "exceedance",
      dfname.string = "ex.ssncom",
      filename.string = "ex%_ssncom")



# #name subset columns
# cols <- c("standard", "analyte")
# 
# #calculate counts and percentages of the whole
# sumtable <- ex.dat.long %>%
#   group_by(across(all_of(cols))) %>%
#   summarise(n = n(),
#             exceedances_n = sum(exceedance),
#             exceedances_freq = signif(sum(exceedance)/n()*100, 2))
# 
# sumtable.small <- subset(sumtable, select = -c(exceedances_n))
# 
# sumtable.wide <- pivot_wider(data = sumtable.small,
#                              names_from = "standard",
#                              values_from = "exceedances_freq")
# view(sumtable.wide)
# write.csv(sumtable.wide, "exceedance%_overall.csv")

#viz ----
ggplot(ex.dat.long, mapping = aes(x = analyte, fill = as.factor(exceedance))) +
  geom_bar() +
  scale_fill_manual(values = wes_palette(name = "Darjeeling2", n = 4),
                    labels = c("No Exceedance", "Exceedance")) +
  #scale_fill_viridis_d() +
  facet_grid(standard~., scales = "free") +
  labs(x = "\nAnalyte",
       y = "Count\n",
       title = "Count of exceedances by analyte and available standards",
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
dev.print(png, "iw_exceedance_overall.png", res=300, height=10, width=7, units="in")


ggplot(ex.dat.long, mapping = aes(x = analyte, y = as.factor(exceedance), fill = as.factor(community))) +
  stat_sum(aes(size = ..n.., group = 1), position = "jitter", shape = 21) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  # #scale_fill_viridis_d() +
  facet_grid(standard~analyte, scales = "free") +
  labs(x = "\nAnalyte",
       y = "Exceedance\n",
       title = "Count of exceedances by analyte and available standards",
       fill = "Community",
       size = "Count")+
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
dev.print(png, "iw_exceedance_com.png", res=300, height=10, width=12, units="in")


#overall top 5 # of exceedances
#AI: Cu, Zn, Mn, Mo, Cd
#DW: Al, Mn, Fe, As, Pb
#FB: Pb, Cu, As
#PB: Pb, Cu
#LDW: Mn, Fe, As, Cu, Pb
# Cu, Mn, Pb, As, Fe

#modeling ----
#in general, models were only run on datasets with at least 6 exceedances

#AI ----
#note that there are not enough exceedances to model most of these by community...or at all
#can probably only do Cu - GM HW TU, Mn - GM, Mo - TU, Zn - TU. Could also try Cu with all communities

##copper ----
### gm ----
### not enough data to really analyze this, season is not significant on its own
aicu.gm.0 <- glm(data = exgm[exgm$standard=="AI"&exgm$analyte=="Cu",],
              exceedance ~ 1,
              family = "binomial")
#maximal.gm model
aicu.gm.1 <- glm(data = exgm[exgm$standard=="AI"&exgm$analyte=="Cu",],
              exceedance ~ season +prox.normal,
              family = "binomial")
summary(aicu.gm.1)
vif(aicu.gm.1)
check_model(aicu.gm.1)
exp(coef(aicu.gm.1))
performance(aicu.gm.1)
#neither variable significant? might not have enough data, but trends make sense

aicu.gm.2 <- stepAIC(aicu.gm.0,scope = list(upper=aicu.gm.1), direction="both", trace = T)
summary(aicu.gm.2) #no significant variable effect
vif(aicu.gm.2)
check_model(aicu.gm.2)
exp(coef(aicu.gm.2))
performance(aicu.gm.2)

# #get confidence interval.gms for estimates
# se <- sqrt(diag(vcov(aicu.gm.2)))
# # table of estimates with 95% CI
# (tab <- cbind(Est = fixef(aicu.gm.1), LL = fixef(aicu.gm.1) - 1.96 * se, UL = fixef(aicu.gm.1) + 1.96 *
#                 se))
# exp(tab)


### hw ----
aicu.hw.0 <- glm(data = exhw[exhw$standard=="AI"&exhw$analyte=="Cu",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.hw model
aicu.hw.1 <- glm(data = exhw[exhw$standard=="AI"&exhw$analyte=="Cu",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(aicu.hw.1)
vif(aicu.hw.1)
check_model(aicu.hw.1)
exp(coef(aicu.hw.1))
performance(aicu.hw.1)
#both variables significant, still continue with analysis

aicu.hw.2 <- stepAIC(aicu.hw.0,scope = list(upper=aicu.hw.1), direction="both", trace = T)
summary(aicu.hw.2) #both variables significant with a trend that makes sense
vif(aicu.hw.2)
check_model(aicu.hw.2)
coefs <- data.frame(t(coef(aicu.hw.2)))
performance(aicu.hw.2)

X1_range <- seq(from=0, to=round(max(exhw$proximity.km),0), by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of Cu Exceedance of Agricultural Irrigation Standard",
       x = "\nDistance from Hayden/Winkelman Smelter (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "aicu_hw_ef.png", res=300, height=6, width=7, units="in")
  

# exp(a_logits)/(1 + exp(a_logits))
# 
# w0 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*0
#            + coefs$prox.normal*0)
# w.5 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*0
#            + coefs$prox.normal*.5)
# w1 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*0
#            + coefs$prox.normal*1)
# w1.5 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*0
#            + coefs$prox.normal*1.5)
# w2 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*0
#            + coefs$prox.normal*2)
# 
# m0 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*1
#            + coefs$prox.normal*0)
# m.5 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*1
#            + coefs$prox.normal*.5)
# m1 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*1
#            + coefs$prox.normal*1)
# m1.5 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*1
#            + coefs$prox.normal*1.5)
# m2 <- logit2prob(coefs$X.Intercept.
#            + coefs$seasonMonsoon*1
#            + coefs$prox.normal*2)
# 
# probs <- data.frame(proximity = c(0, .5, 1, 1.5, 2),
#                     winter = c(w0, w.5, w1, w1.5,w2),
#                     monsoon = c(m0, m.5, m1, m1.5, m2))
# write.csv(probs, "exprobs_aicu_hw.csv")


### tu ----
aicu.tu.0 <- glm(data = extu[extu$standard=="AI"&extu$analyte=="Cu",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.tu model
aicu.tu.1 <- glm(data = extu[extu$standard=="AI"&extu$analyte=="Cu",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(aicu.tu.1)
vif(aicu.tu.1)
check_model(aicu.tu.1)
exp(coef(aicu.tu.1))
performance(aicu.tu.1)
#neither variable significant alone or together

aicu.tu.2 <- stepAIC(aicu.tu.0,scope = list(upper=aicu.tu.1), direction="both", trace = T)
summary(aicu.tu.2) #no significance
vif(aicu.tu.2)
check_model(aicu.tu.2)
exp(coef(aicu.tu.2))
performance(aicu.tu.2)


##manganese ----
### gm ----
aimn.gm.0 <- glm(data = exgm[exgm$standard=="AI"&exgm$analyte=="Mn",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.gm model
aimn.gm.1 <- glm(data = exgm[exgm$standard=="AI"&exgm$analyte=="Mn",],
                 exceedance ~ season +prox.normal,
                 family = "binomial")
summary(aimn.gm.1)
vif(aimn.gm.1)
check_model(aimn.gm.1)
exp(coef(aimn.gm.1))
performance(aimn.gm.1)
#proximity significant

aimn.gm.2 <- stepAIC(aimn.gm.0,scope = list(upper=aimn.gm.1), direction="both", trace = T)
summary(aimn.gm.2) #prox still significant
vif(aimn.gm.2)
check_model(aimn.gm.2)
coefs <- data.frame(t(coef(aimn.gm.2)))
performance(aimn.gm.2)

w0 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*0)
w1 <- logit2prob(coefs$X.Intercept.
                  + coefs$prox.normal*2)
w2 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*4)
w3 <- logit2prob(coefs$X.Intercept.
                   + coefs$prox.normal*6)
w4 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*8)
w5 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*10)

probs <- data.frame(proximity = c(0, 2, 4, 6, 8, 10),
                    prob = c(w0, w1, w2, w3, w4, w5))
probs$percent <- probs$prob*100
write.csv(probs, "exprobs_aimn_gm.csv")



##molybdenum ----
### tu ----
aimo.tu.0 <- glm(data = extu[extu$standard=="AI"&extu$analyte=="Mo",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.tu model
aimo.tu.1 <- glm(data = extu[extu$standard=="AI"&extu$analyte=="Mo",],
                 exceedance ~ season +prox.normal,
                 family = "binomial")
summary(aimo.tu.1)
vif(aimo.tu.1)
check_model(aimo.tu.1)
exp(coef(aimo.tu.1))
performance(aimo.tu.1)
#neither variable significant alone or together, season almost signif

aimo.tu.2 <- stepAIC(aimo.tu.0,scope = list(upper=aimo.tu.1), direction="both", trace = T)
summary(aimo.tu.2) #season almost signif
vif(aimo.tu.2)
check_model(aimo.tu.2)
exp(coef(aimo.tu.2))
performance(aimo.tu.2)

##zinc ----
### tu ----
aizn.tu.0 <- glm(data = extu[extu$standard=="AI"&extu$analyte=="Zn",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.hw model
aizn.tu.1 <- glm(data = extu[extu$standard=="AI"&extu$analyte=="Zn",],
                 exceedance ~ season + prox.normal,
                 family = "binomial")
summary(aizn.tu.1)
vif(aizn.tu.1)
check_model(aizn.tu.1)
exp(coef(aizn.tu.1))
performance(aizn.tu.1)
#prox where further away from airforce base, higher odds of exceedance

aizn.tu.2 <- stepAIC(aizn.tu.0,scope = list(upper=aizn.tu.1), direction="both", trace = T)
summary(aizn.tu.2)
vif(aizn.tu.2)
check_model(aizn.tu.2)
coefs <- data.frame(t(coef(aizn.tu.2)))
performance(aizn.tu.2)
#season not signif, but prox is

w0 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*0)
w1 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*4)
w2 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*8)
w3 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*12)
w4 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*16)
w5 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*20)
w6 <- logit2prob(coefs$X.Intercept.
                 + coefs$prox.normal*24)

probs <- data.frame(proximity = c(0, 4, 8, 12, 16, 20, 24),
                    prob = c(w0, w1, w2, w3, w4, w5, w6))
probs$percent <- probs$prob*100
probs
write.csv(probs, "exprobs_aizn_tu.csv")


#DW ----
#note that there are not enough exceedances to model most of these by community...or at all
#can probably only do Al - DHGM HW TU, As- HW, Cd - GM, Fe - TU, Mn - GM TU, Pb - TU, Zn - Pb Could also try Al with all communities

##aluminum ----
### dh ----
dwal.dh.0 <- glm(data = exdh[exdh$standard=="DW"&exdh$analyte=="Al",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal model
dwal.dh.1 <- glm(data = exdh[exdh$standard=="DW"&exdh$analyte=="Al",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(dwal.dh.1)
vif(dwal.dh.1)
check_model(dwal.dh.1)
exp(coef(dwal.dh.1))
performance(dwal.dh.1)
#season almost signif

dwal.dh.2 <- stepAIC(dwal.dh.0,scope = list(upper=dwal.dh.1), direction="both", trace = T)
summary(dwal.dh.2) #season almost signif
vif(dwal.dh.2)
check_model(dwal.dh.2) #not that normal
coefs <- data.frame(t(coef(dwal.dh.2)))
performance(dwal.dh.2)
#low r2 high rmse

### gm ----
dwal.gm.0 <- glm(data = exgm[exgm$standard=="DW"&exgm$analyte=="Al",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.gm model
dwal.gm.1 <- glm(data = exgm[exgm$standard=="DW"&exgm$analyte=="Al",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(dwal.gm.1)
vif(dwal.gm.1)
check_model(dwal.gm.1)
exp(coef(dwal.gm.1))
performance(dwal.gm.1)
#prox signif, model is not

dwal.gm.2 <- stepAIC(dwal.gm.0,scope = list(upper=dwal.gm.1), direction="both", trace = T)
summary(dwal.gm.2) #prox signif, model is not
vif(dwal.gm.2)
check_model(dwal.gm.2)
exp(coef(dwal.gm.2))
performance(dwal.gm.2)

### hw ----
dwal.hw.0 <- glm(data = exhw[exhw$standard=="DW"&exhw$analyte=="Al",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.hw model
dwal.hw.1 <- glm(data = exhw[exhw$standard=="DW"&exhw$analyte=="Al",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(dwal.hw.1)
vif(dwal.hw.1)
check_model(dwal.hw.1)
exp(coef(dwal.hw.1))
performance(dwal.hw.1)
#prox significant

dwal.hw.2 <- stepAIC(dwal.hw.0,scope = list(upper=dwal.hw.1), direction="both", trace = T)
summary(dwal.hw.2) #prox significant with a trend that makes sense
vif(dwal.hw.2)
check_model(dwal.hw.2)
coefs <- data.frame(t(coef(dwal.hw.2)))
performance(dwal.hw.2)


X1_range <- seq(from=0, to=round(max(exhw$proximity.km),0), by=.01)

winter <- coefs$X.Intercept. + coefs$prox.normal*X1_range
winter.prob <- exp(winter)/(1 + exp(winter))

plot.data <- data.frame(prob=winter.prob, X1=X1_range)

ggplot(plot.data, aes(x=X1, y=prob)) +
  geom_line(lwd=2, color = "slateblue") + 
  # scale_colour_poke(pokemon = "jigglypuff", spread = 2)+
  labs(title = "Probability of Al Exceedance of Drinking Water Standard",
       x = "\nDistance from Hayden/Winkelman Smelter (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "dwal_hw_ef.png", res=300, height=6, width=7, units="in")


### tu ----
dwal.tu.0 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Al",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.tu model
dwal.tu.1 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Al",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(dwal.tu.1)
vif(dwal.tu.1)
check_model(dwal.tu.1)
exp(coef(dwal.tu.1))
performance(dwal.tu.1)
#prox signif

dwal.tu.2 <- stepAIC(dwal.tu.0,scope = list(upper=dwal.tu.1), direction="both", trace = T)
summary(dwal.tu.2) #prox almost signif
vif(dwal.tu.2)
check_model(dwal.tu.2)
exp(coef(dwal.tu.2))
performance(dwal.tu.2)

##arsenic ----
### hw ----
dwas.hw.0 <- glm(data = exhw[exhw$standard=="DW"&exhw$analyte=="As",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal model
dwas.hw.1 <- glm(data = exhw[exhw$standard=="DW"&exhw$analyte=="As",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(dwas.hw.1)
vif(dwas.hw.1)
check_model(dwas.hw.1)
exp(coef(dwas.hw.1))
performance(dwas.hw.1)
#both signif

dwas.hw.2 <- stepAIC(dwas.hw.0,scope = list(upper=dwas.hw.1), direction="both", trace = T)
summary(dwas.hw.2) #both signif
vif(dwas.hw.2)
check_model(dwas.hw.2)
coefs <- data.frame(t(coef(dwas.hw.2)))
performance(dwas.hw.2)

X1_range <- seq(from=0, to=round(max(exhw$proximity.km),0), by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of As Exceedance of Drinking Water Standard",
       x = "\nDistance from Hayden/Winkelman Smelter (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "dwas_hw_ef.png", res=300, height=6, width=7, units="in")


##cadmium ----
### gm ----
dwcd.gm.0 <- glm(data = exgm[exgm$standard=="DW"&exgm$analyte=="Cd",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.gm model
dwcd.gm.1 <- glm(data = exgm[exgm$standard=="DW"&exgm$analyte=="Cd",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(dwcd.gm.1)
vif(dwcd.gm.1)
check_model(dwcd.gm.1)
exp(coef(dwcd.gm.1))
performance(dwcd.gm.1)
#neither signif

dwcd.gm.2 <- stepAIC(dwcd.gm.0,scope = list(upper=dwcd.gm.1), direction="both", trace = T)
summary(dwcd.gm.2) #neither signif
vif(dwcd.gm.2)
check_model(dwcd.gm.2)
exp(coef(dwcd.gm.2))
performance(dwcd.gm.2)


##iron ----
### tu ----
dwfe.tu.0 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Fe",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.tu model
dwfe.tu.1 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Fe",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(dwfe.tu.1)
vif(dwfe.tu.1)
check_model(dwfe.tu.1)
exp(coef(dwfe.tu.1))
performance(dwfe.tu.1)
#season signif

dwfe.tu.2 <- stepAIC(dwfe.tu.0,scope = list(upper=dwfe.tu.1), direction="both", trace = T)
summary(dwfe.tu.2) #season signif
vif(dwfe.tu.2)
check_model(dwfe.tu.2)
exp(coef(dwfe.tu.2))
performance(dwfe.tu.2)


##manganese ----
### gm ----
dwmn.gm.0 <- glm(data = exgm[exgm$standard=="DW"&exgm$analyte=="Mn",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.gm model
dwmn.gm.1 <- glm(data = exgm[exgm$standard=="DW"&exgm$analyte=="Mn",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(dwmn.gm.1)
vif(dwmn.gm.1)
check_model(dwmn.gm.1)
exp(coef(dwmn.gm.1))
performance(dwmn.gm.1)
#both signif

dwmn.gm.2 <- stepAIC(dwmn.gm.0,scope = list(upper=dwmn.gm.1), direction="both", trace = T)
summary(dwmn.gm.2) #both signif, trend makes sense
vif(dwmn.gm.2)
check_model(dwmn.gm.2)
coefs <- data.frame(t(coef(dwmn.gm.2)))
performance(dwmn.gm.2)

X1_range <- seq(from=0, to=round(max(exgm$proximity.km),0), by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of Mn Exceedance of Drinking Water Standard",
       x = "\nDistance from Globe/Miami Mine (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "dwmn_gm_ef.png", res=300, height=6, width=7, units="in")



### tu ----
dwmn.tu.0 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Mn",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.tu model
dwmn.tu.1 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Mn",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(dwmn.tu.1)
vif(dwmn.tu.1)
check_model(dwmn.tu.1)
exp(coef(dwmn.tu.1))
performance(dwmn.tu.1)
#both signif

dwmn.tu.2 <- stepAIC(dwmn.tu.0,scope = list(upper=dwmn.tu.1), direction="both", trace = T)
summary(dwmn.tu.2) #both signif, trend makes sense
vif(dwmn.tu.2)
check_model(dwmn.tu.2)
coefs <- data.frame(t(coef(dwmn.tu.2)))
performance(dwmn.tu.2)

X1_range <- seq(from=0, to=23, by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of Mn Exceedance of Drinking Water Standard",
       x = "\nDistance from Tucson Air Force Base (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "dwmn_tu_ef.png", res=300, height=6, width=7, units="in")


##lead ----
### tu ----
dwpb.tu.0 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Pb",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.tu model
dwpb.tu.1 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Pb",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(dwpb.tu.1)
vif(dwpb.tu.1)
check_model(dwpb.tu.1)
exp(coef(dwpb.tu.1))
performance(dwpb.tu.1)
#neither signif

dwpb.tu.2 <- stepAIC(dwpb.tu.0,scope = list(upper=dwpb.tu.1), direction="both", trace = T)
summary(dwpb.tu.2) #neither
vif(dwpb.tu.2)
check_model(dwpb.tu.2)
exp(coef(dwpb.tu.2))
performance(dwpb.tu.2)


##zinc ----
### tu ----
dwzn.tu.0 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Zn",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.tu model
dwzn.tu.1 <- glm(data = extu[extu$standard=="DW"&extu$analyte=="Zn",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(dwzn.tu.1)
vif(dwzn.tu.1)
check_model(dwzn.tu.1)
exp(coef(dwzn.tu.1))
performance(dwzn.tu.1)
#prox signif, higher further away from afb

dwzn.tu.2 <- stepAIC(dwzn.tu.0,scope = list(upper=dwzn.tu.1), direction="both", trace = T)
summary(dwzn.tu.2) #prox signif, trend makes sense
vif(dwzn.tu.2)
check_model(dwzn.tu.2)
coefs <- data.frame(t(coef(dwzn.tu.2)))
performance(dwzn.tu.2)
# 
# X1_range <- seq(from=0, to=23, by=.01)
# 
# winter <- coefs$X.Intercept. + coefs$prox.normal*X1_range
# winter.prob <- exp(winter)/(1 + exp(winter))
# 
# plot.data <- data.frame(prob=winter.prob, X1=X1_range)
# 
# ggplot(plot.data, aes(x=X1, y=prob)) +
#   geom_line(lwd=2, color = "slateblue") + 
#   # scale_colour_poke(pokemon = "jigglypuff", spread = 2)+
#   labs(title = "Probability of Zn Exceedance of Drinking Water Standard",
#        x = "\nDistance from Tucson Air Force Base (km)",
#        y = "Probability\n",
#        color = "Season") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 13),
#         plot.title = element_text(hjust=.5, face = "bold"),
#         plot.subtitle = element_text(hjust=.5),
#         axis.text = element_text(vjust = .5, color = "black"),
#         # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
#         legend.position="bottom",
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         axis.line.y = element_blank(),
#         axis.line.x = element_blank())
# dev.print(png, "dwzn_tu_ef.png", res=300, height=6, width=7, units="in")


#FB/PB ----
##lead ----
### tu ----
fpbpb.tu.0 <- glm(data = extu[extu$standard=="FB"&extu$analyte=="Pb",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.hw model
fpbpb.tu.1 <- glm(data = extu[extu$standard=="FB"&extu$analyte=="Pb",],
                 exceedance ~ season + prox.normal,
                 family = "binomial")
summary(fpbpb.tu.1)
vif(fpbpb.tu.1)
check_model(fpbpb.tu.1)
exp(coef(fpbpb.tu.1))
performance(fpbpb.tu.1)
#nothing signif

fpbpb.tu.2 <- stepAIC(fpbpb.tu.0,scope = list(upper=fpbpb.tu.1), direction="both", trace = T)
summary(fpbpb.tu.2) #nothing signif
vif(fpbpb.tu.2)
check_model(fpbpb.tu.2)
exp(coef(fpbpb.tu.2))
performance(fpbpb.tu.2)


#LDW ----
##arsenic ----
### hw ----
ldwas.hw.0 <- glm(data = exhw[exhw$standard=="LDW"&exhw$analyte=="As",],
                 exceedance ~ 1,
                 family = "binomial")
#maximal.hw model
ldwas.hw.1 <- glm(data = exhw[exhw$standard=="LDW"&exhw$analyte=="As",],
                 exceedance ~ prox.normal + season,
                 family = "binomial")
summary(ldwas.hw.1)
vif(ldwas.hw.1)
check_model(ldwas.hw.1)
exp(coef(ldwas.hw.1))
performance(ldwas.hw.1)
#both variables significant

ldwas.hw.2 <- stepAIC(ldwas.hw.0,scope = list(upper=ldwas.hw.1), direction="both", trace = T)
summary(ldwas.hw.2) #both variables significant with a trend that makes sense
vif(ldwas.hw.2)
check_model(ldwas.hw.2)
coefs <- data.frame(t(coef(ldwas.hw.2)))
performance(ldwas.hw.2)

X1_range <- seq(from=0, to=round(max(exhw$proximity.km),0), by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of As Exceedance of Livestock Drinking Water Standard",
       x = "\nDistance from Hayden/Winkelman Smelter (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "ldwas_hw_ef.png", res=300, height=6, width=7, units="in")


##iron ----
### tu ----
ldwfe.tu.0 <- glm(data = extu[extu$standard=="LDW"&extu$analyte=="Fe",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.tu model
ldwfe.tu.1 <- glm(data = extu[extu$standard=="LDW"&extu$analyte=="Fe",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(ldwfe.tu.1)
vif(ldwfe.tu.1)
check_model(ldwfe.tu.1)
exp(coef(ldwfe.tu.1))
performance(ldwfe.tu.1)
#season signif significant

ldwfe.tu.2 <- stepAIC(ldwfe.tu.0,scope = list(upper=ldwfe.tu.1), direction="both", trace = T)
summary(ldwfe.tu.2) #season signif
vif(ldwfe.tu.2)
check_model(ldwfe.tu.2)
exp(coef(ldwfe.tu.2))
performance(ldwfe.tu.2)


## manganese ----
### gm ----
ldwmn.gm.0 <- glm(data = exgm[exgm$standard=="LDW"&exgm$analyte=="Mn",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.gm model
ldwmn.gm.1 <- glm(data = exgm[exgm$standard=="LDW"&exgm$analyte=="Mn",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(ldwmn.gm.1)
vif(ldwmn.gm.1)
check_model(ldwmn.gm.1)
exp(coef(ldwmn.gm.1))
performance(ldwmn.gm.1)
#both signif

ldwmn.gm.2 <- stepAIC(ldwmn.gm.0,scope = list(upper=ldwmn.gm.1), direction="both", trace = T)
summary(ldwmn.gm.2) #both signif, trend makes sense
vif(ldwmn.gm.2)
check_model(ldwmn.gm.2)
coefs <- data.frame(t(coef(ldwmn.gm.2)))
performance(ldwmn.gm.2)

X1_range <- seq(from=0, to=round(max(exgm$proximity.km),0), by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of Mn Exceedance of Livestock Drinking Water Standard",
       x = "\nDistance from Globe/Miami Mine (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "ldwmn_gm_ef.png", res=300, height=6, width=7, units="in")


### tu ----
ldwmn.tu.0 <- glm(data = extu[extu$standard=="LDW"&extu$analyte=="Mn",],
                  exceedance ~ 1,
                  family = "binomial")
#maximal.tu model
ldwmn.tu.1 <- glm(data = extu[extu$standard=="LDW"&extu$analyte=="Mn",],
                  exceedance ~ prox.normal + season,
                  family = "binomial")
summary(ldwmn.tu.1)
vif(ldwmn.tu.1)
check_model(ldwmn.tu.1)
exp(coef(ldwmn.tu.1))
performance(ldwmn.tu.1)
#both signif

ldwmn.tu.2 <- stepAIC(ldwmn.tu.0,scope = list(upper=ldwmn.tu.1), direction="both", trace = T)
summary(ldwmn.tu.2) #both signif, trend makes sense
vif(ldwmn.tu.2)
check_model(ldwmn.tu.2)
coefs <- data.frame(t(coef(ldwmn.tu.2)))
performance(ldwmn.tu.2)

X1_range <- seq(from=0, to=23, by=.01)

winter <- coefs$X.Intercept. + coefs$seasonMonsoon*0 + coefs$prox.normal*X1_range
monsoon <- coefs$X.Intercept. + coefs$seasonMonsoon*1 + coefs$prox.normal*X1_range

winter.prob <- exp(winter)/(1 + exp(winter))
monsoon.prob <- exp(monsoon)/(1 + exp(monsoon))

plot.data <- data.frame(winter=winter.prob, monsoon=monsoon.prob, X1=X1_range)
plot.data <- gather(plot.data, key=group, value=prob, winter:monsoon)

ggplot(plot.data, aes(x=X1, y=prob, color=group)) +
  geom_line(lwd=2) + 
  scale_colour_poke(pokemon = "charizard", spread = 2)+
  labs(title = "Probability of Mn Exceedance of Livestock Drinking Water Standard",
       x = "\nDistance from Tucson Air Force Base (km)",
       y = "Probability\n",
       color = "Season") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        # axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "ldwmn_tu_ef.png", res=300, height=6, width=7, units="in")

#scratch models ----
##copper----
#null model
aicu.0 <- glm(data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Cu",],
              exceedance ~ 1,
              family = "binomial")
#maximal model
aicu.1 <- glm(data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Cu",],
          exceedance ~ season+community+proximity.km+community:proximity.km,
          family = "binomial")
summary(aicu.1)
vif(aicu.1)
check_model(aicu.1)
exp(coef(aicu.1))
performance(aicu.1)
#controlling for season and community, proximity to point source doesnt show a substantial or significant influence on odds of AI Cu exceedance

aicu.2 <- stepAIC(aicu.0,scope = list(upper=aicu.1), direction="both", trace = T)
summary(aicu.2) #no significant community effect
vif(aicu.2)
check_model(aicu.2)
exp(coef(aicu.2))
performance(aicu.2)

#get confidence intervals for estimates
se <- sqrt(diag(vcov(aicumm.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(aicumm.1), LL = fixef(aicumm.1) - 1.96 * se, UL = fixef(aicumm.1) + 1.96 *
                se))
exp(tab)


##zinc ----
aizn.0 <- glm(data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Zn",],
              exceedance ~ 1,
              family = "binomial")

aizn.1 <- glm(data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Zn",],
              exceedance ~ season+community+proximity.km+community:proximity.km,
              family = "binomial")
summary(aizn.1)
vif(aizn.1)
check_model(aizn.1)
exp(coef(aizn.1))
performance(aizn.1)
#controlling for season and community, proximity to point source doesnt show a substantial or significant influence on odds of AI zn exceedance

aizn.2 <- step(aizn.0,scope = list(upper=aizn.1), direction="both",test="Chisq", trace = F)
summary(aizn.2) #no significant community effect
vif(aizn.2)
check_model(aizn.2)
exp(coef(aizn.2))
performance(aizn.2)

#predict gives the predicted value in terms of logits
plot.dat <- data.frame(prob = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Zn",]$exceedance/ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Zn",]$proximity.km,
                       proximity = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Zn",]$proximity.km,
                       fit = predict(aicu.2, ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Zn",]))
#convert those logit values to probabilities
plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))

ggplot(plot.dat, aes(x=proximity, y=prob)) + 
  geom_point() +
  geom_line(aes(x=proximity, y=fit_prob))

view(plot.dat)

# exp(0.003453115 + (1.313339251*5))/(1+(exp(0.003453115 + (1.313339251*5))))
# exp(0.003453115)/(1+(exp(0.003453115))) #probability of exceedance is %50


e2 <- glm(data = ex.dat.long[ex.dat.long$standard=="AI"&ex.dat.long$analyte=="Cu",],
          exceedance ~ season,
          family = "binomial")
summary(e2)
check_model(e2)
exp(coef(e2))
performance(e2)

anova(e1, e2)

exp(-4.7431 + .8778*1)/(1+(exp(-4.7431 + .8778*1)))
exp(-4.7431)/(1+(exp(-4.7431)))

                                     
                                     
#odds of having an AI exceedance during winter is 


#Functions ----
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


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
  sumtable <- ex.dat.long %>%
    group_by(across(all_of(cols))) %>%
    summarise(n = n(),
              exceedances_n = sum(.data[[value]]),
              exceedances_freq = signif(sum(.data[[value]])/n()*100, 2))
  
  sumtable.small <- subset(sumtable, select = -c(exceedances_n))
  
  sumtable.wide <- pivot_wider(data = sumtable.small,
                               names_from = "standard",
                               values_from = "exceedances_freq")
  #save as csv file in your working directory
  write.csv(sumtable.wide, paste(filename,"_sum.csv", sep = ""))
  
  #copy to new dataframe with a unique name and place in global environment
  assign(paste(dfname), sumtable.wide, envir=.GlobalEnv)  
}


#Scratch Work ----
predict(aicu.hw.2, type = "response")

#winter, 0km
exp(coefs$X.Intercept. + coefs$seasonMonsoon*0
    + coefs$prox.normal*0)/
  (1+(exp(coefs$X.Intercept.+coefs$seasonMonsoon*0
          + coefs$prox.normal*0)))
#winter, .5km
exp(coefs$X.Intercept. + coefs$seasonMonsoon*0
    + coefs$prox.normal*.5)/
  (1+(exp(coefs$X.Intercept.+coefs$seasonMonsoon*0
          + coefs$prox.normal*.5)))
#winter, 1km
exp(coefs$X.Intercept. + coefs$seasonMonsoon*0
    + coefs$prox.normal*1)/
  (1+(exp(coefs$X.Intercept.+coefs$seasonMonsoon*0
          + coefs$prox.normal*1)))
#winter, 1.5km
exp(coefs$X.Intercept. + coefs$seasonMonsoon*0
    + coefs$prox.normal*1.5)/
  (1+(exp(coefs$X.Intercept.+coefs$seasonMonsoon*0
          + coefs$prox.normal*1.5)))
#winter, 2km
exp(coefs$X.Intercept. + coefs$seasonMonsoon*0
    + coefs$prox.normal*2)/
  (1+(exp(coefs$X.Intercept.+coefs$seasonMonsoon*0
          + coefs$prox.normal*2)))

#monsoon, 0km
exp(coefs$X.Intercept. + coefs$seasonMonsoon*1
    + coefs$prox.normal*0)/
  (1+(exp(coefs$X.Intercept.+coefs$seasonMonsoon*1
          + coefs$prox.normal*0)))

glm.fit.prob=predict(glm.fit, newdata = test2, type="response")


plot(allEffects(aicu.hw.2, typical=median))
plot(Effect(focal.predictors = c("prox.normal","season"), 
            mod = aicu.hw.2,
            xlevels=list(prox.normal=seq(0, 2, .5))),
     multiline = TRUE)

hw_logit_seq <- seq(0, 2, by = .5)
prob_seq <- round(logit2prob(hw_logit_seq), 3)


df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq)

#null model
aicumm.0 <- glmer(data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Cu",],
                  exceedance ~ 1 +
                    (1|site),
                  family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(aicumm.0)

#max model
aicumm.1 <- glmer(data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Cu",],
                  exceedance ~ season+community+proximity.km +
                    (1|site),
                  family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(aicumm.1)
aod(aicumm.1)
#get confidence intervals for estimates
se <- sqrt(diag(vcov(aicumm.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(aicumm.1), LL = fixef(aicumm.1) - 1.96 * se, UL = fixef(aicumm.1) + 1.96 *
                se))
exp(tab)


aicumm.2 <- stepAIC(aicumm.1, direction="both", trace = F,
                    data = ex.dat.long.prox[ex.dat.long.prox$standard=="AI"&ex.dat.long.prox$analyte=="Cu",])



summarize(n = n(),
            min = min(.data[[exceedance]]),
            max = max(.data[[exceedance]]),
            median = median(.data[[exceedance]]),
            mean = mean(.data[[exceedance]]),
            sd = sd(.data[[exceedance]])
            ,
            gmean = geoMean(.data[[exceedance]]),
            gsd = geoSD(.data[[exceedance]])
  )


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



ex.dat$`PBex` <- ifelse(ex.dat$value > ex.dat$PB, 1,0)
ex.dat$`FBex` <- ifelse(ex.dat$value > ex.dat$FB, 1,0)
ex.dat$`DWex`  <- ifelse(ex.dat$value > ex.dat$DW, 1,0)
ex.dat$`AIex`  <- ifelse(ex.dat$value > ex.dat$AI, 1,0)
ex.dat$`LDWex`  <- ifelse(ex.dat$value > ex.dat$LDW, 1,0)

ex.dat.long <- pivot_longer(data = ex.dat,
                            cols = PBex:LDWex,
                            names_to = "standard",
                            values_to = "exceedance")