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

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data
hds <- read_xlsx("data/data_processing/IO_HDS.xlsx", sheet = "stats", col_names = TRUE)
hds.long<- pivot_longer(hds, col_names= TRUE)
#As Q1 What is your roof made of? ----
q1 <- hds
q1 <- q1[!is.na(q1$Q1),]
top5 <- c("Flat BUR (Reflective)", "Asphalt Shingle", "Flat BUR (Tar/Gravel)", "Wood Shakes/Shingles", "Metal Panel")
q1 <- filter(q1, Q1 %in% top5)
q1dat <- full_join(iw.dm, q1, by = c("site"))
q1dat <- q1dat[!is.na(q1dat$Q1),]
q1dat <- q1dat[!is.na(q1dat$community),]

q1dat$Q1 <- factor(q1dat$Q1, levels = c("Flat BUR (Tar/Gravel)","Flat BUR (Reflective)", "Asphalt Shingle", "Wood Shakes/Shingles", "Metal Panel"))

#boxplot(q1dat$As ~ q1dat$Q1+q1dat$community)
#As1 <- aov(log(q1dat$As) ~ q1dat$Q1 + q1dat$community)
#TukeyHSD(As1)

#boxplots of q1 by contaminants=====
Contamlist <- list("Al", "Cd", "Mn", "Ni", "Zn")
ggplot(data = q1dat, mapping = aes(y = Al, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_rooftypecomm.png", res=100, height=6, width=8, units="in")

ggplot(data = q1dat, mapping = aes(y = Al, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_rooftype.png", res=100, height=6, width=8, units="in")

Al0 <- lmer(log(Al)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Al1 <- lmer(log(Al)~ Q1 + community + period + season
            +Q1:community + Q1:period + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Al1)
vif(As1)

As2 <- lmer(log(As)~ Q1 + community + period + season
            + Q1:period + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(As2)
vif(As2)

As3 <- lmer(log(As)~ Q1 + community + period + season
            + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(As3)
vif(As3)

As4 <- lmer(log(As)~ Q1 + community + period + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(As4)
vif(As4)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(As5)
anova(As4, As5)


#Cd-Q1=====
ggplot(data = q1dat, mapping = aes(y = Cd, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Cd_rooftypecomm.png", res=100, height=6, width=8, units="in")

ggplot(data = q1dat, mapping = aes(y = Cd, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Al_rooftype.png", res=100, height=6, width=8, units="in")


#write.csv(q1dat, "q1dat.csv")


#As Q9 Approximately, when was your home built? ---- 
q9 <- hds
q9$Q9 <- as.character(q9$Q9)
q9 <- q9[q9$Q9!="0",]
q9 <- q9[!is.na(q9$Q9),]
q9[q9$Q9=="1",]$Q9 <- "Pre 1940"
q9[q9$Q9=="2",]$Q9 <- "1941-1949"
q9[q9$Q9=="3",]$Q9 <- "1950-1959"
q9[q9$Q9=="4",]$Q9 <- "1960-1969"
q9[q9$Q9=="5",]$Q9 <- "1970-1979"
q9[q9$Q9=="6",]$Q9 <- "1980-1989"
q9[q9$Q9=="7",]$Q9 <- "1990-1999"
q9[q9$Q9=="8",]$Q9 <- "2000-2009"
q9[q9$Q9=="9",]$Q9 <- "2010-2018"

q9[q9$Q9=="1",]$Q9 <- "1940"
q9[q9$Q9=="2",]$Q9 <- "1945"
q9[q9$Q9=="3",]$Q9 <- "1955"
q9[q9$Q9=="4",]$Q9 <- "1965"
q9[q9$Q9=="5",]$Q9 <- "1975"
q9[q9$Q9=="6",]$Q9 <- "1985"
q9[q9$Q9=="7",]$Q9 <- "1995"
q9[q9$Q9=="8",]$Q9 <- "2005"
q9[q9$Q9=="9",]$Q9 <- "2015"

q9$Q9 <- as.numeric(q9$Q9)
q9$age2021 <- 2021 - q9$Q9
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

ggplot(data = q9dat, mapping = aes(y = As, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Approximately, when was your home built?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = As, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Approximately, when was your home built?")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "As_houseagecommssn.png", res=100, height=6, width=12, units="in")


As0 <- lmer(log(As)~
              + (1|community:site),
            data = q9dat,
            REML = T)
As1 <- lmer(log(As)~ Q9 + community + period + season
            +Q9:community + Q9:period + Q9:season
            + (1|community:site),
            data = q9dat,
            REML = T)
summary(As1)
vif(As1)

As2 <- lmer(log(As)~ Q9 + community + period + season
            + Q9:period + Q9:season
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(As2)
vif(As2)

As3 <- lmer(log(As)~ Q9 + community + period + season
            + Q9:season
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(As3)
vif(As3)

As4 <- lmer(log(As)~ Q9 + community + period + season
            + (1|community:site),
            data = q9dat,
            REML = T)
summary(As4)
vif(As4)
anova(As4)
performance(As4)
As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(As5)
anova(As4, As5)
AIC(As0)
AIC(As1)
performance(As4)

dev.print(png, "As_homeageCheck.png", res=100, height=10, width=20, units="in")

model.effects <- allEffects(As4)
plot(model.effects)

as.sum <- summary(As4)
write.csv(as.sum$coefficients, "As4coefhomeageRHI.csv")
as.means <- lsmeans(As4, pairwise~Q9, adjust = "tukey")#
write.csv(as.means$lsmeans, "As4lsmeanshageRHI.csv")
write.csv(as.means$contrasts, "As4contrastshageRHI.csv")

predict.dat <- ggeffect(model = As4,
                        terms = c("Q9"),
                        back.transform = F,
                        type = "re")
predict.dat$Q9 <- predict.dat$x

plot <- ggplot(data= q9dat,
               mapping = aes(x=Q9,y=log(As))) +
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
dev.print(png, "As_rhihomeageln.png", res=300, height=4, width=7, units="in")




ggplot(data = q9dat, aes(x = Q9, y = log(As)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "As by Approximate Home Age\n",
       y = "ln(As) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
# 
# model.effects.as <- ggeffect(model = As4,
#                           back.transform = F,
#                           type = "re")
# hdseffect.as <- model.effects.as$Q9
# hdseffect.as$x <- factor(hdseffect.as$x, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))
# # 
# ggplot(data= hdseffect.df,
#        mapping =(aes(x=Q9,y=fit, ymin = lower, ymax=upper))) +
#   geom_pointrange(position=position_dodge(width=0.75), size = 1) +
#   xlab("Q9") + 
#   ylab("Estimated ln([As]) (ln(ug/L))") + 
#   labs(title="Modeled Effect of Home Age", subtitle = "This figure displays modeled, not real data")+
#   theme_bw()
# dev.print(png, "As_homeageEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q9dat, "q9dat.csv")

#As Q65 How old is your cistern? ----
q65 <- hds
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

q65[q65$Q65=="2",]$Q65 <- "0-2 years"
q65[q65$Q65=="3",]$Q65 <- "0-2 years"
q65[q65$Q65=="4",]$Q65 <- "0-2 years"
q65[q65$Q65=="5",]$Q65 <- "2-4 years"
q65[q65$Q65=="6",]$Q65 <- "2-4 years"
q65[q65$Q65=="7",]$Q65 <- "5+ years"

q65$Q65 <- factor(q65$Q65, levels = c("0-2 years", "2-4 years", "5+ years"))
summary(q65$Q65)
q65dat <- full_join(iw.dm, q65, by = c("site"))
q65dat <- q65dat[!is.na(q65dat$Q65),]
q65dat <- q65dat[!is.na(q65dat$community),]

summary(as.factor(q65dat$Q65))

ggplot(data = q65dat, mapping = aes(y = As, x = Q65))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "How old is your cistern?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = As, x = Q65, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "How old is your cistern?")+
  theme_bw() +   theme(legend.position = "bottom") + facet_wrap(.~samplings)
dev.print(png, "As_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

As0 <- lmer(log(As)~
              + (1|community:site),
            data = q65dat,
            REML = F)
As1 <- lmer(log(As)~ Q65 + community + period + season
            +Q65:community + Q65:period + Q65:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(As1)
vif(As1)

As2 <- lmer(log(As)~ Q65:community + community + period + season
            + Q65:period + Q65:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(As2)
vif(As2)

As3 <- lmer(log(As)~ Q65:period  + community + period + season
            + Q65:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q65:season + community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)

As6 <- lmer(log(As)~ Q65+community + period + season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(As6)
anova(As6)

anova(As4, As5)
anova(As4, As6)
anova(As5, As6)

compare_performance(As4, As5)
check_model(As4)
model.effects <- allEffects(As6)
plot(model.effects)

as.sum <- summary(As6)
write.csv(as.sum$coefficients, "As4coefhomeage.csv")
as.means <- lsmeans(As6, pairwise~Q65, adjust = "tukey")#
write.csv(as.means$lsmeans, "Pb7lsmeansssnHW.csv")
write.csv(as.means$contrasts, "Pb7contrastsssnHW.csv")

predict.dat <- ggeffect(model = As4,
                        terms = c("Q9"),
                        back.transform = F,
                        type = "re")

ggplot(data = q9dat, aes(x = Q9, y = log(As)))+
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
       mapping =(aes(x=Q65,y=fit, ymin = lower, ymax=upper))) +
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


ggplot(data = q60dat, mapping = aes(y = As, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = As, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_cisterntypecomm.png", res=100, height=6, width=8, units="in")

As0 <- lmer(log(As)~
              + (1|community:site),
            data = q60dat,
            REML = F)
As1 <- lmer(log(As)~ Q60 + community + period + season
            +Q60:community + Q60:period + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(As1)
vif(As1)

As2 <- lmer(log(As)~ Q60:community + community + period + season
            + Q60:period + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(As2)
vif(As2)

As3 <- lmer(log(As)~ Q60:period  + community + period + season
            + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q60:period + community + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)

As6 <- lmer(log(As)~ Q60+community + period + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(As6)
anova(As6)

anova(As4, As5)
anova(As4, As6)

ggplot(data= hdseffect.df,
       mapping =(aes(x=Q65,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q65") + 
  ylab("Estimated ln([As]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Cistern Type", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "As_cisterntypeEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q60dat, "q60dat.csv")


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

ggplot(data = q76dat, mapping = aes(y = As, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = As, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "As_firstflushcomm.png", res=100, height=6, width=6, units="in")


As0 <- lmer(log(As)~
              + (1|community:site),
            data = q76dat,
            REML = F)
As1 <- lmer(log(As)~ Q76 + community + period + season
            +Q76:community + Q76:period + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(As1)
vif(As1)

As2 <- lmer(log(As)~ Q76 + community + period + season
            + Q76:period + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(As2)
vif(As2)

As3 <- lmer(log(As)~ Q76 + community + period + season
            + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q76 + community + period + season
            + (1|community:site),
            data = q76dat,
            REML = F)
vif(As4)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q76dat,
            REML = F)

anova(As4, As5)

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

ggplot(data = q77dat, mapping = aes(y = As, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = As, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_screencomm.png", res=100, height=6, width=6, units="in")



As0 <- lmer(log(As)~
              + (1|community:site),
            data = q77dat,
            REML = F)
As1 <- lmer(log(As)~ Q77 + community + period + season
            +Q77:community + Q77:period + Q77:season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(As1)
vif(As1)

As2 <- lmer(log(As)~ Q77 + community + period + season
            + Q77:period + Q77:season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(As2)
vif(As2)
anova(As2)
As3 <- lmer(log(As)~ Q77 + community + period + season
            + Q77:season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q77 + community + period + season
            + (1|community:site),
            data = q77dat,
            REML = F)
vif(As4)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q77dat,
            REML = F)

anova(As4, As5)
anova(As2, As4)
anova(As2, As5)

ggplot(data= hdseffect.df,
       mapping =(aes(x=Q77,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q77") + 
  ylab("Estimated ln([As]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Cistern Screen", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "As_screenEffect.png", res=100, height=5, width=7, units="in")



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

ggplot(data = q44dat, mapping = aes(y = As, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "2 blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = As, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "2 blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_2blockscomm.png", res=100, height=6, width=6, units="in")


As0 <- lmer(log(As)~
              + (1|community:site),
            data = q44dat,
            REML = F)
As1 <- lmer(log(As)~ Q44 + community + period + season
            +Q44:community + Q44:period + Q44:season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(As1)
vif(As1)
anova(As1)
As2 <- lmer(log(As)~ Q44 + community + period + season
            + Q44:period + Q44:season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(As2)
vif(As2)
anova(As2)
As3 <- lmer(log(As)~ Q44 + community + period + season
            + Q44:season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q44:season + community + period + season
            + (1|community:site),
            data = q44dat,
            REML = F)
vif(As4)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q44dat,
            REML = F)

anova(As4, As5)
anova(As2, As4)
anova(As2, As5)

#As Q13 Has your home ever tested positive for lead in the paint? ----
q13 <- hds
q13$Q13 <- as.character(q13$Q13)
q13 <- q13[q13$Q13!="0",]
q13 <- q13[!is.na(q13$Q13),]
q13[q13$Q13=="1",]$Q13 <- "Yes"
q13[q13$Q13=="2",]$Q13 <- "No"

q13$Q13 <- factor(q13$Q13, levels = c("Yes", "No"))
summary(q13$Q13)
q13dat <- full_join(iw.dm, q13, by = c("site"))
q13dat <- q13dat[!is.na(q13dat$Q13),]
q13dat <- q13dat[!is.na(q13dat$community),]

summary(as.factor(q13dat$Q13))

ggplot(data = q13dat, mapping = aes(y = As, x = Q13))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Tested Positive for Lead in Paint?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_paint.png", res=100, height=6, width=6, units="in")

ggplot(data = q13dat, mapping = aes(y = As, x = Q13, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Tested Positive for Lead in Paint?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_paintcomm.png", res=100, height=6, width=6, units="in")


As0 <- lmer(log(As)~
              + (1|community:site),
            data = q13dat,
            REML = F)
As1 <- lmer(log(As)~ Q13 + community + period + season
            +Q13:community + Q13:period + Q13:season
            + (1|community:site),
            data = q13dat,
            REML = F)
summary(As1)
vif(As1)
anova(As1)
As2 <- lmer(log(As)~ Q13 + community + period + season
            + Q13:period + Q13:community
            + (1|community:site),
            data = q13dat,
            REML = F)
summary(As2)
vif(As2)
anova(As2)
As3 <- lmer(log(As)~ Q13 + community + period + season
            + Q13:community
            + (1|community:site),
            data = q13dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q13 + community + period + season
            + (1|community:site),
            data = q13dat,
            REML = F)
vif(As4)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q13dat,
            REML = F)

As6 <- lmer(log(As)~ Q13:community+community + period + season
            + (1|community:site),
            data = q13dat,
            REML = F)

anova(As4, As5)
anova(As2, As5)
anova(As4, As6)
anova(As5, As6)

#As Q18 Is the outside paint chipping or falling off? ----
q18 <- hds
q18$Q18 <- as.character(q18$Q18)
q18 <- q18[q18$Q18!="0",]
q18 <- q18[!is.na(q18$Q18),]
q18[q18$Q18=="1",]$Q18 <- "Yes"
q18[q18$Q18=="2",]$Q18 <- "No"

q18$Q18 <- factor(q18$Q18, levels = c("Yes", "No"))
summary(q18$Q18)
q18dat <- full_join(iw.dm, q18, by = c("site"))
q18dat <- q18dat[!is.na(q18dat$Q18),]
q18dat <- q18dat[!is.na(q18dat$community),]

summary(as.factor(q18dat$Q18))
ggplot(data = q18dat, mapping = aes(y = As, x = Q18))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Paint chips falling outside?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_chips.png", res=100, height=6, width=6, units="in")

ggplot(data = q18dat, mapping = aes(y = As, x = Q18, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Paint chips falling outside?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "As_chipscomm.png", res=100, height=6, width=6, units="in")


As0 <- lmer(log(As)~
              + (1|community:site),
            data = q18dat,
            REML = F)
As1 <- lmer(log(As)~ Q18 + community + period + season
            +Q18:community + Q18:period + Q18:season
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(As1)
vif(As1)
anova(As1)
As2 <- lmer(log(As)~ Q18 + community + period + season
            + Q18:period + Q18:season
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(As2)
vif(As2)
anova(As2)
As3 <- lmer(log(As)~ Q18 + community + period + season
            + Q18:period
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(As3)
vif(As3)
anova(As3)
As4 <- lmer(log(As)~ Q18 + community + period + season
            + (1|community:site),
            data = q18dat,
            REML = F)
vif(As4)
summary(As4)
anova(As4, As3)
anova(As4)

As5 <- lmer(log(As)~ community + period + season
            + (1|community:site),
            data = q18dat,
            REML = F)

anova(As4, As5)


#Pb Q1 What is your roof made of? ----
q1 <- hds
q1 <- q1[!is.na(q1$Q1),]
top5 <- c("Flat BUR (Reflective)", "Asphalt Shingle", "Flat BUR (Tar/Gravel)", "Wood Shakes/Shingles", "Metal Panel")
q1 <- filter(q1, Q1 %in% top5)
q1dat <- full_join(iw.dm, q1, by = c("site"))
q1dat <- q1dat[!is.na(q1dat$Q1),]
q1dat <- q1dat[!is.na(q1dat$community),]

q1dat$Q1 <- factor(q1dat$Q1, levels = c("Flat BUR (Tar/Gravel)","Flat BUR (Reflective)", "Asphalt Shingle", "Wood Shakes/Shingles", "Metal Panel"))

# boxplot(q1dat$Pb ~ q1dat$Q1+q1dat$community)
#Pb1 <- aov(log(q1dat$Pb) ~ q1dat$Q1 + q1dat$community)
#TukeyHSD(Pb1)

ggplot(data = q1dat, mapping = aes(y = Pb, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_rooftypecomm.png", res=100, height=6, width=8, units="in")

ggplot(data = q1dat, mapping = aes(y = Pb, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_rooftype.png", res=100, height=6, width=8, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q1 + community  + season
            +Q1:community + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb1)
vif(Pb1)

Pb2 <- lmer(log(Pb)~ Q1 + community + season
            + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb2)
vif(Pb2)

Pb3 <- lmer(log(Pb)~ Q1 + community + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb4)
vif(Pb4)
anova(Pb4)

anova(Pb4, Pb3)

#write.csv(q1dat, "q1dat.csv")


#Pb Q9 Approximately, when was your home built? ---- 
q9 <- hds
q9$Q9 <- as.character(q9$Q9)
q9 <- q9[q9$Q9!="0",]
q9 <- q9[!is.na(q9$Q9),]
q9[q9$Q9=="1",]$Q9 <- "Pre 1940"
q9[q9$Q9=="2",]$Q9 <- "1941-1949"
q9[q9$Q9=="3",]$Q9 <- "1950-1959"
q9[q9$Q9=="4",]$Q9 <- "1960-1969"
q9[q9$Q9=="5",]$Q9 <- "1970-1979"
q9[q9$Q9=="6",]$Q9 <- "1980-1989"
q9[q9$Q9=="7",]$Q9 <- "1990-1999"
q9[q9$Q9=="8",]$Q9 <- "2000-2009"
q9[q9$Q9=="9",]$Q9 <- "2010-2018"

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

ggplot(data = q9dat, mapping = aes(y = Pb, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Approximately, when was your home built?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Pb, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Approximately, when was your home built?")+
  theme_bw() +   theme(legend.position = "bottom") +
  facet_grid(.~season)
dev.print(png, "Pb_houseagecommssn.png", res=100, height=6, width=12, units="in")


Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q9dat,
            REML = T)
Pb1 <- lmer(log(Pb)~ Q9 + community + season
            +Q9:community + Q9:season
            + (1|community:site),
            data = q9dat,
            REML = T)
summary(Pb1)
vif(Pb1)

Pb2 <- lmer(log(Pb)~ Q9 + community + season
            + Q9:season
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb2)
vif(Pb2)

Pb3 <- lmer(log(Pb)~ Q9 + community + season
            + (1|community:site),
            data = q9dat,
            REML = T)
summary(Pb3)
vif(Pb3)
performance(Pb3)

Pb4 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q9dat,
            REML = T)
summary(Pb4)
anova(Pb4, Pb3)

check_model(Pb4)
dev.print(png, "Pb4_homeageCheck.png", res=100, height=10, width=20, units="in")

model.effects <- allEffects(Pb3)
plot(model.effects)

as.sum <- summary(Pb3)
write.csv(as.sum$coefficients, "Pb3coefhomeageRHI.csv")
as.means <- lsmeans(Pb3, pairwise~Q9, adjust = "tukey")#
write.csv(as.means$lsmeans, "Pb3lsmeanshageRHI.csv")
write.csv(as.means$contrasts, "Pb3contrastshageRHI.csv")

predict.dat <- ggeffect(model = Pb3,
                        terms = c("Q9"),
                        back.transform = F,
                        type = "re")

predict.dat$Q9 <- predict.dat$x

plot <- ggplot(data= q9dat,
               mapping = aes(x=Q9,y=log(Pb))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~Q9, scales = "free")+
  geom_pointrange(data = predict.dat, aes(x=Q9, y = predicted, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nHome Age",
       y = expression(paste("ln[Pb] (µg ",L^-1, ")")))+
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
dev.print(png, "Pb_rhihomeageln.png", res=300, height=4, width=7, units="in")



ggplot(data = q9dat, aes(x = Q9, y = log(Pb)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Pb by Approximate Home Age\n",
       y = "ln(Pb) [µg/L]\n",
       x = "\n Home Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Pb_rhihomeageln.png", res=100, height=6, width=10, units="in")
# 
# model.effects.as <- ggeffect(model = Pb4,
#                           back.transform = F,
#                           type = "re")
# hdseffect.as <- model.effects.as$Q9
# hdseffect.as$x <- factor(hdseffect.as$x, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))
# # 
# ggplot(data= hdseffect.df,
#        mapping =(aes(x=Q9,y=fit, ymin = lower, ymax=upper))) +
#   geom_pointrange(position=position_dodge(width=0.75), size = 1) +
#   xlab("Q9") + 
#   ylab("Estimated ln([Pb]) (ln(ug/L))") + 
#   labs(title="Modeled Effect of Home Age", subtitle = "This figure displays modeled, not real data")+
#   theme_bw()
# dev.print(png, "Pb_homeageEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q9dat, "q9dat.csv")

#Pb Q65 How old is your cistern? ----
q65 <- hds
q65$Q65 <- as.character(q65$Q65)
q65 <- q65[q65$Q65!="0",]
q65 <- q65[q65$Q65!="1",]
q65 <- q65[!is.na(q65$Q65),]
# q65[q65$Q65=="2",]$Q65 <- "<1 year"
# q65[q65$Q65=="3",]$Q65 <- "<1 year"
# q65[q65$Q65=="4",]$Q65 <- "1-2 years"
# q65[q65$Q65=="5",]$Q65 <- "2-3 years"
# q65[q65$Q65=="6",]$Q65 <- "3-4 years"
# q65[q65$Q65=="7",]$Q65 <- "5+ years"
# 
# q65$Q65 <- factor(q65$Q65, levels = c("<1 year", "1-2 years", "2-3 years", "3-4 years", "5+ years"))

q65[q65$Q65=="2",]$Q65 <- "0-2 years"
q65[q65$Q65=="3",]$Q65 <- "0-2 years"
q65[q65$Q65=="4",]$Q65 <- "0-2 years"
q65[q65$Q65=="5",]$Q65 <- "2-4 years"
q65[q65$Q65=="6",]$Q65 <- "2-4 years"
q65[q65$Q65=="7",]$Q65 <- "5+ years"

q65$Q65 <- factor(q65$Q65, levels = c("0-2 years", "2-4 years", "5+ years"))

q65dat <- full_join(iw.dm, q65, by = c("site"))
q65dat <- q65dat[!is.na(q65dat$Q65),]
q65dat <- q65dat[!is.na(q65dat$community),]

summary(as.factor(q65dat$Q65))

ggplot(data = q65dat, mapping = aes(y = Pb, x = Q65))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "How old is your cistern?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Pb, x = Q65, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "How old is your cistern?")+
  theme_bw() +   theme(legend.position = "bottom") + facet_wrap(.~samplings)
dev.print(png, "Pb_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q65dat,
            REML = T)
Pb1 <- lmer(log(Pb)~ Q65 + community + season
            +Q65:community+ Q65:season
            + (1|community:site),
            data = q65dat,
            REML = T)
summary(Pb1)
vif(Pb1)

Pb2 <- lmer(log(Pb)~ Q65 + community + season
            + Q65:season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
Pb3 <- lmer(log(Pb)~ Q65 + community + season
            + (1|community:site),
            data = q65dat,
            REML = T)
summary(Pb3)
vif(Pb3)
anova(Pb3)
performance(Pb3)

anova(Pb2, Pb3)
Pb4 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q65dat,
            REML = T)
summary(Pb4)
anova(Pb4, Pb3)


check_model(Pb4)
dev.print(png, "Pb4_cisternageCheck.png", res=100, height=10, width=20, units="in")

model.effects <- allEffects(Pb3)
plot(model.effects)

as.sum <- summary(Pb3)
write.csv(as.sum$coefficients, "Pb3coefcageRHI.csv")
as.means <- lsmeans(Pb3, pairwise~Q65, adjust = "tukey")#
write.csv(as.means$lsmeans, "Pb3lsmeanscageRHI.csv")
write.csv(as.means$contrasts, "Pb3contrastscageRHI.csv")

predict.dat <- ggeffect(model = Pb3,
                        terms = c("Q65"),
                        back.transform = F,
                        type = "re")
predict.dat$Q65 <- predict.dat$x

plot <- ggplot(data= q65dat,
               mapping = aes(x=Q65,y=log(Pb))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~Q65, scales = "free")+
  geom_pointrange(data = predict.dat, aes(x=Q65, y = predicted, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCistern Age",
       y = expression(paste("ln[Pb] (µg ",L^-1, ")")))+
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
dev.print(png, "Pb_rhicageln.png", res=300, height=4, width=4, units="in")

ggplot(data = q65dat, aes(x = Q65, y = log(Pb)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Pb by Approximate Cistern Age\n",
       y = "ln(Pb) [µg/L]\n",
       x = "\n Cistern Age")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")


#Pb Q60 What is your cistern made out of? ----
q60 <- hds
q60$Q60 <- as.character(q60$Q60)
q60 <- q60[!is.na(q60$Q60),]

q60$Q60 <- factor(q60$Q60, levels = c("Plastic","Metal", "Concrete", "Fiberglass", "Other"))

q60 <- q60[q60$Q60!="Other",]

q60dat <- full_join(iw.dm, q60, by = c("site"))
q60dat <- q60dat[!is.na(q60dat$Q60),]
q60dat <- q60dat[!is.na(q60dat$community),]

summary(as.factor(q60dat$Q60))


ggplot(data = q60dat, mapping = aes(y = Pb, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Pb, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your cistern made out of?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_cisterntypecomm.png", res=100, height=6, width=8, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q60dat,
            REML = T)
Pb1 <- lmer(log(Pb)~ Q60 + community + season
            +Q60:community + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = T)
summary(Pb1)
vif(Pb1)

Pb2 <- lmer(log(Pb)~ Q60:community + community + season
            + Q60:season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
Pb3 <- lmer(log(Pb)~ Q60:community  + community + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~ Q60:season + community + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb4)
anova(Pb4, Pb3)
anova(Pb4)

Pb5 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q60dat,
            REML = T)

Pb6 <- lmer(log(Pb)~ Q60+community + season
            + (1|community:site),
            data = q60dat,
            REML = T)
summary(Pb6)
anova(Pb6)
performance(Pb6)

anova(Pb4, Pb5)
anova(Pb4, Pb6)
anova(Pb5, Pb6)

check_model(Pb6)
dev.print(png, "Pb6_cisternmaterialCheck.png", res=100, height=10, width=20, units="in")

model.effects <- allEffects(Pb6)
plot(model.effects)

as.sum <- summary(Pb6)
write.csv(as.sum$coefficients, "Pb6coefcmaterialRHI.csv")
as.means <- lsmeans(Pb6, pairwise~Q60, adjust = "tukey")#
write.csv(as.means$lsmeans, "Pb6lsmeanscmatRHI.csv")
write.csv(as.means$contrasts, "Pb6contrastscmatRHI.csv")

predict.dat <- ggeffect(model = Pb6,
                        terms = c("Q60"),
                        back.transform = F,
                        type = "re")

predict.dat$Q60 <- predict.dat$x

plot <- ggplot(data= q60dat,
               mapping = aes(x=Q60,y=log(Pb))) +
  geom_point(alpha = .3, position = position_jitter())+
  facet_grid(.~Q60, scales = "free")+
  geom_pointrange(data = predict.dat, aes(x=Q60, y = predicted, ymin = conf.low, ymax=conf.high),position=position_dodge(width=0.75), size = 1) +
  labs(x = "\nCistern Material",
       y = expression(paste("ln[Pb] (µg ",L^-1, ")")))+
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
dev.print(png, "Pb_rhicmatln.png", res=300, height=4, width=6, units="in")

ggplot(data = q60dat, aes(x = Q60, y = log(Pb)))+
  #geom_violin()+
  geom_point(alpha = .5, position = position_jitter())+
  geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
                  color = "#AF5597", size = 1, position = position_dodge(width = .75))+
  labs(title = "Pb by Material\n",
       y = "ln(Pb) [µg/L]\n",
       x = "\n Cistern Material")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Pb_rhicmatln.png", res=100, height=6, width=7, units="in")


#Pb Q76 Does your cistern have first flush? ----
q76 <- hds
q76$Q76 <- as.character(q76$Q76)
q76 <- q76[q76$Q76!="0",]
q76 <- q76[q76$Q76!="3",]
q76 <- q76[!is.na(q76$Q76),]
q76[q76$Q76=="1",]$Q76 <- "Yes"
q76[q76$Q76=="2",]$Q76 <- "No"

q76$Q76 <- factor(q76$Q76, levels = c("Yes", "No"))

q76dat <- full_join(iw.dm, q76, by = c("site"))
q76dat <- q76dat[!is.na(q76dat$Q76),]
q76dat <- q76dat[!is.na(q76dat$community),]

summary(as.factor(q76dat$Q76))

ggplot(data = q76dat, mapping = aes(y = Pb, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Pb, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Does your cistern have first flush?")+
  theme_bw() +   theme(legend.position = "bottom")
dev.print(png, "Pb_firstflushcomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q76dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q76 + community + season
            +Q76:community + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb1)
vif(Pb1)

Pb2 <- lmer(log(Pb)~ Q76 + community + season
            + Q76:season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb2)
vif(Pb2)

Pb3 <- lmer(log(Pb)~ Q76 + community + season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q76dat,
            REML = F)
vif(Pb4)
summary(Pb4)
anova(Pb4, Pb3)
anova(Pb4)


#Pb Q77 Does your cistern have a screen? ----
q77 <- hds
q77$Q77 <- as.character(q77$Q77)
q77 <- q77[q77$Q77!="0",]
q77 <- q77[q77$Q77!="3",]
q77 <- q77[!is.na(q77$Q77),]
q77[q77$Q77=="1",]$Q77 <- "Yes"
q77[q77$Q77=="2",]$Q77 <- "No"

q77$Q77 <- factor(q77$Q77, levels = c("Yes", "No"))

q77dat <- full_join(iw.dm, q77, by = c("site"))
q77dat <- q77dat[!is.na(q77dat$Q77),]
q77dat <- q77dat[!is.na(q77dat$community),]

summary(as.factor(q77dat$Q77))

ggplot(data = q77dat, mapping = aes(y = Pb, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Pb, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Does your cistern have a screen?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_screencomm.png", res=100, height=6, width=6, units="in")



Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q77dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q77 + community + season
            +Q77:community + Q77:season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb1)
vif(Pb1)

Pb2 <- lmer(log(Pb)~ Q77 + community + season+ Q77:season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
Pb3 <- lmer(log(Pb)~  community + season
            + Q77:season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~  Q77 + community + season
            + (1|community:site),
            data = q77dat,
            REML = F)

Pb5 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q77dat,
            REML = F)
vif(Pb4)
summary(Pb4)
anova(Pb4, Pb3)
anova(Pb4)

anova(Pb4, Pb3)
anova(Pb5, Pb4)
anova(Pb3, Pb5)

#Pb Q44 Is the home/community garden located within two blocks of a major roadway, freeway, elevated highway, or other transportation structures? ----
q44 <- hds
q44$Q44 <- as.character(q44$Q44)
q44 <- q44[q44$Q44!="0",]
q44 <- q44[!is.na(q44$Q44),]
q44[q44$Q44=="1",]$Q44 <- "Yes"
q44[q44$Q44=="2",]$Q44 <- "No"

q44$Q44 <- factor(q44$Q44, levels = c("Yes", "No"))

q44dat <- full_join(iw.dm, q44, by = c("site"))
q44dat <- q44dat[!is.na(q44dat$Q44),]
q44dat <- q44dat[!is.na(q44dat$community),]

summary(as.factor(q44dat$Q44))

ggplot(data = q44dat, mapping = aes(y = Pb, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "2 blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Pb, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "2 blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_2blockscomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q44dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q44 + community + season
            +Q44:community + Q44:season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Pb1)
vif(Pb1)
anova(Pb1)
Pb2 <- lmer(log(Pb)~ Q44 + community + season
            + Q44:season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
Pb3 <- lmer(log(Pb)~ Q44 + community + season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~  community + season
            + (1|community:site),
            data = q44dat,
            REML = F)
vif(Pb4)
summary(Pb4)
anova(Pb4, Pb3)
anova(Pb4)


#Pb Q13 Has your home ever tested positive for lead in the paint? ----
q13 <- hds
q13$Q13 <- as.character(q13$Q13)
q13 <- q13[q13$Q13!="0",]
q13 <- q13[!is.na(q13$Q13),]
q13[q13$Q13=="1",]$Q13 <- "Yes"
q13[q13$Q13=="2",]$Q13 <- "No"

q13$Q13 <- factor(q13$Q13, levels = c("Yes", "No"))

q13dat <- full_join(iw.dm, q13, by = c("site"))
q13dat <- q13dat[!is.na(q13dat$Q13),]
q13dat <- q13dat[!is.na(q13dat$community),]

summary(as.factor(q13dat$Q13))

ggplot(data = q13dat, mapping = aes(y = Pb, x = Q13))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Tested Positive for Lead in Paint?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_paint.png", res=100, height=6, width=6, units="in")

ggplot(data = q13dat, mapping = aes(y = Pb, x = Q13, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Tested Positive for Lead in Paint?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_paintcomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q13dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q13 + community + season
            +Q13:community + Q13:season
            + (1|community:site),
            data = q13dat,
            REML = F)
summary(Pb1)
vif(Pb1)
anova(Pb1)
Pb2 <- lmer(log(Pb)~ Q13 + community + season
            + Q13:community
            + (1|community:site),
            data = q13dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
Pb3 <- lmer(log(Pb)~ Q13 + season
            + Q13:community
            + (1|community:site),
            data = q13dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~ Q13:community + season
            + (1|community:site),
            data = q13dat,
            REML = F)
vif(Pb4)
summary(Pb4)
anova(Pb4, Pb3)
anova(Pb4)

Pb5 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q13dat,
            REML = F)

Pb6 <- lmer(log(Pb)~ Q13+community + season
            + (1|community:site),
            data = q13dat,
            REML = F)

anova(Pb4, Pb5)
anova(Pb2, Pb5)
anova(Pb4, Pb6)
anova(Pb5, Pb6)

model.effects <- allEffects(Pb4)
plot(model.effects)
# 
# as.sum <- summary(Pb6)
# write.csv(as.sum$coefficients, "Pb6coefcisternmaterialRHI.csv")
# as.means <- lsmeans(Pb6, pairwise~Q60, adjust = "tukey")#
# write.csv(as.means$lsmeans, "Pb7lsmeanscmatRHI.csv")
# write.csv(as.means$contrasts, "Pb7contrastscmatRHI.csv")
# 
# predict.dat <- ggeffect(model = Pb6,
#                         terms = c("Q60"),
#                         back.transform = F,
#                         type = "re")
# 
# ggplot(data = q60dat, aes(x = Q60, y = log(Pb)))+
#   #geom_violin()+
#   geom_point(alpha = .5, position = position_jitter())+
#   geom_pointrange(data = predict.dat, aes(x=x, y=predicted, ymax = conf.high, ymin = conf.low),
#                   color = "#AF5597", size = 1, position = position_dodge(width = .75))+
#   labs(title = "Pb by Material\n",
#        y = "ln(Pb) [µg/L]\n",
#        x = "\n Cistern Material")+
#   theme_bw()+
#   theme(text = element_text(family = "Avenir", size = 13),
#         panel.grid = element_blank(),
#         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         legend.position = "none")
# dev.print(png, "Pb_rhicmatln.png", res=100, height=6, width=8, units="in")


#Pb Q18 Is the outside paint chipping or falling off? ----
q18 <- hds
q18$Q18 <- as.character(q18$Q18)
q18 <- q18[q18$Q18!="0",]
q18 <- q18[!is.na(q18$Q18),]
q18[q18$Q18=="1",]$Q18 <- "Yes"
q18[q18$Q18=="2",]$Q18 <- "No"

q18$Q18 <- factor(q18$Q18, levels = c("Yes", "No"))

q18dat <- full_join(iw.dm, q18, by = c("site"))
q18dat <- q18dat[!is.na(q18dat$Q18),]
q18dat <- q18dat[!is.na(q18dat$community),]

summary(as.factor(q18dat$Q18))
ggplot(data = q18dat, mapping = aes(y = Pb, x = Q18))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Paint chips falling outside?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_chips.png", res=100, height=6, width=6, units="in")

ggplot(data = q18dat, mapping = aes(y = Pb, x = Q18, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Paint chips falling outside?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_chipscomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q18dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q18 + community + season
            +Q18:community + Q18:season
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(Pb1)
vif(Pb1)
anova(Pb1)
Pb2 <- lmer(log(Pb)~ Q18 + community + season
            + Q18:season
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
Pb3 <- lmer(log(Pb)~ Q18 + community + season
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(Pb3)
vif(Pb3)
anova(Pb3)
Pb4 <- lmer(log(Pb)~ community + season
            + (1|community:site),
            data = q18dat,
            REML = F)
vif(Pb4)
summary(Pb4)
anova(Pb4, Pb3)
anova(Pb4)



#Q9 As and Pb Effect Plot ----
hdseffect.as$analyte <- "As"
hdseffect.pb$analyte <- "Pb"

hdseffect.aspb <- rbind(hdseffect.as, hdseffect.pb)

ggplot(data= hdseffect.aspb,
       mapping =(aes(x=x,y=predicted, ymin =conf.low, ymax=conf.high))) +
  geom_pointrange(aes(color = analyte), size=1) +
  scale_color_manual(values=c("#AF5597","#4068B2"))+
  labs(x = "\nApproximately when was your home built?",
       y = "ln(As) and ln(Pb) [µg/L]\n",
       color = "Contaminant")+
  #coord_cartesian(ylim = c(-2,1.2))+
  facet_grid(analyte~., scales = "free_y")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        strip.text = element_text(family = "Avenir", size = 13),
        strip.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "PbAs_homeageEffect.png", res=150, height=6.5, width=8, units="in")


#Pb Old ----
#Pb Q1 What is your roof made of?
q1 <- hds
q1 <- q1[!is.na(q1$Q1),]
top5 <- c("Flat BUR (Reflective)", "Asphalt Shingle", "Flat BUR (Tar/Gravel)", "Wood Shakes/Shingles", "Metal Panel")
q1 <- filter(q1, Q1 %in% top5)
q1dat <- full_join(iw.dm, q1, by = c("site"))
q1dat <- q1dat[!is.na(q1dat$Q1),]
q1dat <- q1dat[!is.na(q1dat$community),]

q1dat$Q1 <- factor(q1dat$Q1, levels = c("Flat BUR (Tar/Gravel)","Flat BUR (Reflective)", "Asphalt Shingle", "Wood Shakes/Shingles", "Metal Panel"))

ggplot(data = q1dat, mapping = aes(y = Pb, x = Q1, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?",
       subtitle = "Two tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_rooftypecomm.png", res=100, height=6, width=8, units="in")

ggplot(data = q1dat, mapping = aes(y = Pb, x = Q1))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your roof made of?",
       subtitle = "Two tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_rooftype.png", res=100, height=6, width=8, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q1dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q1
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb1)
performance(Pb1)
check_model(Pb1)

Pb2 <- lmer(log(Pb)~ Q1 * community
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb2)
performance(Pb2)
check_model(Pb2)

Pb3 <- lmer(log(Pb)~ Q1 + community
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb3)
performance(Pb3)
check_model(Pb3)

Pb4 <- lmer(log(Pb)~ community
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb4)
performance(Pb4)
check_model(Pb4)


Pb5 <- lmer(log(Pb)~ Q1 + community + samplings
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb5)
performance(Pb5)
check_model(Pb5)

Pb6 <- lmer(log(Pb)~ Q1 + community + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb6)
performance(Pb6)
check_model(Pb6)

Pb7 <- lmer(log(Pb)~ Q1 * community + season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb7)
performance(Pb7)
check_model(Pb7)


Pb8 <- lmer(log(Pb)~ Q1 + community + season + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb8)
performance(Pb8)
check_model(Pb8)

Pb9 <- lmer(log(Pb)~ community + season + Q1:season
            + (1|community:site),
            data = q1dat,
            REML = F)
summary(Pb9)
performance(Pb9)
check_model(Pb9)

Pb10 <- lmer(log(Pb)~ community + season
             + (1|community:site),
             data = q1dat,
             REML = F)
summary(Pb10)
performance(Pb10)
check_model(Pb10)

Pb11 <- lmer(log(Pb)~ season
             + (1|community:site),
             data = q1dat,
             REML = F)
AIC(Pb1)
AIC(Pb2)
AIC(Pb3)
AIC(Pb4)
anova(Pb1, Pb0)
anova(Pb1, Pb3)
anova(Pb3, Pb0)
anova(Pb2, Pb3)
anova(Pb3, Pb4) #community only is better than with rooftype
anova(Pb5, Pb6) 
anova(Pb6, Pb7)
anova(Pb6, Pb8)
anova(Pb9, Pb8)
anova(Pb9, Pb10)
anova(Pb11, Pb10)

#write.csv(q1dat, "q1dat.csv")


#Pb Q9 Approximately, when was your home built?
q9 <- hds
q9$Q9 <- as.character(q9$Q9)
q9 <- q9[q9$Q9!="0",]
q9 <- q9[!is.na(q9$Q9),]
q9[q9$Q9=="1",]$Q9 <- "Pre 1940"
q9[q9$Q9=="2",]$Q9 <- "1941-1949"
q9[q9$Q9=="3",]$Q9 <- "1950-1959"
q9[q9$Q9=="4",]$Q9 <- "1960-1969"
q9[q9$Q9=="5",]$Q9 <- "1970-1979"
q9[q9$Q9=="6",]$Q9 <- "1980-1989"
q9[q9$Q9=="7",]$Q9 <- "1990-1999"
q9[q9$Q9=="8",]$Q9 <- "2000-2009"
q9[q9$Q9=="9",]$Q9 <- "2010-2018"

q9$Q9 <- factor(q9$Q9, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))

q9dat <- full_join(iw.dm, q9, by = c("site"))
q9dat <- q9dat[!is.na(q9dat$Q9),]
q9dat <- q9dat[!is.na(q9dat$community),]

ggplot(data = q9dat, mapping = aes(y = Pb, x = Q9))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Approximately, when was your home built?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") + 
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_houseage.png", res=100, height=6, width=8, units="in")

ggplot(data = q9dat, mapping = aes(y = Pb, x = Q9, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Approximately, when was your home built?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") + 
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_houseagecomm.png", res=100, height=6, width=8, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q9dat,
            REML = F)

Pb1 <- lmer(log(Pb)~ Q9
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb1)
performance(Pb1)
check_model(Pb1)

Pb2 <- lmer(log(Pb)~ Q9 * community
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb2)
performance(Pb2)
check_model(Pb2)

Pb3 <- lmer(log(Pb)~ Q9 + community
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb3)
performance(Pb3)
check_model(Pb3)

Pb4 <- lmer(log(Pb)~ community
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb4)
performance(Pb4)
check_model(Pb4)

Pb5 <- lmer(log(Pb)~ Q9 + community + samplings
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb5)
performance(Pb5)
check_model(Pb5)

Pb6 <- lmer(log(Pb)~ Q9 + community + season
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb6)
performance(Pb6)
check_model(Pb6)

Pb7 <- lmer(log(Pb)~ Q9 * community + samplings
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb7)
performance(Pb7)
check_model(Pb7)


Pb8 <- lmer(log(Pb)~ Q9 + community + samplings + Q9:samplings
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb8)
performance(Pb8)
check_model(Pb8)

Pb9 <- lmer(log(Pb)~ community + samplings
            + (1|community:site),
            data = q9dat,
            REML = F)
summary(Pb9)
performance(Pb9)
check_model(Pb9)


AIC(Pb1)
AIC(Pb2)
AIC(Pb3)
AIC(Pb4)
AIC(Pb5)
AIC(Pb6)
AIC(Pb9)
anova(Pb1, Pb0)
anova(Pb1, Pb3)
anova(Pb2, Pb3)
anova(Pb3, Pb4) #home age signif even with community

anova(Pb6, Pb5)

anova(Pb3, Pb5)
anova(Pb5, Pb9)
anova(Pb5, Pb7)
anova(Pb5, Pb8)

anova(Pb6, Pb8)

dev.print(png, "Pb_homeageCheck.png", res=100, height=10, width=20, units="in")

plot(allEffects(Pb6))

model.effects <- allEffects(Pb6)
plot(model.effects)

model.effects.pb <- ggeffect(model = Pb6,
                             back.transform = F,
                             type = "re")

hdseffect.pb <- model.effects.pb$Q9
hdseffect.pb$x <- factor(hdseffect.pb$x, levels = c("Pre 1940", "1941-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018"))

ggplot(data= hdseffect.df,
       mapping =(aes(x=Q9,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q9") + 
  ylab("Estimated ln([Pb]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Home Age", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "Pb_homeageEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q9dat, "q9dat.csv")

#Pb Q60 What is your cistern made out of?
q60 <- hds
q60$Q60 <- as.character(q60$Q60)
q60 <- q60[!is.na(q60$Q60),]

q60$Q60 <- factor(q60$Q60, levels = c("Plastic","Metal", "Concrete", "Fiberglass", "Other"))

q60dat <- full_join(iw.dm, q60, by = c("site"))
q60dat <- q60dat[!is.na(q60dat$Q60),]
q60dat <- q60dat[!is.na(q60dat$community),]

ggplot(data = q60dat, mapping = aes(y = Pb, x = Q60))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "What is your cistern made out of?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_cisterntype.png", res=100, height=6, width=8, units="in")

ggplot(data = q60dat, mapping = aes(y = Pb, x = Q60, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "What is your cistern made out of?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30)) + facet_wrap(.~samplings)
dev.print(png, "Pb_cisterntypecommsamp.png", res=100, height=6, width=12, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q60dat,
            REML = F)

Pb1 <- lmer(log(Pb)~ Q60
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb1)
performance(Pb1)
check_model(Pb1)

Pb2 <- lmer(log(Pb)~ Q60 * community
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb2)
performance(Pb2)
check_model(Pb2)

Pb3 <- lmer(log(Pb)~ Q60 + community
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb3)
performance(Pb3)
check_model(Pb3)

Pb4 <- lmer(log(Pb)~ community
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb4)
performance(Pb4)
check_model(Pb4)

Pb5 <- lmer(log(Pb)~ Q60 + community + samplings
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb5)
performance(Pb5)
check_model(Pb5)

Pb6 <- lmer(log(Pb)~ Q60 + community + season
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb6)
performance(Pb6)
check_model(Pb6)

Pb7 <- lmer(log(Pb)~ community + samplings
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb7)
performance(Pb7)
check_model(Pb7)

Pb8 <- lmer(log(Pb)~ Q60 * community + samplings
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb8)
performance(Pb8)
check_model(Pb8)

Pb9 <- lmer(log(Pb)~ Q60 * samplings + community
            + (1|community:site),
            data = q60dat,
            REML = F)
summary(Pb9)
performance(Pb9)
check_model(Pb9)


AIC(Pb1)
AIC(Pb2)
AIC(Pb3)
AIC(Pb4)
AIC(Pb5)
AIC(Pb6)
AIC(Pb7)

anova(Pb1, Pb0)
anova(Pb2, Pb3)
anova(Pb1, Pb3)
anova(Pb4, Pb3)
anova(Pb1, Pb4)
anova(Pb3, Pb5)
anova(Pb3, Pb6)
anova(Pb6, Pb5)
anova(Pb5, Pb7) #
anova(Pb5, Pb8)
anova(Pb5, Pb9)

dev.print(png, "Pb_cisterntypeCheck.png", res=100, height=10, width=20, units="in")


model.effects <- allEffects(Pb5)
plot(model.effects)

hdseffect <- model.effects[1]
hdseffect.df <- as.data.frame(hdseffect$Q60)

hdseffect.df$Q60 <- factor(hdseffect.df$Q60, levels = c("Plastic","Metal", "Concrete", "Fiberglass", "Other"))


ggplot(data= hdseffect.df,
       mapping =(aes(x=Q60,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q60") + 
  ylab("Estimated ln([Pb]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Cistern Type", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "Pb_cisterntypeEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q60dat, "q60dat.csv")

#Pb Q65 How old is your cistern?
q65 <- hds
q65$Q65 <- as.character(q65$Q65)
q65 <- q65[q65$Q65!="0",]
q65 <- q65[q65$Q65!="1",]
q65 <- q65[!is.na(q65$Q65),]
q65[q65$Q65=="2",]$Q65 <- "<6 months"
q65[q65$Q65=="3",]$Q65 <- "6 months-1 year"
q65[q65$Q65=="4",]$Q65 <- "1-2 years"
q65[q65$Q65=="5",]$Q65 <- "2-3 years"
q65[q65$Q65=="6",]$Q65 <- "3-4 years"
q65[q65$Q65=="7",]$Q65 <- "5+ years"

q65$Q65 <- factor(q65$Q65, levels = c("<6 months", "6 months-1 year", "1-2 years", "2-3 years", "3-4 years", "5+ years"))

q65dat <- full_join(iw.dm, q65, by = c("site"))
q65dat <- q65dat[!is.na(q65dat$Q65),]
q65dat <- q65dat[!is.na(q65dat$community),]

ggplot(data = q65dat, mapping = aes(y = Pb, x = Q65))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "How old is your cistern?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_cisternage.png", res=100, height=6, width=8, units="in")

ggplot(data = q65dat, mapping = aes(y = Pb, x = Q65, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "How old is your cistern?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30)) + facet_wrap(.~samplings)
dev.print(png, "As_cisternagecommsamp.png", res=100, height=6, width=12, units="in")

Pb0 <- lmer(log(Pb)~
              + (1|community:site),
            data = q65dat,
            REML = F)
Pb1 <- lmer(log(Pb)~ Q65
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb1)
performance(Pb1)
check_model(Pb1)

Pb2 <- lmer(log(Pb)~ Q65 * community
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb2)
performance(Pb2)
check_model(Pb2)

Pb3 <- lmer(log(Pb)~ Q65 + community
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb3)
performance(Pb3)
check_model(Pb3)

Pb4 <- lmer(log(Pb)~ community
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb4)
performance(Pb4)
check_model(Pb4)

Pb5 <- lmer(log(Pb)~ Q65 + community + samplings
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb5)
performance(Pb5)
check_model(Pb5)

Pb6 <- lmer(log(Pb)~ Q65 + community + season
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb6)
performance(Pb6)
check_model(Pb6)

Pb7 <- lmer(log(Pb)~ community + samplings
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb7)
performance(Pb7)
check_model(Pb7)

Pb8 <- lmer(log(Pb)~ Q65 * community + samplings
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb8)
performance(Pb8)
check_model(Pb8)

Pb9 <- lmer(log(Pb)~ Q65 * samplings + community
            + (1|community:site),
            data = q65dat,
            REML = F)
summary(Pb9)
performance(Pb9)
check_model(Pb9)

AIC(Pb1)
AIC(Pb2)
AIC(Pb3)
AIC(Pb4)
AIC(Pb5)
AIC(Pb6)
AIC(Pb7)

anova(Pb1, Pb0)
anova(Pb1, Pb3)
anova(Pb4, Pb3)
anova(Pb2, Pb3)
anova(Pb3, Pb5)
anova(Pb6, Pb5)
anova(Pb5, Pb7)
anova(Pb5, Pb8)
anova(Pb5, Pb9)

dev.print(png, "Pb_cisternageCheck.png", res=100, height=10, width=20, units="in")


model.effects <- allEffects(Pb5)
plot(model.effects)

hdseffect <- model.effects[1]
hdseffect.df <- as.data.frame(hdseffect$Q65)

hdseffect.df$Q65 <- factor(hdseffect.df$Q65, levels = c("<6 months", "6 months-1 year", "1-2 years", "2-3 years", "3-4 years", "5+ years"))


ggplot(data= hdseffect.df,
       mapping =(aes(x=Q65,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q65") + 
  ylab("Estimated ln([Pb]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Cistern Age", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "Pb_cisternageEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q65dat, "q65dat.csv")

#Pb Q76 Does your cistern have first flush?
q76 <- hds
q76$Q76 <- as.character(q76$Q76)
q76 <- q76[q76$Q76!="0",]
q76 <- q76[q76$Q76!="3",]
q76 <- q76[!is.na(q76$Q76),]
q76[q76$Q76=="1",]$Q76 <- "Yes"
q76[q76$Q76=="2",]$Q76 <- "No"

q76$Q76 <- factor(q76$Q76, levels = c("Yes", "No"))

q76dat <- full_join(iw.dm, q76, by = c("site"))
q76dat <- q76dat[!is.na(q76dat$Q76),]
q76dat <- q76dat[!is.na(q76dat$community),]

ggplot(data = q76dat, mapping = aes(y = Pb, x = Q76))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Does your cistern have first flush?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_firstflush.png", res=100, height=6, width=6, units="in")

ggplot(data = q76dat, mapping = aes(y = Pb, x = Q76, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Does your cistern have first flush?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +   theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_firstflushcomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~ 
              + (1|community:site),
            data = q76dat,
            REML = F)

Pb1 <- lmer(log(Pb)~ Q76
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb1)
performance(Pb1)
check_model(Pb1)

Pb2 <- lmer(log(Pb)~ Q76 * community
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb2)
performance(Pb2)
check_model(Pb2)

Pb3 <- lmer(log(Pb)~ Q76 + community
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb3)
performance(Pb3)
check_model(Pb3)

Pb4 <- lmer(log(Pb)~ community
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb4)
performance(Pb4)
check_model(Pb4)

Pb5 <- lmer(log(Pb)~ Q76 + community + samplings
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb5)
performance(Pb5)
check_model(Pb5)

Pb6 <- lmer(log(Pb)~ Q76 + community + season
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb6)
performance(Pb6)
check_model(Pb6)

Pb7 <- lmer(log(Pb)~ community + samplings
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb7)
performance(Pb7)
check_model(Pb7)

Pb8 <- lmer(log(Pb)~ Q76 * community + samplings
            + (1|community:site),
            data = q76dat,
            REML = F)
summary(Pb8)
performance(Pb8)
check_model(Pb8)


AIC(Pb1)
AIC(Pb2)
AIC(Pb3)
AIC(Pb4)
AIC(Pb5)
AIC(Pb6)
AIC(Pb7)

anova(Pb0, Pb1)
anova(Pb2, Pb3)
anova(Pb1, Pb3)
anova(Pb4, Pb3)

anova(Pb4, Pb5)
anova(Pb4, Pb6)
anova(Pb6, Pb5)
anova(Pb5, Pb7)
anova(Pb4, Pb7)

dev.print(png, "Pb_firstflushCheck.png", res=100, height=10, width=20, units="in")


model.effects <- allEffects(Pb5)
plot(model.effects)

hdseffect <- model.effects[1]
hdseffect.df <- as.data.frame(hdseffect$Q65)

ggplot(data= hdseffect.df,
       mapping =(aes(x=Q76,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q76") + 
  ylab("Estimated ln([Pb]) (ln(ug/L))") + 
  labs(title="Modeled Effect of First Flush", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "Pb_firstflushEffect.png", res=100, height=5, width=7, units="in")

#write.csv(q76dat, "q76dat.csv")


#Pb Q77 Does your cistern have a screen?
q77 <- hds
q77$Q77 <- as.character(q77$Q77)
q77 <- q77[q77$Q77!="0",]
q77 <- q77[q77$Q77!="3",]
q77 <- q77[!is.na(q77$Q77),]
q77[q77$Q77=="1",]$Q77 <- "Yes"
q77[q77$Q77=="2",]$Q77 <- "No"

q77$Q77 <- factor(q77$Q77, levels = c("Yes", "No"))

q77dat <- full_join(iw.dm, q77, by = c("site"))
q77dat <- q77dat[!is.na(q77dat$Q77),]
q77dat <- q77dat[!is.na(q77dat$community),]

ggplot(data = q77dat, mapping = aes(y = Pb, x = Q77))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Does your cistern have a screen?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_screen.png", res=100, height=6, width=6, units="in")

ggplot(data = q77dat, mapping = aes(y = Pb, x = Q77, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Does your cistern have a screen?",
       subtitle = "Two Tucson max values not shown ~300 and ~75 ppb")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  coord_cartesian(ylim = c(0,30))
dev.print(png, "Pb_screencomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~ 
              + (1|community:site),
            data = q77dat,
            REML = F)

Pb1 <- lmer(log(Pb)~ Q77
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb1)
performance(Pb1)
check_model(Pb1)

Pb2 <- lmer(log(Pb)~ Q77 * community
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb2)
performance(Pb2)
check_model(Pb2)

Pb3 <- lmer(log(Pb)~ Q77 + community
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb3)
performance(Pb3)
check_model(Pb3)

Pb4 <- lmer(log(Pb)~ community
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb4)
performance(Pb4)
check_model(Pb4)

Pb5 <- lmer(log(Pb)~ Q77 + community + samplings
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb5)
performance(Pb5)
check_model(Pb5)

Pb6 <- lmer(log(Pb)~ Q77 + community + season
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb6)
performance(Pb6)
check_model(Pb6)

Pb7 <- lmer(log(Pb)~ community + samplings
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb7)
performance(Pb7)
check_model(Pb7)

Pb8 <- lmer(log(Pb)~ Q77 * community + samplings
            + (1|community:site),
            data = q77dat,
            REML = F)
summary(Pb8)
performance(Pb8)
check_model(Pb8)


AIC(Pb1)
AIC(Pb2)
AIC(Pb3)
AIC(Pb4)
AIC(Pb5)
AIC(Pb6)
AIC(Pb7)

anova(Pb0, Pb1)
anova(Pb2, Pb3)
anova(Pb1, Pb3)
anova(Pb4, Pb3)
anova(Pb4, Pb5)
anova(Pb4, Pb6)
anova(Pb6, Pb5)
anova(Pb5, Pb7)
anova(Pb4, Pb7)

dev.print(png, "Pb_screenCheck.png", res=100, height=10, width=20, units="in")


model.effects <- allEffects(Pb5)
plot(model.effects)

hdseffect <- model.effects[1]
hdseffect.df <- as.data.frame(hdseffect$Q65)

ggplot(data= hdseffect.df,
       mapping =(aes(x=Q77,y=fit, ymin = lower, ymax=upper))) +
  geom_pointrange(position=position_dodge(width=0.75), size = 1) +
  xlab("Q77") + 
  ylab("Estimated ln([Pb]) (ln(ug/L))") + 
  labs(title="Modeled Effect of Cistern Screen", subtitle = "This figure displays modeled, not real data")+
  theme_bw()
dev.print(png, "Pb_screenEffect.png", res=100, height=5, width=7, units="in")



#write.csv(q77dat, "q77dat.csv")

#Pb Q44 Is the home/community garden located within two blocks of a major roadway, freeway, elevated highway, or other transportation structures?
q44 <- hds
q44$Q44 <- as.character(q44$Q44)
q44 <- q44[q44$Q44!="0",]
q44 <- q44[!is.na(q44$Q44),]
q44[q44$Q44=="1",]$Q44 <- "Yes"
q44[q44$Q44=="2",]$Q44 <- "No"

q44$Q44 <- factor(q44$Q44, levels = c("Yes", "No"))

q44dat <- full_join(iw.dm, q44, by = c("site"))
q44dat <- q44dat[!is.na(q44dat$Q44),]
q44dat <- q44dat[!is.na(q44dat$community),]

ggplot(data = q44dat, mapping = aes(y = Pb, x = Q44))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "2 blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_2blocks.png", res=100, height=6, width=6, units="in")

ggplot(data = q44dat, mapping = aes(y = Pb, x = Q44, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  coord_cartesian(ylim = c(0,75))+
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "2 blocks from road?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_2blockscomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~ 
              + (1|community:site),
            data = q44dat,
            REML = F)

Pb1 <- lmer(log(Pb)~ Q44
            + (1|community:site),
            data = q44dat,
            REML = F)

Pb2 <- lmer(log(Pb)~ Q44 + community + season
            + (1|community:site),
            data = q44dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
performance(Pb1)
check_model(Pb1)

#Pb Q13 Has your home ever tested positive for lead in the paint?
q13 <- hds
q13$Q13 <- as.character(q13$Q13)
q13 <- q13[q13$Q13!="0",]
q13 <- q13[!is.na(q13$Q13),]
q13[q13$Q13=="1",]$Q13 <- "Yes"
q13[q13$Q13=="2",]$Q13 <- "No"

q13$Q13 <- factor(q13$Q13, levels = c("Yes", "No"))

q13dat <- full_join(iw.dm, q13, by = c("site"))
q13dat <- q13dat[!is.na(q13dat$Q13),]
q13dat <- q13dat[!is.na(q13dat$community),]

ggplot(data = q13dat, mapping = aes(y = Pb, x = Q13))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Tested Positive for Lead in Paint?")+
  coord_cartesian(ylim = c(0,75))+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_paint.png", res=100, height=6, width=6, units="in")

ggplot(data = q13dat, mapping = aes(y = Pb, x = Q13, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Tested Positive for Lead in Paint?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_paintcomm.png", res=100, height=6, width=6, units="in")

#Pb Q18 Is the outside paint chipping or falling off?
q18 <- hds
q18$Q18 <- as.character(q18$Q18)
q18 <- q18[q18$Q18!="0",]
q18 <- q18[!is.na(q18$Q18),]
q18[q18$Q18=="1",]$Q18 <- "Yes"
q18[q18$Q18=="2",]$Q18 <- "No"

q18$Q18 <- factor(q18$Q18, levels = c("Yes", "No"))

q18dat <- full_join(iw.dm, q18, by = c("site"))
q18dat <- q18dat[!is.na(q18dat$Q18),]
q18dat <- q18dat[!is.na(q18dat$community),]

ggplot(data = q18dat, mapping = aes(y = Pb, x = Q18))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  #geom_point(position = position_dodge(width=0.75, preserve = "total"), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#655CACA","#4068B2"))+
  labs(title = "Paint chips falling outside?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_chips.png", res=100, height=6, width=6, units="in")

ggplot(data = q18dat, mapping = aes(y = Pb, x = Q18, fill = community))+
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') +
  geom_point(position = position_dodge(width=0.75, preserve = "total"), aes(group=community), shape = 21, size = 2) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  labs(title = "Paint chips falling outside?")+
  theme_bw() +
  theme(legend.position = "bottom")
dev.print(png, "Pb_chipscomm.png", res=100, height=6, width=6, units="in")


Pb0 <- lmer(log(Pb)~ 
              + (1|community:site),
            data = q18dat,
            REML = F)

Pb1 <- lmer(log(Pb)~ Q18
            + (1|community:site),
            data = q18dat,
            REML = F)

Pb2 <- lmer(log(Pb)~ Q18 + community + season + period
            + (1|community:site),
            data = q18dat,
            REML = F)
summary(Pb2)
vif(Pb2)
anova(Pb2)
performance(Pb1)
check_model(Pb1)


