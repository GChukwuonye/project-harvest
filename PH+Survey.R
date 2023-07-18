library(readxl)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(lme4)
library(lmerTest)
library(ggeffects)
library(performance)
library(effects)
library(ggpubr)
library(emmeans)
library(multcomp)
library(sparklyr)
library(tensorflow)
library(keras)
library(table1)

#set working directory
#setwd("/users/godsgiftnkechichukwuonye/Desktop")
setwd("/users/godsgiftnkechichukwuonye/Desktop")
hds2 <- read_xlsx("hds2.xlsx", col_names = TRUE) 
hds3 <- read_xlsx("hds2.xlsx", sheet= "Sheet3", col_names = TRUE) 
hds4 <- read_xlsx("hds2.xlsx", sheet= "Sheet4", col_names = TRUE) 
hds <- read_xlsx("hds2.xlsx", sheet= "survey", col_names = TRUE) 

hds2.long <- pivot_longer(data = hds2,
                         cols = D301:D321,
                         names_to = "site",
                         values_transform = as.numeric)
na.omit(hds2.long$value)
na.omit(hds2.long$site)

write_csv(hds2.long, "hds_new.csv")

hds3.long <- pivot_longer(data = hds3,
                          cols = D301:D321,
                          names_to = "site",
                          values_transform = as.numeric)
na.omit(hds3.long$value)
na.omit(hds3.long$site)

write_csv(hds3.long, "hds_home.csv")

hds4.long <- pivot_longer(data = hds4,
                          cols = D301:T199,
                          names_to = "site",
                          values_transform = as.numeric)
na.omit(hds4.long$value)
na.omit(hds4.long$site)

write_csv(hds4.long, "hds_roof.csv")



summary(hds2.long$Question== "Q1")

kruskal.test(hds2.long, hds2.long$Question== "Q1" ~ hds2.long$Question== "Q2" )

a <- full_join(iw.dm, hds, by = c("site"))
na.omit(a)
a$Q1 <- factor(a$Q1, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "15", "16", "100"))
kruskal.test(a, a$Al ~ a$Q1)
pairwise.wilcox.test(a, a$Al ~ a$Q2)               


is.na(a$Q1)
head(a$Q1)
na.omit(a)
ggplot(data = a,
       mapping = aes(x=Q1,
                     y=Al, na.rm= TRUE))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())

ggplot(data = a,
       mapping = aes(x=Q11,
                     y=Al, na.rm= TRUE))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
kruskal.test(a, a$Al ~ a$Q11)
pairwise.wilcox.test(a, a$Al ~ a$Q1)

model <- aov(Al~Q4, data=a)
summary(model)
TukeyHSD(model, conf.level=.95)

plot(TukeyHSD(model, conf.level=.95), las = 2)

ggplot(data = a,
       mapping = aes(x=Q4,
                     y=Al, na.rm= TRUE))+
  geom_boxplot() +
  stat_boxplot(geom = 'errorbar') +
  labs(y = expression(paste("Pb Concentration (µg ",L^-1, ")", "\n")),
       x = paste("\nCommunity"))+
  theme_bw() +
  theme(text = element_text(size=7, family = "Arial"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())

