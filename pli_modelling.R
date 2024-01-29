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

#workflow= load data wrangling first, then load the community sheet to get ward nd location data first


#pli without hds=====
#tucson=====
pli_tucson<- tucsondat
pli_tucson<- pli_tucson%>%
  drop_na(prox.normal)
pli_tucson<- pli_tucson%>%
  drop_na(pli.ln)
pli_tucson<-pli_tucson%>%
  drop_na(season)
pli_tucson<- pli_tucson%>%
  drop_na(pH)

plt0 <- lmer(data = pli_tucson,
             pli.ln ~ (1|community:site),
             REML = T) 

plt2 <- lmer(data = pli_tucson,
             pli.ln ~  season + prox.normal + season:prox.normal+ pH+ward+pH+ pH:prox.normal+
               (1|community:site),
             REML = F)
plt.step <- step(plt2)
plt.step
plt3 <- get_model(plt.step)
print(summary(plt3))
check_model(plt3)
vif(plt3)
anova(plt3)
print(anova(plt3))
performance(plt3)

#tucson effect plot====
predict.dat.tu <- ggeffect(model = plt3,
                        terms = c("prox.normal"),
                        back.transform = F,
                        type = "re")

ggplot(data = pli_tucson, aes(x = prox.normal, y = pli.ln))+
  geom_point()+
  geom_ribbon(data = predict.dat.tu, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat.tu, mapping = aes(x=x, y = predicted))+
  labs(title = "",
       y = "ln(PLI)\n",
       x = "\n Normalized Distance From Tucson International Airport (km)")+
  facet_grid(season ~ .) +  
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "PLI_TUdisteffectln.png", res=300, height=6, width=8, units="in")


# ggplot(data = iw.dm.long, mapping = aes(y = log(pli), x = prox.normal)) + 
#   geom_point(size = 1, color= "blue")+
#   facet_wrap(community~., scales = "free")+
#   stat_smooth(method=lm)
# 
# ggplot(data = iw.dm.long, mapping = aes(y = log(pli), x = pH)) + 
#   geom_point(size = 1, color= "green")+
#   facet_wrap(community~., scales = "free")+
#   stat_smooth(method=lm,color= "black")

#pli dewey modeling ----
pli_dewey<- deweydat
pli_dewey<- pli_dewey%>%
  drop_na(prox.normal)
pli_dewey<- pli_dewey%>%
  drop_na(pli.ln)
pli_dewey<-pli_dewey%>%
  drop_na(season)
pli_dewey<-pli_dewey%>%
  drop_na(pH)
pld0 <- lmer(data =pli_dewey,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(pld0)

pld <- lmer(data = pli_dewey,
            pli.ln ~  season + prox.normal + pH + location+ pH:prox.normal+
              season:prox.normal
            + (1|site),
            REML = F) #ML for comparison, REML for final
summary(pld)

pld.step <- step(pld)
pld.step
pld2 <- get_model(pld.step)
pld2
print(summary(pld2))
check_model(pld2)
anova(pld2)
print(anova(pld2))
performance(pld2)

pli_dewey$season <- as.factor(pli_dewey$season)
str(pli_dewey)
str(predict.dat.dh)

#deweyeffect plot====
predict.dat.dh <- ggeffect(model = pld2,
                           terms = c("prox.normal"),
                           back.transform = F,
                           type = "re")

ggplot(data = pli_dewey, aes(x = prox.normal, y = pli.ln)) +
  geom_point() +
  geom_ribbon(data = predict.dat.dh, mapping = aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = .5, fill = "#95CACA") +
  geom_line(data = predict.dat.dh, mapping = aes(x = x, y = predicted)) +
  labs(title = "",
       y = "ln(PLI)\n",
       x = "\n Normalized Distance From Smelter (km)") +
  facet_grid(season ~ .) +  
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

dev.print(png, "PLI_DWdisteffectln.png", res=300, height=6, width=8, units="in")

#pli hayden modeling ----
pli_hayden<- haydendat
pli_hayden<- pli_hayden%>%
  drop_na(prox.normal)
pli_hayden<- pli_hayden%>%
  drop_na(pli.ln)
pli_hayden<-pli_hayden%>%
  drop_na(season)
pli_hayden<-pli_hayden%>%
  drop_na(pH)
plh0 <- lmer(data =pli_hayden,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plh0)
plh <- lmer(data = pli_hayden,
            pli.ln ~  season + prox.normal + pH+ season:prox.normal +pH:prox.normal+location+
            + (1|site),
            REML = F) #ML for comparison, REML for final

summary(plh)
plh.step <- step(plh)
plh.step
plh2 <- get_model(plh.step)
plh2
print(summary(plh2))
check_model(plh2)
vif(plh2)
anova(plh2)
print(anova(plh2))
performance(plh2)


#haydeneffect plot====
predict.dat.hw <- ggeffect(model = plh2,
                           terms = c("prox.normal"),
                           back.transform = F,
                           type = "re")

ggplot(data = pli_hayden, aes(x = prox.normal, y = pli.ln))+
  geom_point()+
  geom_ribbon(data = predict.dat.hw, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat.hw, mapping = aes(x=x, y = predicted))+
  labs(title = "",
       y = "ln(PLI)\n",
       x = "\n Normalized Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "PLI_HWdisteffectln.png", res=300, height=6, width=8, units="in")





#pli globe modeling ----
pli_globe<- globedat
pli_globe<- pli_globe%>%
  drop_na(prox.normal)
pli_globe<- pli_globe%>%
  drop_na(pli.ln)
pli_globe<-pli_globe%>%
  drop_na(season)
pli_globe<-pli_globe%>%
  drop_na(pH)
plg0 <- lmer(data =pli_globe,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plg0)


plg <- lmer(data = pli_globe,
            pli.ln ~  season + prox.normal+pH+ season:prox.normal+pH:prox.normal+ location_2+ 
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

#globeeffect plot====
predict.dat.gm <- ggeffect(model = plg2,
                           terms = c("prox.normal"),
                           back.transform = F,
                           type = "re")

ggplot(data = pli_globe, aes(x = prox.normal, y = pli.ln))+
  geom_point()+
  geom_ribbon(data = predict.dat.gm, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat.gm, mapping = aes(x=x, y = predicted))+
  labs(title = "",
       y = "ln(PLI)\n",
       x = "\n Normalized Distance From Mine (km)")+
  theme_bw()+
  facet_grid(season ~ .) + 
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

dev.print(png, "PLI_GMdisteffectln.png", res=300, height=6, width=8, units="in")


# #pli tucson modeling ----
# pli_tucson<- iw.dm[iw.dm$community=="Tucson",]
# pli_tucson$pli.ln<- na.omit(pli_tucson$pli.ln)
# pli_tucson$score_bin<- na.omit(pli_tucson$score_bin)
# pli_tucson$prox.normal<- na.omit(pli_tucson$season)
# pli_tucson$season<- na.omit(pli_tucson$season)
# 
# plt0 <- lmer(data = pli_tucson,
#              pli.ln ~ (1|community:site),
#              REML = T) #ML for comparison, REML for final
# 
# plt2 <- lmer(data = pli_tucson,
#             pli.ln ~  season + prox.normal + score_bin +
#               season:score_bin+
#               prox.normal:score_bin+
#             (1|site),
#             REML = F) 
# plt.step <- step(plt2)
# plt.step
# plt3 <- get_model(plt.step)
# print(summary(plt3))
# check_model(plt3)
# anova(plt3)
# print(anova(plt3))
# performance(plt3)
# 
# 
# 
# 
# #pli dewey modeling ----
# 
# pli_dewey<- iw.dm[iw.dm$community=="Dewey-Humboldt",]
# 
# pld0 <- lmer(data =pli_dewey,
#              pli.ln ~ (1|community:site),
#              REML = T) #ML for comparison, REML for final
# summary(pld0)
# 
# pld <- lmer(data = pli_dewey,
#             pli.ln ~  season + prox.normal + score_bin +
#               season:score_bin+
#               prox.normal:score_bin
#             + (1|site),
#             REML = F) #ML for comparison, REML for final
# summary(pld)
# 
# pld.step <- step(pld)
# pld.step
# pld2 <- get_model(pld.step)
# 
# print(summary(pld2))
# check_model(pld2)
# anova(pld2)
# print(anova(pld2))
# performance(pld2)
# 
# 
# 
# #pli globe modeling ----
# pli_globe<- iw.dm[iw.dm$community=="Globe/Miami",]
# plg0 <- lmer(data =pli_globe,
#              pli.ln ~ (1|community:site),
#              REML = T) #ML for comparison, REML for final
# summary(plg0)
# 
# 
# plg <- lmer(data = pli_globe,
#          pli.ln ~  season + prox.normal + score_bin+
#               season:score_bin+
#               prox.normal:score_bin
#             + (1|community:site),
#             REML = F) #ML for comparison, REML for final
# summary(plg)
# plg.step <- step(plg)
# plg.step
# plg2 <- get_model(plg.step)
# 
# print(summary(plg2))
# check_model(plg2)
# anova(plg2)
# print(anova(plg2))
# performance(plg2)
# 
# 
# #pli hayden modeling ----
# pli_hayden<- iw.dm[iw.dm$community=="Hayden/Winkelman",]
# plh0 <- lmer(data =pli_hayden,
#              pli.ln ~ (1|community:site),
#              REML = T) #ML for comparison, REML for final
# summary(plh0)
# plh <- lmer(data = pli_hayden,
#            pli.ln ~  season + prox.normal + score_bin +
#               season:score_bin+
#               prox.normal:score_bin
#             + (1|site),
#             REML = F) #ML for comparison, REML for final
# summary(plh)
# plh.step <- step(plh)
# plh.step
# plh2 <- get_model(plh.step)
# 
# print(summary(plh2))
# check_model(plh2)
# vif(plh2)
# anova(plh2)
# print(anova(plh2))
# performance(plh2)


#individual model====
#tucson individual model=====
#pli_tucson<- iw.dm[iw.dm$community=="Tucson",]

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
pli_tucson67 <- full_join(tucsondat, hds67, by = c("site"))
pli_tucson67 <- pli_tucson67 %>%
  drop_na(prox.normal)
pli_tucson67 <- pli_tucson67 %>%
  drop_na(season)
pli_tucson67 <- pli_tucson67 %>%
  drop_na(Q67)
pli_tucson67$Q67<- as.factor(pli_tucson67$Q67)
summary(pli_tucson67$Q67)
tuc0<- lmer(data = pli_tucson67,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(tuc0)

tuc1<- lmer(data= pli_tucson67,
              pli.ln~ season+ prox.normal+ Q67+ ward+ prox.normal:season+ pH+ ward:Q67+
                (1|community:site),
              REML = F) #ML for comparison, REML for final

tuc.step <- step(tuc1, direction= "both")
tuc.step
tuc2 <- get_model(tuc.step)

print(summary(tuc2))
check_model(tuc2)
vif(tuc2)
anova(tuc2)
print(anova(tuc2))
performance(tuc2)


#tucson hds plot====
predict.dat.tu3 <- ggeffect(model = tuc2,
                           terms = c("Q67"),
                           back.transform = F,
                           type = "re")

ggplot(data = pli_tucson67, aes(x = Q67, y = pli.ln))+
  geom_point()+
  geom_ribbon(data = predict.dat.tu3, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat.tu3, mapping = aes(x=x, y = predicted))+
  labs(title = "",
       x = "Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?\n",
       y = "\n Normalized Distance From Mine (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

dev.print(png, "PLI_HDSTUQ67effectln.png", res=300, height=6, width=8, units="in")






#Q71====
#Do you treat or wash your cistern with anything?
pli_tucson71 <- full_join(tucsondat, hds71, by = c("site"))
pli_tucson71 <- pli_tucson71 %>%
  drop_na(prox.normal)
pli_tucson71 <- pli_tucson71 %>%
  drop_na(season)
pli_tucson71 <- pli_tucson71 %>%
  drop_na(Q71)
pli_tucson71$Q71<- as.factor(pli_tucson71$Q71)
summary(pli_tucson71$Q71)
tuc71 <- lmer(data = pli_tucson71,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(tuc71)

tuc71b<- lmer(data= pli_tucson71,
              pli.ln~ prox.normal+ Q71+ ward+ prox.normal:season+ pH+  season:ward+ 
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
pli_tucson79 <- full_join(tucsondat, hds79, by = c("site"))
pli_tucson79 <- pli_tucson79 %>%
  drop_na(prox.normal)
pli_tucson79 <- pli_tucson79 %>%
  drop_na(season)
pli_tucson79 <- pli_tucson79 %>%
  drop_na(Q79)
model1 <- lmer(data = pli_tucson79,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson79,
              pli~  prox.normal+ Q79+ ward+ prox.normal:season+ pH+ season:ward+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q79 is not significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)

print(summary(model3))
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
performance(model3 )

#Q76====
#Does your cistern have a first flush?
pli_tucson76 <- full_join(tucsondat, hds76, by = c("site"))
pli_tucson76 <- pli_tucson76 %>%
  drop_na(prox.normal)
pli_tucson76 <- pli_tucson76 %>%
  drop_na(season)
pli_tucson76 <- pli_tucson76 %>%
  drop_na(Q76)
model1 <- lmer(data = pli_tucson76,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson76,
              pli~  prox.normal+ Q76+  ward+ prox.normal:season+ pH+  Q76:ward +season:ward+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q76 is not significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)

print(summary(model3))
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
performance(model3 )


#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
pli_tucson77 <- full_join(tucsondat, hds77, by = c("site"))
pli_tucson77 <- pli_tucson77 %>%
  drop_na(prox.normal)
pli_tucson77 <- pli_tucson77 %>%
  drop_na(season)
pli_tucson77 <- pli_tucson77 %>%
  drop_na(Q77)
model1 <- lmer(data = pli_tucson77,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson77,
              pli~ season+ prox.normal+ Q77+ward+ prox.normal:season+ pH+season:ward+ Q77:ward+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)

print(summary(model3))
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
performance(model3 )

#Q60: What is your cistern made of?----
pli_tucson60 <- full_join(tucsondat, hds60, by = c("site"))
pli_tucson60 <- pli_tucson60 %>%
  drop_na(prox.normal)
pli_tucson60 <- pli_tucson60 %>%
  drop_na(season)
pli_tucson60 <- pli_tucson60%>%
  drop_na(Q60)
pli_tucson60$Q60<- as.factor(pli_tucson60$Q60)
summary(pli_tucson$Q60)
model1 <- lmer(data = pli_tucson60,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson60,
              pli~ season+ prox.normal+ Q60+ward+ prox.normal:season+ pH+   Q60:ward+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )

#Q65: How old is your cistern:----
pli_tucson65 <- full_join(tucsondat, hds65, by = c("site"))
pli_tucson65 <- pli_tucson65 %>%
  drop_na(prox.normal)
pli_tucson65 <- pli_tucson65 %>%
  drop_na(season)
pli_tucson65 <- pli_tucson65%>%
  drop_na(Q65)
pli_tucson65$Q65<- as.factor(pli_tucson65$Q65)
summary(pli_tucson65$Q65)
model1 <- lmer(data = pli_tucson65,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson65,
              pli~ season+ prox.normal+ Q65+ prox.normal:season+ pH+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )


#dewey individual model=====
  #Q67====
dew<-full_join(deweydat, hds67, by = c("site"))
dew <- dew %>%
  drop_na(prox.normal)
dew <- dew %>%
  drop_na(season)
dew <- dew %>%
  drop_na(Q67)
dew$Q67<- as.factor(dew$Q67)
summary(dew$Q67)
dew1 <- lmer(data =dew,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew1)

dew2<- lmer(data= dew,
              pli.ln~ season+ prox.normal+ Q67+ prox.normal:season+ pH+
                (1|site),
              REML = F) #ML for comparison, REML for final
summary(dew2)                
anova(dew2) 
dew.step <- step(dew2)
dew.step
dew3 <- get_model(dew.step)
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
print(summary(dew3))
performance(dew3)

#Q71====
#Do you treat or wash your cistern with anything?
dew71<-full_join(deweydat, hds71, by = c("site"))
# dew71 <- dew71 %>%
#   drop_na(prox.normal)
# dew71 <- dew71%>%
#   drop_na(season)
pli_dewey <- dew71%>%
  drop_na(Q71)
pli_dewey$Q71<- as.factor(pli_dewey$Q71)
summary(pli_dewey$Q71)
dew71 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew71)

dew71b<- lmer(data= pli_dewey,
              pli.ln~ season+ prox.normal+ Q71+ prox.normal:season+ pH+
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
dew79<-full_join(deweydat, hds79, by = c("site"))
dew79 <- dew79 %>%
  drop_na(prox.normal)
dew79 <- dew79%>%
  drop_na(season)
pli_dewey <- dew79%>%
  drop_na(Q79)
pli_dewey$Q79<- as.factor(pli_dewey$Q79)
summary(pli_dewey$Q79)
dew79 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew79)

dew79b<- lmer(data=pli_dewey,
              pli.ln~ season+ prox.normal+ Q79+ prox.normal:season+ pH+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew79b)  
dew.step <- step(dew79b)
dew.step
dew3 <- get_model(dew.step)
dew3
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
print(summary(dew3))
performance(dew3)


#Q76====
dew76<-full_join(deweydat, hds76, by = c("site"))
dew76 <- dew76 %>%
  drop_na(prox.normal)
dew76<- dew76%>%
  drop_na(season)
dew76<- dew76%>%
  drop_na(Q76)
pli_dewey<- as.factor(dew76$Q76)

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
dew77<-full_join(deweydat, hds77, by = c("site"))
dew77 <- dew77 %>%
  drop_na(prox.normal)
dew77<- dew77%>%
  drop_na(season)
pli_dewey<-  dew77%>%
  drop_na(Q77)
pli_dewey$Q77<- as.factor(pli_dewey$Q77)
summary(pli_dewey$Q77)
dew77 <- lmer(data = pli_dewey,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew77)

dew77b<- lmer(data= pli_dewey,
              pli.ln~ season+ prox.normal+ Q77+ prox.normal:season+ pH+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(dew77b)                
dew.step <- step(dew77b)
dew.step
dew3 <- get_model(dew.step)
check_model(dew3)
vif(dew3)
anova(dew3)
print(anova(dew3))
print(summary(dew3))
performance(dew3)

#Q60: What is your cistern made of?----
pli_dewey60 <- full_join(deweydat, hds60, by = c("site"))
pli_dewey60 <- pli_dewey60 %>%
  drop_na(prox.normal)
pli_dewey60 <- pli_dewey60 %>%
  drop_na(season)
pli_dewey60 <- pli_dewey60%>%
  drop_na(Q60)
pli_dewey60$Q60<- as.factor(pli_dewey60$Q60)
summary(pli_dewey$Q60)
model1 <- lmer(data = pli_dewey60,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_dewey60,
              pli~ season+ prox.normal+ Q60+ prox.normal:season+ pH+   
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
dew.step2 <- step(model2)
dew.step2
model3 <- get_model(dew.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )


#Q65: How old is your cistern:----
pli_dewey65 <- full_join(deweydat, hds65, by = c("site"))
pli_dewey65 <- pli_dewey65 %>%
  drop_na(prox.normal)
pli_dewey65 <- pli_dewey65 %>%
  drop_na(season)
pli_dewey65 <- pli_dewey65%>%
  drop_na(Q65)
pli_dewey65$Q65<- as.factor(pli_dewey65$Q65)
summary(pli_dewey65$Q65)
model1 <- lmer(data = pli_dewey65,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_dewey65,
              pli~ season+ prox.normal+ Q65+ prox.normal:season+ pH+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )



#hayden individual model=====

#Q67====
# #Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
# hay67<- iw.dm67[iw.dm67$community=="Hayden/Winkelman",]
# hay67 <- hay67%>%
#   drop_na(prox.normal)
# hay67<- hay67%>%
#   drop_na(season)
# hay <- lmer(data = hay67,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(hay)
# 
# hay67b<- lmer(data=  hay67,
#               pli.ln~ season+ prox.normal+ Q67 + 
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(hay67b)                
# hay.step <- step(hay67b)
# hay.step
# hay2 <- get_model(hay.step)
# 
# print(summary(hay2))
# check_model(hay2)
# vif(hay2)
# anova(hay2)
# print(anova(hay2))
# performance(hay2)
# #no response from Hayden- All were zero
# 
# #Q71====
# #Do you treat or wash your cistern with anything?
# hay<- iw.dm71[iw.dm71$community=="Dewey-Humboldt",]
# hay <- hay%>%
#   drop_na(prox.normal)
# hay<- hay%>%
#   drop_na(season)
# hay<- hay%>%
#   drop_na(Q71)
# hay71 <- lmer(data =  hay,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(model1)
# 
# hay71b<- lmer(data= hay,
#               pli.ln~ season+ prox.normal+ Q71+ 
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(hay71b)                
# hay.step <- step(hay71b)
# hay.step
# hay2 <- get_model(hay.step)
# 
# print(summary(hay2))
# check_model(hay2)
# vif(hay2)
# anova(hay2)
# print(anova(hay2))
# performance(hay2)


#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
hay79<-full_join(haydendat, hds79, by = c("site"))
hay79 <- hay79%>%
  drop_na(prox.normal)
hay79<- hay79%>%
  drop_na(season)
hay79<- hay79%>%
  drop_na(Q79)
hay79$Q79<- as.factor(hay79$Q79)
hay79a<- lmer(data =  hay79,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay79a)

hay79b<- lmer(data= hay79,
              pli.ln~ season+ prox.normal+ Q79+  prox.normal:season+ pH+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay79b)                
hay.step <- step(hay79b)
hay.step
hay2 <- get_model(hay.step)
hay2
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
print(summary(hay2))
performance(hay2)

# #Q76====
# #Does your cistern have a first flush?
# hay76<- iw.dm76[iw.dm76$community=="Dewey-Humboldt",]
# hay76 <- hay76%>%
#   drop_na(prox.normal)
# hay76<- hay76%>%
#   drop_na(season)
# hay76<- hay76%>%
#   drop_na(Q76)
# hay76a <- lmer(data =  hay76,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(hay76a)
# 
# hay76b<- lmer(data= hay76,
#               pli.ln~ season+ prox.normal+ Q76+ 
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(hay76b)                
# 
# hay.step <- step(hay76b)
# hay.step
# hay2 <- get_model(hay.step)
# 
# print(summary(hay2))
# check_model(hay2)
# vif(hay2)
# anova(hay2)
# print(anova(hay2))
# performance(hay2)



#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
hay77<-full_join(haydendat, hds77, by = c("site"))
hay77 <- hay77%>%
  drop_na(prox.normal)
hay77<- hay77%>%
  drop_na(season)
hay77<- hay77%>%
  drop_na(Q77)
hay77$Q77<- as.factor(hay77$Q77)
summary(hay77$Q77)
hay77a<- lmer(data =  hay77,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay77a)

hay77b<- lmer(data= hay77,
              pli.ln~ season+ prox.normal+ Q77+ prox.normal:season+ pH+Q77:season+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(hay77b)                


hay.step <- step(hay77b)
hay.step
hay2 <- get_model(hay.step)
hay2
print(summary(hay2))
check_model(hay2)
vif(hay2)
anova(hay2)
print(anova(hay2))
performance(hay2)

#Q60: What is your cistern made of?----
pli_hayden60 <- full_join(haydendat, hds60, by = c("site"))
pli_hayden60 <- pli_hayden60 %>%
  drop_na(prox.normal)
pli_hayden60 <- pli_hayden60 %>%
  drop_na(season)
pli_hayden60 <- pli_hayden60%>%
  drop_na(Q60)
pli_hayden60$Q60<- as.factor(pli_hayden60$Q60)
summary(pli_hayden60$Q60)
model1 <- lmer(data = pli_hayden60,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_hayden60,
              pli~ season+ prox.normal+ Q60+ prox.normal:season+ pH+   
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
dew.step2 <- step(model2)
dew.step2
model3 <- get_model(dew.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )


#Q65: How old is your cistern:----
pli_hayden65 <- full_join(haydendat, hds65, by = c("site"))
pli_hayden65 <- pli_hayden65 %>%
  drop_na(prox.normal)
pli_hayden65 <- pli_hayden65 %>%
  drop_na(season)
pli_hayden65 <- pli_hayden65%>%
  drop_na(Q65)
pli_hayden65$Q65<- as.factor(pli_hayden65$Q65)
summary(pli_hayden65$Q65)
model1 <- lmer(data = pli_hayden65,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_hayden65,
              pli~ season+ prox.normal+ Q65+ prox.normal:season+ pH+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
tuc.step2 <- step(model2)
tuc.step2
model3 <- get_model(tuc.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )

#globe individual model=====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
globe<-full_join(globedat, hds67, by = c("site"))
globe <- globe%>%
  drop_na(prox.normal)
globe<- globe%>%
  drop_na(season)
globe<- globe%>%
  drop_na(Q67)
globe$Q67<- as.factor(globe$Q67)
summary(globe$Q67)
globea <- lmer(data = globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globea)

globe67<- lmer(data= globe,
              pli.ln~ season+ prox.normal+ Q67+ prox.normal:season+ pH+location_2+location_2:Q67+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe67)    

globe67c<- lmer(data= globe,
               pli.ln~ season+ prox.normal+ Q67+ prox.normal:season+ 
                 (1|community:site),
               REML = F) #ML for comparison, REML for final
summary(globe67c)  
globe.step<- step(globe67c)
globe.step
globe2 <- get_model(globe.step)
globe2
print(summary(globe2))
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
performance(globe2)

#Q71====
#Do you treat or wash your cistern with anything?
globe71<-full_join(globedat, hds71, by = c("site"))
globe71 <- globe71%>%
  drop_na(prox.normal)
globe71<- globe71%>%
  drop_na(season)
globe71<- globe71%>%
  drop_na(Q71)
globe71$Q71<- as.factor(globe71$Q71)
globe71a <- lmer(data = globe71,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe71a)

globe71b<- lmer(data= globe71,
              pli.ln~ season+ prox.normal+ Q71+ prox.normal:season+ pH+location_2+location_2:Q71+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe71b)         

globe71c<- lmer(data= globe71,
                pli.ln~ season+ prox.normal+ Q71+ prox.normal:season+
                  (1|community:site),
                REML = F) #ML for comparison, REML for final
summary(globe71c)  
vif(globe71c)
globe.step <- step(globe71c)
globe.step
globe2 <- get_model(globe.step)


check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
print(summary(globe2))
performance(globe2)



#Q79====
globe79<-full_join(globedat, hds79, by = c("site"))
globe79 <- globe79%>%
  drop_na(prox.normal)
globe79<- globe79%>%
  drop_na(season)
globe79<- globe79%>%
  drop_na(Q79)
globe79$Q79<- as.factor(globe79$Q79)
globe <- lmer(data = globe79,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe79b<- lmer(data=globe79,
              pli.ln~  prox.normal+ Q79+ prox.normal:season+ pH+ location_2:Q79+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe79b)                

globe.step <- step(globe79b)
globe.step
globe79c <- get_model(globe.step)


check_model(globe79c)
vif(globe79c)
anova(globe79c)
print(anova(globe79c))
print(summary(globe79c))
performance(globe79c)


#Q76====
#Does your cistern have a first flush?
globe76<-full_join(globedat, hds76, by = c("site"))
globe76 <- globe76%>%
  drop_na(prox.normal)
globe76<- globe76%>%
  drop_na(season)
globe76<- globe76%>%
  drop_na(Q76)
globe76$Q76<- as.factor(globe76$Q76)
globe <- lmer(data = globe76,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe76b<- lmer(data= globe76,
              pli.ln~ season+ prox.normal+ Q76+ prox.normal:season+ pH+location_2:Q76+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe76b)                
vif(globe76b)
globe76c<- lmer(data= globe76,
                pli.ln~ season+ prox.normal+ Q76+ prox.normal:season+ 
                  (1|community:site),
                REML = F) #ML for comparison, REML for final
summary(globe76c)                
vif(globe76c)


globe.step <- step(globe76c)
globe.step
globe2 <- get_model(globe.step)
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
print(summary(globe2))
performance(globe2)


#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
globe77<-full_join(globedat, hds77, by = c("site"))
globe77 <- globe77%>%
  drop_na(prox.normal)
globe77<- globe77%>%
  drop_na(season)
globe77<- globe77%>%
  drop_na(Q77)
globe77$Q77<- as.factor(globe77$Q77)
summary(globe77$Q77)
globe <- lmer(data = globe77,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe77b<- lmer(data= globe77,
              pli.ln~ season+ prox.normal+ Q77+ prox.normal:season+ pH+location_2:Q77+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe77b)   
anova(globe77b)
vif(globe77b)

globe2<- lmer(data= globe77,
                pli.ln~ season+ Q77+ prox.normal:season+ 
                  (1|community:site),
                REML = F) #ML for comparison, REML for final


print(summary(globe2))
check_model(globe2)
vif(globe2)
print(anova(globe2))
print(summary(globe2))
performance(globe2)

#Q60: What is your cistern made of?----
pli_globe60 <- full_join(globedat, hds60, by = c("site"))
pli_globe60 <- pli_globe60 %>%
  drop_na(prox.normal)
pli_globe60 <- pli_globe60 %>%
  drop_na(season)
pli_globe60 <- pli_globe60%>%
  drop_na(Q60)
pli_globe60$Q60<- as.factor(pli_globe60$Q60)
summary(pli_globe60$Q60)
model1 <- lmer(data = pli_globe60,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe60,
              pli~ season+ prox.normal+ Q60+ prox.normal:season+ pH+location_2:Q60+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
glo.step2 <- step(model2)
glo.step2
model3 <- get_model(glo.step2)

model3<- lmer(data= pli_globe60,
              pli~ season+ Q60+ prox.normal:season+
                (1|community:site),
              REML = F) 

check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )



#Q65: How old is your cistern:----
pli_globe65 <- full_join(globedat, hds65, by = c("site"))
pli_globe65 <- pli_globe65 %>%
  drop_na(prox.normal)
pli_globe65 <- pli_globe65 %>%
  drop_na(season)
pli_globe65 <- pli_globe65%>%
  drop_na(Q65)
pli_globe65$Q65<- as.factor(pli_globe65$Q65)
summary(pli_globe65$Q65)
model1 <- lmer(data = pli_globe65,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe65,
              pli~ season+ prox.normal+ Q65+ prox.normal:season+ pH+ location_2:Q65+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
model3<- lmer(data= pli_globe65,
              pli~ season+ prox.normal+prox.normal:season+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )



=======

#pli without hds=====
#tucson=====
pli_tucson<- iw.dm[iw.dm$community=="Tucson",]
pli_tucson<- pli_tucson%>%
  drop_na(prox.normal)
pli_tucson<- pli_tucson%>%
  drop_na(pli.ln)
pli_tucson<- pli_tucson%>%
  drop_na(season)
pli_tucson<- pli_tucson%>%
  drop_na(pH)

plt0 <- lmer(data = pli_tucson,
             pli.ln ~ (1|community:site),
             REML = T) 

plt2 <- lmer(data = pli_tucson,
            pli.ln ~  season + prox.normal + season:prox.normal+ pH+
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
ggplot(data = iw.dm.long, mapping = aes(y = log(pli), x = prox.normal)) + 
  geom_point(size = 1, color= "blue")+
  facet_wrap(community~., scales = "free")+
  stat_smooth(method=lm)

ggplot(data = iw.dm.long, mapping = aes(y = log(pli), x = pH)) + 
  geom_point(size = 1, color= "green")+
  facet_wrap(community~., scales = "free")+
  stat_smooth(method=lm,color= "black")

#pli dewey modeling ----
pli_dewey<- iw.dm[iw.dm$community=="Dewey-Humboldt",]
pli_dewey<- pli_dewey%>%
  drop_na(prox.normal)
pli_dewey<- pli_dewey%>%
  drop_na(pli.ln)
pli_dewey<-pli_dewey%>%
  drop_na(season)
pli_dewey<-pli_dewey%>%
  drop_na(pH)
pld0 <- lmer(data =pli_dewey,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(pld0)

pld <- lmer(data = pli_dewey,
            pli.ln ~  season + prox.normal + pH +
              season:prox.normal
            + (1|site),
            REML = F) #ML for comparison, REML for final
summary(pld)

pld.step <- step(pld)
pld.step
pld2 <- get_model(pld.step)
pld2
print(summary(pld2))
check_model(pld2)
anova(pld2)
print(anova(pld2))
performance(pld2)

#pli hayden modeling ----
pli_hayden<- iw.dm[iw.dm$community=="Hayden/Winkelman",]
pli_hayden<- pli_hayden%>%
  drop_na(prox.normal)
pli_hayden<- pli_hayden%>%
  drop_na(pli.ln)
pli_hayden<-pli_hayden%>%
  drop_na(season)
pli_hayden<-pli_hayden%>%
  drop_na(pH)
plh0 <- lmer(data =pli_hayden,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plh0)
plh <- lmer(data = pli_hayden,
           pli.ln ~  season + prox.normal + pH+ season:prox.normal
            + (1|site),
            REML = F) #ML for comparison, REML for final
summary(plh)
plh.step <- step(plh)
plh.step
plh2 <- get_model(plh.step)
plh2
print(summary(plh2))
check_model(plh2)
vif(plh2)
anova(plh2)
print(anova(plh2))
performance(plh2)


#pli globe modeling ----
pli_globe<- iw.dm[iw.dm$community=="Globe/Miami",]
pli_globe<- pli_globe%>%
  drop_na(prox.normal)
pli_globe<- pli_globe%>%
  drop_na(pli.ln)
pli_globe<-pli_globe%>%
  drop_na(season)
pli_globe<-pli_globe%>%
  drop_na(pH)
plg0 <- lmer(data =pli_globe,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(plg0)


plg <- lmer(data = pli_globe,
         pli.ln ~  season + prox.normal+pH+ season:prox.normal
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
>>>>>>> c008f9a47e8c93eb2f43a2d39c905a79884dcbc2
