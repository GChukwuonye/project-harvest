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
library(broom)
library(patchwork)
library(lme4)
library(broom.mixed)
library(dotwhisker)

#workflow= load data wrangling first, then load the community sheet to get ward nd location data first


#pli without hds=====
#tucson=====
# pli_tucson<- tucsondat
# pli_tucson<- pli_tucson%>%
#   drop_na(prox.normal)
# pli_tucson<- pli_tucson%>%
#   drop_na(pli.ln)
# pli_tucson<-pli_tucson%>%
#   drop_na(season)
# pli_tucson<- pli_tucson%>%
#   drop_na(pH)
pli_tucson<-iws.tu
pli_tucson$pli.ln<- na.omit(pli_tucson$pli.ln)
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
plt4 <- lmer(data = pli_tucson,
             pli.ln ~  season + prox.normal + season:prox.normal+ pH+
               (1|community:site),
             REML = F)
anova(plt4)
vif(plt4)
AIC(plt3, plt4)
print(summary(plt4))
check_model(plt4)
print(anova(plt4))
performance(plt4)

#tucson effect plot====
predict.dat.tu <- ggeffect(model = plt4,
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


#Forest plots---------
library(lme4)
library(broom.mixed)
library(dotwhisker)

tidy_plt4 <- tidy(plt4, effects = "fixed", conf.int = TRUE)
ggplot(tidy_plt4, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the PLI Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()
#dev.print(png, "forestplot_tucson.png", res=400, height=14, width=14, units="in")



#pli dewey modeling ----

pli_dewey<- iws.dh
pli_dewey$pli.ln<- na.omit(pli_dewey$pli.ln)
pli_dewey$location<- as.factor (pli_dewey$location)
summary(pli_dewey$location)
deweydat <- full_join(dewey, pli_dewey, by = c("site"))
pli_dewey<-deweydat
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
vif(pld)
pld.step <- step(pld)
pld.step
pld2 <- get_model(pld.step)
print(summary(pld2))
performance(pld2)

tidy_pld2 <- tidy(pld2, effects = "fixed", conf.int = TRUE)
ggplot(tidy_pld2, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the PLI Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()

#deweyeffect plot====
predict.dat.dh <- ggeffect(model = pld2,
                           terms = c("prox.normal"),
                           back.transform = F,
                           type = "re")

ggplot(data = pli_hayden, aes(x = prox.normal, y = pli.ln)) +
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
pli_hayden<- iws.hw
haydendat <- full_join(hdistance, pli_hayden, by = c("site"))
pli_hayden<-haydendat
pli_hayden <- pli_hayden[!is.na(pli_hayden$mlod.name), ]
# pli_hayden<- haydendat
# pli_hayden<- pli_hayden%>%
#   drop_na(prox.normal)
# pli_hayden<- pli_hayden%>%
#   drop_na(pli.ln)
# pli_hayden<-pli_hayden%>%
#   drop_na(season)
# pli_hayden<-pli_hayden%>%
#   drop_na(pH)
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
                           terms = c("season"),
                           back.transform = F,
                           type = "re")

ggplot(data = pli_hayden, aes(x = season, y = pli.ln))+
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


tidy_plh2 <- tidy(plh2, effects = "fixed", conf.int = TRUE)
ggplot(tidy_plh2, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Relevant Factors in the PLI Model") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()


#pli globe modeling ----
pli_globe<- iws.gm

globedat <- full_join(glo, pli_globe, by = c("site"))
pli_globe<-globendat
pli_globe <- pli_globe[!is.na(pli_globe$pli.ln), ]

# pli_globe<- globedat
# pli_globe<- pli_globe%>%
#   drop_na(prox.normal)
# pli_globe<- pli_globe%>%
#   drop_na(pli.ln)
# pli_globe<-pli_globe%>%
#   drop_na(season)
# pli_globe<-pli_globe%>%
#   drop_na(pH)
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

tidy_plg2 <- tidy(plg2, effects = "fixed", conf.int = TRUE)
ggplot(tidy_plg2, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Model Variables") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()
dev.print(png, "PLI_GMdisteffectln.png", res=300, height=6, width=8, units="in")


#combined forest plots for PLI effects====
tidy_plg2 <- tidy(plg2, effects = "fixed", conf.int = TRUE)
tidy_plh2 <- tidy(plh2, effects = "fixed", conf.int = TRUE)
tidy_pld2 <- tidy(pld2, effects = "fixed", conf.int = TRUE)
tidy_plt4 <- tidy(plt4, effects = "fixed", conf.int = TRUE)

tidy_plg2$Model <- 'Globe'
tidy_plg2 <- tidy_plg2[, !colnames(tidy_plg2) %in% c("effect", "df")]
tidy_plh2$Model <- 'Hayden-Winklemann'
tidy_pld2$Model <- 'Dewey-Humboldt'
tidy_plt4$Model <- 'Tucson'
tidy_plt4 <- tidy_plt4[, !colnames(tidy_plt4) %in% c("effect", "df")]

combined_models <- rbind(tidy_plg2, tidy_plh2, tidy_pld2, tidy_plt4)
combined_models$Significant <- ifelse(combined_models$p.value < 0.05, "Significant", "Not Significant")

ggplot(combined_models, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size = 1, color = "darkred") +
  geom_point(aes(fill = Significant), color = "black", size = 3, shape = 5) +  # Fill color based on significance
  geom_errorbarh(aes(height = 0.2, color = Significant)) +  # Error bar color based on significance
  facet_wrap(~Model, scales = "free_y", ncol = 1) +
  theme_minimal() + 
  theme(
    legend.position = "right",
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    axis.title.y = element_text(color = "black", size = 8, face = "bold"),
    axis.text = element_text(color = "black", size = 8, face = "bold"),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth = 2),
    strip.text = element_text(size = 10, face = "bold", color= "darkblue"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(10, "pt")
  ) +
  labs(x = "PLI estimates and 95% confidence intervals", y = "PLI Variables") +
  scale_fill_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +  # Manual color scale for significance
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue"))  # Match error bar colors
dev.print(png, "PLI_ModelSummary.png", res=300, height=6, width=8, units="in")


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
# #pli hayden modeling ----
# 
# pli_hayden<- iw.dm[iw.dm$community=="hayden-Humboldt",]
# 
# pld0 <- lmer(data =pli_hayden,
#              pli.ln ~ (1|community:site),
#              REML = T) #ML for comparison, REML for final
# summary(pld0)
# 
# pld <- lmer(data = pli_hayden,
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

pli_tucson$Q67<- as.factor(pli_tucson$Q67)
summary(pli_tucson$Q67)
pli_tucson <- pli_tucson[!(pli_tucson$Q67==0), ]
summary(pli_tucson$Q67)
tuc0<- lmer(data = pli_tucson,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(tuc0)

tuc1<- lmer(data= pli_tucson,
              pli.ln~ season+ prox.normal+ Q67+ ward+ prox.normal:season+ pH+ ward:Q67+prox.normal:pH+
                (1|community:site),
              REML = F) #ML for comparison, REML for final

tuc1<- lmer(data= pli_tucson,
            pli.ln~ season+ prox.normal+ Q67+ ward+ prox.normal:season+ pH+ ward:Q67+prox.normal:pH+
              (1|community:site),
            REML = F) #ML for comp

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

ggplot(data = pli_tucson, aes(x = Q67, y = pli.ln))+
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

#forest plot----
tidy_tuc2 <- tidy(tuc2, effects = "fixed", conf.int = TRUE)
tidy_tuc2 <- tidy_tuc2 %>%
  mutate(term = ifelse(term == "Q672", "Do not clean roof drain", term))


tidy_tuc2$Significant <- ifelse(tidy_tuc2$p.value < 0.05, "Significant", "Not Significant")

ggplot(tidy_tuc2, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Model Variables") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()











#Q71====
#Do you treat or wash your cistern with anything?
pli_tucson$Q71<- as.factor(pli_tucson$Q71)
summary(pli_tucson$Q71)
tuc71 <- lmer(data = pli_tucson,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(tuc71)

tuc71b<- lmer(data= pli_tucson,
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
pli_tucson$Q79<- as.factor(pli_tucson$Q79)
summary(pli_tucson$Q79)
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
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
pli_tucson$Q76<- as.factor(pli_tucson$Q76)
pli_tucson <- pli_tucson[!(pli_tucson$Q76==0), ]
summary(pli_tucson$Q76)

model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
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
pli_tucson$Q77<- as.factor(pli_tucson$Q77)
pli_tucson <- pli_tucson[!(pli_tucson$Q77==0), ]
summary(pli_tucson$Q77)
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
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

pli_tucson$Q60<- as.factor(pli_tucson$Q60)
summary(pli_tucson$Q60)
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
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

pli_tucson$Q65<- as.factor(pli_tucson$Q65)
summary(pli_tucson$Q65)
model1 <- lmer(data = pli_tucson,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_tucson,
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
pli_dewey$Q67<- as.factor(pli_dewey$Q67)
summary(pli_dewey$Q67)
dew<-pli_dewey
dew <-dew[!is.na(dew$Q67), ]
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

# #Q71====
# Do you treat or wash your cistern with anything?
# dew71<-full_join(haydendat, hds71, by = c("site"))
# dew71 <- dew71 %>%
#   drop_na(prox.normal)
#  dew71 <- dew71%>%
#   drop_na(season)
#  pli_hayden <- dew71%>%
# drop_na(Q71)
# dew$Q71<- as.factor(dew$Q71)
#  summary(dew$Q71)
#
# pli_dewey$Q71<- as.factor(pli_dewey$Q71)
# summary(pli_dewey$Q71)
# dew<-pli_dewey
# dew <-dew[!is.na(dew$Q71), ]
# summary(dew$Q71)
# dew71 <- lmer(data = dew,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(dew71)
#
# dew71b<- lmer(data= pli_dewn,
#               pli.ln~ season+ prox.normal+ Q71+ prox.normal:season+ pH+
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(dew71b)
# dew.step <- step(dew71b)
# dew.step
# dew3 <- get_model(dew.step)
#
# print(summary(dew3))
# check_model(dew3)
# vif(dew3)
# anova(dew3)
# print(anova(dew3))
# performance(dew3)
# #not relevant for hayden. All hayden participants responded with no.




#Q79====
#Do you ever remove the screen/filter and leave your cistern without the filter?
# dew79<-full_join(haydendat, hds79, by = c("site"))
# dew79 <- dew79 %>%
#   drop_na(prox.normal)
# dew79 <- dew79%>%
#   drop_na(season)
# pli_hayden <- dew79%>%
#   drop_na(Q79)
# dew$Q79<- as.factor(dew$Q79)
# summary(dew$Q79)
# dew <-dew[!is.na(dew$Q79), ]
# 
# dew79 <- lmer(data = pli_hayden,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(dew79)
# 
# dew79b<- lmer(data=pli_hayden,
#               pli.ln~ season+ prox.normal+ Q79+ prox.normal:season+ pH+
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(dew79b)
# dew.step <- step(dew79b)
# dew.step
# dew3 <- get_model(dew.step)
# dew3
# check_model(dew3)
# vif(dew3)
# anova(dew3)
# print(anova(dew3))
# print(summary(dew3))
# performance(dew3)
# 

#Q76====
# pli_dewey$Q76<- as.factor(pli_dewey$Q76)
# summary(pli_dewey$Q76)
# dew<-pli_dewey
# dew <-dew[!is.na(dew$Q76), ]
# summary(dew$Q76)
# #
# dew76 <- lmer(data = pli_hayden,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(dew76)
#
# dew76b<- lmer(data= pli_hayden,
#               pli.ln~ season+ prox.normal+ Q76+
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(dew76b)
#
#
# dew.step <- step(dew76b)
# dew.step
# dew3 <- get_model(dew.step) #model irrelevant. all hayden residents answerted with no.
#
# print(summary(dew3))
# check_model(dew3)
# vif(dew3)
# anova(dew3)
# print(anova(dew3))
# performance(dew3)

#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
# dew77<-full_join(haydendat, hds77, by = c("site"))
# dew77 <- dew77 %>%
#   drop_na(prox.normal)
# dew77<- dew77%>%
#   drop_na(season)
# pli_hayden<-  dew77%>%
#   drop_na(Q77)
dew$Q77<- as.factor(dew$Q77)
summary(dew$Q77)
dew <-dew[!(dew$Q77==0), ]
summary(dew$Q77)
dew <-dew[!is.na(dew$Q77), ]


dew77 <- lmer(data = dew,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(dew77)

dew77b<- lmer(data= dew,
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
# pli_hayden60 <- full_join(haydendat, hds60, by = c("site"))
# pli_hayden60 <- pli_hayden60 %>%
#   drop_na(prox.normal)
# pli_hayden60 <- pli_hayden60 %>%
#   drop_na(season)
# pli_hayden60 <- pli_hayden60%>%
#   drop_na(Q60)
dew$Q60<- as.factor(dew$Q60)
summary(dew$Q60)
model1 <- lmer(data = dew,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= dew,
              pli~ season+ prox.normal+ Q60+ prox.normal:season+ pH+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)
anova(model2) #Q77 not  significant
dew.step2 <- step(model2)
dew.step2
model3 <- get_model(dew.step2)
check_model(model3 )
vif(model2)
anova(model2 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )

#Q65: How old is your cistern:----
# pli_hayden <- full_join(haydendat, hds65, by = c("site"))
# pli_hayden65 <- pli_hayden65 %>%
#   drop_na(prox.normal)
# pli_hayden65 <- pli_hayden65 %>%
#   drop_na(season)
# pli_hayden65 <- pli_hayden65%>%
#   drop_na(Q65)


dew$Q65<- as.factor(dew$Q65)
summary(dew$Q65)
model1 <- lmer(data = dew,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= dew,
              pli~ season+ prox.normal+ Q65+ prox.normal:season+ pH+ 
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

#hayden individual model=====

#Q67====
#Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
# hay67<- iw.dm67[iw.dm67$community=="Hayden/Winkelman",]
# hay67 <- hay67%>%
#   drop_na(prox.normal)
# hay67<- hay67%>%
#   drop_na(season)

# pli_hayden$Q67<- as.factor(pli_hayden$Q67)
# summary(pli_hayden$Q67)
# pli_tucson <- pli_tucson[!(pli_tucson$Q67==0), ]
# summary(pli_tucson$Q67)
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

# #Q71====
# #Do you treat or wash your cistern with anything?
# hay<- iw.dm71[iw.dm71$community=="hayden-Humboldt",]
# hay <- hay%>%
#   drop_na(prox.normal)
# hay<- hay%>%
#   drop_na(season)
# hay<- hay%>%
#   drop_na(Q71)
# pli_hayden$Q71<- as.factor(pli_hayden$Q71)
# summary(pli_hayden$Q71)
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
# hay79<-full_join(haydendat, hds79, by = c("site"))
# hay79 <- hay79%>%
#   drop_na(prox.normal)
# hay79<- hay79%>%
#   drop_na(season)
# hay79<- hay79%>%
#   drop_na(Q79)
# hay79$Q79<- as.factor(hay79$Q79)
# pli_hayden$Q79<- as.factor(pli_hayden$Q79)
# summary(pli_hayden$Q79)
# hay79a<- lmer(data =  hay79,
#                pli.ln ~ (1|community:site),
#                REML = T) #ML for comparison, REML for final
# summary(hay79a)
# 
# hay79b<- lmer(data= hay79,
#               pli.ln~ season+ prox.normal+ Q79+  prox.normal:season+ pH+
#                 (1|community:site),
#               REML = F) #ML for comparison, REML for final
# summary(hay79b)                
# hay.step <- step(hay79b)
# hay.step
# hay2 <- get_model(hay.step)
# hay2
# check_model(hay2)
# vif(hay2)
# anova(hay2)
# print(anova(hay2))
# print(summary(hay2))
# performance(hay2)

# #Q76====
#Does your cistern have a first flush?
# hay76<- iw.dm76[iw.dm76$community=="hayden-Humboldt",]
# hay76 <- hay76%>%
#   drop_na(prox.normal)
# hay76<- hay76%>%
#   drop_na(season)
# hay76<- hay76%>%
#   drop_na(Q76)
pli_hayden$Q76<- as.factor(pli_hayden$Q76)
summary(pli_hayden$Q76)

hay76a <- lmer(data =  pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay76a)

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
# hay77<-full_join(haydendat, hds77, by = c("site"))
# hay77 <- hay77%>%
#   drop_na(prox.normal)
# hay77<- hay77%>%
#   drop_na(season)
# hay77<- hay77%>%
#   drop_na(Q77)
pli_hayden$Q77<- as.factor(pli_hayden$Q77)
summary(pli_hayden$Q77)
pli_hayden <- pli_hayden[!(pli_hayden$Q77==0), ]


hay77a<- lmer(data =  pli_hayden,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(hay77a)

hay77b<- lmer(data= pli_hayden,
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
#pli_hayden60 <- full_join(haydendat, hds60, by = c("site"))
pli_hayden$Q60<- as.factor(pli_hayden$Q60)
summary(pli_hayden$Q60)
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
# pli_hayden65 <- full_join(haydendat, hds65, by = c("site"))
# pli_hayden65 <- pli_hayden65 %>%
#   drop_na(prox.normal)
# pli_hayden65 <- pli_hayden65 %>%
#   drop_na(season)
# pli_hayden65 <- pli_hayden65%>%
#   drop_na(Q65)
pli_hayden$Q65<- as.factor(pli_hayden$Q65)
summary(pli_hayden$Q65)
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
# globe<-full_join(globedat, hds67, by = c("site"))
# globe <- globe%>%
#   drop_na(prox.normal)
# globe<- globe%>%
#   drop_na(season)
# globe<- globe%>%
#   drop_na(Q67)
pli_globe$Q67<- as.factor(pli_globe$Q67)
summary(pli_globe$Q67)
globea <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globea)

globe67<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q67+ prox.normal:season+ pH+location_2+location_2:Q67+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe67)    

globe67c<- lmer(data= pli_globe,
               pli.ln~ season+ prox.normal+ Q67+ prox.normal:season+ 
                 (1|community:site),
               REML = F) #ML for comparison, REML for final
summary(globe67c)  
globe.step<- step(globe67)
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
# globe71<-full_join(globedat, hds71, by = c("site"))
# globe71 <- globe71%>%
#   drop_na(prox.normal)
# globe71<- globe71%>%
#   drop_na(season)
# globe71<- globe71%>%
#   drop_na(Q71)
pli_globe$Q71<- as.factor(pli_globe$Q71)
summary(pli_globe$Q71)
globe71a <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe71a)

globe71b<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q71+ prox.normal:season+ pH+location_2+location_2:Q71+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe71b)         
globe.step <- step(globe71b)
globe.step
globe2 <- get_model(globe.step)
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
print(summary(globe2))
performance(globe2)



#Q79====
# globe79<-full_join(globedat, hds79, by = c("site"))
# globe79 <- globe79%>%
#   drop_na(prox.normal)
# globe79<- globe79%>%
#   drop_na(season)
# globe79<- globe79%>%
#   drop_na(Q79)
pli_globe$Q79<- as.factor(pli_globe$Q79)
summary(pli_globe$Q79)
pli_globe <- pli_globe[!(pli_globe$Q79==100), ]
summary(pli_globe$Q79)

globe <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe79b<- lmer(data=pli_globe,
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



#forest plot----
tidy_globe79c <- tidy(globe79c, effects = "fixed", conf.int = TRUE)
tidy_globe79c <- tidy_globe79c %>%
  mutate(term = ifelse(term == "Q792", "Do not Remove cistern filter/screen", term))
tidy_globe79c <- tidy_globe79c %>%
  mutate(term = ifelse(term == "Q791:location_2Globe Area", "Remove cistern filter/screen (Globe Area)", term))
tidy_globe79c <- tidy_globe79c %>%
  mutate(term = ifelse(term == "Q792:location_2Globe Area", "Do not Remove cistern filter/screen (Globe Area)", term))
tidy_globe79c <- tidy_globe79c %>%
  mutate(term = ifelse(term == "	Q792:location_2Globe Area", "Do not Remove cistern filter/screen (Globe Area)", term))
tidy_globe79c <- tidy_globe79c %>%
  mutate(term = ifelse(term == "Q791:location_2Canyons Area", "Remove cistern filter/screen (Canyons Area)", term))
tidy_globe79c <- tidy_globe79c %>%
           mutate(term = ifelse(term == "Q792:location_2Canyons Area", "Remove cistern filter/screen (Canyons Area)", term))     
tidy_globe79c$Significant <- ifelse(tidy_globe79c$p.value < 0.05, "Significant", "Not Significant")

ggplot(tidy_globe79c, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Model Variables") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()




#Q76====
#Does your cistern have a first flush?
# globe76<-full_join(globedat, hds76, by = c("site"))
# globe76 <- globe76%>%
#   drop_na(prox.normal)
# globe76<- globe76%>%
#   drop_na(season)
# globe76<- globe76%>%
#   drop_na(Q76)
pli_globe$Q76<- as.factor(pli_globe$Q76)
summary(pli_globe$Q76)
globe <- lmer(data = pli_globe
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe76b<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q76+ prox.normal:season+ pH+location_2:Q76+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe76b)                
vif(globe76b)

globe.step <- step(globe76b)
globe.step
globe2 <- get_model(globe.step)
check_model(globe2)
vif(globe2)
anova(globe2)
print(anova(globe2))
print(summary(globe2))
performance(globe2)

#forest plot----
tidy_globe76b <- tidy(globe2, effects = "fixed", conf.int = TRUE)
tidy_globe76b <- tidy_globe76b%>%
  mutate(term = ifelse(term == "Q762", "No first flush", term))
tidy_globe76b <- tidy_globe76b %>%
  mutate(term = ifelse(term == "Q761:location_2Globe Area", "Have First Flush (Globe Area)", term))
tidy_globe76b <- tidy_globe76b %>%
  mutate(term = ifelse(term == "Q762:location_2Globe Area", "No First Flush (Globe Area)", term))
tidy_globe76b <- tidy_globe76b %>%
  mutate(term = ifelse(term == "Q761:location_2Canyons Area", "Have First Flush (Canyons Area)", term))
tidy_globe76b <- tidy_globe76b %>%
  mutate(term = ifelse(term == "Q762:location_2Canyons Area", "No First Flush (Canyons Area)", term))
tidy_globe76b$Significant <- ifelse(tidy_globe76b$p.value < 0.05, "Significant", "Not Significant")

ggplot(tidy_globe76b, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Model Variables") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()













#Q77====
#Does your cistern have a screen/filter for incoming water from down spout on top of the tank?
# globe77<-full_join(globedat, hds77, by = c("site"))
# globe77 <- globe77%>%
#   drop_na(prox.normal)
# globe77<- globe77%>%
#   drop_na(season)
# globe77<- globe77%>%
#   drop_na(Q77)
pli_globe$Q77<- as.factor(pli_globe$Q77)
summary(pli_globe$Q77)
globe <- lmer(data = pli_globe,
               pli.ln ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(globe)

globe77b<- lmer(data= pli_globe,
              pli.ln~ season+ prox.normal+ Q77+ prox.normal:season+ pH+location_2:Q77+location_2+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(globe77b)   
anova(globe77b)
vif(globe77b)
globe.step <- step(globe77b)
globe.step
globe2b <- get_model(globe.step)
print(summary(globe2b))
check_model(globe2b)
vif(globe2b)
print(anova(globe2b))
print(summary(globe2b))
performance(globe2b)


#forest plot----
tidy_globe2b <- tidy(globe2b, effects = "fixed", conf.int = TRUE)
tidy_globe2b <- tidy_globe2b%>%
  mutate(term = ifelse(term == "Q772", "No cistern screen", term))
tidy_globe2b <- tidy_globe2b %>%
  mutate(term = ifelse(term == "location_2Globe Area", "Globe Area", term))
tidy_globe2b <- tidy_globe2b %>%
  mutate(term = ifelse(term == "location_2Canyons Area", "Canyons Area", term))
tidy_globe2b <- tidy_globe2b %>%
  mutate(term = ifelse(term == "Q772:location_2Globe Area", "No cistern screen (Globe Area)", term))
tidy_globe2b <- tidy_globe2b %>%
  mutate(term = ifelse(term == "Q772:location_2Canyons Area", "No cistern screen (Canyons Area)", term))

ggplot(tidy_globe2b, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Model Variables") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()


#Q60: What is your cistern made of?----

pli_globe$Q60<- as.factor(pli_globe$Q60)
summary(pli_globe$Q60)
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe,
              pli~ season+ prox.normal+ Q60+ prox.normal:season+ pH+location_2+
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)
anova(model2) #Q77 not  significant
globe.step2 <- step(model2)
globe.step2
model3 <- get_model(globe.step2)
check_model(model3 )
vif(model3 )
anova(model3 )
print(anova(model3 ))
print(summary(model3))
performance(model3 )

#forest plot----
tidy_globe3 <- tidy(model3, effects = "fixed", conf.int = TRUE)
tidy_globe3 <- tidy_globe3%>%
  mutate(term = ifelse(term == "Q60Other", "Other Materials", term))
tidy_globe3 <- tidy_globe3 %>%
  mutate(term = ifelse(term == "Q60Plastic", "Plastic Cistern", term))


ggplot(tidy_globe3, aes(x = estimate, y = reorder(term, estimate), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size= 1, color = "darkred") +
  geom_point(aes(fill = "blue"), color = "black", size = 14, shape = 5) + # Use shape 21 for fill and border color
  geom_errorbarh(aes(height = 0.2), color = "darkblue", width = 1) + # Dark red color, specified width
  labs(x = "Model Point estimates and 95% confidence intervals", y = "Model Variables") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text = element_text(color = "black", size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth= 2),
    legend.position = "none" # Hide legend since we're not varying fill based on data
  ) +
  scale_fill_identity()




#overall pli forest=====

tidy_tuc2$Community <- 'Tucson'
tidy_tuc2$PLI<- 'Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?'
tidy_tuc2<- tidy_tuc2[, !colnames(tidy_tuc2) %in% c("effect", "df", "Significant")]
tidy_globe79c$Community<- 'Globe'
tidy_globe79c$PLI<- 'Do you ever remove the screen/filter and leave your cistern without the filter?'
tidy_globe79c <- tidy_globe79c[, !colnames(tidy_globe79c) %in% c("Significant")]
tidy_globe76b$Community <- 'Globe'
tidy_globe76b$PLI<- 'Does your cistern have a first flush?'
tidy_globe76b <- tidy_globe76b[, !colnames(tidy_globe76b)  %in% c("effect", "df", "Significant")]
tidy_globe2b$Community <- 'Globe'
tidy_globe2b$PLI<- 'Does your cistern have a screen/filter for incoming water from down spout on top of the tank?'
tidy_globe3$Community<- 'Globe'
tidy_globe3$PLI<- 'How old is your cistern'
  
combined_models2 <- rbind(tidy_tuc2, tidy_globe79c, tidy_globe76b, tidy_globe2b, tidy_globe3)

combined_models2$Significant <- ifelse(combined_models2$p.value < 0.05, "Significant", "Not Significant")

ggplot(combined_models2, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "solid", size = 0.1, color = "darkred") +
  geom_point(aes(fill = Significant), color = "black", size = 2.5, shape = 5) +  # Fill color based on significance
  geom_errorbarh(aes(height = 0.4, color = Significant)) +  # Error bar color based on significance
  facet_wrap(~PLI + Community, scales = "free_y", ncol = 1) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(color = "black", size = 10, face = "bold"),
    axis.title.y = element_text(color = "black", size = 10, face = "bold"),
    axis.text = element_text(color = "black", size = 8, face = "bold"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size =10, hjust = 0.5),
    plot.background = element_rect(color = "darkblue", fill = "white", linewidth = 2),
    strip.text = element_text(size = 8, face = "bold", color= "darkblue"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(1, "pt")
  ) +
  labs(x = "Point estimates and 95% confidence intervals", y= "Variables") +
  scale_fill_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +  # Manual color scale for significance
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue"))  # Match error bar colors
dev.print(png, "RHI_ModelSummary.png", res=300, height=8, width=8, units="in")

  

#Q65: How old is your cistern:----
# globe65 <- full_join(haydendat, hds65, by = c("site"))
# globe65 <- globe65 %>%
#   drop_na(prox.normal)
# globe65 <- globe65 %>%
#   drop_na(season)
# globe65 <- globe65%>%
#   drop_na(Q65)
pli_globe$Q65<- as.factor(pli_globe$Q65)
summary(pli_globe$Q65)
model1 <- lmer(data = pli_globe,
               pli ~ (1|community:site),
               REML = T) #ML for comparison, REML for final
summary(model1)

model2<- lmer(data= pli_globe,
              pli~ season+ prox.normal+ Q65+ prox.normal:season+ pH+ 
                (1|community:site),
              REML = F) #ML for comparison, REML for final
summary(model2)                
anova(model2) #Q77 not  significant
glo.step2 <- step(model2)
glo.step2
model3 <- get_model(glo.step2)
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

#pli hayden modeling ----
pli_hayden<- iw.dm[iw.dm$community=="hayden-Humboldt",]
pli_hayden<- pli_hayden%>%
  drop_na(prox.normal)
pli_hayden<- pli_hayden%>%
  drop_na(pli.ln)
pli_hayden<-pli_hayden%>%
  drop_na(season)
pli_hayden<-pli_hayden%>%
  drop_na(pH)
pld0 <- lmer(data =pli_hayden,
             pli.ln ~ (1|community:site),
             REML = T) #ML for comparison, REML for final
summary(pld0)

pld <- lmer(data = pli_hayden,
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






