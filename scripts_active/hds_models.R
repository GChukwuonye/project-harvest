#Kunal Palawat, Gift Chukwuonye
#Description: modeling analytes with rhrw infrastructure maintainence score
#

#load libraries----
#base
library(readxl) #read excel files
library(tidyverse)
library(ggplot2)

#models
library(EnvStats)
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

#data formatting----
#iw.score contains samples with homes that had at least one response to the home description survey. if not all 5 questions were answered, a value of 0 was automatically assigned as a conservative assumption of behavior
#view(iw.score)
iw.score.long <- pivot_longer(data = iw.score,
                              cols = Be:Pb,
                              values_to = "concentration",
                              names_to = "analyte")

#split dataframe into different ones for each community
iws.c <- iw.score %>%
  group_by(community) %>%
  group_split()

iws.dh <- iws.c[[1]]
iws.gm <- iws.c[[2]]
iws.hw <- iws.c[[3]]
iws.tu <- iws.c[[4]]

#initial viz----
ggplot(iw.score.long, mapping = aes(y = log(concentration), x = score_bin, fill = community)) +
  geom_boxplot() +
  #scale_fill_manual(values = wes_palette(name = "Darjeeling2", n = 5)) +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  #scale_fill_viridis_d() +
  facet_grid(.~analyte, scales = "free") +
  labs(x = "\nMaintenance Score",
       y = "[analyte] (ln(ug/L))\n",
       title = "Boxplots of maintenance score by analyte",
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
dev.print(png, "maintenancescore_analyte_com.png", res=300, height=8, width=18, units="in")

#summaries ----
sumFX(datalongDF = iw.score.long,
      subset.vector.string = c("analyte", "score_bin","season", "community"),
      value.string = "concentration",
      dfname.string = "sum.score.ssn.com",
      filename.string = "score_ssncom")

#zinc, aluminum, copper, iron, manganese have highest standard deviations
#manganese, lead, copper, cadmium, aluminum, zinc, nickel have highest geometric standard deviations

#analytes with a sd > 1
#DH
#Al,As,Ba,Cr,Cu,Fe,Mn,Mo,Ni,Pb,Se,V,Zn
#GM
#Al,As,Ba,Cd,Co,Cr,Cu,Fe,Mn,Mo,Ni,Pb,Se,Sn,V,Zn
#
#HW
#Al,As,Ba,Cr,Cu,Fe,Mn,Mo,Pb,Sb,Se,Sn,V,Zn
#
#TU
#Al,As,Ba,Cr,Cu,Fe,Mn,Mo,Ni,Pb,Sb,Se,V,Zn

#initial modeling ----
#anovas

zn.0 <- lm(data = iw.score,
           log(Zn) ~ score_bin+community + season + prox.normal)
summary(zn.0)
check_model(zn.0)
performance(zn.0) #very low r2
anova(zn.0) #score almost signif
plot(allEffects(zn.0))

al.0 <- lm(data = iw.score,
           log(Al) ~ score_bin + community + season + prox.normal)
summary(al.0)
check_model(al.0)
performance(al.0) #low r2
anova(al.0) #score not signif
plot(allEffects(al.0))

cu.0 <- lm(data = iw.score,
           log(Cu) ~ score_bin + community + season + prox.normal)
summary(cu.0)
check_model(cu.0)
performance(cu.0)
anova(cu.0) #score signif
plot(allEffects(cu.0))

pb.0 <- lm(data = iw.score,
           log(Pb) ~ score_bin + community + season + prox.normal)
summary(pb.0)
check_model(pb.0)
performance(pb.0) #low r2
anova(pb.0) #score signif
plot(allEffects(pb.0))

cd.0 <- lm(data = iw.score,
           log(Cd) ~ score_bin + community + season + prox.normal)
summary(cd.0)
check_model(cd.0)
performance(cd.0)
anova(cd.0) #score signif
plot(allEffects(cd.0))


mn.0 <- lm(data = iw.score,
           log(Mn) ~ score_bin + community + season + prox.normal)
summary(mn.0)
check_model(mn.0)
performance(mn.0)
anova(mn.0) #score not signif
plot(allEffects(mn.0))

#stepwise
#stepwise community modeling ----
## gm ----

hdslmerstepFX <- function(dataDF, analyte.string, dfname.string){
  
  dat <- dataDF
  analyte <- analyte.string
  dfname <- dfname.string
  
  mm.0 <- lmer(data = dat,
               log(dat[[analyte]]) ~
                 (1|site),
               REML = F)
  summary(mm.0)
  assign(paste(dfname, ".0", sep = ""), mm.0, envir=.GlobalEnv)
  
  mm.1 <- lmer(data = dat,
               log(dat[[analyte]]) ~ season + prox.normal + score_bin +
                 (1|site),
               REML = F)
  summary(mm.1)
  assign(paste(dfname, ".1", sep = ""), mm.1, envir=.GlobalEnv)
  
  mm.2.step <- lmerTest::step(mm.1)
  mm.2.step
  mm.2 <- get_model(mm.2.step)
  print(summary(mm.2))
  print(anova(mm.2))
  assign(paste(dfname, ".2", sep = ""), mm.2, envir=.GlobalEnv)
  
}

hdslmerstepFX(dataDF = iws.gm,
              analyte.string = "Al",
              dfname.string = "al.gm")


mm.0 <- lmer(data = datt,
                 log(datt[[analytee]]) ~ season +
              (1|site),
                 REML = F)
datt <- iws.gm
dfnamee <- "al.gm.0"
analytee <- "Al"
assign(paste(dfnamee), mm.0, envir=.GlobalEnv)

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
exp(coef(aimn.gm.2))
performance(aimn.gm.2)


#functions ----
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
