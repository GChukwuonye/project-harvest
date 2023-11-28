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

iws.tu <- iws.tu %>%
  drop_na(prox.normal)

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
contam_list <- list("Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")

## dh ----
###Mn ----
mn.dh.0 <- lmer(data = iws.dh,
             log(Mn) ~
               (1|site),
             REML = F)
print(summary(mn.dh.0))

mn.dh.1 <- lmer(data = iws.dh,
             log(Mn) ~ season + prox.normal + score_bin +
               (1|site),
             REML = F)
print(summary(mn.dh.1))

mn.dh.2.step <- step(mn.dh.1)
mn.dh.2.step
mn.dh.2 <- get_model(mn.dh.2.step)
print(summary(mn.dh.2))
anova(mn.dh.1)
print(anova(mn.dh.2))
#season only

###Ba ----
ba.dh.0 <- lmer(data = iws.dh,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.dh.0))

ba.dh.1 <- lmer(data = iws.dh,
                log(Ba) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(ba.dh.1))

ba.dh.2.step <- step(ba.dh.1)
ba.dh.2.step
ba.dh.2 <- get_model(ba.dh.2.step)
print(summary(ba.dh.2))
anova(ba.dh.1)
print(anova(ba.dh.2))
#season only

###Mo ----
mo.dh.0 <- lmer(data = iws.dh,
                log(Mo) ~
                  (1|site),
                REML = F)
print(summary(mo.dh.0))

mo.dh.1 <- lmer(data = iws.dh,
                log(Mo) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(mo.dh.1))

mo.dh.2.step <- step(mo.dh.1)
mo.dh.2.step
mo.dh.2 <- get_model(mo.dh.2.step)
print(summary(mo.dh.2))
anova(mo.dh.1)
print(anova(mo.dh.2))
#season only

###Ni ----
ni.dh.0 <- lmer(data = iws.dh,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.dh.0))

ni.dh.1 <- lmer(data = iws.dh,
                log(Ni) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(ni.dh.1))

ni.dh.2.step <- step(ni.dh.1)
ni.dh.2.step
ni.dh.2 <- get_model(ni.dh.2.step)
print(summary(ni.dh.2))
anova(ni.dh.1)
print(anova(ni.dh.2))
#season only

###V ----
v.dh.0 <- lmer(data = iws.dh,
                log(V) ~
                  (1|site),
                REML = F)
print(summary(v.dh.0))

v.dh.1 <- lmer(data = iws.dh,
                log(V) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(v.dh.1))

v.dh.2.step <- step(v.dh.1)
v.dh.2.step
v.dh.2 <- get_model(v.dh.2.step)
print(summary(v.dh.2))
anova(v.dh.1)
print(anova(v.dh.2))
#season only
# 
# 
# lapply(X = contam_list,
#        FUN = hdslmerstepFX,
#        dataDF = iw.score.long,
#        dfname.string = ".dh")
# 
# hdslmerstepFX(datalongDF = iws.dh[iws.dh$analyte == "Mn",],
#               analyte.string = "Mn",
#               dfname.string = ".dh")


## gm ----
###V ----
v.gm.0 <- lmer(data = iws.gm,
               log(V) ~
                 (1|site),
               REML = F)
print(summary(v.gm.0))

v.gm.1 <- lmer(data = iws.gm,
               log(V) ~ season + prox.normal + score_bin +
                 (1|site),
               REML = F)
print(summary(v.gm.1))

v.gm.2.step <- step(v.gm.1)
v.gm.2.step
v.gm.2 <- get_model(v.gm.2.step)
print(summary(v.gm.2))
anova(v.gm.1)
print(anova(v.gm.2))
#season only

###Co ----
co.gm.0 <- lmer(data = iws.gm,
               log(Co) ~
                 (1|site),
               REML = F)
print(summary(co.gm.0))

co.gm.1 <- lmer(data = iws.gm,
               log(Co) ~ season + prox.normal + score_bin +
                 (1|site),
               REML = F)
print(summary(co.gm.1))

co.gm.2.step <- step(co.gm.1)
co.gm.2.step
co.gm.2 <- get_model(co.gm.2.step)
print(summary(co.gm.2))
anova(co.gm.1)
print(anova(co.gm.2))
#season only

###Mn ----
mn.gm.0 <- lmer(data = iws.gm,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(mn.gm.0))

mn.gm.1 <- lmer(data = iws.gm,
                log(Mn) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(mn.gm.1))

mn.gm.2.step <- step(mn.gm.1)
mn.gm.2.step
mn.gm.2 <- get_model(mn.gm.2.step)
print(summary(mn.gm.2))
anova(mn.gm.1)
print(anova(mn.gm.2))
#season only, almost prox

###Al ----
al.gm.0 <- lmer(data = iws.gm,
                log(Al) ~
                  (1|site),
                REML = F)
print(summary(al.gm.0))

al.gm.1 <- lmer(data = iws.gm,
                log(Al) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(al.gm.1))

al.gm.2.step <- step(al.gm.1)
al.gm.2.step
al.gm.2 <- get_model(al.gm.2.step)
print(summary(al.gm.2))
anova(al.gm.1)
print(anova(al.gm.2))
#prox and score

###Cr ----
cr.gm.0 <- lmer(data = iws.gm,
                log(Cr) ~
                  (1|site),
                REML = F)
print(summary(cr.gm.0))

cr.gm.1 <- lmer(data = iws.gm,
                log(Cr) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(cr.gm.1))

cr.gm.2.step <- step(cr.gm.1)
cr.gm.2.step
cr.gm.2 <- get_model(cr.gm.2.step)
print(summary(cr.gm.2))
anova(cr.gm.1)
print(anova(cr.gm.2))
#season only



lapply(X = contam_list,
       FUN = hdslmerstepFX,
       dataDF = iws.gm,
       dfname.string = ".gm")

## hw ----
###Mn ----
mn.hw.0 <- lmer(data = iws.hw,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(mn.hw.0))

mn.hw.1 <- lmer(data = iws.hw,
                log(Mn) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(mn.hw.1))

mn.hw.2.step <- step(mn.hw.1)
mn.hw.2.step
mn.hw.2 <- get_model(mn.hw.2.step)
print(summary(mn.hw.2))
anova(mn.hw.1)
print(anova(mn.hw.2))
#season and prox

###Cu ----
cu.hw.0 <- lmer(data = iws.hw,
                log(Cu) ~
                  (1|site),
                REML = F)
print(summary(cu.hw.0))

cu.hw.1 <- lmer(data = iws.hw,
                log(Cu) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(cu.hw.1))

cu.hw.2.step <- step(cu.hw.1)
cu.hw.2.step
cu.hw.2 <- get_model(cu.hw.2.step)
print(summary(cu.hw.2))
anova(cu.hw.1)
print(anova(cu.hw.2))
#season and prox

###As ----
as.hw.0 <- lmer(data = iws.hw,
                log(As) ~
                  (1|site),
                REML = F)
print(summary(as.hw.0))

as.hw.1 <- lmer(data = iws.hw,
                log(As) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(as.hw.1))

as.hw.2.step <- step(as.hw.1)
as.hw.2.step
as.hw.2 <- get_model(as.hw.2.step)
print(summary(as.hw.2))
vif(as.hw.1)
anova(as.hw.1)
print(anova(as.hw.2))
#season and prox

###Pb ----
pb.hw.0 <- lmer(data = iws.hw,
                log(Pb) ~
                  (1|site),
                REML = F)
print(summary(pb.hw.0))

pb.hw.1 <- lmer(data = iws.hw,
                log(Pb) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(pb.hw.1))

pb.hw.2.step <- step(pb.hw.1)
pb.hw.2.step
pb.hw.2 <- get_model(pb.hw.2.step)
print(summary(pb.hw.2))
anova(pb.hw.1)
print(anova(pb.hw.2))
#season and prox

###Co ----
co.hw.0 <- lmer(data = iws.hw,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(co.hw.0))

co.hw.1 <- lmer(data = iws.hw,
                log(Co) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(co.hw.1))

co.hw.2.step <- step(co.hw.1)
co.hw.2.step
co.hw.2 <- get_model(co.hw.2.step)
print(summary(co.hw.2))
print(summary(co.hw.1))
anova(co.hw.1)
anova(co.hw.2)
#season and prox and score


lapply(X = contam_list,
       FUN = hdslmerstepFX,
       dataDF = iws.hw,
       dfname.string = ".hw")

## tu ----
###Mn ----
mn.tu.0 <- lmer(data = iws.tu,
                log(Mn) ~
                  (1|site),
                REML = F)
print(summary(mn.tu.0))

mn.tu.1 <- lmer(data = iws.tu,
                log(Mn) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(mn.tu.1))
mn.tu.2.step <- step(mn.tu.1)
mn.tu.2.step
mn.tu.2 <- get_model(mn.tu.2.step)
print(summary(mn.tu.2))
anova(mn.tu.1)
print(anova(mn.tu.2))
#season

###Co ----
co.tu.0 <- lmer(data = iws.tu,
                log(Co) ~
                  (1|site),
                REML = F)
print(summary(co.tu.0))

co.tu.1 <- lmer(data = iws.tu,
                log(Co) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(co.tu.1))
co.tu.2.step <- step(co.tu.1)
co.tu.2.step
co.tu.2 <- get_model(co.tu.2.step)
print(summary(co.tu.2))
anova(co.tu.1)
print(anova(co.tu.2))
#season

###Ni ----
ni.tu.0 <- lmer(data = iws.tu,
                log(Ni) ~
                  (1|site),
                REML = F)
print(summary(ni.tu.0))

ni.tu.1 <- lmer(data = iws.tu,
                log(Ni) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(ni.tu.1))
ni.tu.2.step <- step(ni.tu.1)
ni.tu.2.step
ni.tu.2 <- get_model(ni.tu.2.step)
print(summary(ni.tu.2))
anova(ni.tu.1)
print(anova(ni.tu.2))
#season

###Zn ----
zn.tu.0 <- lmer(data = iws.tu,
                log(Zn) ~
                  (1|site),
                REML = F)
print(summary(zn.tu.0))

zn.tu.1 <- lmer(data = iws.tu,
                log(Zn) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(zn.tu.1))
zn.tu.2.step <- step(zn.tu.1)
zn.tu.2.step
zn.tu.2 <- get_model(zn.tu.2.step)
print(summary(zn.tu.2))
anova(zn.tu.1)
print(anova(zn.tu.2))
#prox

###Ba ----
ba.tu.0 <- lmer(data = iws.tu,
                log(Ba) ~
                  (1|site),
                REML = F)
print(summary(ba.tu.0))

ba.tu.1 <- lmer(data = iws.tu,
                log(Ba) ~ season + prox.normal + score_bin +
                  (1|site),
                REML = F)
print(summary(ba.tu.1))
ba.tu.2.step <- step(ba.tu.1)
ba.tu.2.step
ba.tu.2 <- get_model(ba.tu.2.step)
print(summary(ba.tu.2))
anova(ba.tu.1)
print(anova(ba.tu.2))
#season


lapply(X = contam_list,
       FUN = hdslmerstepFX,
       dataDF = iws.tu,
       dfname.string = ".tu")

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

hdslmerstepFX <- function(datalongDF, analyte.string, dfname.string){
  
  dat <- datalongDF
  analyte.s <- analyte.string
  dfname <- dfname.string
  
  #dat <- datlong[datlong$analyte == analyte.s,]
  
  mm.0 <- lmer(data = dat,
               log(concentration) ~
                 (1|site),
               REML = F)
  print(summary(mm.0))
  assign(paste(analyte.s, dfname, ".0", sep = ""), mm.0, envir=.GlobalEnv)
  
  mm.1 <- lmer(data = dat,
               log(concentration) ~ season + prox.normal + score_bin +
                 (1|site),
               REML = F)
  print(summary(mm.1))
  assign(paste(analyte.s, dfname, ".1", sep = ""), mm.1, envir=.GlobalEnv)
  
  mm.2.step <- step(mm.1)
  #mm.2.step
  #mm.2 <- get_model(mm.2.step)
  #print(summary(mm.2))
  #print(anova(mm.2))
  assign(paste(analyte.s, dfname, ".2", sep = ""), mm.2, envir=.GlobalEnv)
  
}

hdslmerstepFX(datalongDF = iw.score.long[is.score.long$analyte == "Mn",],
              analyte.string = "Mn",
              dfname.string = ".all")

ForwardStep <- function(df,yName, Xs, XsMin) {
  Data <- df[, c(yName,Xs)]
  fit <- glm(formula = paste(yName, " ~ ", paste0(XsMin, collapse = " + ")),
             data = Data, family = binomial(link = "logit") )
  ScopeFormula <- list(lower = paste(yName, " ~ ", paste0(XsMin, collapse = " + ")), 
                       upper = paste(yName, " ~ ", paste0(Xs, collapse = " + ")))
  result <- step(fit, direction = "forward", scope = ScopeFormula, trace = 1 )
  
  return(result)
}


ForwardStep <- function(df,Yname, Xs, XsMin) {
  Data <- df[, c(Yname,Xs)]
  f1 <- as.formula(paste(Yname, " ~ ", paste0(XsMin, collapse = " + ")))
  
  fit <- glm(formula = f1,
             data = Data, family = binomial(link = "logit") )
  f2 <- as.formula(paste(Yname, " ~ ", paste0(XsMin, collapse = " + ")))
  f3 <- as.formula(paste(Yname, " ~ ", paste0(Xs, collapse = " + ")))
  
  ScopeFormula <- list(lower = f2, 
                       upper = f3)
  step(fit, direction = "forward", scope = ScopeFormula, trace = 1)
}






