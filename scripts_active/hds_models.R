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

#initial viz
ggplot(iw.score.long, mapping = aes(y = log(concentration), x = score_bin, fill = score_bin)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(name = "Darjeeling2", n = 5)) +
  #scale_fill_viridis_d() +
  facet_wrap(analyte~., scales = "free") +
  labs(x = "\nMaintenance Score",
       y = "[analyte] (ln(ug/L))\n",
       title = "Boxplots of maintenance score by analyte",
       fill = "")+
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 15),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        axis.text = element_text(vjust = .5, color = "black"),
        axis.text.x = element_text(vjust = .5),
        legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())
dev.print(png, "maintenancescore_analyte.png", res=300, height=10, width=12, units="in")

#summary ----
##project ----
sumFX(datalongDF = iw.score.long,
      subset.vector.string = c("analyte", "score_bin"),
      value.string = "concentration",
      dfname.string = "sum.score",
      filename.string = "score")

#zinc, aluminum, copper, iron, manganese have highest standard deviations
#manganese, lead, copper, cadmium, aluminum, zinc, nikel have highest geometric standard deviations

#initial modeling ----
#anovas

zn.0 <- lm(data = iw.score,
           log(Zn) ~ score_bin + community + season + proximity.km)
summary(zn.0)
check_model(zn.0)
performance(zn.0)
anova(zn.0) #score signif
plot(allEffects(zn.0))

al.0 <- lm(data = iw.score,
           log(Al) ~ score_bin + community + season + proximity.km)
summary(al.0)
check_model(al.0)
performance(al.0)
anova(al.0) #score signif
plot(allEffects(al.0))

cu.0 <- lm(data = iw.score,
           log(Cu) ~ score_bin + community + season + proximity.km)
summary(cu.0)
check_model(cu.0)
performance(cu.0)
anova(cu.0) #score signif
plot(allEffects(cu.0))

pb.0 <- lm(data = iw.score,
           log(Pb) ~ score_bin + community + season + proximity.km)
summary(pb.0)
check_model(pb.0)
performance(pb.0)
anova(pb.0) #score not signif
plot(allEffects(pb.0))

cd.0 <- lm(data = iw.score,
           log(Cd) ~ score_bin + community + season + proximity.km)
summary(cd.0)
check_model(cd.0)
performance(cd.0)
anova(cd.0) #score not signif
plot(allEffects(cd.0))

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
