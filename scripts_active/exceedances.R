#
##
###
###
###
###
###
###
###
###
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

#summary ----
#name subset columns
cols <- c("standard", "analyte")

#calculate counts and percentages of the whole
sumtable <- ex.dat.long %>%
  group_by(across(all_of(cols))) %>%
  summarise(n = n(),
            exceedances_n = sum(exceedance),
            exceedances_freq = signif(sum(exceedance)/n()*100, 2))

sumtable.small <- subset(sumtable, select = -c(exceedances_n))

sumtable.wide <- pivot_wider(data = sumtable.small,
                             names_from = "standard",
                             values_from = "exceedances_freq")
view(sumtable.wide)
write.csv(sumtable.wide, "exceedance%_overall.csv")

#viz ----
ggplot(ex.dat.long, mapping = aes(x = analyte, fill = as.factor(exceedance))) +
  geom_bar() +
  scale_fill_manual(values = wes_palette(name = "Darjeeling2", n = 4),
                    labels = c("No Exceedance", "Exceedance")) +
  #scale_fill_viridis_d() +
  facet_grid(standard~., scales = "free") +
  labs(x = "\nAnalyte",
       y = "Counte\n",
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
ex.dat.long.prox <- ex.dat.long %>%
  drop_na(proximity.km)

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

#Scratch Work ----

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