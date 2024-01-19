#Kunal Palawat
#January 11th, 2024
#Description: Code to analyze rainwater data by sociodemographics
#analyzing the following demographics: zipcode, primary language, race/ethnicity, BIPOC, education, household size, income level
#notes
#87 sites with values for all the demographics listed above - might be possible to analyze all of them at once with pli/specific analytes
#control by season and then list all these variables???
#community can be a proxy for race, language, income, zipcode
#for education - do pre and post high school + trade school


#load libraries ----
library(readxl)
library(tidyverse)
library(EnvStats)
library(corrplot)

#summaries----
##demo summaries ----
###overall ----
facsumFX(datalongDF = demo.long,
      subset.vector.string = c("demographic", "value"),
      dfname.string = "sum.demo",
      filename.string = "demo")

###community ----
facsumFX(datalongDF = demo.long,
         subset.vector.string = c("demographic", "community", "value"),
         dfname.string = "sum.demo.com",
         filename.string = "demo_com")

###community_2 ----
facsumFX(datalongDF = demo.long,
         subset.vector.string = c("demographic", "community_2", "value"),
         dfname.string = "sum.demo.com2",
         filename.string = "demo_com2")

##concentration summaries----
###overall----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte"),
      value.string = "value",
      dfname.string = "sum.iwdemo",
      filename.string = "iwdemo")

###community----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community"),
      value.string = "value",
      dfname.string = "sum.iwdemo.com",
      filename.string = "iwdemo_com")

###community_2----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community_2"),
      value.string = "value",
      dfname.string = "sum.iwdemo.com2",
      filename.string = "iwdemo_com2")

###community, season----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community", "season"),
      value.string = "value",
      dfname.string = "sum.iwdemo.comssn",
      filename.string = "iwdemo_comssn")

###community_2, season----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "community_2", "season"),
      value.string = "value",
      dfname.string = "sum.iwdemo.com2ssn",
      filename.string = "iwdemo_com2ssn")

###demos----
####zipcode----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Zip"),
      value.string = "value",
      dfname.string = "sum.iwdemo.zip",
      filename.string = "iwdemo_zip")

####primary language----
#assuming the primary language of the participant is the primary language of the household
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Primary Language"),
      value.string = "value",
      dfname.string = "sum.iwdemo.lang",
      filename.string = "iwdemo_lang")

####race ethnicity----
#assuming all members of the household are the same race as the participant
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Race Ethnicity"),
      value.string = "value",
      dfname.string = "sum.iwdemo.race",
      filename.string = "iwdemo_race")

####BIPOC----
#assuming all members of the household are the same race as the participant
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "BIPOC"),
      value.string = "value",
      dfname.string = "sum.iwdemo.bipoc",
      filename.string = "iwdemo_bipoc")

####Education level----
#assuming the participant is the one in the household with the highest education level
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Highest Levelof Education Completed"),
      value.string = "value",
      dfname.string = "sum.iwdemo.ed",
      filename.string = "iwdemo_ed")

sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Education_grouped"),
      value.string = "value",
      dfname.string = "sum.iwdemo.ed2",
      filename.string = "iwdemo_ed2")

####Household Size----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Household Size"),
      value.string = "value",
      dfname.string = "sum.iwdemo.size",
      filename.string = "iwdemo_size")

####Household Size 2----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Household Size 2"),
      value.string = "value",
      dfname.string = "sum.iwdemo.size2",
      filename.string = "iwdemo_size2")

####Income Level----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Income Level"),
      value.string = "value",
      dfname.string = "sum.iwdemo.inc",
      filename.string = "iwdemo_inc")

####Low Income Level----
sumFX(datalongDF = iw.demo.long,
      subset.vector.string = c("analyte", "Low Income"),
      value.string = "value",
      dfname.string = "sum.iwdemo.inc",
      filename.string = "iwdemo_inc")


#Initial correlations ----
##community----
#community x zip are correlated, definitely dont use both in a single model
#community x language are correlated
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Primary Language",
      filename.string = "X2_comlang")

#community and race are correlated - does this mean we cannot have both those variables in the same model?
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Race Ethnicity",
      filename.string = "X2_comrace")

xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "BIPOC",
      filename.string = "X2_compoc")

#com x ed are correlated
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Education_grouped",
      filename.string = "X2_comed")

#com x household size
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Household Size 2",
      filename.string = "X2_comsize")

#com x income
xtest(dataDF = demo,
      var1.string = "community_2",
      var2.string = "Low Income",
      filename.string = "X2_comsize")

##proximity ----
#prox x language not collinear?

plang <- lm(data = demo,
            prox.normal~`Primary Language`+community)
summary(plang)

ggplot(data = demo, mapping = aes(x = prox.normal, fill = `Primary Language`))+
  geom_histogram() +
  facet_wrap(.~community_2)

#prox x race only correlated for Tucson
ggplot(data = demo, mapping = aes(x = prox.normal, fill = `Race Ethnicity`))+
  geom_histogram() +
  facet_grid(.~community_2) +
  theme(legend.position = "bottom")

#correlated for globe?
ggplot(data = demo, mapping = aes(x = prox.normal, fill = `BIPOC`))+
  geom_histogram() +
  facet_grid(.~community) +
  theme(legend.position = "bottom")

#prox x education only correlated for Tucson
ggplot(data = demo, mapping = aes(x = prox.normal, fill = `Education_grouped`))+
  geom_histogram() +
  #facet_grid(.~community) +
  theme(legend.position = "bottom")

#prox x household size no clear correlation
ggplot(data = demo, mapping = aes(x = prox.normal, fill = `Household Size 2`))+
  geom_histogram() +
  facet_grid(.~community) +
  theme(legend.position = "bottom")

#prox x income
ggplot(data = demo, mapping = aes(x = proximity.km, fill = `Low Income`))+
  geom_histogram() +
  facet_grid(.~community_2) +
  theme(legend.position = "bottom")

#ANOVAs ----
##pli, multivariate ----
iwm.demo <- iw.demo %>%
  filter(BIPOC!="Other") %>%
  drop_na(Zip)%>%
  drop_na(`Primary Language`)%>%
  drop_na(BIPOC)%>%
  drop_na(Education_grouped)%>%
  drop_na(prox.normal)%>%
  drop_na(community_2) %>%
  drop_na(`Low Income`)%>%
  semi_join(demo %>%
              count(Zip) %>%
              filter(n >=3))

#Language removed because spanish speakers are only in tucson

pli.demo.1 <- lm(data = iwm.demo,
            log(pli) ~ season + community_2+ BIPOC + `Low Income`+ `Education_grouped`+Zip+prox.normal)
summary(pli.demo.1)
vif(pli.demo.1)
check_model(pli.demo.1)
#as expected, zipcode, community_2, and language are highly correlated

pli.demo.2 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC + `Low Income`+ `Primary Language` + `Education_grouped`+Zip)
summary(pli.demo.2)
vif(pli.demo.2)
check_model(pli.demo.2)
#zipcode is highly colinear...but not sure with what

pli.demo.3 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC + `Low Income`+ `Primary Language` + `Education_grouped`+community_2)
summary(pli.demo.3)
vif(pli.demo.3)
check_model(pli.demo.3)
#community_2 colinear

pli.demo.4 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC + `Low Income`+ `Primary Language` + `Education_grouped`)
summary(pli.demo.4)
vif(pli.demo.4)
check_model(pli.demo.4)
anova(pli.demo.4)
performance(pli.demo.4)

step(pli.demo.1)
###best linear model ----
pli.demo.5 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC+Zip+season)
summary(pli.demo.5)
vif(pli.demo.5)
check_model(pli.demo.5)
anova(pli.demo.5)
performance(pli.demo.5)
plot(allEffects(pli.demo.5))
#season and ZIP and BIPOC significant

pli.demo.5.1 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC+Zip)
anova(pli.demo.5, pli.demo.5.1)
compare_performance(pli.demo.5, pli.demo.5.1)

pli.demo.6 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC*Zip)
summary(pli.demo.6)
vif(pli.demo.6)
check_model(pli.demo.6)
anova(pli.demo.6)

pli.demo.7 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC+community_2+season+Zip+prox.normal)
summary(pli.demo.7)
vif(pli.demo.7)
check_model(pli.demo.7)
anova(pli.demo.7) #proximity is NOT significant

pli.demo.7.1 <- lm(data = iwm.demo,
                 log(pli) ~ prox.normal)
summary(pli.demo.7.1)


##pli bivariate ----
pli.demorace.1 <- lm(data = iwm.demo,
                 log(pli) ~ BIPOC*community_2)
summary(pli.demorace.1)
vif(pli.demorace.1)
check_model(pli.demorace.1)
#all collinear...expected, not sure what to do

pli.demorace.2 <- lm(data = iwm.demo,
                     log(pli) ~ BIPOC+community_2)
summary(pli.demorace.2)
vif(pli.demorace.2)
check_model(pli.demorace.2)
anova(pli.demorace.2)
performance(pli.demorace.2)

step(pli.demorace.1)

#Functions----
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
    drop_na(all_of(cols))%>%
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

facsumFX <- function(datalongDF, subset.vector.string, dfname.string, filename.string){
  
  #load libraries
  library(tidyverse)
  library(EnvStats)
  
  #assign data
  dat.long <- datalongDF
  cols <- subset.vector.string
  dfname <- dfname.string
  filename <- filename.string
  
  #calculate summary stats
  demon <- demo.long %>%
    mutate(value = factor(value)) %>%
    mutate(demographic = factor(demographic))%>%
    group_by(across(all_of(cols))) %>%
    summarise(n=n())
  
  demoN <- demon%>%
    group_by(demographic)%>%
    summarize(N=sum(n))
  
  sumtable <- full_join(demon, demoN, by = "demographic")
  sumtable$perc <- paste(round(sumtable$n/sumtable$N*100, 1), "%", sep="")
  
  #save as csv file in your working directory
  write.csv(sumtable, paste(filename,"_sum.csv", sep = ""))
  
  #copy to new dataframe with a unique name and place in global environment
  assign(paste(dfname), sumtable, envir=.GlobalEnv)
  
}

xtest <- function(dataDF, var1.string, var2.string, filename.string){
  library(corrplot)
  library(tidyverse)
  
  dat2 <- dataDF
  var1 <- var1.string
  var2 <- var2.string
  filename <- filename.string
  
  dat <- dat2 %>%
    drop_na(all_of(var1)) %>%
    drop_na(all_of(var2))
    
  X2 <- chisq.test(table(pull(dat,var1), pull(dat,var2)))
  print(X2)
  X2$residuals
  p1 <- corrplot(X2$residuals, is.cor = FALSE)
  print(p1)
  dev.print(jpeg, paste(filename, ".jpg", sep = ""), res=300, height=7, width=7, units="in")
  
  # contrib <- 100*X2$residuals^2/X2$statistic
  # corrplot(contrib, is.cor = FALSE)
  # dev.print(jpeg, paste(filename, ".jpg", sep = ""), res=300, height=7, width=7, units="in")
  # 
  
}


#scratch ----
# 
# demon <- demo.long %>%
#   mutate(value = factor(value)) %>%
#   mutate(demographic = factor(demographic))%>%
#   group_by(demographic, value) %>%
#   summarise(n=n())
# 
# demoN <- demon%>%
#   group_by(demographic)%>%
#   summarize(N=sum(n))
# 
# democount <- full_join(demon, demoN, by = "demographic")
# democount$perc <- paste(round(democount$n/democount$N*100, 1), "%", sep="")
