#Kunal Palawat
#January 11th, 2024
#Description: Code to analyze rainwater data by sociodemographics
#analyzing the following demographics: zipcode, primary language, race/ethnicity, BIPOC, education, household size, income level

#load libraries ----
library(readxl)
library(tidyverse)
library(EnvStats)

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
