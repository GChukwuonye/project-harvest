#Detection analysis
#Kunal Palawat
# Code to analyze counts of Project Harvest Detections
# 

#load libraries ----
library(tidyverse)
library(EnvStats)

#load data ----
iw.dm.detects <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_clean/IW_DM_Y123.xlsx", sheet = "Detection", col_names = TRUE)

mlod <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/IPSW_MLODS.xlsx", sheet = "corrected - 12.22.20", col_names = TRUE)
iw.mlod <- mlod[mlod$`Sample Type`=="IW",]
iw.mlod.dm <- iw.mlod[iw.mlod$Analysis=="DM",]
#For the detection data,
#detect data
#add period and season variables
iw.dm.detects$period <- iw.dm.detects$samplings
iw.dm.detects$season <- iw.dm.detects$samplings


#redefine them
iw.dm.detects[iw.dm.detects$period=="First Winter",]$period <- "First"
iw.dm.detects[iw.dm.detects$period=="Last Winter",]$period <- "Last"
iw.dm.detects[iw.dm.detects$period=="First Monsoon",]$period <- "First"
iw.dm.detects[iw.dm.detects$period=="Last Monsoon",]$period <- "Last"

iw.dm.detects[iw.dm.detects$season=="First Winter",]$season <- "Winter"
iw.dm.detects[iw.dm.detects$season=="Last Winter",]$season <- "Winter"
iw.dm.detects[iw.dm.detects$season=="First Monsoon",]$season <- "Monsoon"
iw.dm.detects[iw.dm.detects$season=="Last Monsoon",]$season <- "Monsoon"


#changing year
iw.dm.detects$year<-iw.dm.detects$sampling_year

iw.dm.detects[iw.dm.detects$sampling_year=="2017-2018",]$sampling_year <- "Water Year 1"
iw.dm.detects[iw.dm.detects$sampling_year=="2018-2019",]$sampling_year <- "Water Year 2"
iw.dm.detects[iw.dm.detects$sampling_year=="2019-2020",]$sampling_year <- "Water Year 3"

#remove year 3 monsoon samples
iw.dm.detects$ssnyear <- paste(iw.dm.detects$season, iw.dm.detects$sampling_year)
iw.dm.detects <- iw.dm.detects[iw.dm.detects$ssnyear!="Monsoon Water Year 3", ]

#remove field blanks. to remove anything, type !="Value" to remove them.
iw.dm.detects <- iw.dm.detects[iw.dm.detects$type!="B", ]

#remove ATS samples because ATS samples are not included in PH research.
iw.dm.detects <- iw.dm.detects[iw.dm.detects$site!="ATS1", ]

#confirm correct order of categorical variables
#iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("FW", "LW", "FM", "LM"))
iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
iw.dm.detects$period <- factor(iw.dm.detects$period, levels = c("First", "Last"))
iw.dm.detects$season <- factor(iw.dm.detects$season, levels = c("Winter", "Monsoon"))
iw.dm.detects$sampling_year <- factor(iw.dm.detects$sampling_year, levels = c("Water Year 1", "Water Year 2", "Water Year 3"))
iw.dm.detects$community <- factor(iw.dm.detects$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))

iw.dm.detects.long <- pivot_longer(iw.dm.detects,
                           cols = Be:Pb,
                           values_to = "status",
                           names_to = "analyte")

#turn detections into 1s and 0s
iw.dm.detects.long[iw.dm.detects.long$status=="Detect",]$status <- "1"
iw.dm.detects.long[iw.dm.detects.long$status=="Non-Detect",]$status <- "0"

iw.dm.detects.long$status <- as.numeric(iw.dm.detects.long$status)

#calculate summaries ----
sumdetectFX(datalongDF = iw.dm.detects.long,
      subset.vector.string = c("analyte","season", "community"),
      value.string = "status",
      dfname.string = "detect.ssncom",
      filename.string = "detect%_ssncom")

#calculate MLOD ranges ----
iw.mlod.dm.long <- pivot_longer(data = iw.mlod.dm,
                                cols = Be:Pb,
                                values_to = "mlod.value",
                                names_to = "analyte")

mlodsum <- iw.mlod.dm.long %>%
  group_by(across(analyte)) %>%
  summarise(min = min(mlod.value),
            max = max(mlod.value))

write.csv(mlodsum, "mlodranges.csv")

#Functions ----
sumdetectFX <- function(datalongDF, subset.vector.string, value.string, dfname.string, filename.string){
  
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
    summarise(n = n(),
              detections_n = sum(.data[[value]]),
              detections_freq = signif(sum(.data[[value]])/n()*100, 3))
  
  sumtable.small <- subset(sumtable, select = -c(detections_n))
  
  # sumtable.wide <- pivot_wider(data = sumtable.small,
  #                              names_from = "standard",
  #                              values_from = "detection_freq")
  #save as csv file in your working directory
  write.csv(sumtable.small, paste(filename,"_sum.csv", sep = ""))
  
  #copy to new dataframe with a unique name and place in global environment
  assign(paste(dfname), sumtable.small, envir=.GlobalEnv)  
}
