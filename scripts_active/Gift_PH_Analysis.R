#Authors: Kunal Palawat, Gift Chukwuonye
#Description: Functions for summary tables and figures of Project Harvest inorganic analyte data

# Functions ----

summaryTable.samp.yearFX <- function(tableDF, mlodlongDF, name.string, sampletype.string){
  
  #load libraries
  library(table1)
  library(tidyr)
  library(htmltools)
  library(webshot)
  
  #specify objects
  dat <- tableDF
  dat$ID <- rownames(dat) #for table1 function purposes
  analytename <- name.string #must match the name of the column with contaminant concentrations
  MLOD <- mlodlongDF[mlodlongDF$analyte==analytename,]
  sampletype <- sampletype.string
  unit <- dat$units[[1]]
  
  contaminantData <- pivot_wider(dat, id_cols = c(ID, samplings, analytename, sampling_year), names_from = community, values_from = analytename)
  
  #create table
  contaminantTable <- table1(~`Dewey-Humboldt` + `Globe/Miami` + `Hayden/Winkelman` + `Tucson` |  sampling_year + samplings, #name the (rows | columns) you want to show, max of 2 variables for columns
                             data = contaminantData,
                             render=c("n"="N", 'Geometric Mean' ="GMEAN", 'Median [Min, Max]'="Median [Min, Max]"), #manually change units as needed
                             missing=F,
                             digits = 4,
                             overall=F, #change if total column changes
                             rowlabelhead=paste(sampletype, ": ",analytename, sep = ""),
                             footnote = paste("All values are in units of ", unit, ". ", "The MLOD ranged from ", min(MLOD$value), "-", max(MLOD$value), " ", unit, ".",sep = ""))
  
  #save table as html
  save_html(print(contaminantTable),
            file = paste(analytename, sampletype, "Table-sampyear.html", sep = ""),
            libdir = getwd())
  
  #save html as .png
  webshot(paste(analytename,  sampletype, "Table-sampyear.html", sep = "") , paste(analytename,  sampletype, "Table-sampyear.png", sep = ""), delay = 0.2, vheight = 5)
  
  print("Finished")
  
}

summaryTable.sampFX <- function(tableDF, mlodlongDF, name.string, sampletype.string){
  
  #load libraries
  library(table1)
  library(tidyr)
  library(htmltools)
  library(webshot)
  library(EnvStats)
  #specify objects
  dat <- tableDF
  dat$ID <- rownames(dat) #for table1 function purposes
  analytename <- name.string #must match the name of the column with contaminant concentrations
  MLOD <- mlodlongDF[mlodlongDF$analyte==analytename,]
  sampletype <- sampletype.string
  unit <- dat$units[[1]]
  
  contaminantData <- pivot_wider(dat, id_cols = c(ID, samplings, analytename, sampling_year), names_from = community, values_from = analytename)
  
  #create table
  contaminantTable <- table1(~`AZ Background`+ `Dewey-Humboldt` + `Globe/Miami` + `Hayden/Winkelman` + `Tucson` | samplings, #name the (rows | columns) you want to show, max of 2 variables for columns
                             data = contaminantData,
                             render=c("n"="N", 'Geometric Mean' ="GMEAN", 'Median [Min, Max]'="Median [Min, Max]"), #manually change units as needed
                             missing=F,
                             digits = 3,
                             overall="All Sampling Periods", #change if total column changes
                             rowlabelhead=paste(sampletype, ": ",analytename, sep = ""),
                             footnote = paste("All values are in units of ", unit, ". ", "The MLOD ranged from ", min(MLOD$value), "-", max(MLOD$value), " ", unit, ".",sep = ""))
  
  #save table as html
  save_html(print(contaminantTable),
            file = paste(analytename, sampletype, "Table-samp.html", sep = ""),
            libdir = getwd())
  
  #save html as .png
  webshot(paste(analytename,  sampletype, "Table-samp.html", sep = "") , paste(analytename, sampletype, "Table-samp.png", sep = ""), delay = 0.2, vheight = 5)
  
  print("Finished")
  
}

summaryTable.ssnFX <- function(tableDF, mlodlongDF, name.string, sampletype.string){
  
  #load libraries
  library(table1)
  library(tidyr)
  library(htmltools)
  library(webshot)
  
  #specify objects
  dat <- tableDF
  dat$ID <- rownames(dat) #for table1 function purposes
  analytename <- name.string #must match the name of the column with contaminant concentrations
  MLOD <- mlodlongDF[mlodlongDF$analyte==analytename,]
  sampletype <- sampletype.string
  unit <- dat$units[[1]]
  
  contaminantData <- pivot_wider(dat, id_cols = c(ID, season, analytename, sampling_year), names_from = community, values_from = analytename)
  
  #create table
  contaminantTable <- table1(~`Dewey-Humboldt` + `Globe/Miami` + `Hayden/Winkelman` + `Tucson` | season, #name the (rows | columns) you want to show, max of 2 variables for columns
                             data = contaminantData,
                             render=c("n"="N", 'Geometric Mean' ="GMEAN", 'Median [Min, Max]'="Median [Min, Max]"), #manually change units as needed
                             missing=F,
                             digits = 4,
                             overall="All Seasons", #change if total column changes
                             rowlabelhead=paste(sampletype, ": ",analytename, sep = ""),
                             footnote = paste("All values are in units of ", unit, ". ", "The MLOD ranged from ", min(MLOD$value), "-", max(MLOD$value), " ", unit, ".",sep = ""))
  
  #save table as html
  save_html(print(contaminantTable),
            file = paste(analytename, sampletype, "Table-ssn.html", sep = ""),
            libdir = getwd())
  
  #save html as .png
  webshot(paste(analytename,  sampletype, "Table-ssn.html", sep = "") , paste(analytename, sampletype, "Table-ssn.png", sep = ""), delay = 0.2, vheight = 5)
  
  print("Finished")
  
}

contaminantBoxplot.fullFX <- function(analyte.string, subset.string, subset.title.string, dataDF, sampletype.string){
  library(ggplot2)
  
  analyte <- analyte.string #define the metal you are graphing
  subset <- subset.string
  subset.title <- subset.title.string
  #standards <- stdsDF #define the standards for that metal
  concentrationData <- dataDF #define the concentrations for that metal
  unit <- concentrationData$units[[1]]
  sampletype <- sampletype.string
  # PB <- standards$PB  #place all standard values into objects to be called in the ggplot command
  # FB <- standards$FB 
  # DW <- standards$DW 
  # AI <- standards$AI 
  # LDW <- standards$LDW
  # MLOD <- standards$MLOD
  
  #make graphs of [metal] by samplings  
  Full <-  ggplot(data = concentrationData,
                  mapping=aes_string(x=subset,
                                     y=analyte,
                                     fill = "community")) +
    # geom_hline(yintercept = FB, linetype = "solid", color = "#4670B4", size = 1.5) +
    # geom_hline(yintercept = PB, linetype = "solid", color = "#C27571", size = 1.5) +
    #geom_hline(yintercept = 200, linetype = "solid", color = "#77BEBD", size = 1.5) +
    # geom_hline(yintercept = LDW, linetype = "solid", color = "#AF5597", size = 1.5)+
    # geom_hline(yintercept = DW, linetype = "solid", color = "#A2A9D6", size = 1.5) +
    # geom_hline(yintercept = MLOD, linetype = "solid", color = "#FBCD5F", size = 1) +
    geom_boxplot() +
    stat_boxplot(geom = 'errorbar') +
    geom_point(position = position_dodge(width=0.75, preserve = "total"),
               aes(group=community), 
               shape = 21,
               size = 2.5) +
    labs(title = paste("Distributions of", analyte, "by Community and", subset.title),
         y = paste(analyte," Concentration (", unit, ")\n", sep=""),
         x = paste("\n", subset.title),
         subtitle = paste(sampletype),
         fill = "Community")+
    scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom",
          legend.title = element_blank())
  
  print(Full)
  dev.print(png, paste(analyte, "_IWDM_full-boxplot_samp.png"), res=150, height=6, width=12, units="in")
  
}

contaminantBoxplot.zoomFX <- function(analyte.string, zoomMax.value, subset.string="samplings", subset.title.string = "Sampling Period", dataDF = iw.dm, sampletype.string = "Harvested Rainwater - DM"){
  library(ggplot2)
  
  analyte <- analyte.string #define the metal you are graphing
  subset <- subset.string
  subset.title <- subset.title.string
  #standards <- stdsDF #define the standards for that metal
  sampletype <- sampletype.string
  concentrationData <- dataDF #define the concentrations for that metal
  unit <- concentrationData$units[[1]]
  # PB <- standards$PB  #place all standard values into objects to be called in the ggplot command
  # FB <- standards$FB 
  # DW <- standards$DW 
  # AI <- standards$AI 
  # LDW <- standards$LDW
  # MLOD <- standards$MLOD
  zoomMax <- zoomMax.value
  
  #make graphs of [metal] by samplings  
  Zoom <-  ggplot(data = concentrationData,
                  mapping=aes_string(x=subset,
                                     y=analyte,
                                     fill = "community")) +
    # geom_hline(yintercept = FB, linetype = "solid", color = "#4670B4", size = 1.5) +
    # geom_hline(yintercept = PB, linetype = "solid", color = "#C27571", size = 1.5) +
    #geom_hline(yintercept = 200, linetype = "solid", color = "#77BEBD", size = 1.5) +
    # geom_hline(yintercept = LDW, linetype = "solid", color = "#AF5597", size = 1.5)+
    # geom_hline(yintercept = DW, linetype = "solid", color = "#A2A9D6", size = 1.5) +
    # geom_hline(yintercept = MLOD, linetype = "solid", color = "#FBCD5F", size = 1) +
    geom_boxplot() +
    stat_boxplot(geom = 'errorbar') +
    geom_point(position = position_dodge(width=0.75, preserve = "total"),
               aes(group=community), 
               shape = 21,
               size = 2.5) +
    labs(title = paste("Distributions of", analyte, "by Community and", subset.title),
         y = paste(analyte," Concentration (", unit, ")\n", sep=""),
         x = paste("\n", subset.title),
         subtitle = paste(sampletype,"\ny-axis subset to 0 -", zoomMax, unit),
         fill = "Community")+
    scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
    coord_cartesian(ylim = c(0, zoomMax)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(Zoom)
  dev.print(png, paste(analyte, "_IWDM_subset-boxplot_samp.png"), res=150, height=6, width=12, units="in")
  
}

comsampbarplotFX <- function(datalongDF, name.string, units.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- datalongDF
  analyte <- name.string
  units <- units.string
  
  dat <- dat[dat$analyte == analyte,]
  
  gmeans <- aggregate(dat$value,
                      by = list(dat$community, dat$samplings),
                      FUN = geoMean)
  
  gsds <- aggregate(dat$value,
                    by = list(dat$community, dat$samplings),
                    FUN = geoSD)
  
  gdat <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  names(gdat) <- c("community", "samplings","gmean", "gsd")
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  
  p <- ggplot(gdat,
              mapping = aes(fill=community, y=gmean, x=samplings)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(group = community, x = samplings, ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    scale_fill_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2")) +
    labs(title = paste("Geometric Mean ", analyte, " Concentrations by Community and Sampling Window", sep = ""),
         subtitle = paste("Harvested Rainwater"),
         y = paste(analyte, " Concentration (",units,")\n", sep=""),
         x = "\nSampling Window") +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom",
          legend.title = element_blank())
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_barplot_comsamp.png"), res=150, height=7, width=10, units="in")
  
}


IWcomsampbarplotFX <- function(datalongDF, name.string, units.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- datalongDF
  analyte <- name.string
  units <- units.string
  
  dat <- dat[dat$analyte == analyte,]
  
  gmeans <- aggregate(dat$value,
                      by = list(dat$community),
                      FUN = geoMean)
  
  gsds <- aggregate(dat$value,
                    by = list(dat$community),
                    FUN = geoSD)
  
  gdat <- full_join(gmeans, gsds, by = c("Group.1"))
  names(gdat) <- c("community","gmean", "gsd")
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  
  p <- ggplot(gdat,
              mapping = aes(fill=community, y=gmean, x=community)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(group = community, ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    scale_fill_manual(values=c("#7346D0","#F9A785", "#00A8C6", "#95CACA","#4068B2")) +
    labs(title = paste("Geometric Mean ", analyte, " Concentrations by Community", sep = ""),
         subtitle = paste("Harvested Rainwater"),
         y = paste(analyte, " Concentration (",units,")\n", sep=""),
         x = "\nCommunity") +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom",
          legend.title = element_blank())
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_barplot_com.png"), res=150, height=7, width=10, units="in")
  
}

#load libraries ----
library(readxl) #read excel files
library(tidyverse)
library(ggplot2)
#set working directory
#setwd("")

#load data ----
#IW DM
iw.dm <- read_excel("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_clean/IW_DM_Y123.xlsx", sheet = "Corrected") #corrected mean the corrected tab in the excel sheet
# iw.dm.detects <- read_xlsx("data/data_clean/IW_DM_Y123.xlsx", sheet = "Detection", col_names = TRUE)
# mlod <- read_xlsx("data/data_processing/IPSW_MLODS.xlsx", sheet = "corrected - 12.22.20", col_names = TRUE)
# iw.mlod <- mlod[mlod$`Sample Type`=="IW",]
# iw.mlod.dm <- iw.mlod[iw.mlod$Analysis=="DM",]

#wrangle data
#add period and season variables
iw.dm$period <- iw.dm$samplings
iw.dm$season <- iw.dm$samplings

#redefine them
iw.dm[iw.dm$period=="First Winter",]$period <- "First"
iw.dm[iw.dm$period=="Last Winter",]$period <- "Last"
iw.dm[iw.dm$period=="First Monsoon",]$period <- "First"
iw.dm[iw.dm$period=="Last Monsoon",]$period <- "Last"

iw.dm[iw.dm$season=="First Winter",]$season <- "Winter"
iw.dm[iw.dm$season=="Last Winter",]$season <- "Winter"
iw.dm[iw.dm$season=="First Monsoon",]$season <- "Monsoon"
iw.dm[iw.dm$season=="Last Monsoon",]$season <- "Monsoon"


#changing year #Make a new variable with this

iw.dm$year <- iw.dm$sampling_year
iw.dm[iw.dm$year=="2017-2018",]$year <- "Water Year 1"
iw.dm[iw.dm$year=="2018-2019",]$year <- "Water Year 2"
iw.dm[iw.dm$year=="2019-2020",]$year <- "Water Year 3"

#remove field blanks. to remove anything, type !="Value" to remove them.
iw.dm <- iw.dm[iw.dm$type!="B", ]

#remove ATS samples because ATS samples are not included in PH research. 
iw.dm <- iw.dm[iw.dm$site!="ATS1", ]

#remove year 3 monsoon samples
iw.dm$ssnyear <- paste(iw.dm$season, iw.dm$year)
iw.dm <- iw.dm[iw.dm$ssnyear!="Monsoon Water Year 3", ]


#confirm correct order of categorical variables
#iw.dm$samplings <- factor(iw.dm$samplings, levels = c("FW", "LW", "FM", "LM"))
iw.dm$samplings <- factor(iw.dm$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
iw.dm$period <- factor(iw.dm$period, levels = c("First", "Last"))
iw.dm$season <- factor(iw.dm$season, levels = c("Winter", "Monsoon"))
iw.dm$sampling_year <- factor(iw.dm$sampling_year, levels = c("Water Year 1", "Water Year 2", "Water Year 3"))
iw.dm$community <- factor(iw.dm$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))
# 
# 
# #For the detection data,
# #detect data
# #add period and season variables
# iw.dm.detects$period <- iw.dm.detects$samplings
# iw.dm.detects$season <- iw.dm.detects$samplings
# 
# 
# #redefine them
# iw.dm.detects[iw.dm.detects$period=="First Winter",]$period <- "First"
# iw.dm.detects[iw.dm.detects$period=="Last Winter",]$period <- "Last"
# iw.dm.detects[iw.dm.detects$period=="First Monsoon",]$period <- "First"
# iw.dm.detects[iw.dm.detects$period=="Last Monsoon",]$period <- "Last" 
# 
# iw.dm.detects[iw.dm.detects$season=="First Winter",]$season <- "Winter"
# iw.dm.detects[iw.dm.detects$season=="Last Winter",]$season <- "Winter"
# iw.dm.detects[iw.dm.detects$season=="First Monsoon",]$season <- "Monsoon"
# iw.dm.detects[iw.dm.detects$season=="Last Monsoon",]$season <- "Monsoon"
# 
# 
# #changing year
# iw.dm.detects$year<-iw.dm.detects$sampling_year
# 
# iw.dm.detects[iw.dm.detects$sampling_year=="2017-2018",]$sampling_year <- "Water Year 1"
# iw.dm.detects[iw.dm.detects$sampling_year=="2018-2019",]$sampling_year <- "Water Year 2"
# iw.dm.detects[iw.dm.detects$sampling_year=="2019-2020",]$sampling_year <- "Water Year 3"
# 
# #remove year 3 monsoon samples
# iw.dm.detects$ssnyear <- paste(iw.dm.detects$season, iw.dm.detects$sampling_year)
# iw.dm.detects <- iw.dm.detects[iw.dm.detects$ssnyear!="Monsoon Water Year 3", ]
# 
# #remove field blanks. to remove anything, type !="Value" to remove them.
# iw.dm.detects <- iw.dm.detects[iw.dm.detects$type!="B", ]
# 
# 
# #remove ATS samples because ATS samples are not included in PH research. 
# iw.dm.detects <- iw.dm.detects[iw.dm.detects$site!="ATS1", ]
# 
# #confirm correct order of categorical variables
# #iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("FW", "LW", "FM", "LM"))
# iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
# iw.dm.detects$period <- factor(iw.dm.detects$period, levels = c("First", "Last"))
# iw.dm.detects$season <- factor(iw.dm.detects$season, levels = c("Winter", "Monsoon"))
# iw.dm.detects$sampling_year <- factor(iw.dm.detects$sampling_year, levels = c("Water Year 1", "Water Year 2", "Water Year 3"))
# iw.dm.detects$community <- factor(iw.dm.detects$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))
# 
# 

#pH and EC data
#append pH and EC data
iw.pHec <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_clean/IW_pHEC_Y123.xlsx", sheet = 1, col_names = TRUE)
iw.pHec <- iw.pHec[iw.pHec$type!="B",] #removing field blanks
iw.dm <- full_join(iw.dm, iw.pHec, by = c("sample.name", "type")) #joins the phec data with the original iw.dm we had before
iw.dm <- iw.dm[!is.na(iw.dm$community),]
# na.omit(iw.dm$community)

#ph EC summary ----
iw.pHec <- iw.dm[!is.na(iw.dm$pH),]
na.omit(iw.dm$pH)
iw.pHec <- iw.dm[!is.na(iw.dm$EC),]
na.omit(iw.dm$EC)
aggregate(iw.pHec$EC,
          by = list(iw.pHec$season),
          FUN = max)



median(iw.pHec$EC)

# longer ----
iw.dm.long <- pivot_longer(iw.dm,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")

# iw.dm.detects.long <- pivot_longer(iw.dm.detects,
#                                    cols = Be:Pb,
#                                    values_to = "detection",
#                                    names_to = "analyte")
# 
# 
# iw.mlod.dm.long <- pivot_longer(iw.mlod.dm,
#                                 cols=Be:Pb,
#                                 values_to = "value",
#                                 names_to = "analyte")


#reset working directory for figures ----
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles/Figures")

#Scratch? All Elements ----
Contamlist <- c("Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")
Contamlist <- list("Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")
Contamlist <- list("As", "Pb")
Zoomlist <- list(650, 5, 42, 100, .25, 5, 10, 3, 900, 350, 25, 220, 15, 10, 10, .2, 1, 10, 1250)


#IW - DM 
detectTable.allelementsFX(tablelongDF = iw.dm.detects.long,
                          sampletype = "Harvested Rainwater - DM",
                          analytes.string = "All Contaminants",
                          analytes.abbrev.string = "all",
                          digit.num = 2)

#Scratch? All Sample Types ----
#Function Name: detectTableFX

#Description: This function creates a table with detection status of samples by year and community with an all year total by community as well.

#Input: a data frame with all variables of interest (tableDF), a dataframe with MLODs (mlodDF), the name of the element you want to tabulate in quotes ("name"), and the sample type you are summarizing in quotes ("sampletype"). Note that "name" must match the title of the column with contaminant concentrations.

#Output: a .html file and .png file

detectTable.yearFX <- function(tableDF, mlodlongDF, name.string, sampletype.string, digit.num){
  
  #load libraries
  library(table1)
  library(tidyr)
  library(htmltools)
  library(webshot)
  
  #specify objects
  dat <- tableDF
  dat$ID <- rownames(dat) #for table1 function purposes
  analytename <- name.string #must match the name of the column with contaminant concentrations
  MLOD <- mlodlongDF[mlodlongDF$analyte==analytename,]
  sampletype <- sampletype.string
  digit <- digit.num
  unit <- dat$units[[1]]
  
  dat <- as.data.frame(unclass(dat))
  dat[,c(analytename)] <- factor(dat[,c(analytename)], levels = c("Non-Detect", "Detect"))
  
  contaminantData <- pivot_wider(dat, id_cols = c(ID, analytename, sampling_year), names_from = community, values_from = analytename)
  
  contaminantTable <- table1(~`Dewey-Humboldt` + `Globe/Miami` + `Hayden/Winkelman` + `Tucson` | sampling_year, 
                             data = contaminantData,
                             render=c("n (%)"="FREQ (PCTnoNA%)"), overall="All Years",
                             rowlabelhead=paste(sampletype, ": ", analytename, sep = ""),
                             footnote = paste("The MLOD ranged from ", min(MLOD$value), "-", max(MLOD$value), ' ', unit, ".",sep = ""),
                             digits = digit.num)
  
  #save table as html
  save_html(print(contaminantTable),
            file = paste(analytename,  sampletype, "_detectTable-year.html", sep = ""),
            libdir = getwd())
  
  #save html as .png
  webshot(paste(analytename,  sampletype, "_detectTable-year.html", sep = "") , paste(analytename,  sampletype, "_detectTable-year.png", sep = ""), delay = 0.2, vheight = 5)
  
  print("Finished")
}

#Function Name: detectBarplotFX

#Description: This function creates a stacked barplot with detection status of samples by year and community. Detection is indicated by the transparency of the bars.

#Input: a data frame with all variables of interest (tableDF), a dataframe with MLODs (mlodDF), the name of the element you want to tabulate in quotes ("name"), and the sample type you are summarizing in quotes ("sampletype"). Note that "name" must match the title of the column with contaminant concentrations. All values in the analyte columns must say "Detect" or "Non-Detect"

#Output: a .png file

detectBarplotFX <- function(tablelongDF, mlodlongDF, name.string, sampletype.string){
  #load libraries
  library(ggplot2)
  library(stringr)
  
  #specify objects
  dat <- tablelongDF
  analytename <- name.string #must match the name of the column with contaminant concentrations
  MLOD <- mlodlongDF[mlodlongDF$analyte==analytename,]
  sampletype <- sampletype.string
  unit <- dat$units[[1]]
  
  dat <- dat[dat$analyte==analytename,]
  dat <- as.data.frame(unclass(dat))
  dat[,c("detection")] <- factor(dat[,c("detection")], levels = c("Non-Detect", "Detect"))
  
  detectplot <- ggplot(data=dat,
                       mapping=aes(x=sampling_year,
                                   fill=community)) +
    geom_bar(aes(alpha = detection,
                 y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), position = "fill") + 
    scale_y_continuous(labels=scales::percent, limits = c(0,1), expand = c(0,0))+
    scale_alpha_manual(values = c(.65,1), drop = FALSE)+
    geom_text(aes(label = paste("n =",..count..),
                  group = detection,
                  y= (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
              stat = "count",
              position = "fill",
              vjust = 1.5,
              size=5,
              family="Avenir") +
    labs(title = paste(str_to_title(analytename), "Detection Summary"),
         subtitle = paste(sampletype, "\nN =", nrow(dat)),
         caption = paste("The MLOD ranged from ", min(MLOD$value), "-", max(MLOD$value), ' (', unit, ')', sep = ""),
         fill="Community",
         alpha="Detection Status",
         y = "Frequency (%)",
         x = "Sampling Water Year") +
    facet_grid(.~community, scales = "free_x")+
    scale_fill_manual(values = c("#7346D0", "#F9A785", "#00A8C6", "#95CACA", "#4068B2")) +
    theme_bw() +
    theme(strip.text.y = element_blank(),
          strip.background.x = element_rect(fill="white", color = "white"),
          text = element_text(size=20, family = "Avenir"),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
          panel.grid = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 20, color = "black", face = "italic"),
          plot.caption = element_text(size = 15, face="italic", hjust = 0),
          legend.key.size = unit(1, "cm"),
          legend.title = element_text(size = 20, hjust = 0),
          axis.text.y = element_text(size=20, color = "black", face = "bold"),
          axis.text.x = element_text(size=20, color = "black", face = "bold"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.border = element_blank())
  
  print(detectplot)
  
  dev.print(png, paste(analytename, sampletype, "_detectPlot.png", sep=""), res=100, height=10, width=28, units="in")
  
}


detectBarplot.sampFX <- function(tablelongDF, mlodlongDF, name.string, sampletype.string){
  #load libraries
  library(ggplot2)
  library(stringr)
  
  #specify objects
  dat <- tablelongDF
  analytename <- name.string #must match the name of the column with contaminant concentrations
  MLOD <- mlodlongDF[mlodlongDF$analyte==analytename,]
  sampletype <- sampletype.string
  unit <- dat$units[[1]]
  
  dat <- dat[dat$analyte==analytename,]
  dat <- as.data.frame(unclass(dat))
  dat[,c("detection")] <- factor(dat[,c("detection")], levels = c("Non-Detect", "Detect"))
  
  detectplot <- ggplot(data=dat,
                       mapping=aes(x=samplings,
                                   fill=community)) +
    geom_bar(aes(alpha = detection,
                 y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), position = "fill") + 
    scale_y_continuous(labels=scales::percent, limits = c(0,1), expand = c(0,0))+
    scale_alpha_manual(values = c(.65,1), drop = FALSE)+
    geom_text(aes(label = paste("n =",..count..),
                  group = detection,
                  y= (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
              stat = "count",
              position = "fill",
              vjust = 1.5,
              size=5,
              family="Avenir") +
    labs(caption = paste("The MLOD ranged from ", min(MLOD$value), "-", max(MLOD$value), ' (', unit, ')', sep = ""),
         fill="Community",
         alpha="Detection Status",
         y = "Frequency (%)\n",
         x = "\nSampling Window") +
    facet_grid(.~community, scales = "free_x")+
    scale_fill_manual(values = c("#F9A785", "#00A8C6", "#95CACA", "#4068B2")) +
    theme_bw() +
    theme(strip.text.y = element_blank(),
          strip.background.x = element_rect(fill="white", color = "white"),
          text = element_text(size=20, family = "Avenir"),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
          panel.grid = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 20, color = "black", face = "italic"),
          plot.caption = element_text(size = 15, face="italic", hjust = 0),
          legend.key.size = unit(1, "cm"),
          legend.title = element_text(size = 20, hjust = 0),
          axis.text.y = element_text(size=20, color = "black", face = "bold"),
          axis.text.x = element_text(size=20, color = "black", face = "bold", angle = 45, hjust = 1),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.border = element_blank())
  
  # title = paste(str_to_title(analytename), "Detection Summary"),
  # subtitle = paste(sampletype, "\nN =", nrow(dat)),
  
  print(detectplot)
  
  dev.print(png, paste(analytename, sampletype, "_sampdetectPlot.png", sep=""), res=100, height=10, width=32, units="in")
  
}


detectTable.allelementsFX <- function(tablelongDF, sampletype.string, analytes.string, analytes.abbrev.string, digit.num){
  
  #load libraries
  library(table1)
  library(tidyr)
  library(htmltools)
  library(webshot)
  
  #specify objects
  dat <- tablelongDF
  dat$ID <- rownames(dat) #for table1 function purposes
  sampletype <- sampletype.string
  analytes <- analytes.string
  analytes.abbrev <- analytes.abbrev.string
  digit <- digit.num
  
  dat <- as.data.frame(unclass(dat))
  dat$detection <- factor(dat$detection, levels = c("Non-Detect", "Detect"))
  
  contaminantData <- pivot_wider(dat, id_cols = c(ID, analyte, sampling_year), names_from = community, values_from = detection)
  
  contaminantTable <- table1(~`Dewey-Humboldt` + `Globe/Miami` + `Hayden/Winkelman` + `Tucson`  | analyte, 
                             data = contaminantData,
                             render=c("n (%)"="FREQ (PCTnoNA%)"), overall=F,
                             rowlabelhead=paste(sampletype, ": ", analytes, sep = ""),
                             digits = digit)
  
  #save table as html
  save_html(print(contaminantTable),
            file = paste(analytes.abbrev, sampletype, "detectTable.html", sep = ""),
            libdir = getwd())
  
  #save html as .png
  webshot(paste(analytes.abbrev, sampletype, "detectTable.html", sep = "") , paste(analytes.abbrev, sampletype, "detectTable.png", sep = ""), delay = 0.2, vheight = 5)
  
  print("Finished")
}

summaryTable.allelementsFX <- function(tablelongDF, sampletype.string, analytes.string, analytes.abbrev.string, digit.num){
  
  #load libraries
  library(table1)
  library(tidyr)
  library(htmltools)
  library(webshot)
  
  #specify objects
  dat <- tablelongDF
  dat$ID <- rownames(dat) #for table1 function purposes
  sampletype <- sampletype.string
  analytes <- analytes.string
  analytes.abbrev <- analytes.abbrev.string
  digit <- digit.num
  unit <- dat$units[[1]]
  
  contaminantData <- pivot_wider(dat, id_cols = c(ID, analyte, sampling_year), names_from = community, values_from = value)
  
  contaminantTable <- table1(~`Dewey-Humboldt` + `Globe/Miami` + `Hayden/Winkelman` + `Tucson`  | analyte, 
                             data = contaminantData,
                             render=c("n"="N", 'Geometric Mean' ="GMEAN", 'Median [Min, Max]'="Median [Min, Max]"), #manually change units as needed
                             rowlabelhead=paste(sampletype, ": ", analytes, sep = ""),
                             overall = F,
                             footnote = paste("All values are in units of ", unit, ".", sep=""),
                             digits = digit)
  
  #save table as html
  save_html(print(contaminantTable),
            file = paste(analytes.abbrev, sampletype, "summaryTable.html", sep = ""),
            libdir = getwd())
  
  #save html as .png
  webshot(paste(analytes.abbrev, sampletype, "summaryTable.html", sep = "") , paste(analytes.abbrev, sampletype, "summaryTable.png", sep = ""), delay = 0.2, vheight = 5)
  
  print("Finished")
  
}

detectBarplot.sampFX(tablelongDF = iw.dm.detects.long, mlodlongDF = iw.mlod.dm.long, name.string = "As", sampletype.string = "Harvested Rain Water")
#Scratch? All elements----
Contamlist <- list("Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")
lapply(X=Contamlist,
       FUN=detectBarplot.sampFX,
       tablelongDF = iw.dm.detects.long, mlodlongDF = iw.mlod.dm.long,sampletype.string = "Harvested Rainwater"
       )




