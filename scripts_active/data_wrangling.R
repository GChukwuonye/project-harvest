#Kunal Palawat and Gift Chukwuonye
#Description: Script to load and clean up datasets for analysis. RUN THIS every time before coding.
#
#Load libraries====
library(readxl) #read excel files
library(tidyverse)
library(ggplot2)
library(table1)

#load data ----
#IW DM
#setwd("~/Documents/GitHub/ProjectHarvest/WorkingFiles//data/data_processing")
#iw.dm <- read_excel("/Users/gift/Documents/GitHub/WorkingFiles/data/data_processing/IW_DM_Y123.xlsx", sheet = "Corrected") #corrected means the corrected tab in the excel sheet
iw.dm <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_clean/IW_DM_Y123.xlsx", sheet = "Corrected")
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

#pH and EC data----
#append pH and EC data
#iw.pHec <- read_xlsx("/Users/gift/Documents/GitHub/WorkingFiles/data/data_clean/IW_pHEC_Y123.xlsx", sheet = 1, col_names = TRUE)
iw.pHec <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_clean/IW_pHEC_Y123.xlsx", sheet = 1, col_names = TRUE)

iw.pHec <- iw.pHec[iw.pHec$type!="B",] #removing field blanks
iw.dm <- full_join(iw.dm, iw.pHec, by = c("sample.name", "type")) #joins the phec data with the original iw.dm we had before
iw.dm <- iw.dm[!is.na(iw.dm$community),]
# na.omit(iw.dm$community)

#add mining community vs urban community
iw.dm$landuse <- "Mining Community"
iw.dm[iw.dm$community=="Tucson",]$landuse <- "Urban Community"
iw.dm[iw.dm$community=="Dewey-Humboldt",]$landuse <- "Legacy Mining Community"
summary(as.factor(iw.dm$landuse))
# #ph EC summary ----
# iw.pHec <- iw.dm[!is.na(iw.dm$pH),]
# na.omit(iw.dm$pH)
# iw.pHec <- iw.dm[!is.na(iw.dm$EC),]
# na.omit(iw.dm$EC)
# aggregate(iw.pHec$EC,
#           by = list(iw.pHec$season),
#           FUN = max)
# 
# 
# 
# median(iw.pHec$EC)


#add proximity to point source ----
#com <- read_xlsx("/Users/gift/Documents/GitHub/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "community", col_names = TRUE)
com <- read_xlsx("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/LATLOGSITE.xlsx", sheet = "community", col_names = TRUE)


iw.dm <- full_join(iw.dm, com, by = c("site"))
iw.dm <- iw.dm[!is.na(iw.dm$mlod.name),]


#add pollution load index ----
#pli <- read.csv("/Users/gift/Documents/GitHub/WorkingFiles/data/data_processing/pollution_load_selected_analytes.csv")
pli <- read.csv("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/pollution_load_selected_analytes.csv")
iw.dm$pli <- pli$pli_contaminants

iw.dm$pli.ln <- log(iw.dm$pli)

#outliers ----
#remove samples 19 and 39 from analysis because they were outliers based on MFA and remove all samples from H22 because they are a proximity outlier south of Winkelman
#G428IWA23-20190730 and H209IWA23-20190709
iw.dm <- iw.dm[-c(19,39),]
iw.dm <- iw.dm[iw.dm$site!="H222",]

#contamination factor=====
pli_dat<- pli[-c(19,39),]
pli_dat <- pli_dat[pli_dat$site!="H222",]
pli_short<-  pli_dat[, -c(15, 19, 25, 26, 28, 29)]
pli_dat2<- pivot_longer(pli_short,
                        cols = Be:Pb,
                        values_to = "contamination_factor",
                        names_to = "analytes")

#longer ----
iw.dm.long <- pivot_longer(iw.dm,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")

#calculate natural log ----
iw.dm.long$ln_value <- log(iw.dm.long$value)

#create longer version to compare transformation
iw.dm.longer <- pivot_longer(data = iw.dm.long,
                           cols = c(value,ln_value),
                           names_to = "transformation",
                           values_to = "concentration")

iw.dm.longer[iw.dm.longer$transformation == "value",]$transformation <- "untransformed"
iw.dm.longer[iw.dm.longer$transformation == "ln_value",]$transformation <- "natural log"

iw.dm.longer$transformation <- as.factor(iw.dm.longer$transformation)

#create natural log dataframes for analysis
iw.ln.dm.long <- subset(iw.dm.long, select = -c(value))
iw.ln.dm <- pivot_wider(data = iw.ln.dm.long,
                         values_from = "ln_value",
                         names_from ="analyte")

# #sociodemographic data ----
# demo <- read_excel("/Users/gift/Documents/GitHub/WorkingFiles/data/data_processing/ph_demos_kp.xlsx")
demo <- read_excel("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/ph_demos_kp.xlsx")

#remove unecessary columns
demo <- subset(demo, select=(-c(Participant, Participant_2)))

#remove data for those who did not consent and those who dropped out of the project
demo <- demo %>%
  drop_na(Consent) %>%
  filter(Consent!='Unknown') %>%
  filter(Consent!="No") %>%
  filter(`Dropped Out` != "Yes") %>%
  mutate(Zip = factor(Zip))%>%
  mutate(`Household Size`=factor(`Household Size`))%>%
  mutate(`household_size`=factor(`household_size`))

#make demo data longer for summaries
demo.long <- pivot_longer(demo,
                          cols = c(Zip:`Low Income`),
                          values_to = "value",
                          names_to = "demographic")
#clean up data for summaries
demo.long <- demo.long %>%
  drop_na(value) %>%
  filter(value!='No response') %>%
  filter(value!='555') %>%
  filter(value!='999')

#combine demo to concentration data
iw.demo <- full_join(iw.dm, demo, by = c("site", "community"))

#add prox to just demo data
demo <- full_join(demo, com, by = c("site"))
demo <- demo %>%
  drop_na(Consent)

#remove demo data that does not match to any samples and any samples that do not have demo data
iw.demo <- iw.demo %>%
  drop_na(mlod.name) %>%
  drop_na(Consent)
iw.demo$Zip <- as.factor(iw.demo$Zip)

#make longer
iw.demo.long <- pivot_longer(iw.demo,
                             cols = c(Be:Pb,pli),
                             values_to = "value",
                             names_to = "analyte")
#log transform
iw.demo.long$value.ln <- log(iw.demo.long$value)

#make demo data longer for summaries
iw.demo.longer <- pivot_longer(iw.demo.long,
                          cols = c(Zip:`Low Income`),
                          values_to = "response",
                          names_to = "demographic")

#clean up data for summaries
iw.demo.longer <- iw.demo.longer %>%
  filter(response!='No response') %>%
  filter(response!='555') %>%
  filter(response!='999')%>%
  drop_na(response)

iw.demo.long <- pivot_wider(iw.demo.longer,
                            values_from = "response",
                            names_from = "demographic")


#Home description survey ----
#hds <- read_excel("/Users/gift/Documents/GitHub/WorkingFiles/data/data_processing/IO_HDS.xlsx")
hds <- read_excel("~/Documents/GitHub/ProjectHarvest/WorkingFiles/data/data_processing/IO_HDS.xlsx", sheet = "stats", col_names = TRUE)
#NA, unsure, and blanks assumed to be NA, removed from analysis

#duplicate cistern material
hds$Q78b <- hds$Q78

#put all columns in character for pivoting
hds <- hds %>%
  mutate(across(everything(),as.character))

hds.long <- pivot_longer(data = hds,
                         cols = Q9:Q78b,
                         names_to = "question",
                         values_to = "response")
#remove NAs
hds.long <- hds.long%>%
  filter(response!=0)%>%
  filter(response!=100)%>%
  filter(response!=101)%>%
  drop_na(response)

#Q9: rename home age
hds.long[hds.long$question == "Q9"& hds.long$response=="1",]$response <- "Pre 1940"
hds.long[hds.long$question == "Q9"& hds.long$response=="2",]$response <- "1941-1949"
hds.long[hds.long$question == "Q9"& hds.long$response=="3",]$response <- "1950-1959"
hds.long[hds.long$question == "Q9"& hds.long$response=="4",]$response <- "1960-1969"
hds.long[hds.long$question == "Q9"& hds.long$response=="5",]$response <- "1970-1979"
hds.long[hds.long$question == "Q9"& hds.long$response=="6",]$response <- "1980-1989"
hds.long[hds.long$question == "Q9"& hds.long$response=="7",]$response <- "1990-1999"
hds.long[hds.long$question == "Q9"& hds.long$response=="8",]$response <- "2000-2009"
hds.long[hds.long$question == "Q9"& hds.long$response=="9",]$response <- "2010-2018"

#Q18: rename paint peeling
hds.long[hds.long$question == "Q18"& hds.long$response=="1",]$response <- "Yes"
hds.long[hds.long$question == "Q18"& hds.long$response=="2",]$response <- "No"

#Q44: rename proximity to road
hds.long[hds.long$question == "Q44"& hds.long$response=="1",]$response <- "Yes"
hds.long[hds.long$question == "Q44"& hds.long$response=="2",]$response <- "No"

#Q1: roof material
#remove the following from analysis due to low SITE sample size (1 or 2): Composition Tile; Fiberglass; Flat BUR (Reflective), Asphalt Shingle; Flat BUR (Reflective), Metal Panel; Flat BUR (Tar/Gravel), Asphalt Shingle; Metal Panel, Slate; Plastic; Slate; Steel
#
#May need to remove roofs with 3 SITES: Flat BUR (Tar/Gravel), Flat BUR (Reflective); Flat BUR (Tar/Gravel), Metal Panel; Rubber Membrane
#or 4 SITES: Clay/Concrete Tile, Flat BUR (Reflective)
#

summary(as.factor(hds.long[hds.long$question=="Q1",]$response))
hds.long <- hds.long%>%
  filter(response!="Composition Tile")%>%
  filter(response!="Fiberglass")%>%
  filter(response!="Flat BUR (Reflective), Asphalt Shingle")%>%
  filter(response!="Flat BUR (Reflective), Metal Panel")%>%
  filter(response!="Flat BUR (Tar/Gravel), Asphalt Shingle")%>%
  filter(response!="Metal Panel, Slate")%>%
  filter(response!="Plastic")%>%
  filter(response!="Slate")%>%
  filter(response!="Steel")%>%
  filter(response!="Clay/Concrete Tile")%>%
  filter(response!="Flat BUR (Tar/Gravel), Metal Panel")
  
summary(as.factor(hds.long[hds.long$question=="Q1",]$response))


#Q67: roof cleaning
hds.long[hds.long$question == "Q67"& hds.long$response=="1",]$response <- "Yes"
hds.long[hds.long$question == "Q67"& hds.long$response=="2",]$response <- "No"

#Q68: how often do you clean
hds.long[hds.long$question == "Q68"& hds.long$response=="1",]$response <- "As needed"
hds.long[hds.long$question == "Q68"& hds.long$response=="2",]$response <- "Monthly"
hds.long[hds.long$question == "Q68"& hds.long$response=="3",]$response <- "Quarterly"
hds.long[hds.long$question == "Q68"& hds.long$response=="4",]$response <- "Yearly"

#Q60: cistern material
hds.long[hds.long$question == "Q60"& hds.long$response=="1",]$response <- "Metal"
hds.long[hds.long$question == "Q60"& hds.long$response=="2",]$response <- "Plastic"
hds.long[hds.long$question == "Q60"& hds.long$response=="3",]$response <- "Concrete"
hds.long[hds.long$question == "Q60"& hds.long$response=="4",]$response <- "Fiberglass"
hds.long[hds.long$question == "Q60"& hds.long$response=="5",]$response <- "Other"

#Q65: cistern age
hds.long[hds.long$question == "Q65"& hds.long$response=="2",]$response <- "<6 months"
hds.long[hds.long$question == "Q65"& hds.long$response=="3",]$response <- "6 months-1 year"
hds.long[hds.long$question == "Q65"& hds.long$response=="4",]$response <- "1-2 years"
hds.long[hds.long$question == "Q65"& hds.long$response=="5",]$response <- "2-3 years"
hds.long[hds.long$question == "Q65"& hds.long$response=="6",]$response <- "3-4 years"
hds.long[hds.long$question == "Q65"& hds.long$response=="7",]$response <- "5+ years"

#Q76: first flush
hds.long[hds.long$question == "Q76"& hds.long$response=="1",]$response <- "Yes"
hds.long[hds.long$question == "Q76"& hds.long$response=="2",]$response <- "No"

#Q77: screen
hds.long[hds.long$question == "Q77"& hds.long$response=="1",]$response <- "Yes"
hds.long[hds.long$question == "Q77"& hds.long$response=="2",]$response <- "No"

#Q78: screen material
hds.long[hds.long$question == "Q78"& hds.long$response=="1",]$response <- "Metal (Unspecified)"
hds.long[hds.long$question == "Q78"& hds.long$response=="2",]$response <- "Metal (Aluminum)"
hds.long[hds.long$question == "Q78"& hds.long$response=="3",]$response <- "Metal (Stainless Steel)"
hds.long[hds.long$question == "Q78"& hds.long$response=="4",]$response <- "Metal (Galvanized Steel)"
hds.long[hds.long$question == "Q78"& hds.long$response=="5",]$response <- "Plastic"
hds.long[hds.long$question == "Q78"& hds.long$response=="6",]$response <- "Cotton/Cloth"
hds.long[hds.long$question == "Q78"& hds.long$response=="9",]$response <- "Rocks"
hds.long[hds.long$question == "Q78"& hds.long$response=="10",]$response <- "Metal (Steel)"
hds.long[hds.long$question == "Q78"& hds.long$response=="11",]$response <- "Metal (Tin)"
hds.long[hds.long$question == "Q78"& hds.long$response=="1, 5",]$response <- "Metal (Unspecified), Plastic"
hds.long[hds.long$question == "Q78"& hds.long$response=="2, 5",]$response <- "Metal (Aluminum), Plastic"


#Q78b: screen material simplified
hds.long[hds.long$question == "Q78b"& hds.long$response=="1",]$response <- "Metal"
hds.long[hds.long$question == "Q78b"& hds.long$response=="2",]$response <- "Metal"
hds.long[hds.long$question == "Q78b"& hds.long$response=="3",]$response <- "Metal"
hds.long[hds.long$question == "Q78b"& hds.long$response=="4",]$response <- "Metal"
hds.long[hds.long$question == "Q78b"& hds.long$response=="5",]$response <- "Plastic"
hds.long[hds.long$question == "Q78b"& hds.long$response=="6",]$response <- "Cotton/Cloth"
hds.long[hds.long$question == "Q78b"& hds.long$response=="9",]$response <- "Rocks"
hds.long[hds.long$question == "Q78b"& hds.long$response=="10",]$response <- "Metal"
hds.long[hds.long$question == "Q78b"& hds.long$response=="11",]$response <- "Metal"
hds.long[hds.long$question == "Q78b"& hds.long$response=="1, 5",]$response <- "Metal, Plastic"
hds.long[hds.long$question == "Q78b"& hds.long$response=="2, 5",]$response <- "Metal, Plastic"

#pivot wider to combine with rainwater data
hds.wide <- pivot_wider(data = hds.long,
                        names_from = "question",
                        values_from = "response")

#remove unnecessary columns
hds.wide <- hds.wide%>%
  dplyr::select(-c(community, Q87, Q62, Q13, Q71, Q79))
  
#join data by site
iw.hds <- full_join(iw.dm, hds.wide, by = c("site"))

#clean up data
iw.hds <- iw.hds %>%
  drop_na(community)

#for each individual question, you will need to remove NAs as needed

#
#
#
#
#













# #load in data and if there is a missing value, assume participant does not follow best practices - conservative estimate. No is a zero. Yes is a 1
# #assumption: if there is a missing value, we assume a participant does not follow best practices to use a risk-averse analysis framework
# #assumption: this index assumes equal influence of each best practice on rainwater quality
# #assumption/limitation: we assume that the maintenance intervention reported was consistent across the duration of the study
# #assumption: sites with no HDS survey were assumed to have maintenance scores of 0/None.
# ##Q67 Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)? ----
# hds67<- hds
# hds67$Q67 <- as.character(hds67$Q67)
# #hds67[is.na(hds67$Q67),]$Q67 <- "0"
# hds67 <- hds67 %>%
#   drop_na(Q67)
# hds67[hds67$Q67=="0",]$Q67 <- "0"
# hds67[hds67$Q67=="1",]$Q67 <- "1"
# hds67[hds67$Q67=="2",]$Q67 <- "2"
# hds67$Q67 <- as.numeric(hds67$Q67)
# summary(as.factor(hds67$Q67))
# 
# ##Q60 What is your cistern made of: ----
# hds60<- hds
# hds60$Q60 <- as.character(hds60$Q60)
# summary(as.factor(hds60$Q60))
# 
# 
# ##Q65 How old is your cistern:  ----
# hds65<- hds
# hds65 <- hds65 %>%
#   drop_na(Q65)
# hds65$Q65 <- as.character(hds65$Q65)
# hds65[hds65$Q65=="0",]$Q65 <- "NA"
# hds65[hds65$Q65=="1",]$Q65 <- "NA"
# hds65[hds65$Q65=="2",]$Q65 <- "0-2"
# hds65[hds65$Q65=="3",]$Q65 <- "0-2"
# hds65[hds65$Q65=="4",]$Q65 <- "0-2"
# hds65[hds65$Q65=="5",]$Q65 <- "2-5 years"
# hds65[hds65$Q65=="6",]$Q65 <- "2-5 years"
# hds65[hds65$Q65=="7",]$Q65 <- "5+ years"
# hds65 <- hds65 %>% filter(Q65 != "NA")
# 
# 
# #Q62What is the capacity of your cistern (in gallons)?  ----
# hds62<- hds
# hds62 <- hds62 %>%
#   drop_na(Q62)
# hds62[hds62$Q62=="0",]$Q62 <- "NA"
# hds62[hds62$Q62=="14",]$Q62 <- "NA"
# hds62[hds62$Q62=="1",]$Q62 <- "small(<100)"
# hds62[hds62$Q62=="2",]$Q62 <- "small(<100)"
# hds62[hds62$Q62=="3",]$Q62 <- "small(<100)"
# hds62[hds62$Q62=="4",]$Q62 <- "small(<100)"
# hds62[hds62$Q62=="5",]$Q62 <- "medium(101-1000)"
# hds62[hds62$Q62=="6",]$Q62 <- "medium(101-1000)"
# hds62[hds62$Q62=="7",]$Q62 <- "medium(101-1000)"
# hds62[hds62$Q62=="8",]$Q62 <- "medium(101-1000)"
# hds62[hds62$Q62=="9",]$Q62 <- "large(>1000)"
# hds62[hds62$Q62=="10",]$Q62 <-"large(>1000)"
# hds62[hds62$Q62=="11",]$Q62 <-"large(>1000)"
# hds62[hds62$Q62=="12",]$Q62 <- "large(>1000)"
# hds62[hds62$Q62=="13",]$Q62 <- "large(>1000)"
# hds62 <- hds62 %>% filter(Q62 != "NA")
# hds62 <- hds62 %>% filter(Q62 != "N/A")
# hds62$Q62<- as.factor(hds62$Q62)
# summary(hds62$Q62)
# 
# ##Q78: How often do you clean parts of your roof draining system (like the debris filter, gutters, scuppers) ----
# hds78<- hds
# hds78 <- hds78 %>%
#   drop_na(Q78)
# hds78[hds78$Q78=="0",]$Q78 <- "No Answer"
# hds78[hds78$Q78=="100",]$Q78 <- "Unsure"
# hds78[hds78$Q78=="1",]$Q78 <- "As Needed"
# hds78[hds78$Q78=="2",]$Q78 <- "Monthly"
# hds78[hds78$Q78=="3",]$Q78 <- "Quarterly"
# hds78[hds78$Q78=="4",]$Q78 <- "Yearly"
# hds78 <- hds78 %>% filter(Q78 != "NA")
# hds78 <- hds78 %>% filter(Q78 != "N/A")
# hds78$Q78<- as.factor(hds78$Q78)
# summary(hds78$Q78)
# 
# ##Q78: What is the screen/filter made of? What type is it?- ----
# hds78<- hds
# hds78 <- hds78 %>%
#   drop_na(Q78)
# hds78[hds78$Q78=="0",]$Q78 <- "Unspecified"
# hds78[hds78$Q78=="100",]$Q78 <- "Unspecified"
# hds78[hds78$Q78=="1",]$Q78 <- "Metal"
# hds78[hds78$Q78=="2",]$Q78 <-  "Metal"
# hds78[hds78$Q78=="3",]$Q78 <-  "Metal"
# hds78[hds78$Q78=="4",]$Q78 <-  "Metal"
# hds78[hds78$Q78=="5",]$Q78 <- "Non-metal"
# hds78[hds78$Q78=="2, 5",]$Q78 <- "Non-metal"
# hds78[hds78$Q78=="1, 5",]$Q78 <- "Non-metal"
# hds78[hds78$Q78=="6",]$Q78 <-"Non-metal"
# hds78[hds78$Q78=="7",]$Q78 <- "Unspecified"
# hds78[hds78$Q78=="8",]$Q78 <- "No Screen"
# hds78[hds78$Q78=="9",]$Q78 <- "Non-metal"
# hds78[hds78$Q78=="10",]$Q78 <-  "Metal"
# hds78[hds78$Q78=="11",]$Q78 <-  "Metal"
# hds78[hds78$Q78=="12",]$Q78 <- "Unspecified"
# hds78[hds78$Q78=="13",]$Q78 <-  "Unspecified"
# hds78 <- hds78 %>% filter(Q78 != "NA")
# hds78 <- hds78 %>% filter(Q78 != "N/A")
# hds78$Q78<- as.factor(hds78$Q78)
# summary(hds78$Q78)
# 
# 
# ##Q71 Do you treat or wash your cistern with anything? ----
# hds71<- hds
# hds71$Q71 <- as.character(hds$Q71)
# hds71 <- hds71 %>%
#   drop_na(Q71)
# #hds[is.na(hds$Q71),]$Q71 <- "0"
# hds71[hds71$Q71=="0",]$Q71 <- "0"
# hds71[hds71$Q71=="1",]$Q71 <- "1"
# hds71[hds71$Q71=="2",]$Q71 <- "2"
# hds71$Q71 <- as.numeric(hds71$Q71)
# summary(as.factor(hds71$Q71))
# 
# ##Q76 Does your cistern have a first flush? ----
# hds76<- hds
# hds76$Q76 <- as.character(hds76$Q76)
# hds76 <- hds76 %>%
#   drop_na(Q76)
# #hds76[is.na(hds76$Q76),]$Q76 <- "0"
# hds76[hds76$Q76=="0",]$Q76 <- "0"
# hds76[hds76$Q76=="1",]$Q76 <- "1"
# hds76[hds76$Q76=="2",]$Q76 <- "2"
# hds76[hds76$Q76=="3",]$Q76 <- "0"
# hds76$Q76 <- as.numeric(hds76$Q76)
# summary(as.factor(hds76$Q76))
# 
# ##Q77 Does your cistern have a screen/filter for incoming water from down spout on top of the tank? ----
# hds77<- hds
# hds77$Q77 <- as.character(hds77$Q77)
# hds77 <- hds77 %>%
#   drop_na(Q77)
# #hds77[is.na(hds77$Q77),]$Q77 <- "0"
# hds77[hds77$Q77=="0",]$Q77 <- "0"
# hds77[hds77$Q77=="1",]$Q77 <- "1"
# hds77[hds77$Q77=="2",]$Q77 <- "2"
# hds77[hds77$Q77=="3",]$Q77 <- "0"
# hds77$Q77 <- as.numeric(hds77$Q77)
# summary(as.factor(hds77$Q77))
# 
# ##Q79 Do you ever remove the screen/filter and leave your cistern without the filter? ----
# ##note, when we were doing the HDS score models, this question was edited so that the Y/N response goes the same direction as the other questions - doing the best practice was a +1. We added a NOT to the question and the responses were reversed.
# 
# hds79<- hds
# hds79$Q79 <- as.character(hds79$Q79)
# hds79 <- hds79 %>%
#   drop_na(Q79)
# #hds79[is.na(hds79$Q79),]$Q79 <- "0"
# hds79[hds79$Q79=="0",]$Q79 <- "0"
# hds79[hds79$Q79=="1",]$Q79 <- "1"
# hds79[hds79$Q79=="2",]$Q79 <- "2"
# hds79[hds79$Q79=="100",]$Q79 <- "0"
# hds79$Q79 <- as.numeric(hds79$Q79)
# summary(as.factor(hds79$Q79))
# 
# #hds$score <- hds$Q67 + hds$Q71 + hds$Q76 + hds$Q77 + hds$Q79
# #hds$score <- as.character(hds$score)
# #hds$score_bin <- hds$score
# #hds[hds$score_bin=="0",]$score_bin <- "None"
# #hds[hds$score_bin=="1",]$score_bin <- "Medium"
# #hds[hds$score_bin=="2",]$score_bin <- "Medium"
# #hds[hds$score_bin=="3",]$score_bin <- "High"
# #hds[hds$score_bin=="4",]$score_bin <- "High"
# #hds[hds$score_bin=="5",]$score_bin <- "High"
# #hds$score_bin <- factor(hds$score_bin, levels = c("None", "Medium", "High"))
# #summary(hds$score_bin)
# 
# #iw.dm <- full_join(iw.dm, hds, by = c("site"))
# #iw.dm67- combining iw.dm with Q67=====
# iw.dm67 <- full_join(iw.dm, hds67, by = c("site"))
# iw.dm67 <- iw.dm67[!is.na(iw.dm67$community),]
# iw.dm67<-iw.dm67 %>%
#   drop_na(Q67)
# 
# 
# #iw.dm71- combining iw.dm with Q71=====
# iw.dm71 <- full_join(iw.dm, hds71, by = c("site"))
# iw.dm71 <- iw.dm71[!is.na(iw.dm71$community),]
# iw.dm71<-iw.dm71 %>%
#   drop_na(Q71)
# 
# #iw.dm76- combining iw.dm with Q76=====
# iw.dm76 <- full_join(iw.dm, hds76, by = c("site"))
# iw.dm76 <- iw.dm76[!is.na(iw.dm76$community),]
# iw.dm76<-iw.dm76 %>%
#   drop_na(Q76)
# 
# #iw.dm77- combining iw.dm with Q77=====
# iw.dm77 <- full_join(iw.dm, hds77, by = c("site"))
# iw.dm77 <- iw.dm77[!is.na(iw.dm77$community),]
# iw.dm77<-iw.dm77 %>%
#   drop_na(Q77)
# 
# #iw.dm79- combining iw.dm with Q79=====
# iw.dm79 <- full_join(iw.dm, hds79, by = c("site"))
# iw.dm79 <- iw.dm79[!is.na(iw.dm79$community),]
# iw.dm79<-iw.dm79 %>%
#   drop_na(Q79)
# 
# #iw.dm65- combining iw.dm with Q65=====
# iw.dm65 <- full_join(iw.dm, hds65, by = c("site"))
# iw.dm65 <- iw.dm65[!is.na(iw.dm65$community),]
# summary(as.factor(iw.dm65$Q67))
# 
# iw.dm65<-iw.dm65 %>%
#   drop_na(Q65)
# 
# #iw.dm60- combining iw.dm with Q60=====
# iw.dm60 <- full_join(iw.dm, hds60, by = c("site"))
# iw.dm60 <- iw.dm60[!is.na(iw.dm60$community),]
# iw.dm60<-iw.dm60 %>%
#   drop_na(Q60)
# 
# summary(as.factor(iw.dm60[iw.dm60$community=="Globe/Miami",]$Q60))

#write.csv(iw.score, "score_test.csv")
#iw.dm[is.na(iw.dm$score_bin),]$score_bin <- "None"
#iw.dm <- iw.dm[!is.na(iw.dm$community),]
#table(iw.score$score_bin, iw.score$community)
# 
# ##Q67b Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)? ----
# hds$Q67b <- as.character(hds$Q67)
# hds[is.na(hds$Q67b),]$Q67b <- "-1"
# hds[hds$Q67b=="0",]$Q67b <- "-1"
# hds[hds$Q67b=="1",]$Q67b <- "1"
# hds[hds$Q67b=="2",]$Q67b <- "-1"
# hds$Q67b <- as.numeric(hds$Q67b)
# summary(as.factor(hds$Q67b))
# 
# ##Q71b Do you treat or wash your cistern with anything? ----
# hds$Q71b <- as.character(hds$Q71)
# hds[is.na(hds$Q71b),]$Q71b <- "-1"
# hds[hds$Q71b=="0",]$Q71b <- "-1"
# hds[hds$Q71b=="1",]$Q71b <- "1"
# hds[hds$Q71b=="2",]$Q71b <- "-1"
# hds$Q71b <- as.numeric(hds$Q71b)
# summary(hds$Q71b)
# 
# ##Q76b Does your cistern have a first flush? ----
# hds$Q76b <- as.character(hds$Q76)
# hds[is.na(hds$Q76b),]$Q76b <- "-1"
# hds[hds$Q76b=="0",]$Q76b <- "-1"
# hds[hds$Q76b=="1",]$Q76b <- "1"
# hds[hds$Q76b=="2",]$Q76b <- "-1"
# hds[hds$Q76b=="3",]$Q76b <- "-1"
# hds$Q76b <- as.numeric(hds$Q76b)
# summary(hds$Q76b)
# 
# ##Q77b Does your cistern have a screen/filter for incoming water from down spout on top of the tank? ----
# hds$Q77b <- as.character(hds$Q77)
# hds[is.na(hds$Q77b),]$Q77b <- "-1"
# hds[hds$Q77b=="0",]$Q77b <- "-1"
# hds[hds$Q77b=="1",]$Q77b <- "1"
# hds[hds$Q77b=="2",]$Q77b <- "-1"
# hds[hds$Q77b=="3",]$Q77b <- "-1"
# hds$Q77b <- as.numeric(hds$Q77b)
# summary(hds$Q77b)
# 
# ##Q79b Do you ever NOT remove the screen/filter and leave your cistern without the filter? ----
# ##question edited so that the Y/N response goes the same direction as the other questions
# hds$Q79b <- as.character(hds$Q79)
# hds[hds$Q79b=="0",]$Q79b <- "-1"
# hds[hds$Q79b=="1",]$Q79b <- "1"
# hds$Q79b <- as.numeric(hds$Q79b)
# summary(hds$Q79b)
# 
# hds$scoreb <- hds$Q67b + hds$Q71b + hds$Q76b + hds$Q77b + hds$Q79b
# summary(as.factor(hds$scoreb))
# hds$scoreb <- as.character(hds$scoreb)
# hds$scoreb_bin <- hds$scoreb
# hds[hds$scoreb_bin=="-5",]$scoreb_bin <- "None"
# hds[hds$scoreb_bin=="1",]$scoreb_bin <- "Medium"
# hds[hds$scoreb_bin=="2",]$scoreb_bin <- "Medium"
# hds[hds$scoreb_bin=="3",]$scoreb_bin <- "High"
# hds[hds$scoreb_bin=="4",]$scoreb_bin <- "High"
# hds[hds$scoreb_bin=="5",]$scoreb_bin <- "High"
# hds$scoreb_bin <- factor(hds$scoreb_bin, levels = c("None", "Medium", "High"))
# summary(hds$scoreb_bin)
# 
# iw.dm <- full_join(iw.dm, hds, by = c("site"))
# 
# #write.csv(iw.scoreb, "scoreb_test.csv")
# iw.dm[is.na(iw.dm$scoreb_bin),]$scoreb_bin <- "None"
# iw.dm <- iw.dm[!is.na(iw.dm$community),]
# table(iw.scoreb$scoreb_bin, iw.scoreb$community)
# 
# #boxplot(log(iw.score$Cd)~iw.score$score)

# ##Q67 Do you clean parts of your roof draining system (like the debris filter, gutters, scuppers, etc.)?
# q67 <- hds
# q67$Q67 <- as.character(q67$Q67)
# q67 <- q67[q67$Q67!="0",]
# q67 <- q67[q67$Q67!="N/A",]
# q67 <- q67[!is.na(q67$Q67),]
# q67[q67$Q67=="1",]$Q67 <- "Yes"
# q67[q67$Q67=="2",]$Q67 <- "No"
# q67$Q67 <- as.factor(q67$Q67)
# summary(q67$Q67)

####----
####
####STOP
####DONT RUN THIS
####UNLESS 
####YOU WANT
####A TON OF
####FIGURES AND
####ERROS
####
####
####----
#Summaries ----
contam_list <- list("Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")


#distributions
lapply(X = contam_list,
       FUN = violintransFX,
       dataDF = iw.dm.longer,
       type.string = "iw",
       subset.string = "community",
       subset.title.string = "Community",
       facet.string = "transformation",
       facet.title.string = "",
       units.string = "ln(mg/kg) and (mg/kg)")

#by community
#boxplots by community
boxplotFX(dataDF = iw.ln.dm,
          type.string = "iw",
          analyte.string = "pH",
          subset.string = "community",
          subset.title.string = "Community",
          units.string = "")

lapply(X=contam_list,
       FUN = boxplotFX,
       dataDF = iw.ln.dm,
       type.string = "iw",
       subset.string = "community",
       subset.title.string = "Community",
       units.string = "(ln(mg/kg))")

#by community and sampling window
violinfacFX(dataDF = iw.ln.dm,
          type.string = "iw",
          analyte.string = "pH",
          subset.string = "samplings",
          subset.title.string = "Sampling Window",
          units.string = "",
          facet.string = "community",
          facet.title.string = "Community")

lapply(X=contam_list,
       FUN = violinfacFX,
       dataDF = iw.ln.dm,
       type.string = "iw",
       subset.string = "samplings",
       subset.title.string = "sampling_window",
       facet.string = "community",
       facet.title.string = "Community",
       units.string = "(ln(mg/kg))")

#Functions ----
violintransFX <- function(dataDF, analyte.string, subset.string, subset.title.string, facet.string, facet.title.string, units.string, type.string){

  #load libraries
  library(ggplot2)
  library(tidyverse)

  #assign data
  dat <- dataDF
  type <- type.string
  analyte <- analyte.string
  subset <- subset.string
  subset.title <- subset.title.string
  fac <- facet.string
  fac.title <- facet.title.string
  units <- units.string

  #format data
  dat <- dat %>%
    drop_na(!!subset)

  #write graph
  p <- ggplot(data = dat[dat$analyte == c(analyte),],
              mapping = aes_string(y = "concentration", x = subset, fill = subset)) +
    geom_violin() +
    geom_jitter(color="black", size=0.7, alpha=0.25) +
    labs(title = paste(analyte,"Concentrations by", subset.title, fac.title),
         fill = "",
         x = paste("\n",subset.title),
         y = paste("[",analyte,"] ",units,"\n", sep="")) +
    scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4078B2"))+
    facet_wrap(facets = as.formula(paste("~", fac)),scales = "free")+
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 15),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          strip.background = element_rect(fill = "white"),
          axis.text = element_text(vjust = .5, color = "black"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank())
  print(p)
  dev.print(png, paste(type,"_", analyte, "_vplot_", subset, ".png", sep=""), res=300, height=7, width=12, units="in")

}

# violintransFX(dataDF = iw.dm.longer,
#             type.string = "iw",
#             analyte.string = "Zn",
#             subset.string = "community",
#             subset.title.string = "Community",
#             facet.string = "transformation",
#             facet.title.string = "",
#             units.string = "ln(mg/kg) and (mg/kg)")


boxplotFX <- function(dataDF, analyte.string, subset.string, subset.title.string, units.string, type.string){

  library(ggplot2)
  library(tidyverse)

  dat <- dataDF
  type <- type.string
  analyte <- analyte.string
  subset <- subset.string
  subset.title <- subset.title.string
  units <- units.string

  p <- ggplot(data = dat,
              mapping = aes_string(y = analyte, x = subset, fill = subset)) +
    geom_boxplot() +
    labs(title = paste(analyte,"Concentrations by", subset.title),
         fill = "",
         x = paste("\n",subset.title),
         y = paste("[",analyte,"] ",units,"\n", sep="")) +
    scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4078B2"))+
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 15),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          axis.text = element_text(vjust = .5, color = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
          legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank())
  print(p)
  dev.print(png, paste(type,"_", analyte, "bplot_", subset,".png", sep=""), res=300, height=7, width=10, units="in")

}

violinfacFX <- function(dataDF, analyte.string, subset.string, subset.title.string, facet.string, facet.title.string, units.string, type.string){

  #load libraries
  library(ggplot2)
  library(tidyverse)

  #assign data
  dat <- dataDF
  type <- type.string
  analyte <- analyte.string
  subset <- subset.string
  subset.title <- subset.title.string
  fac <- facet.string
  fac.title <- facet.title.string
  units <- units.string

  #format data
  dat <- dat %>%
    drop_na(!!subset)

  #write graph
  p <- ggplot(data = dat,
              mapping = aes_string(y = analyte, x = subset, fill = subset)) +
    geom_violin() +
    geom_jitter(color="black", size=0.7, alpha=0.25) +
    labs(title = paste(analyte,"Concentrations by", subset.title, "and", fac.title),
         fill = "",
         x = paste("\n",subset.title),
         y = paste("[",analyte,"] ",units,"\n", sep="")) +
    scale_fill_viridis_d()+
    # scale_y_reverse()+
    facet_wrap(facets = as.formula(paste("~", fac)),scales = "free")+
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 15),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          axis.text = element_text(vjust = .5, color = "black"),
          axis.text.x = element_text(),
          legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank())
  print(p)
  dev.print(png, paste(type,"_", analyte, "_vplot_", subset, "_", fac, ".png", sep=""), res=300, height=7, width=10, units="in")

}



