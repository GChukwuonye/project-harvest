library("FactoMineR")
library("factoextra")
library(readxl)
library(ggplot2)
library(GGally)
library(tidyr)
library(ggpubr)
library(Hmisc)
library("EnvStats")
library(vegan)

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

#load data
#IW DM
iw.dm <- read_xlsx("data/data_clean/IW_DM_Y123.xlsx", sheet = "Corrected", col_names = TRUE) #corrected mean the corrected tab in the excel sheet
iw.dm.detects <- read_xlsx("data/data_clean/IW_DM_Y123.xlsx", sheet = "Detection", col_names = TRUE)
mlod <- read_xlsx("data/data_processing/IPSW_MLODS.xlsx", sheet = "corrected - 12.22.20", col_names = TRUE)
iw.mlod <- mlod[mlod$`Sample Type`=="IW",]
iw.mlod.dm <- iw.mlod[iw.mlod$Analysis=="DM",]

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

#remove field blanks. to remove anything, use a type!="VAlue" to remove them.
iw.dm <- iw.dm[iw.dm$type!="B", ]
iw.dm.detects <- iw.dm.detects[iw.dm.detects$type!="B", ]

#remove ATS samples because ATS samples are not included in PH research. 
iw.dm <- iw.dm[iw.dm$site!="ATS1", ]
iw.dm.detects <- iw.dm.detects[iw.dm.detects$site!="ATS1", ]

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


#confirm correct order of categorical variables
#iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("FW", "LW", "FM", "LM"))
iw.dm.detects$samplings <- factor(iw.dm.detects$samplings, levels = c("First Winter", "Last Winter", "First Monsoon", "Last Monsoon"))
iw.dm.detects$period <- factor(iw.dm.detects$period, levels = c("First", "Last"))
iw.dm.detects$season <- factor(iw.dm.detects$season, levels = c("Winter", "Monsoon"))
iw.dm.detects$sampling_year <- factor(iw.dm.detects$sampling_year, levels = c("Water Year 1", "Water Year 2", "Water Year 3"))
iw.dm.detects$community <- factor(iw.dm.detects$community, levels = c("Dewey-Humboldt", "Globe/Miami", "Hayden/Winkelman", "Tucson"))



#pH and EC data
#append pH and EC data
iw.pHec <- read_xlsx("data/data_clean/IW_pHEC_Y123.xlsx", sheet = 1, col_names = TRUE)
iw.pHec <- iw.pHec[iw.pHec$type!="B",] #removing field blanks
iw.dm <- full_join(iw.dm, iw.pHec, by = c("sample.name", "type")) #joins the phec data with the original iw.dm we had before
iw.dm <- iw.dm[!is.na(iw.dm$community),]
na.omit(iw.dm$community)

iw.pHec <- iw.dm[!is.na(iw.dm$pH),]
na.omit(iw.dm$pH)
iw.pHec <- iw.dm[!is.na(iw.dm$EC),]
na.omit(iw.dm$EC)
aggregate(iw.pHec$EC,
          by = list(iw.pHec$season),
          FUN = max)



median(iw.pHec$EC)
iw.dm.long <- pivot_longer(iw.dm,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")

iw.dm.detects.long <- pivot_longer(iw.dm.detects,
                                   cols = Be:Pb,
                                   values_to = "detection",
                                   names_to = "analyte")


iw.mlod.dm.long <- pivot_longer(iw.mlod.dm,
                                cols=Be:Pb,
                                values_to = "value",
                                names_to = "analyte")
#redundancy analysis
iwdm.pca <- rda(as.data.frame(iw.dm[,9:27]), scale = T)
summary(iwdm.pca)
screeplot(iwdm.pca)
plot(iwdm.pca, display="sites", xlab="PC1 (25%)", ylab="PC2 (14%)")
biplot(iwdm.pca, scaling=1, xlab="PC1 (25%)", ylab="PC2 (14%)") #Focus on variables (species)
biplot(iwdm.pca, scaling=2, xlab="PC1 (25%)", ylab="PC2 (14%)") #Focus on objects (sites)

#scaled
data("varechem")
str(varechem)
iw.dm.scaled <- scale(as.data.frame(iw.dm[,9:27]))
iwdm.scaled.pca <- rda(iw.dm.scaled, scale = T)
summary(iwdm.scaled.pca)
screeplot(iwdm.scaled.pca)

plot(iwdm.scaled.pca, display="sites", xlab="PC1 (25%)", ylab="PC2 (14%)")
biplot(iwdm.scaled.pca, scaling=1, xlab="PC1 (25%)", ylab="PC2 (14%)") #Focus on variables (species)
biplot(iwdm.scaled.pca, scaling=2, xlab="PC1 (25%)", ylab="PC2 (14%)") #Focus on objects (sites)


#Multiple Factor Analysis ----
iw.dm.longer <- pivot_longer(data=iw.dm,
                             cols=c(9:27),
                             names_to = "analyte",
                             values_to = "value")
facto.dat <- iw.dm[,c(4,9:30)]
facto.dat$com <- facto.dat$community
facto.dat <- facto.dat[,-1]
facto.dat <- iw.dm.longer
facto.dat <- facto.dat[!is.na(facto.dat$value),]

facto.dat <- aggregate(facto.dat$value,
                       by = list(facto.dat$community, facto.dat$season, facto.dat$period, facto.dat$analyte, facto.dat$year),
                       FUN = geoMean)
facto.dat <- pivot_wider(data=facto.dat,
                         names_from = "Group.4",
                         values_from = "x")
rownames(facto.dat) <- paste(facto.dat$Group.1, facto.dat$Group.2, facto.dat$Group.3)
iw.mfa <- MFA(facto.dat, 
              group = c(1, 3, 19), 
              type = c("n", "n", "s"),
              name.group = c("location", "time", "metals"),
              # num.group.sup = 4,
              graph = FALSE)
summary(iw.mfa)
eig.val <- get_eigenvalue(iw.mfa)
head(eig.val)

fviz_screeplot(iw.mfa)+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_blank())
dev.print(jpeg, "screeplot-3.jpg", res=1000, width=90, units="mm")

group <- get_mfa_var(iw.mfa, "group")
group
# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

fviz_mfa_var(iw.mfa, "group")+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_blank())
dev.print(jpeg, "group-3.jpg", res=1000, width=3543, height = 5557, units="px")

# Contribution to the first dimension
fviz_contrib(iw.mfa, "group", axes = 1)+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_text(hjust=.5))
dev.print(jpeg, "dim1cont-3.jpg", res=1000, width=90, units="mm")

# Contribution to the second dimension
fviz_contrib(iw.mfa, "group", axes = 2)+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_text(hjust=.5))
dev.print(jpeg, "dim2cont-3.jpg", res=1000, width=90, units="mm")


quanti.var <- get_mfa_var(iw.mfa, "quanti.var")
quanti.var 
# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)

write.csv(file = "contributions.csv", quanti.var$contrib)

fviz_mfa_var(iw.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

# Contributions to dimension 1
fviz_contrib(iw.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "qcontribdim1-3.png", res=100, height=6, width=8, units="in")

# Contributions to dimension 2
fviz_contrib(iw.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "qcontribdim2-3.png", res=100, height=6, width=8, units="in")

fviz_mfa_var(iw.mfa, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_blank())
dev.print(jpeg, "contrib-3.jpg", res=1000, width=90, units="mm")

# Color by cos2 values: quality on the factor map
fviz_mfa_var(iw.mfa, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE)+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "cos2-3.png", res=100, height=7, width=7, units="in")

fviz_cos2(iw.mfa, choice = "quanti.var", axes = 1)+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "qcos2dim1-3.png", res=100, height=6, width=8, units="in")

fviz_cos2(iw.mfa, choice = "quanti.var", axes = 2)+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "qcos2dim2-3.png", res=100, height=6, width=8, units="in")

#plot individuals
fviz_mfa_ind(iw.mfa, col.ind = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "indcontrib-3.png", res=100, height=6, width=8, units="in")

fviz_mfa_ind(iw.mfa, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)+
  theme_classic()+
  theme(text=element_text(family="Avenir"),
        plot.title = element_text(hjust=.5))
dev.print(png, "indcos2-3.png", res=100, height=6, width=8, units="in")









