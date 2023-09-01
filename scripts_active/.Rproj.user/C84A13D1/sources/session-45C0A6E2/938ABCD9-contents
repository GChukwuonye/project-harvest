#Kunal Palawat | kpalawat@email.arizona.edu, palawat.kunal@gmail.com
#Date Created: March 1st, 2021
#Description: Code to create hayden specific figures for Project Harvest soil and water data
#Notes
#Make functions and location/ssn barplots to be consistent with the other communities

#load libraries
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(effects)

#set working directory
setwd("/users/godsgiftnkechichukwuonye/Documents/GitHub/WorkingFiles")

is.dry <- read_xlsx("data/data_processing/IS_TM_Y123.xlsx", sheet = "Corrected - Dry", col_names = TRUE)


#load water and soil data from contaminant descriptions file, subset hayden/winkelman data only
hayden <- iw.dm[iw.dm$community=="Hayden/Winkelman",]
hayden.is <- is.dry[is.dry$community=="Hayden/Winkelman",]

#assign water samples pre and post strike
hayden$collection_date <- substr(hayden$sample.name, 11, 18) #subset sample name to create collection date
hayden$strike <- as.numeric(hayden$collection_date) #make numeric
hayden[hayden$strike<20191013,]$strike <- 0 #assign pre strike values 0
hayden[hayden$strike>=20191013,]$strike <- 1 #assign post strike values 1
hayden$strike <- as.factor(hayden$strike) #make factor
hayden$strike <- ifelse(hayden$strike==0, "Operating", "Not Operating") #label with words
hayden$strike <- factor(hayden$strike, levels = c("Operating", "Not Operating")) #order the labels

#load hayden distance data
distance <- read_xlsx("data/data_processing/LATLOGSITE.xlsx", sheet = "hayden", col_names = TRUE)
distance <- subset(distance, select = c("site", "siteID", "distance.rounded", "location", "distance.to.tailings.round", "dist.tailings.km"))

#merge with water and soil data by site
dat <- full_join(hayden, distance, by = c("site"))
is.dat <- full_join(hayden.is, distance, by = c("site"))

#remove rows without data
dat <- dat[!is.na(dat$community),]
is.dat <- is.dat[!is.na(is.dat$community),]

dat$direction <- factor(dat$location.y, levels = c("Hayden", "Winkelman"))
is.dat$direction <- factor(is.dat$location, levels = c("Hayden", "Winkelman"))

dat$distance.rounded.km <- dat$distance.rounded/1000
dat <- dat[dat$site!="H222",]
#subset winter data because only winter data exists post strike
dat.winter <- dat[dat$season=="Winter",]

#correlation between tailings and smelter
stest <- rcorr(as.matrix(dat[,c(40,42)]), type = "spearman")
stest$r
stest$P
# means <- aggregate(iw.dm.long[iw.dm.long$site=="H221",]$value,
#           by=list(iw.dm.long[iw.dm.long$site=="H221",]$samplings,iw.dm.long[iw.dm.long$site=="H221",]$analyte),
#           FUN=mean)
# write.csv(means, "h221meansSoil.csv")
#Barplot Direction Strike ----
HWdirstrikebarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$direction, dat.long$strike),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$direction, dat.long$strike),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
  
  names(gdat) <- c("direction", "strike","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$strike),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  
  names(rdat) <- c("direction", "strike","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=strike, y=gmean, x=direction)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = strike), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = strike, y =0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    scale_fill_manual(values = c("#AF5597", "#4068B2"))+
    labs(title = paste("Winter Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nLocation",
         shape = "",
         fill = "Smelter Operation Status") +
    #guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_HWdirectionstrike.png"), res=300, height=6.5, width=15, units="in")
  
}
HWdirstrikebarplotFX(dataDF = dat.winter,
                     name.string = "Al")
HWdirstrikebarplotFX(dataDF = dat.winter,
                     name.string = "Cd")
HWdirstrikebarplotFX(dataDF = dat.winter,
                     name.string = "Cu")
HWdirstrikebarplotFX(dataDF = dat.winter,
                     name.string = "Zn")
HWdirstrikebarplotFX(dataDF = dat.winter,
                     name.string = "Mn")
lapply(X=Contamlist,
       HWstrikebarplotFX,
       dataDF = hayden.dist[hayden.dist$season=="Winter",])

#Boxplot Strike ----
HWstrikeboxplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  dat <- dat.winter
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  p <- ggplot(dat.long,
              mapping = aes_string(fill="strike", y="value", x="location.y")) +
    geom_boxplot() +
    stat_boxplot(geom = "errorbar")+
    scale_fill_manual(values = c("#AF5597", "#4068B2"))+
    labs(title = paste("Winter Hayden/Winkelman ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nLocation",
         fill = "Smelter Operation Status") +
    #guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_HWlocationstrike.png"), res=100, height=6, width=8, units="in")
  
}
HWstrikeboxplotFX(dataDF = dat.winter,
                  name.string = "Al")

HWstrikeboxplotFX(dataDF = dat.winter,
                  name.string = "Cd")
HWstrikeboxplotFX(dataDF = dat.winter,
                  name.string = "Zn")
HWstrikeboxplotFX(dataDF = dat.winter,
                  name.string = "Mn")
HWstrikeboxplotFX(dataDF = dat.winter,
                  name.string = "Cu")

lapply(X=Contamlist,
       HWstrikebarplotFX,
       dataDF = hayden.dist[hayden.dist$season=="Winter",])


#Barplot Direction  ----
HWdirectionbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$direction),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$direction),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("direction", "gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$direction),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("direction", "min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=direction, y=gmean, x=direction)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = direction), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = direction, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    scale_fill_manual(values = c("#77B180", "#A2A9D6"))+
    labs(title = paste("Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nLocation",
         shape = "") +
    guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_HWlocation.png"), res=100, height=6, width=8, units="in")
  
}
HWdirectionbarplotFX(dataDF = dat,
                     name.string = "Al")
HWdirectionbarplotFX(dataDF = dat,
                     name.string = "Cd")
HWdirectionbarplotFX(dataDF = dat,
                     name.string = "Cu")
HWdirectionbarplotFX(dataDF = dat,
                     name.string = "Mn")
HWdirectionbarplotFX(dataDF = dat,
                     name.string = "Zn")
lapply(X=Contamlist,
       HWdirectionbarplotFX,
       dataDF = hayden.is.dist)


#Barplot Direction & Season ----
HWdirectionssnbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$direction, dat.long$season),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$direction, dat.long$season),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1", "Group.2"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1", "Group.2"))
  
  names(gdat) <- c("direction", "season","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$direction, dat.long$season),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1", "Group.2"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1", "Group.2"))
  
  names(rdat) <- c("direction", "season","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=direction)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+    
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    #geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = season), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = season, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    labs(title = paste("Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " Concentration (µg/L)\n", sep = ""),
         x = "\nLocation",
         fill = "Season",         
         shape = "") +
    #guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_HWlocationssn_nopoints.png"), res=100, height=6, width=8, units="in")
  
}
HWdirectionssnbarplotFX(dataDF = dat,
                        name.string = "Al")
HWdirectionssnbarplotFX(dataDF = dat,
                        name.string = "Cd")
HWdirectionssnbarplotFX(dataDF = dat,
                        name.string = "Cu")
HWdirectionssnbarplotFX(dataDF = dat,
                        name.string = "Mn")
HWdirectionssnbarplotFX(dataDF = dat,
                        name.string = "Zn")
lapply(X=Contamlist,
       HWdirectionssnbarplotFX,
       dataDF = hayden.dist)

#Barplot Season ----
HWssnbarplotFX <- function(dataDF, name.string){
  library(ggplot2)
  library(EnvStats)
  library(tidyverse)
  
  dat <- dataDF
  analyte <- name.string
  
  dat.long <- pivot_longer(data = dat,
                           cols = Be:Pb,
                           values_to = "value",
                           names_to = "analyte")
  
  dat.long <- dat.long[dat.long$analyte == analyte,]
  
  dat.long$count <- 1
  
  gmeans <- aggregate(dat.long$value,
                      by = list(dat.long$season),
                      FUN = geoMean)
  
  gsds <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = geoSD)
  
  gcounts <- aggregate(dat.long$count,
                       by = list(dat.long$season),
                       FUN = sum)
  
  gdat1 <- full_join(gmeans, gsds, by = c("Group.1"))
  gdat <- full_join(gdat1, gcounts, by = c("Group.1"))
  
  names(gdat) <- c("season","gmean", "gsd", "count")
  
  gdat$gmean <- as.numeric(gdat$gmean)
  gdat$gsd <- as.numeric(gdat$gsd)
  gdat$count <- as.numeric(gdat$count)
  
  rmin <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = min)
  
  rmax <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = max)
  
  rmed <- aggregate(dat.long$value,
                    by = list(dat.long$season),
                    FUN = median)
  
  rdat1 <- full_join(rmin, rmax, by = c("Group.1"))
  rdat <- full_join(rdat1, rmed, by = c("Group.1"))
  
  names(rdat) <- c("season","min", "max", "median")
  
  rdat.long <- pivot_longer(data = rdat,
                            cols = min:median,
                            values_to = "value",
                            names_to = "range")
  rdat.long$range <- factor(rdat.long$range, levels = c("min", "median", "max"))
  
  p <- ggplot(gdat,
              mapping = aes(fill=season, y=gmean, x=season)) +
    geom_bar(position="dodge", stat="identity", color = "black") +
    geom_errorbar(aes(ymax = gmean*gsd, ymin = gmean/gsd), width = 0.2, position = position_dodge(.9))+    
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    geom_point(data = rdat.long, fill = 'black', size = 2.2, mapping = aes(shape=range, y=value,group = season), position = position_dodge(.9))+
    geom_text(aes(label = paste("n =", count),group = season, y=0),
              vjust = 1.2,
              position = position_dodge(.9),
              family="Avenir") +
    labs(title = paste("Hayden/Winkelman Geometric Mean ", analyte, " Concentrations", sep = ""),
         subtitle = paste("Rooftop Harvested Rainwater"),
         y = paste(analyte, " [µg/L]\n", sep = ""),
         x = "\nSeason",
         fill = "",         
         shape = "") +
    guides(fill = "none")+
    #scale_y_continuous(expand = c(0, 0.1)) +
    theme_bw() +
    theme(strip.text = element_blank(),
          text = element_text(size=15, family = "Avenir"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.title = element_text(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust=.5, face = "bold"),
          plot.subtitle = element_text(hjust=.5),
          legend.position = "bottom")
  
  print(p)
  
  dev.print(png, paste(analyte,"_IWDM_HWssn.png"), res=100, height=6, width=8, units="in")
  
}
HWssnbarplotFX(dataDF = dat,
               name.string = "Al")
HWssnbarplotFX(dataDF = dat,
               name.string = "Cd")
HWssnbarplotFX(dataDF = dat,
               name.string = "Cu")
HWssnbarplotFX(dataDF = dat,
               name.string = "Mn")
HWssnbarplotFX(dataDF = dat,
               name.string = "Zn")
lapply(X=Contamlist,
       HWssnbarplotFX,
       dataDF = hayden.dist)


#Scatterplot West & South ----
HWsubsetScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
  library(ggpubr)
  library(ggplot2)
  
  dat <- dataDF
  analyte <- analyte.string
  xvar <- x.string
  subset <- subset.string
  subset.title <- subset.title.string
  test <- test.string
  xlabel <- xlabel.string
  
  p <- ggscatter(data=dat, x = xvar, y = analyte, color = subset,
                 add = "reg.line",
                 conf.int = TRUE,
                 #cor.coef = FALSE,
                 cor.coeff.args = list(method = test, label.x.npc = "middle"),
                 ylab = paste(analyte, "Concentration (mg/kg)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = direction),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#77B180", "#A2A9D6"))+
    scale_fill_manual(values = c("#77B180", "#A2A9D6"))+
    labs(title = paste(analyte, "Concentrations in Residential Soils\nby Distance From Smelter"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
          legend.position = "bottom",
          plot.caption = element_text(size = 13, face="italic", hjust = 0),
          axis.text = element_text(size=15, color = "black", face = "bold"),
          axis.title = element_text(size=15, color = "black", face = "bold"))
  print(p)
  dev.print(png, paste(analyte, "_IS_HWscatter.jpeg", sep = ""), res=100, height=6, width=7, units="in")
  print("Great Job!")
}

cor.test(hayden.dist[hayden.dist$direction=="West",]$Al, hayden.dist[hayden.dist$direction=="West",]$distance.rounded, method = "spearman")

HWsubsetScatterFX(dataDF = hayden.is.dist[hayden.is.dist$distance.rounded<2500,],
                  analyte.string = "Pb",
                  x.string = "distance.rounded",
                  subset.string = "direction",
                  subset.title.string = "Direction From Smelter",
                  test.string = "spearman",
                  xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = hayden.dist[hayden.dist$distance.rounded<2500,],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")

#Scatterplot West ----
HWWestsubsetScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
  library(ggpubr)
  library(ggplot2)
  
  dat <- dataDF
  analyte <- analyte.string
  xvar <- x.string
  subset <- subset.string
  subset.title <- subset.title.string
  test <- test.string
  xlabel <- xlabel.string
  
  p <- ggscatter(data=dat, x = xvar, y = analyte, color = subset,
                 add = "reg.line",
                 conf.int = FALSE,
                 #cor.coef = FALSE,
                 cor.coeff.args = list(method = test, label.x.npc = "middle"),
                 ylab = paste(analyte, "Concentration (mg/kg)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = direction),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    #geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#77B180", "#A2A9D6"))+
    scale_fill_manual(values = c("#77B180", "#A2A9D6"))+
    coord_cartesian(ylim=c(0,200))+
    labs(title = paste(analyte, "Concentrations in Residential Soils\nby Distance From Smelter"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          axis.line = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
          legend.position = "bottom",
          plot.caption = element_text(size = 13, face="italic", hjust = 0),
          axis.text = element_text(size=15, color = "black", face = "bold"),
          axis.title = element_text(size=15, color = "black", face = "bold"))
  print(p)
  dev.print(png, paste(analyte, "_IS_HWWestscatter.jpeg", sep = ""), res=100, height=6, width=7, units="in")
  print("Great Job!")
}

HWWestsubsetScatterFX(dataDF = hayden.dist[hayden.dist$distance.rounded<2500 & hayden.dist$direction=="West",],
                      analyte.string = "Pb",
                      x.string = "distance.rounded",
                      subset.string = "direction",
                      subset.title.string = "Direction From Smelter",
                      test.string = "spearman",
                      xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = hayden.dist[hayden.dist$distance.rounded<2500 & hayden.dist$direction=="West",],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")


#Water


#Scatterplot West & South & Season ----
HWsubsetssnScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
  library(ggpubr)
  library(ggplot2)
  
  dat <- dataDF
  analyte <- analyte.string
  xvar <- x.string
  subset <- subset.string
  subset.title <- subset.title.string
  test <- test.string
  xlabel <- xlabel.string
  
  p <- ggscatter(data=dat, x = xvar, y = analyte, color = subset,
                 add = "reg.line",
                 conf.int = TRUE,
                 #cor.coef = FALSE,
                 cor.coeff.args = list(method = test, label.x.npc = "middle"),
                 ylab = paste(analyte, "Concentration (µg/L)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = season),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste(analyte, "Concentrations in Harvested Rainwater\nby Distance From Smelter"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    facet_grid(.~direction, scales = "free_x")+
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
          legend.position = "bottom",
          plot.caption = element_text(size = 13, face="italic", hjust = 0),
          axis.text = element_text(size=15, color = "black", face = "bold"),
          axis.title = element_text(size=15, color = "black", face = "bold"))
  print(p)
  dev.print(png, paste(analyte, "_IWDM_HWdirssnscatter.jpeg", sep = ""), res=100, height=6, width=12, units="in")
  print("Great Job!")
}
Contamlist <- c("Al",  "Cd",  "Mn",  "Ni",  "Zn")


Zoomlist <- list(650,  5,  220,  10, 1250)

HWsubsetssnScatterFX(dataDF = hayden.dist[hayden.dist$distance.rounded<2500&hayden.dist$direction=="Hayden",],
                     analyte.string = "Al",
                     x.string = "distance.rounded",
                     subset.string = "season",
                     subset.title.string = "Season",
                     test.string = "spearman",
                     xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = hayden.dist[hayden.dist$distance.rounded<2500,],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")

#Scatterplot West & Season ----
HWWestsubsetssnScatterFX <- function(dataDF, analyte.string, x.string, subset.string, subset.title.string, test.string, xlabel.string){
  library(ggpubr)
  library(ggplot2)
  
  dat <- dataDF
  analyte <- analyte.string
  xvar <- x.string
  subset <- subset.string
  subset.title <- subset.title.string
  test <- test.string
  xlabel <- xlabel.string
  
  p <- ggscatter(data=dat, x = xvar, y = analyte, color = subset,
                 add = "reg.line",
                 conf.int = TRUE,
                 #cor.coef = FALSE,
                 cor.coeff.args = list(method = test, label.x.npc = "middle"),
                 ylab = paste(analyte, "Concentration (µg/L)\n"),
                 xlab = paste("\n",xlabel),
                 cor.coef.size = 6, point = FALSE) +
    stat_cor(method=test,
             mapping = aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color = season),
             label.x.npc = "middle",
             family = "Avenir",
             size = 6) +
    geom_point(mapping = aes_string(fill = subset), color = "black", size = 2.5, shape = 21) +
    scale_color_manual(values = c("#A2A9D6", "#F2C893"))+
    scale_fill_manual(values = c("#A2A9D6", "#F2C893"))+
    labs(title = paste(analyte, "Concentrations in Harvested Rainwater\nby Distance From Smelter"),
         color = paste(subset.title),
         fill = paste(subset.title)) +
    facet_grid(.~direction, scales = "free_x")+
    theme_bw() +
    theme(text = element_text(size=13, family = "Avenir"),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
          legend.position = "bottom",
          plot.caption = element_text(size = 13, face="italic", hjust = 0),
          axis.text = element_text(size=15, color = "black", face = "bold"),
          axis.title = element_text(size=15, color = "black", face = "bold"))
  print(p)
  dev.print(png, paste(analyte, "_IWDM_HWWestdirssnscatter.jpeg", sep = ""), res=100, height=6, width=7, units="in")
  print("Great Job!")
}

HWWestsubsetssnScatterFX(dataDF = hayden.dist[hayden.dist$distance.rounded<2500&hayden.dist$direction=="West",],
                         analyte.string = "Pb",
                         x.string = "distance.rounded",
                         subset.string = "season",
                         subset.title.string = "Season",
                         test.string = "spearman",
                         xlabel.string = "Distance From Smelter (m)")

lapply(X=Contamlist,
       HWsubsetScatterFX,
       dataDF = hayden.dist[hayden.dist$distance.rounded<2500,],
       x.string = "distance.rounded",
       subset.string = "direction",
       subset.title.string = "Direction from Smelter",
       test.string = "spearman",
       xlabel.string = "Distance From Smelter (m)")

#Cd----
Cd0 <- lmer(data = dat,
            log(Cd) ~ (1|site),
            REML = T)
Cd1 <- lmer(data = dat,
            log(Cd) ~ location.y + season+ (1|site),
            REML = F)
summary(Cd1)
anova(Cd1)
Cd1.5 <- lmer(data = dat,
              log(Cd) ~ strike+ location.y+distance.rounded.km + dist.tailings.km + season 
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
Cd2 <- lmer(data = dat,
              log(Cd) ~ strike+location.y+distance.rounded.km + dist.tailings.km + season 
              + location.y:season +  (1|site),
              REML = T)
step(Cd1.5, scope=list(lower=Cd0), direction="backward")
summary(Cd1.5)
anova(Cd1.5)
report(Cd1.5)
car::vif(Cd1.5)
car::vif(Cd2)
AIC(Cd0)
AIC(Cd1.5)
performance(Cd1.5)

ggplot(data = dat, aes(x = dist.tailings.km, y = Cd))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")

dev.print(png, "Cd_checkhayden7.png", res=100, height=12, width=30, units="in")

plot(allEffects(Cd1))
dev.print(png, "Cd_checkhayden1.png", res=300, height=10, width=10, units="in")
cd.sum <- summary(Cd1)
write.csv(cd.sum$coefficients, "Cd1coefHW.csv")
cd.ssnmeans <- lsmeans(Cd1, pairwise~season,  adjust = "tukey")#
write.csv(cd.ssnmeans$lsmeans, "Cd1lsmeansssnHW.csv")
write.csv(cd.ssnmeans$contrasts, "Cd7contrastsssnHW.csv")
cd.locmeans <- lsmeans(Cd1, pairwise~location.y,  adjust = "tukey")#
write.csv(cd.locmeans$lsmeans, "Cd1lsmeanslocHW.csv")
write.csv(cd.locmeans$contrasts, "Cd7contrastslocHW.csv")


aggregate(dat$Cd,
          by = list(dat$period),
          FUN = geoMean)

predict.dat <- ggeffect(model = Cd1,
                        terms = c("location.y"),
                        back.transform = F,
                        type = "re")

ggplot(data = dat, aes(x = location.y, y = log(Cd)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Pb by Distance From Smelter and Effect Trendline\n",
       y = "ln(Pb) [µg/L]\n",
       x = "\n Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Cd_HWdisteffectln.png", res=100, height=6, width=8, units="in")


#Al----
Al0 <- lmer(data = dat,
            log(Al) ~ (1|site),
            REML = T)
Al1 <- lmer(data = dat,
            log(Al) ~ season + distance.rounded.km  + strike + (1|site),
            REML = F)
summary(Al1)
anova(Al1)
vif(Al1)
Al1.5 <- lmer(data = dat,
              log(Al) ~ strike+location.y+distance.rounded.km + dist.tailings.km + season 
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
Al2 <- lmer(data = dat,
            log(Al) ~ strike+location.y+distance.rounded.km + dist.tailings.km + season 
            + location.y:season +  (1|site),
            REML = T)
step(Al1.5, scope=list(lower=Cd0), direction="backward")
summary(Al1)
anova(Al1)
report(Al1)
car::vif(Al1)
car::vif(Al2)
AIC(Al0)
AIC(Al1)
performance(Al1)

ggplot(data = dat, aes(x = dist.tailings.km, y = Al))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")
dev.print(png, "Al_checkhayden9.png", res=300, height=5, width=5, units="in")


ggplot(data = dat, aes(x = season, y = Al))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")


dev.print(png, "Al_checkhayden7.png", res=300, height=5, width=5, units="in")

plot(allEffects(Al1))
dev.print(png, "Al_checkhayden11.png", res=300, height=10, width=10, units="in")
al.sum <- summary(Al1)
write.csv(al.sum$coefficients, "Al1coefHW.csv")
al.ssnmeans <- lsmeans(Al1, pairwise~season, adjust = "tukey")#
write.csv(al.ssnmeans$lsmeans, "Al1lsmeansssnHW.csv")
write.csv(al.ssnmeans$contrasts, "Al1contrastsssnHW.csv")

aggregate(dat$Al,
          by = list(dat$period),
          FUN = geoMean)

predict.dat <- ggeffect(model = Al1,
                        terms = c("distance.rounded.km"),
                        back.transform = F,
                        type = "re")

ggplot(data = dat, aes(x = distance.rounded.km, y = log(Al)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Al by Distance From Smelter and Effect Trendline\n",
       y = "ln(Al) [µg/L]\n",
       x = "\n Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Al_HWdisteffectln.png", res=100, height=6, width=8, units="in")





ggplot(data = dat, aes(x = distance.rounded.km, y = Al))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")




#Zn====
Zn0 <- lmer(data = dat,
            log(Zn) ~ (1|site),
            REML = T)
Zn1 <- lmer(data = dat,
            log(Zn) ~  season+ (1|site),
            REML = F)
summary(Zn1)
anova(Zn1)
Zn1.5 <- lmer(data = dat,
              log(Zn) ~ strike+location.y+distance.rounded.km + dist.tailings.km + season 
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
Zn2 <- lmer(data = dat,
            log(Zn) ~ strike+location.y+distance.rounded.km + dist.tailings.km + season 
            + location.y:season +  (1|site),
            REML = T)
step(Zn1.5, scope=list(lower=Zn0), direction="backward")
summary(Zn1.5)
anova(Zn1.5)
report(Zn1.5)
car::vif(Zn1.5)



ggplot(data = dat, aes(x = season, y = Zn))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")

dev.print(png, "Zn_checkhayden7.png", res=300, height=5, width=10, units="in")

plot(allEffects(Zn1))
dev.print(png, "Zn_checkhayden8.png", res=300, height=5, width=10, units="in")

zn.sum <- summary(Zn1)
write.csv(zn.sum$coefficients, "Zn1coefHW.csv")
zn.ssnmeans <- lsmeans(Zn1, pairwise~season, adjust = "tukey")#
write.csv(zn.ssnmeans$lsmeans, "Zn1lsmeansssnHW.csv")
write.csv(zn.ssnmeans$contrasts, "Zn7contrastsssnHW.csv")

aggregate(dat$Zn,
          by = list(dat$period),
          FUN = geoMean)

predict.dat <- ggeffect(model = Zn1,
                        terms = c("season"),
                        back.transform = F,
                        type = "re")

ggplot(data = dat, aes(x = season, y = log(Zn)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Zn by Distance From Smelter and Effect Trendline\n",
       y = "ln(Zn) [µg/L]\n",
       x = "\n Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Zn_HWdisteffectln.png", res=100, height=6, width=8, units="in")

#Mn====
Mn0 <- lmer(data = dat,
            log(Mn) ~ (1|site),
            REML = T)
Mn1 <- lmer(data = dat,
            log(Mn) ~  distance.rounded.km + season + (1|site),
            REML = F)
summary(Mn1)
anova(Mn1)

Mn1.5 <- lmer(data = dat,
              log(Mn) ~ strike+location.y+distance.rounded.km + dist.tailings.km + season 
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
Mn2 <- lmer(data = dat,
            log(Mn) ~   strike+distance.rounded.km + location.y + dist.tailings.km + season 
            + location.y:season +  (1|site),
            REML = T)
step(Mn2, scope=list(lower=Mn0), direction="backward")
summary(Mn1.5)
anova(Mn1.5)
report(Mn1.5)
car::vif(Mn1.5)
car::vif(Mn2)
AIC(Mn0)
AIC(Mn1.5)
performance(Mn1.5)

ggplot(data = dat, aes(x = dist.tailings.km, y = Mn))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")

dev.print(png, "Mn_checkhayden7.png", res=300, height=5, width=10, units="in")

plot(allEffects(Mn1))
dev.print(png, "Mn_checkhayden5.png", res=300, height=5, width=10, units="in")
mn.sum <- summary(Mn1)
write.csv(mn.sum$coefficients, "Mn1coefHW.csv")
mn.ssnmeans <- lsmeans(Mn1, pairwise~season, adjust = "tukey")#
write.csv(mn.ssnmeans$lsmeans, "Mn1lsmeansssnHW.csv")
write.csv(mn.ssnmeans$contrasts, "Mn7contrastsssnHW.csv")

aggregate(dat$Mn,
          by = list(dat$season),
          FUN = geoMean)

predict.dat <- ggeffect(model = Mn1,
                        terms = c("season"),
                        back.transform = F,
                        type = "re")

ggplot(data = dat, aes(x = season, y = log(Mn)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Mn by Distance From Smelter and Effect Trendline\n",
       y = "ln(Mn) [µg/L]\n",
       x = "\n Distance From Smelter (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
dev.print(png, "Mn_HWdisteffectln.png", res=100, height=6, width=8, units="in")

#Cu====
Cu0 <- lmer(data = dat,
            log(Cu) ~ (1|site),
            REML = T)
Cu1 <- lmer(data = dat,
            log(Cu) ~  location.y + season + (1|site),
            REML = F)
summary(Cu1)
anova(Cu1)

Cu1.5 <- lmer(data = dat,
              log(Cu) ~ strike+ location.y+distance.rounded.km + dist.tailings.km + season 
              + location.y:distance.rounded.km + location.y:dist.tailings.km + location.y:season + dist.tailings.km:distance.rounded.km
              + (1|site),
              REML = T)
Cu2 <- lmer(data = dat,
            log(Cu) ~  strike+ distance.rounded.km + location.y + dist.tailings.km + season 
            + location.y:season +  (1|site),
            REML = T)
step(Cu1.5, scope=list(lower=Mn0), direction="backward")
summary(Cu1.5)
anova(Cu1.5)
report(Mn1.5)
car::vif(Mn1.5)
car::vif(Mn2)
AIC(Mn0)
AIC(Mn1.5)
performance(Mn1.5)

ggplot(data = dat, aes(x = location.y, y = Cu))+
  geom_point(aes(fill = site), shape=21)+
  facet_wrap(.~location.y)+
  theme_classic()+
  theme(legend.position="none")

dev.print(png, "Cu_checkhayden7.png", res=100, height=12, width=30, units="in")

plot(allEffects(Cu1))
dev.print(png, "Cu_checkhayden8.png", res=300, height=5, width=10, units="in")
cu.sum <- summary(Cu1)
write.csv(cu.sum$coefficients, "Cu1coefHW.csv")
cu.ssnmeans <- lsmeans(Cu1, pairwise~season, adjust = "tukey")#
write.csv(cu.ssnmeans$lsmeans, "Cu1lsmeansssnHW.csv")
write.csv(cu.ssnmeans$contrasts, "Cu7contrastsssnHW.csv")

cu.locmeans <- lsmeans(Cu1, pairwise~location.y, adjust = "tukey")#
write.csv(cu.locmeans$lsmeans, "Cu1lsmeanslocHW.csv")
write.csv(cu.locmeans$contrasts, "Cu7contrastslocHW.csv")
aggregate(dat$Cu,
          by = list(dat$season),
          FUN = geoMean)

predict.dat <- ggeffect(model = Cu1,
                        terms = c("season"),
                        back.transform = F,
                        type = "re")

ssn<- ggplot(data = dat, aes(x = season, y = log(Cu)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Cu by Season Effect Trendline\n",
       y = "ln(Cu) [µg/L]\n",
       x = "\n Season (km)")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggplot(data = dat, aes(x = location.y, y = log(Cu)))+
  geom_point()+
  geom_ribbon(data = predict.dat, mapping = aes(x=x, y = predicted, ymin = conf.low, ymax =conf.high), alpha = .5, fill = "#95CACA")+ #adds shading for error
  geom_line(data = predict.dat, mapping = aes(x=x, y = predicted))+
  labs(title = "Cu by Location\n",
       y = "ln(Cu) [µg/L]\n",
       x = "\n Location")+
  theme_bw()+
  theme(text = element_text(family = "Avenir", size = 13),
        panel.grid = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

