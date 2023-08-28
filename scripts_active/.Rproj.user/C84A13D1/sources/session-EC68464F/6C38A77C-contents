#Kunal Palawat | kpalawat@email.arizona.edu
#April 3rd, 2020
#Description: Correlations and Multiple Factor Analysis


#load libraries
library(readxl)
library(ggplot2)
library(GGally)
library(tidyr)
library(ggpubr)
library(Hmisc)

#spearman correlations ----
iw.dm.cor <- rcorr(as.matrix(iw.dm[,c(9:27, 74)]), type = "spearman")
iw.dm.rho <- formatC(signif(iw.dm.cor$r,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- formatC(signif(iw.dm.cor$P,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- as.data.frame(iw.dm.p)
iw.dm.p$ID <- rownames(iw.dm.p)
iw.dm.p.long <- pivot_longer(data=iw.dm.p,
                             cols = Be:pH,
                             values_to = "value",
                             names_to = "analyte")
iw.dm.p.long$value <- ifelse(as.numeric(iw.dm.p.long$value)<.0001, "<0.0001",as.character(iw.dm.p.long$value)) #NAs are ok!
iw.dm.p.wide <- pivot_wider(data=iw.dm.p.long,
                            names_from = "analyte",
                            values_from = "value")
write.csv(iw.dm.rho, "spearmancorrho.csv")
write.csv(iw.dm.p.wide, "spearmancorp.csv")

iw.dm.cor <- rcorr(as.matrix(iw.dm[iw.dm$community=="Hayden/Winkelman",c(9:27, 31,33)]), type = "spearman")
iw.dm.rho <- formatC(signif(iw.dm.cor$r,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- formatC(signif(iw.dm.cor$P,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- as.data.frame(iw.dm.p)
iw.dm.p$ID <- rownames(iw.dm.p)
iw.dm.p.long <- pivot_longer(data=iw.dm.p,
                             cols = Be:pH,
                             values_to = "value",
                             names_to = "analyte")
iw.dm.p.long$value <- ifelse(as.numeric(iw.dm.p.long$value)<.0001, "<0.0001",as.character(iw.dm.p.long$value)) #NAs are ok!
iw.dm.p.wide <- pivot_wider(data=iw.dm.p.long,
                            names_from = "analyte",
                            values_from = "value")
write.csv(iw.dm.rho, "spearmancorrhoHW.csv")
write.csv(iw.dm.p.wide, "spearmancorpHW.csv")


iw.dm.cor <- rcorr(as.matrix(iw.dm[iw.dm$community=="Dewey-Humboldt",c(9:27, 31,33)]), type = "spearman")
iw.dm.rho <- formatC(signif(iw.dm.cor$r,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- formatC(signif(iw.dm.cor$P,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- as.data.frame(iw.dm.p)
iw.dm.p$ID <- rownames(iw.dm.p)
iw.dm.p.long <- pivot_longer(data=iw.dm.p,
                             cols = Be:pH,
                             values_to = "value",
                             names_to = "analyte")
iw.dm.p.long$value <- ifelse(as.numeric(iw.dm.p.long$value)<.0001, "<0.0001",as.character(iw.dm.p.long$value)) #NAs are ok!
iw.dm.p.wide <- pivot_wider(data=iw.dm.p.long,
                            names_from = "analyte",
                            values_from = "value")
write.csv(iw.dm.rho, "spearmancorrhoDH.csv")
write.csv(iw.dm.p.wide, "spearmancorpDH.csv")

iw.dm.cor <- rcorr(as.matrix(iw.dm[iw.dm$community=="Globe/Miami",c(9:27, 31,33)]), type = "spearman")
iw.dm.rho <- formatC(signif(iw.dm.cor$r,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- formatC(signif(iw.dm.cor$P,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- as.data.frame(iw.dm.p)
iw.dm.p$ID <- rownames(iw.dm.p)
iw.dm.p.long <- pivot_longer(data=iw.dm.p,
                             cols = Be:pH,
                             values_to = "value",
                             names_to = "analyte")
iw.dm.p.long$value <- ifelse(as.numeric(iw.dm.p.long$value)<.0001, "<0.0001",as.character(iw.dm.p.long$value)) #NAs are ok!
iw.dm.p.wide <- pivot_wider(data=iw.dm.p.long,
                            names_from = "analyte",
                            values_from = "value")
write.csv(iw.dm.rho, "spearmancorrhoGM.csv")
write.csv(iw.dm.p.wide, "spearmancorpGM.csv")

iw.dm.cor <- rcorr(as.matrix(iw.dm[iw.dm$community=="Tucson",c(9:27, 31,33)]), type = "spearman")
iw.dm.rho <- formatC(signif(iw.dm.cor$r,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- formatC(signif(iw.dm.cor$P,digits=2), digits=2,format="fg", flag="#")
iw.dm.p <- as.data.frame(iw.dm.p)
iw.dm.p$ID <- rownames(iw.dm.p)
iw.dm.p.long <- pivot_longer(data=iw.dm.p,
                             cols = Be:pH,
                             values_to = "value",
                             names_to = "analyte")
iw.dm.p.long$value <- ifelse(as.numeric(iw.dm.p.long$value)<.0001, "<0.0001",as.character(iw.dm.p.long$value)) #NAs are ok!
iw.dm.p.wide <- pivot_wider(data=iw.dm.p.long,
                            names_from = "analyte",
                            values_from = "value")
write.csv(iw.dm.rho, "spearmancorrhoT.csv")
write.csv(iw.dm.p.wide, "spearmancorpT.csv")

ic.dm.cor <- rcorr(as.matrix(ic.dm[,c(11, 14, 17:33)]), type = "spearman")
ic.dm.rho <- formatC(signif(ic.dm.cor$r,digits=2), digits=2,format="fg", flag="#")
ic.dm.p <- formatC(signif(ic.dm.cor$P,digits=2), digits=2,format="fg", flag="#")
ic.dm.p <- as.data.frame(ic.dm.p)
ic.dm.p$ID <- rownames(ic.dm.p)
ic.dm.p.long <- pivot_longer(data=ic.dm.p,
                             cols = Be:Pb,
                             values_to = "value",
                             names_to = "analyte")
ic.dm.p.long$value <- ifelse(as.numeric(ic.dm.p.long$value)<.0001, "<0.0001",as.character(ic.dm.p.long$value)) #NAs are ok!
ic.dm.p.wide <- pivot_wider(data=ic.dm.p.long,
                            names_from = "analyte",
                            values_from = "value")
write.csv(ic.dm.rho, "spearmancorrhoAZB.csv")
write.csv(ic.dm.p.wide, "spearmancorpAZB.csv")

# iw.dm.cor2 <- cor(iw.dm[,9:27], method="spearman")
# write.csv(iw.dm.cor, "spearmancor.csv")
# iw.dm.cor.hw <- cor(iw.dm[iw.dm$community=="Hayden/Winkelman",9:27], method="spearman")
# write.csv(iw.dm.cor.hw, "spearmancorHW.csv")
# iw.dm.cor.dh <- cor(iw.dm[iw.dm$community=="Dewey-Humboldt",9:27], method="spearman")
# write.csv(iw.dm.cor.dh, "spearmancorDH.csv")
# iw.dm.cor.gm <- cor(iw.dm[iw.dm$community=="Globe/Miami",9:27], method="spearman")
# write.csv(iw.dm.cor.gm, "spearmancorGM.csv")
# iw.dm.cor.t <- cor(iw.dm[iw.dm$community=="Tucson",9:27], method="spearman")
# write.csv(iw.dm.cor.t, "spearmancorT.csv")
# 
# iw.dm.cor.az <- cor(dat[dat$community=="AZ Background",9:27], method="spearman")
# write.csv(iw.dm.cor.az, "spearmancorAZ.csv")
# 
# iw.dm.lncor <- cor(log(iw.dm[,9:27]), method="pearson")
# ggpairs(iw.dm[,9:27])
# 
# cor.test(iw.dm$Zn, iw.dm$V, method = "spearman")
#plot(iw.dm$As, iw.dm$Pb)

#Exploratory PCA ----
#only for numeric variables
#unscaled
library(vegan)
iwdm.pca <- rda(as.data.frame(iw.dm[,c(9:27,32,34)]), scale = T)
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
library("FactoMineR")
library("factoextra")
library("EnvStats")

iw.dm.longer <- pivot_longer(data=iw.dm,
                             cols=c(9:27),
                             names_to = "analyte",
                             values_to = "value")

# facto.dat <- aggregate(iw.dm.long$value,
#                        by = list(iw.dm.long$site, iw.dm.long$analyte, iw.dm.long$community),
#                        FUN = mean)
# facto.dat <- pivot_wider(data=facto.dat,
#                          names_from = "Group.2",
#                          values_from = "x")
# facto.dat <- facto.dat[,-1]

#577 "sites"
facto.dat <- iw.dm[,c(4,9:29)]
facto.dat$com <- facto.dat$community
facto.dat <- facto.dat[,-1]

#16 "sites" | community X sampling window
facto.dat <- iw.dm.longer
facto.dat <- facto.dat[!is.na(facto.dat$value),]

facto.dat <- aggregate(facto.dat$value,
                       by = list(facto.dat$community, facto.dat$season, facto.dat$period, facto.dat$analyte),
                       FUN = geoMean)
facto.dat <- pivot_wider(data=facto.dat,
                         names_from = "Group.4",
                         values_from = "x")
# facto.dat$ph <- facto.dat$pH
# facto.dat$ec <- facto.dat$EC
# facto.dat <- facto.dat[,-c(13, 19, 23, 24)] #just to get ph and ec at the end of the columns
rownames(facto.dat) <- paste(facto.dat$Group.1, facto.dat$Group.2, facto.dat$Group.3)
iw.mfa <- MFA(facto.dat, 
              group = c(1, 2, 19), 
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

# fviz_mfa_quali_biplot(iw.mfa, repel = FALSE, 
#                       habillage = c("Group.1","Group.2","Group.3"), addEllipses = TRUE, ellipse.level = 0.95)

#group by community
fviz_mfa_ind(iw.mfa, 
             habillage = "Group.1", # color by groups 
             palette = c("#F9A785", "#00A8C6", "#95CACA","#4068B2"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
)+
  labs(fill = "Community",
       color = "Community",
       title = "MFA Individuals by Community")+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_blank(),
        legend.position = "bottom")
dev.print(jpeg, "ind.comm.jpg", res=1000, width=3543, height = , units="px")

#group by season
fviz_mfa_ind(iw.mfa, 
             habillage = "Group.2", # color by groups 
             palette = c("#A2A9D6", "#F2C893"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) +
  labs(fill = "Season",
       color = "Season",
       title = "MFA Individuals by Season")+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_blank(),
        legend.position = "bottom")
dev.print(jpeg, "ind.ssn.jpg", res=1000, width=90, units="mm")


#group by period
fviz_mfa_ind(iw.mfa, 
             habillage = "Group.3", # color by groups 
             palette = c("#C5D7D2","#F5D1C4"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) +
  labs(fill = "Period",
       color = "Period",
       title = "MFA Individuals by Period")+
  theme_classic()+
  theme(text=element_text(family="Arial", size = 6),
        plot.title = element_blank(),
        legend.position = "bottom")
dev.print(jpeg, "ind.per.jpg", res=1000, width=90, units="mm")




#Old ----
# #set working directory
# setwd("/users/kunalpalawat/Box/Research/Ramírez-Andreotta_IEHRL/ProjectHarvest/InorganicData/WorkingFiles")
# 
# #load data
# dat.2 <- read_xlsx("IW_DA.xlsx", sheet = "Combined", col_names = TRUE)
# 
# #transform values for better graphical aesthetics
# dat.2[dat.2$samplings=="First Winter",]$samplings <- "FW"
# dat.2[dat.2$samplings=="Last Winter",]$samplings <- "LW"
# dat.2[dat.2$samplings=="First Monsoon",]$samplings <- "FM"
# dat.2[dat.2$samplings=="Last Monsoon",]$samplings <- "LM"
# 
# dat.2$period <- dat.2$samplings
# dat.2[dat.2$period=="FW",]$period <- "F"
# dat.2[dat.2$period=="LW",]$period <- "L"
# dat.2[dat.2$period=="FM",]$period <- "F"
# dat.2[dat.2$period=="LM",]$period <- "L"
# 
# dat.2$season <- dat.2$samplings
# dat.2[dat.2$season=="FW",]$season <- "W"
# dat.2[dat.2$season=="LW",]$season <- "W"
# dat.2[dat.2$season=="FM",]$season <- "M"
# dat.2[dat.2$season=="LM",]$season <- "M"
# 
# dat.2[dat.2$community=="D",]$community <- "Dewey"
# dat.2[dat.2$community=="G",]$community <- "Globe"
# dat.2[dat.2$community=="H",]$community <- "Hayden"
# dat.2[dat.2$community=="T",]$community <- "Tucson"
# 
# #confirm correct order of samplings
# dat.2$samplings <- factor(dat.2$samplings, levels = c("FW", "LW", "FM", "LM"))
# dat.2$period <- factor(dat.2$period, levels = c("F", "L"))
# dat.2$season <- factor(dat.2$season, levels = c("W", "M"))
# 
# #remove rows with NA and remove field blanks
# dat.2 <- dat.2[!is.na(dat.2$arsenic.DM),]
# dat.2 <- dat.2[dat.2$type!="B", ]
# 
# #consolidate columns for easier ggplot graphing
# dat.3 <- pivot_longer(data=dat.2, 
#                       cols=beryllium.DM:lead.DM,
#                       names_to="compound",
#                       values_to="real.con")
dat.3$ln.con <- log(dat.3$real.con)

iw.dm.long$ln.con <- log(iw.dm.long$value)

ggplot(data=iw.dm.long,
       mapping=(aes(x=value, fill=analyte)))+
  geom_histogram()+
  facet_wrap(.~analyte, scales = "free")

ggplot(data=iw.dm.long,
       mapping=(aes(x=ln.con, fill=analyte)))+
  geom_histogram()+
  facet_wrap(.~analyte, scales = "free")

# Trying to put the two figures figures togeter for easier comparisons between untransformed and log transformed data

dat.4 <- pivot_longer(data=dat.3,
                      cols=real.con:ln.con,
                      names_to="transformation",
                      values_to="value")

ggplot(data=dat.4,
       mapping=(aes(x=value, fill=compound)))+
  geom_histogram(alpha=.8)+
  ggtitle("Histograms of untransformed and ln transformed Y1 and Y2 dissolved metal(loid) data")+
  facet_wrap(compound~transformation, scales = "free") +
  theme_bw() +
  theme(text = element_text(size = 20,family = "Palatino"),
        panel.grid = element_blank())
dev.print(jpeg, "IW_histogram_Y1Y2.jpeg", res=300, height=14, width=18, units="in")

ggplot(data=dat.4,
       mapping=(aes(x=value, fill=compound)))+
  geom_density(alpha=.8)+
  ggtitle("Density plots of untransformed and ln transformed Y1 and Y2 dissolved metal(loid) data")+
  facet_wrap(compound~transformation, scales = "free", ncol=6) +
  theme_bw() +
  theme(text = element_text(size = 20,family = "Palatino"),
        panel.grid = element_blank())
dev.print(jpeg, "IW_density_Y1Y2.jpeg", res=300, height=18, width=20, units="in")

hist(dat.2$pH)
hist(dat.2$EC)
hist(log(dat.2$EC))

# ggplot(data=dat.4,
#        mapping=(aes(x=value, fill=compound)))+
#   geom_density(alpha=.8)+
#   facet_wrap(.~transformation, scales = "free") +
#   theme_bw() +
#   theme(panel.grid = element_blank())


#remove variables with many non detects and had non-normal distributions, silver just not interested
drops <- c("beryllium.DM", "chromium.DM","selenium.DM", "cadmium.DM", "tin.DM", 
           "silver.DM")
dat.5 <- dat.2[,!(names(dat.2) %in% drops)]
str(dat.5)
totransform <- c("aluminum.DM","vanadium.DM","manganese.DM","iron.DM","cobalt.DM","nickel.DM","copper.DM","zinc.DM","arsenic.DM","molybdenum.DM","antimony.DM","barium.DM","lead.DM","EC")

transformed <- lapply(dat.5[totransform],log)
dat.5ln <- cbind(dat.5[,1:8], transformed, dat.5[,23:25])
str(dat.5ln)

#quick visualizations of correlations
#pairs(dat.5[,9:23])
pairs(dat.5ln[,9:23])


ggcorr(iw.dm[,9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IW_DM_Y123_corr.jpeg", res=150, height=10, width=10, units="in")

ggcorr(iw.dm[iw.dm$community=='Tucson',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IW_DM_Y123_corrTucson.jpeg", res=150, height=10, width=10, units="in")

ggcorr(iw.dm[iw.dm$community=='Hayden/Winkelman',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IW_DM_Y123_corrHayden.jpeg", res=150, height=10, width=10, units="in")
ggcorr(iw.dm[iw.dm$community=='Globe/Miami',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IW_DM_Y123_corrGlobe.jpeg", res=150, height=10, width=10, units="in")

ggcorr(iw.dm[iw.dm$community=='Dewey-Humboldt',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IW_DM_Y123_corrDewey.jpeg", res=150, height=10, width=10, units="in")

ggpairs(data = select(iw.dm, select = -c(Sb, Se, Ag, Sn, Be)),
        columns = 9:22,
        mapping = aes(color = community),
        upper = list(continuous = wrap("cor", method= "spearman"))) 

ggcorr(is.dry[,9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IS_dry_Y123_corr.jpeg", res=150, height=10, width=10, units="in")

ggcorr(is.dry[is.dry$community=='Tucson',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IS_dry_Y123_corrTucson.jpeg", res=150, height=10, width=10, units="in")

ggcorr(is.dry[is.dry$community=='Hayden/Winkelman',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IS_dry_Y123_corrHayden.jpeg", res=150, height=10, width=10, units="in")
ggcorr(is.dry[is.dry$community=='Globe/Miami',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IS_dry_Y123_corrGlobe.jpeg", res=150, height=10, width=10, units="in")

ggcorr(is.dry[is.dry$community=='Dewey-Humboldt',9:27], method = c("complete.obs", "spearman"),
       label= TRUE,
       label_round = 2,
       label_size = 3,
       label_alpha = TRUE,
       legend.size = 10,
       size=5,
       hjust = .5,
       geom = "tile",
       angle=0)
dev.print(jpeg, "IS_dry_Y123_corrDewey.jpeg", res=150, height=10, width=10, units="in")


iwis <- full_join(iw.dm, is.dry, by = c("site", "sampling_year", "community"))
plot(iwis$Pb.x, iwis$Pb.y)

iwisPb <- ggscatter(iwis, x = "Pb.x", y = "Pb.y", 
                    add = "reg.line",
                    conf.int = TRUE,
                    cor.coef = FALSE,
                    cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
                    xlab = "Rainwater Pb Concentration (ug/L)",
                    ylab = "Soil Pb Concentration (mg/kg)",
                    cor.coef.size = 6, point = FALSE) +
  stat_cor(method="spearman",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "middle",
           family = "Avenir",
           size = 6) +
  geom_point(size = 2, fill = "black", color = "black", shape = 21) +
  labs(title = "Correlation Between [Pb] in Rooftop Harvested Rainwater\nand [Pb] in Residential Soil from the same site and year",
       # subtitle = paste("n =",nrow(iw.dist)),
       caption = "A spearman correlation test was conducted") +
  theme_bw() +
  theme(text = element_text(size=10, family = "Avenir"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        #panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
        plot.caption = element_text(size = 13, face="italic", hjust = 0),
        axis.text = element_text(size=15, color = "black", face = "bold"),
        axis.title = element_text(size=15, color = "black", face = "bold"),
        axis.ticks = element_blank())
iwisPb
dev.print(jpeg, "iwis_Pb.jpeg", res=150, height=8, width=8, units="in")

iwisAs <- ggscatter(iwis, x = "As.x", y = "As.y", 
                    add = "reg.line",
                    conf.int = TRUE,
                    cor.coef = FALSE,
                    cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
                    xlab = "Rainwater As Concentration (ug/L)",
                    ylab = "Soil As Concentration (mg/kg)",
                    cor.coef.size = 6, point = FALSE) +
  stat_cor(method="spearman",
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "middle",
           family = "Avenir",
           size = 6) +
  geom_point(size = 2, fill = "black", color = "black", shape = 21) +
  labs(title = "Correlation Between [As] in Rooftop Harvested Rainwater\nand [As] in Residential Soil from the same site and year",
       # subtitle = paste("n =",nrow(iw.dist)),
       caption = "A spearman correlation test was conducted") +
  theme_bw() +
  theme(text = element_text(size=10, family = "Avenir"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        #panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
        plot.caption = element_text(size = 13, face="italic", hjust = 0),
        axis.text = element_text(size=15, color = "black", face = "bold"),
        axis.title = element_text(size=15, color = "black", face = "bold"),
        axis.ticks = element_blank())
iwisAs
dev.print(jpeg, "iwis_As.jpeg", res=200, height=8, width=8, units="in")

iwisPb <- ggscatter(iwis, x = "Pb.x", y = "Pb.y", color = "community",
                    add = "reg.line",
                    conf.int = TRUE,
                    cor.coef = FALSE,
                    cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
                    xlab = "Rainwater Pb Concentration (ug/L)",
                    ylab = "Soil Pb Concentration (mg/kg)",
                    cor.coef.size = 6, point = FALSE) +
  stat_cor(method="spearman",
           mapping = aes(color = community, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "middle",
           family = "Avenir",
           size = 6) +
  geom_point(size = 2, fill = "black", color = "black", shape = 21) +
  labs(title = "Correlation Between [Pb] in Rooftop Harvested Rainwater\nand [Pb] in Residential Soil from the same site and year",
       # subtitle = paste("n =",nrow(iw.dist)),
       caption = "A spearman correlation test was conducted") +
  theme_bw() +
  theme(text = element_text(size=10, family = "Avenir"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        #panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
        plot.caption = element_text(size = 13, face="italic", hjust = 0),
        axis.text = element_text(size=15, color = "black", face = "bold"),
        axis.title = element_text(size=15, color = "black", face = "bold"),
        axis.ticks = element_blank())
iwisPb
dev.print(jpeg, "iwis_comm_Pb.jpeg", res=150, height=8, width=8, units="in")


iwisAs <- ggscatter(iwis, x = "As.x", y = "As.y", color = "community",
                    add = "reg.line",
                    conf.int = TRUE,
                    cor.coef = FALSE,
                    cor.coeff.args = list(method = "spearman", label.x.npc = "middle"),
                    xlab = "Rainwater As Concentration (ug/L)",
                    ylab = "Soil As Concentration (mg/kg)",
                    cor.coef.size = 6, point = FALSE) +
  stat_cor(method="spearman",
           mapping = aes(color = community, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "middle",
           family = "Avenir",
           size = 6) +
  geom_point(size = 2, fill = "black", color = "black", shape = 21) +
  labs(title = "Correlation Between [As] in Rooftop Harvested Rainwater\nand [As] in Residential Soil from the same site and year",
       # subtitle = paste("n =",nrow(iw.dist)),
       caption = "A spearman correlation test was conducted") +
  theme_bw() +
  theme(text = element_text(size=10, family = "Avenir"),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        #panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "black", face = "italic"),
        plot.caption = element_text(size = 13, face="italic", hjust = 0),
        axis.text = element_text(size=15, color = "black", face = "bold"),
        axis.title = element_text(size=15, color = "black", face = "bold"),
        axis.ticks = element_blank())
iwisAs
dev.print(jpeg, "iwis_comm_As.jpeg", res=150, height=8, width=8, units="in")


dat.6 <- pivot_longer(data=dat.5ln, 
                      cols=aluminum.DM:pH,
                      names_to="measurement",
                      values_to="value")
dat.6$compound <- dat.6$measurement
dat.6$concentration <- dat.6$value

ggscatter(dat.6, x="concentration", y="value",
          add = "reg.line",
          conf.int = TRUE,
          cor.coef = FALSE,
          cor.method = "pearson",
          cor.coef.size = 6, size = 3) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = 3) +
  facet_wrap(compound~measurement, scales = "free") +
  geom_point(mapping = aes(color = community), size = 2) +
  ggtitle("Correlation Plots of Total Coliforms by Metal") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_text(size = 15),
        axis.title = element_text(size=18),
        axis.text = element_text(size=10))

ggpairs(dat.5ln[,9:12])











