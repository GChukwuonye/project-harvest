#Kunal Palawat Gift Chukwuonye
#Date:
#Description: Space PCA analysis to narrow down which analytes to focus on for modeling
#
#NOTES  
  # looking at detection summaries, all analytes are generally above 30% detection except for Be and Se in Y3, but in Y1 and Y2, they are above 30%, so these analytes will be included in analysis
  # need to run this analysis for each community separately


#load libraries
library(elasticnet)
library(scPCA)
library(sparsepca)

#load data----
#remove samples with NAs for pH (54 samples)
dat <- iw.ln.dm %>%
  drop_na(pH)

#split dataframe into different ones for each community
datc <- dat %>%
  group_by(community) %>%
  group_split()

datdh <- datc[[1]]
datgm <- datc[[2]]
dathw <- datc[[3]]
dattu <- datc[[4]]

datdh <- datdh[,c("pH","Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")]
datgm <- datgm[,c("pH","Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")]
dathw <- dathw[,c("pH","Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")]
dattu <- dattu[,c("pH","Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")]

#sparce PCA with elasticnet ----
#k = 6 means only the first 6 principal componants are shown
#adding the penalty helps to zero out some of the variables and prevent over fitting (this essentially does variable selection for us!)
penalties <- exp(seq(log(10), log(1000), length.out = 6))

##dh----
spca_sim_p <- elasticnet::spca(x=datdh, K = 6, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
#write.csv(spca_sim_p$loadings, "dh_elasticloadings.csv")

##gm ----
spca_sim_p <- elasticnet::spca(x=datgm, K = 6, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
write.csv(spca_sim_p$loadings, "gm_elasticloadings.csv")

##hw ----
spca_sim_p <- elasticnet::spca(x=dathw, K = 6, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
write.csv(spca_sim_p$loadings, "hw_elasticloadings.csv")

##tu ----
spca_sim_p <- elasticnet::spca(x=dattu, K = 6, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
write.csv(spca_sim_p$loadings, "tu_elasticloadings.csv")

#correlation ----
# Plot the correlation matrix of the generated data, natural log transformed
cor_matrix <- cor(dat, use = "complete.obs")
# Convert the correlation matrix to a long format
cor_matrix_long <- reshape2::melt(cor_matrix)
# Create the heatmap plot using ggplot2
heatmap_plot <- ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#02d7f2", high = "#E93CAC", name = "Correlation") +
  labs(title = "Correlation matrix of 515 RHRW samples",
       subtitle = "spearment correlations conducted on natural log transformed data")
# Print the heatmap plot
print(heatmap_plot)
dev.print(png, "corrheatmap.png", res=300, height=8, width=10, units="in")



#correlation ----
# Plot the correlation matrix of the generated data, natural log transformed
cor_matrix <- cor(dat, use = "complete.obs")
# Convert the correlation matrix to a long format
cor_matrix_long <- reshape2::melt(cor_matrix)
# Create the heatmap plot using ggplot2
heatmap_plot <- ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#02d7f2", high = "#E93CAC", name = "Correlation") +
  labs(title = "Correlation matrix of 515 RHRW samples",
       subtitle = "spearment correlations conducted on natural log transformed data")
# Print the heatmap plot
print(heatmap_plot)
dev.print(png, "corrheatmap.png", res=300, height=8, width=10, units="in")





#sparse contrastive PCA with scPCA ----
scpca_sim <- scPCA(target = dat,
                   background = background_df,
                   n_eigen = 5,
                   n_centers = 4,
                   penalties = exp(seq(log(0.001), log(10), length.out = 10)),
                   alg = "var_proj")
print(scpca_sim)