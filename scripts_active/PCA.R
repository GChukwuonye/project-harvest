#Kunal Palawat Gift Chukwuonye
#Date:
#Description: Space PCA analysis to narrow down which analytes to focus on for modeling
#
#NOTES  
  # looking at detection summaries, all analytes are generally above 30% detection except for Be and Se in Y3, but in Y1 and Y2, they are above 30%, so these analytes will be included in analysis


#load libraries
library(elasticnet)
library(scPCA)
library(sparsepca)

#correlation ----
# Plot the correlation matrix of the generated data, natural log transformed
data <- log(iw.dm[,9:27])
cor_matrix <- cor(data)
# Convert the correlation matrix to a long format
cor_matrix_long <- reshape2::melt(cor_matrix)
# Create the heatmap plot using ggplot2
heatmap_plot <- ggplot(cor_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#02d7f2", high = "#E93CAC", name = "Correlation") +
  labs(title = "Correlation Matrix of Generated Data")
# Print the heatmap plot
print(heatmap_plot)

#sparce PCA with elasticnet ----
penalties <- exp(seq(log(10), log(1000), length.out = 6))
spca_sim_p <- elasticnet::spca(data, K = 6, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
