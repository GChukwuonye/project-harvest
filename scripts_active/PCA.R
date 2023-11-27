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

# Load necessary libraries
library(sparsepca)
library(MASS)  # For generating multivariate normal data
library(caret)  # For cross-validation
library(ggplot2)
library(glmnet)
library(elasticnet)  # Load the elasticnet package for sparse PCA
library(matrixcalc)  # Load necessary library for nearestPD function
library(Matrix)
library(scPCA)
library(dplyr)

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

datpca <- dat[,c("pH","Al", "Sb", "As", "Ba", "Be", "Cd", "Cr", "Co", "Cu", "Fe", "Pb", "Mn", "Mo", "Ni", "Se", "Ag", "Sn", "V", "Zn")]


#PCA
pca_sim <- prcomp(datpca)
pca_sim <- prcomp(datdh)
pca_sim <- prcomp(datgm)
pca_sim <- prcomp(dathw)
pca_sim <- prcomp(dattu)

# plot the 2D rep using first 2 components
df <- data.frame(list("PC1" = pca_sim$x[, 1],
                      "PC2" = pca_sim$x[, 2]))
p_pca <- ggplot(df, aes(x = PC1, y = PC2)) +
  ggtitle("PCA on All Communities Data") +
  geom_point(alpha = 0.5) +
  theme_minimal()
p_pca

# Extract eigenvalues
eigenvalues <- (pca_sim$sdev)^2

# Calculate the percentage of variance explained
variance_explained <- eigenvalues / sum(eigenvalues) * 100

# Create a data frame for the scree plot
scree_data <- data.frame(PC = 1:length(variance_explained), VarianceExplained = variance_explained)

# Create the scree plot as a scatterplot with a connecting line
scree_plot <- ggplot(scree_data, aes(x = PC, y = VarianceExplained)) +
  geom_point(color = "steelblue", size = 3) +
  geom_line(color = "red") +
  labs(title = "Scree Plot for PCA",
       x = "Principal Component",
       y = "Percentage of Variance Explained") +
  theme_minimal()

# Display the scree plot
print(scree_plot)

# 
# We may use the skree plot above to choose the number of principal components. We look for the "elbow" in the plot where diminishing returns from including an additional principal component begin to appear.
# All - 3 PCs
# DH - 4 PCs
# GM - 2 Pcs
# HW - 4 PCs
# TU - 4 PCs
# 
# Elbow happens 2 - 4 PCs, tune sparce PCA with 4 components.
# 
# The parameter we are tuning, alpha, controls an L1 penalty term. We will tune the sparse PCA using the Index of Sparseness or IS. The index of sparseness is defined as
# $$IS = \mathrm{PEV}_{pca}\times \mathrm{PEV}_{sparse}\times \mathrm{PS}$$
#   Where $\mathrm{PEV}_{pca}, \mathrm{PEV}_{sparse}$, and $\mathrm{PS}$ are the Percentage of Explained Variance (PEV) from the selected principal components of the standard PCA, the PEV from the selected principal components of the sparse PCA, and the proportion of sparsity (number of entries equal to zero in the component loadings) respectively.
# 
# Tuning penalty term $\alpha$ using index of sparseness:
pca_result <- prcomp(datpca)

# Calculate PEV_pca
PEV_pca <- sum(pca_result$sdev[1:3]^2) / sum(pca_result$sdev^2)

# Set up a grid of alpha values to search
alpha_grid <- seq(0.05, 1, by = 0.05)

# Create an empty list to store the results
results <- list()

# Perform cross-validation to tune alpha
#k=4 because we chose to use 4 prinicipal components
for (alpha in alpha_grid) {
  spca_model <- elasticnet::spca(datpca, K = 4, para = rep(alpha, 4), type = "predictor", sparse = "penalty")
  results[[as.character(alpha)]] <- spca_model
}
# Calculate PEV_sparse for each alpha
for (alpha in alpha_grid) {
  spca_model <- results[[as.character(alpha)]]
  PEV_sparse <- sum(spca_model$pev)
  results[[as.character(alpha)]]$PEV_sparse <- PEV_sparse
}
# Calculate the proportion of sparsity for each alpha
for (alpha in alpha_grid) {
  spca_model <- results[[as.character(alpha)]]
  proportion_of_sparsity <- sum(spca_model$loadings == 0) / length(spca_model$loadings)
  results[[as.character(alpha)]]$proportion_of_sparsity <- proportion_of_sparsity
  # Calculate IS
  results[[as.character(alpha)]]$IS <- PEV_pca * PEV_sparse * proportion_of_sparsity
}
# Find the alpha with the maximum IS
optimal_alpha <- alpha_grid[which.max(sapply(results, function(result) result$IS))]

# Get the sparse PCA model with the optimal alpha
optimal_spca_model <- results[[as.character(optimal_alpha)]]
# Extract IS values and alpha values
IS_values <- sapply(results, function(result) result$IS)
alpha_values <- as.numeric(names(results))

# Plot IS versus alpha
plot(alpha_values, IS_values, type = "l", xlab = "Alpha (Sparsity Parameter)", ylab = "Index of Sparseness (IS)", 
     main = "IS vs. Alpha")

# Add a point for the optimal alpha
points(optimal_alpha, results[[as.character(optimal_alpha)]][["IS"]], col = "red", pch = 19)
legend("topleft", legend = c("Optimal Alpha"), col = c("red"), pch = c(19))


# From the plot above, we can see that alpha values from 0.60 to roughly 1.0 maximize the Index of Sparseness value. Opting for the lowest optimal penalty value, the loadings for the Optimal Sparse PCA with $\alpha =0.6$ are shown below:
#   
optimal_spca_model$loadings


#sparce PCA with elasticnet ----
#k = 4 means only the first 6 principal componants are shown
#adding the penalty helps to zero out some of the variables and prevent over fitting (this essentially does variable selection for us!)
penalties <- exp(seq(log(10), log(1000), length.out = 4))

##dh----
spca_sim_p <- elasticnet::spca(x=datdh, K = 4, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
#write.csv(spca_sim_p$loadings, "dh_elasticloadings.csv")

##gm ----
spca_sim_p <- elasticnet::spca(x=datgm, K = 4, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
write.csv(spca_sim_p$loadings, "gm_elasticloadings.csv")

##hw ----
spca_sim_p <- elasticnet::spca(x=dathw, K = 4, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
write.csv(spca_sim_p$loadings, "hw_elasticloadings.csv")

##tu ----
spca_sim_p <- elasticnet::spca(x=dattu, K = 4, para = rep(penalties, 20),
                               type = "predictor", sparse = "penalty")
print(spca_sim_p$pev)
print(spca_sim_p$loadings)
write.csv(spca_sim_p$loadings, "tu_elasticloadings.csv")

#correlation ----
# Function to generate a positive definite covariance matrix
generate_positive_definite_cov_matrix <- function(n_features, sparsity) {
  # Generate a random sparse covariance matrix
  cov_matrix <- matrix(0, n_features, n_features)
  non_zero_entries <- sample(1:(n_features^2), sparsity)
  cov_matrix[non_zero_entries] <- runif(sparsity, 0.1, 0.5)
  cov_matrix <- Matrix::nearPD(cov_matrix)$mat  # Ensure positive definiteness
  return(cov_matrix)
}

# Set random seed for reproducibility
set.seed(123)

# Number of samples and features
n_samples_pca <- 515
n_features <- 20
sparsity <- 400  # Number of non-zero entries in the covariance matrix


# Generate a positive definite covariance matrix with sparsity
true_cov_matrix <- generate_positive_definite_cov_matrix(n_features, sparsity)

# Generate correlated data using the covariance matrix
background_df <- mvrnorm(n_samples, mu = rep(0, n_features), Sigma = true_cov_matrix)

background_df <- data.frame(background_df)

cpca_sim <- scPCA(target = datpca[, 1:20],
                  background = background_df,
                  penalties = .6,
                  n_centers = 4,
                  n_eigen = 4,
                  scale = TRUE)
cPC1 <- cpca_sim$x[,1]
cPC2 <- cpca_sim$x[,2]

cpca_df <- data.frame(cPC1, cPC2)
# plot the results
p_cpca <- ggplot(cpca_df, aes(x = cPC1, y = cPC2)) +
  geom_point(alpha = 0.5) +
  ggtitle("cPCA of Data") +
  theme_minimal()
p_cpca

print(cpca_sim)
#4, 6, 7, 9 , 17: As, Be, Cd, Co, Ag


#correlations
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