# Functions to perform PCSL regression analysis

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(reshape2)

# Read in data
gdp_cat_df <- read.csv('./part1/data/clean_cat.csv')
gdp_mono_df <- read.csv('./part1/data/clean_mono.csv')

# Keep only numeric cols
num_cat <- gdp_cat_df[, c('length', 'year', 'gdp')]
num_mono <- gdp_mono_df[, c('length', 'year', 'gdp')]

# Calculate correlation matrix
cat_cormat <- cor(num_cat, method='pearson')
mono_cormat <- cor(num_mono, method='pearson')

# Correlation heatmap
meltcat <- melt(cat_cormat)
meltmono <- melt(mono_cormat)

hmap_cat <- ggplot(meltcat, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text annotations
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Catamarans Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation")

hmap_mono <- ggplot(meltmono, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text annotations
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Monohull Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation")

# Extract principal components
cat_pca <- princomp(cat_cormat, cor=TRUE, scores=TRUE)
mono_pca <- princomp(mono_cormat, cor=TRUE, scores=TRUE)

summary(cat_pca)
summary(mono_pca)

# Catamaran visualizations
fviz_cos2(cat_pca, choice = "var", axes = 1:2) # Cos2
fviz_eig(cat_pca) # Scree

# Monohull visualizations
fviz_cos2(mono_pca, choice = "var", axes = 1:2) # Cos2
fviz_eig(mono_pca) # Scree
