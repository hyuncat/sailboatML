# Functions to perform PCSL regression analysis

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(reshape2)

# Read in Hong Kong data
hk_data <- read_csv('./part2/hk_data_after.csv')

# Filter monohull data
hk_mono_df <- hk_data[hk_data$mono == TRUE, ]

# Filter catamaran data
hk_cat_df <- hk_data[hk_data$cat == TRUE, ]

# Keep only numeric cols
num_cat <- hk_mono_df[, c('length', 'year', 'gdp', 'hdi')]
num_mono <- hk_cat_df[, c('length', 'year', 'gdp', 'hdi')]

# Calculate correlation matrix
cat_cormat <- cor(num_cat, method='pearson')
mono_cormat <- cor(num_mono, method='pearson')

# Correlation heatmap
meltcat <- melt(cat_cormat)
meltmono <- melt(mono_cormat)

hmap_cat <- ggplot(meltcat, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text annotations
  scale_fill_gradient2(low = "#009E73", mid = "#D55E00", high = "#CC79A7", midpoint = 0) +
  theme_minimal() +
  labs(title = "Catamarans Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation")

hmap_mono <- ggplot(meltmono, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text annotations
  scale_fill_gradient2(low = "#009E73", mid = "#D55E00", high = "#CC79A7", midpoint = 0) +
  theme_minimal() +
  labs(title = "Monohull Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation")

# Extract principal components
cat_pca <- princomp(cat_cormat, cor=TRUE, scores=TRUE)
mono_pca <- princomp(mono_cormat, cor=TRUE, scores=TRUE)

summary(cat_pca)
summary(mono_pca)

# Catamaran PCA visualizations
fviz_cos2(cat_pca, choice = "var", axes = 1:2) # Cos2
fviz_eig(cat_pca) # Scree
fviz_pca_biplot(cat_pca) # Biplot

# Monohull PCA visualizations
fviz_cos2(mono_pca, choice = "var", axes = 1:2) # Cos2
fviz_eig(mono_pca) # Scree
fviz_pca_biplot(mono_pca) # Biplot

# Select only top 2 components
cat_pcs <- predict(cat_pca, newdata = num_cat)[, 1:2]
cat_pcs_data <- as.data.frame(cbind(price=gdp_cat_df$price, cat_pcs))
mono_pcs <- predict(mono_pca, newdata = num_mono)[, 1:2]
mono_pcs_data <- as.data.frame(cbind(price=gdp_mono_df$price, mono_pcs))

# PCA regression models
cat_pcs_model <- lm(price ~ Comp.1 + Comp.2, data = cat_pcs_data)
mono_pcs_model <- lm(price ~ Comp.1 + Comp.2, data = mono_pcs_data)
# Print summary statistics
summary(cat_pcs_model)
summary(mono_pcs_model)

# Non-PCA regression models (these yielded higher R^2 values)
num_cat2 <- gdp_cat_df[, c('price', 'length', 'year', 'gdp', 'hdi')]
num_mono2 <- gdp_mono_df[, c('price', 'length', 'year', 'gdp', 'hdi')]

cat_raw_model <- lm(price ~ length+year+gdp+hdi, data=num_cat2)
mono_raw_model <- lm(price ~ length+year+gdp+hdi, data=num_mono2)

summary(cat_raw_model)
summary(mono_raw_model)
