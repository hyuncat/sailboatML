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
hk_num_cat <- hk_mono_df[, c('length', 'year')]
hk_num_mono <- hk_cat_df[, c('length', 'year')]

# Calculate correlation matrix
hk_cat_cormat <- cor(hk_num_cat, method='pearson')
hk_mono_cormat <- cor(hk_num_mono, method='pearson')

# Correlation heatmap
meltcat <- melt(hk_cat_cormat)
meltmono <- melt(hk_mono_cormat)

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

# pca test

hk_cat_pca <- prcomp(hk_cat_cormat)
hk_mono_pca <- prcomp(hk_mono_cormat)

# Extract principal components
cat_pcs <- hk_cat_pca$rotation[, 1:2]
mono_pcs <- hk_mono_pca$rotation[, 1:2]

# Continue with the rest of your analysis

summary(hk_cat_pca)
summary(hk_mono_pca)

# Catamaran PCA visualizations
fviz_cos2(hk_cat_pca, choice = "var", axes = 1:2) # Cos2
fviz_eig(hk_cat_pca) # Scree
fviz_pca_biplot(hk_cat_pca) # Biplot

# Monohull PCA visualizations
fviz_cos2(hk_mono_pca, choice = "var", axes = 1:2) # Cos2
fviz_eig(hk_mono_pca) # Scree
fviz_pca_biplot(hk_mono_pca) # Biplot

# Select only top 2 components
cat_pcs <- predict(hk_cat_pca, newdata = hk_num_cat)[, 1:2]
cat_pcs_data <- as.data.frame(cbind(price = hk_cat_df$price, cat_pcs))
mono_pcs <- predict(hk_mono_pca, newdata = hk_num_mono)[, 1:2]
mono_pcs_data <- as.data.frame(cbind(price = hk_mono_df$price, mono_pcs))

# PCA regression models
cat_pcs_model <- lm(price ~ PC1 + PC2, data = cat_pcs_data)
mono_pcs_model <- lm(price ~ PC1 + PC2, data = mono_pcs_data)
# Print summary statistics
summary(cat_pcs_model)
summary(mono_pcs_model)

# Non-PCA regression models (these yielded higher R^2 values)
hk_num_cat2 <- hk_cat_df[, c('price', 'length', 'year')]
hk_num_mono2 <- hk_mono_df[, c('price', 'length', 'year')]

hk_cat_raw_model <- lm(price ~ length+year, data=hk_num_cat2)
hk_mono_raw_model <- lm(price ~ length+year, data=hk_num_mono2)

summary(hk_cat_raw_model)
summary(hk_mono_raw_model)


# Create the scatter plot with the regression line
p1 <- ggplot(hk_num_cat2, aes(x = length+year, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(title = "Hong Kong Catamaran Non-PCA Regression Model", x = "Length (feet) + Year", y = "Price (US$)")
p1 + theme_classic()

p2 <- ggplot(hk_num_mono2, aes(x = length+year, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(title = "Hong Kong Monohull Non-PCA Regression Model", x = "Length (feet) + Year", y = "Price (US$)")
p2 + theme_classic()

residuals1 <- residuals(hk_cat_raw_model)
plot(fitted(hk_cat_raw_model), residuals1,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot for HK Catamaran Sailboats",
     pch = 16, col = "steelblue")
abline(h = 0, col = "red", lty = 2)

residuals2 <- residuals(hk_mono_raw_model)
plot(fitted(hk_mono_raw_model), residuals2,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residual Plot for HK Monohull Sailboats",
     pch = 16, col = "steelblue")
abline(h = 0, col = "red", lty = 2)

