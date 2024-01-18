# Functions for exploratory data analysis

library(tidyverse)
library(ggplot2)
library(scales)

mono_data <- read.csv('./part2/data/mono_data.csv')
cat_data <- read.csv('./part2/data/cat_data.csv')

# Histograms of price by region
ggplot(mono_data, aes(x = big_region, y = price)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  labs(title = "Histogram of Monohull Sailboat Price by Region",
       x = "Region",
       y = "Price") +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggplot(cat_data, aes(x = big_region, y = price)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  labs(title = "Histogram of Catamaran Price by Region",
       x = "Region",
       y = "Price") +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

# Get the most frequently appearing boat in each dataset
top_mono_variant <- names(head(sort(table(mono_data$variant), decreasing=TRUE), 1))
top_cat_variant <- names(head(sort(table(cat_data$variant), decreasing=TRUE), 1))

top_mono_df <- mono_data[mono_data$variant %in% top_mono_variant, ]
top_cat_df <- cat_data[cat_data$variant %in% top_cat_variant, ]

# Histograms of price of the same boat by region
ggplot(top_mono_df, aes(x = small_region, y = price)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  labs(title = "Bavaria Cruiser 46 Price by Region",
       x = "Region",
       y = "Price") +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

ggplot(top_cat_df, aes(x = small_region, y = price)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  labs(title = "Lagoon 450 Price by Region",
       x = "Region",
       y = "Price") +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()

summary(mono_data)
