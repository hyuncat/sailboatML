# Programming simulations here...
library(tidyverse)
library(lightgbm)
library(shapr)
library(readxl)
library(rvest)

data <- read_excel('./part1/2023_MCM_Problem_Y_Boats.xlsx')
colnames(data) <- c('make', 'variant', 'length', 'big_region',
                    'small_region', 'price', 'year')

# Data Cleaning
# (1) Outlier handling
remove_outliers <- function(data) {
  n <- nrow(data)
  # Get z_scores for price
  z_data <- as.data.frame(sapply(data$price, function(x) {
    abs(x-mean(data$price)) / sd(data$price)
  }))
  # Set colnames for z_data
  colnames(z_data) <- c('z_score')
  # Return TRUE if z_score > 3
  z_data <- z_data %>%
    select(z_score) %>%
    mutate(outlier = ifelse(z_score>3, TRUE, FALSE))
  # Filter out outlier rows
  data <- data[!(z_data$outlier == TRUE), ]
  print(paste("Removed", n-nrow(data), "extreme values from data"))
  return(data)
}

cleaned_data <- remove_outliers(data)


