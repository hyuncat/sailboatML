# Chi-square tests to rank the importance of sailboat variables on price
library(tidyverse)

# Chi-square test for monohulled sailboats data

mono_data <- read_csv('./part2/data/mono_data.csv')
summary(mono_data$length)

# Create a categorical variable based on length
mono_data$category <- ifelse(mono_data$length < median(mono_data$length), "low", "high")

# Calculate observed and expected frequencies
mono_observed <- table(mono_data$category)
mono_expected <- rep(mean(mono_data$price), length(observed))

# Perform chi-square test
mono_chi_square_result <- chisq.test(mono_observed, p = mono_expected / sum(mono_expected))

# Print chi-square test results
print(mono_chi_square_result)

# Chi-square test for catamaran sailboats data

cat_data <- read_csv('./part2/data/cat_data.csv')
summary(cat_data$length)

# Create a categorical variable based on length
cat_data$category <- ifelse(cat_data$length < median(cat_data$length), "low", "high")

# Calculate observed and expected frequencies
cat_observed <- table(cat_data$category)
cat_expected <- rep(mean(cat_data$price), length(observed))

# Perform chi-square test
cat_chi_square_result <- chisq.test(observed, p = cat_expected / sum(expected))

# Print chi-square test results
print(cat_chi_square_result)
