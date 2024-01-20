# Chi-square tests to rank the importance of sailboat variables on price
library(tidyverse)

# Chi-square test for lengths of monohull sailboats data
mono_data <- read_csv('./part2/data/mono_data.csv')
summary(mono_data$length)

# Create a categorical variable based on length
mono_data$category <- ifelse(mono_data$length < median(mono_data$length), "low", "high")

mono_observed <- table(mono_data$category)
mono_expected <- rep(mean(mono_data$price), length(observed))

mono_chi_square_result <- chisq.test(mono_observed, p = mono_expected / sum(mono_expected))

print(mono_chi_square_result)


# Chi-square test for lengths of catamaran sailboats data
cat_data <- read_csv('./part2/data/cat_data.csv')
summary(cat_data$length)

# Create a categorical variable based on length
cat_data$category <- ifelse(cat_data$length < median(cat_data$length), "low", "high")

cat_observed <- table(cat_data$category)
cat_expected <- rep(mean(cat_data$price), length(observed))

cat_chi_square_result <- chisq.test(observed, p = cat_expected / sum(expected))

print(cat_chi_square_result)


# Chi-square test for the years when monohull sailboats were built
summary(mono_data$year)

# Create a categorical variable based on year
mono_data$category <- ifelse(mono_data$year < median(mono_data$year), "low", "high")

mono_observed <- table(mono_data$category)
mono_expected <- rep(mean(mono_data$year), length(observed))

mono_chi_square_result <- chisq.test(mono_observed, p = mono_expected / sum(mono_expected))

print(mono_chi_square_result)

# Chi-square test for the years when catamaran sailboats were built
summary(cat_data$year)

# Create a categorical variable based on year
cat_data$category <- ifelse(cat_data$year < median(cat_data$year), "low", "high")

cat_observed <- table(cat_data$category)
cat_expected <- rep(mean(cat_data$year), length(observed))

cat_chi_square_result <- chisq.test(cat_observed, p = cat_expected / sum(cat_expected))

print(cat_chi_square_result)


# Chi-square test for the human development index (HDI) of the regions of monohull sailboats
summary(mono_data$hdi)

# Create a categorical variable based on HDI
mono_data$category <- ifelse(mono_data$hdi < median(mono_data$hdi), "low", "high")

mono_observed <- table(mono_data$category)
mono_expected <- rep(mean(mono_data$hdi), length(observed))

mono_chi_square_result <- chisq.test(mono_observed, p = mono_expected / sum(mono_expected))

print(mono_chi_square_result)

# Chi-square test for the human development index (HDI) of the regions of catamaran sailboats
summary(cat_data$hdi)

# Create a categorical variable based on HDI
cat_data$category <- ifelse(cat_data$hdi < median(cat_data$hdi), "low", "high")

cat_observed <- table(cat_data$category)
cat_expected <- rep(mean(cat_data$hdi), length(observed))

cat_chi_square_result <- chisq.test(cat_observed, p = cat_expected / sum(cat_expected))

print(cat_chi_square_result)


# Chi-square test for the gross domestic product (GDP) of the regions of monohull sailboats
summary(mono_data$gdp)

# Create a categorical variable based on GDP
mono_data$category <- ifelse(mono_data$gdp < median(mono_data$gdp), "low", "high")

mono_observed <- table(mono_data$category)
mono_expected <- rep(mean(mono_data$gdp), length(observed))

mono_chi_square_result <- chisq.test(mono_observed, p = mono_expected / sum(mono_expected))

print(mono_chi_square_result)

# Chi-square test for the gross domestic product (GDP) of the regions of catamaran sailboats
summary(cat_data$gdp)

# Create a categorical variable based on GDP
cat_data$category <- ifelse(cat_data$gdp < median(cat_data$gdp), "low", "high")

cat_observed <- table(cat_data$category)
cat_expected <- rep(mean(cat_data$gdp), length(observed))

cat_chi_square_result <- chisq.test(cat_observed, p = cat_expected / sum(cat_expected))

print(cat_chi_square_result)

