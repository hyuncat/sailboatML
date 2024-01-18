# Function to add disposable income data for part 2

library(tidyverse)
library(dplyr)

# Add disposable income data to country_df
add_income_to_country_df <- function(country_df) {
  country_df <- read_csv('./part2/data/country_df.csv')
  income_data <- read_csv('./part2/data/income_per_capita.csv')
  income_data <- income_data %>% rename(country = `Country Name`)
  income_vec <- c()

  for (i in 1:nrow(country_df)) {
    cn <- country_df$country[i]
    if (cn %in% income_data$country) {
      income <- income_data[income_data$country==cn, '2022']
    } else {
      income <- NA
    }
    income_vec <- c(income_vec, income)
  }
  country_df$income <- income_vec
}

# Add income col to dataset
add_income_col_new <- function(df) {
  income_vec <- c()
  for (row in 1:nrow(df)) {
    if (df$big_region[row] == 'USA') {
      country <- df$big_region[row]
    } else {
      country <- df$small_region[row]
    }
    income <- country_df[country, 'income']
    income_vec <- c(income_vec, income)
  }
  df$income <- income_vec
  return(df)
}

# Don't run if sourcing functions into other file
if (sys.nframe() == 0) {
  country_df <- read_csv('./part2/data/country_df.csv')
  country_df <- add_income_to_country_df('country_df')
  mono_gdp_df <- read_csv('./part2/data/mono_data.csv')
  cat_gdp_df <- read_csv('./part2/data/cat_data.csv')
  mono_gdp_df <- add_income_col_new(mono_gdp_df)
  cat_gdp_df <- add_income_col_new(cat_gdp_df)

  mono_gdp_df <- apply(mono_gdp_df, 2, as.character)
  cat_gdp_df <- apply(cat_gdp_df, 2, as.character)
  country_df <- apply(country_df, 2, as.character)

  # Save as files
  write.csv(mono_gdp_df, './part2/data/mono_data.csv', row.names=FALSE)
  write.csv(cat_gdp_df, './part2/data/cat_data.csv', row.names=FALSE)
  write.csv(country_df, './part2/data/country_df.csv', row.names=FALSE)
}
