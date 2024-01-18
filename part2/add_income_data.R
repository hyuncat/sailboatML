# Function to add disposable income data for part 2

library(tidyverse)
library(dplyr)

# Add disposable income data to country_df
add_income_to_country_df <- function(country_df) {
  income_data <- read_csv('./part2/data/income_per_capita.csv')

  # Rename cols
  income_data <- income_data %>% rename(country = `Country Name`)
  income_data <- income_data %>% rename(country_code = `Country Code`)

  # Add 2 letter country code
  country_codes <- sapply(income_data$country, function(x) countrycode(x, origin='country.name', destination='iso2c'))
  income_data$country_code <- country_codes

  income_vec <- c()
  for (i in 1:nrow(country_df)) {

    income <- NA
    cc <- country_df$code[i]

    if (cc %in% income_data$country_code) {
      income_row <- as.data.frame(income_data[income_data$country_code %in% cc, ])
      for (j in ncol(income_row):5) {
        if (!is.na(income_row[1, j])) {
          income <- income_row[1, j]
          print(paste(cc, income))
          break
        }
      }
    }

    income_vec <- c(income_vec, income)
  }
  income_vec <- unlist(income_vec)
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
  country_df <- add_income_to_country_df(country_df)

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
