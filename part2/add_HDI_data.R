# Functions to add HDI data to our data for part 2

library(tidyverse)
library(readxl)

# Add HDI data to supplementary country_df (not cat/mono data)
add_hdi_to_country_df <- function(country_df) {
  hdi_data <- read_excel('./part2/data/hdi_data2.xlsx')
  colnames(hdi_data) <- c('hdi_rank', 'country', 'hdi', 'life_exp_at_birth', 'exp_yrs_schooling', 'mean_yrs_schooling', 'gni_percapita', 'gni_minus_hdi')

  country_codes <- sapply(hdi_data$country, function(x) countrycode(x, origin='country.name', destination='iso2c'))
  hdi_data$country_code <- country_codes

  hdi_vec <- c()
  for (i in 1:nrow(country_df)) {
    cc <- country_df$code[i]
    if (cc %in% hdi_data$country_code) {
      hdi <- hdi_data[hdi_data$country_code==cc, 'hdi']
    } else {
      hdi <- NA
    }
    hdi_vec <- c(hdi_vec, hdi)
  }
  country_df$hdi <- hdi_vec

  # Supplementary data from https://en.populationdata.net/rankings/hdi/americas/
  country_df[country_df$country=='Cayman Islands', 'hdi'] <- .888
  country_df[country_df$country=='Monaco', 'hdi'] <- .956
  country_df[country_df$country=='Puerto Rico', 'hdi'] <- .921
  country_df[country_df$country=='West Indies', 'hdi'] <- .749
  country_df[country_df$country=='Sint Maarten (Dutch part)', 'hdi'] <- 0.941
  country_df[country_df$country=='Aruba', 'hdi'] <- 0.908

  return(country_df)
}

# Add hdi col to dataset
add_hdi_col_new <- function(df) {
  hdi_vec <- c()
  for (row in 1:nrow(df)) {
    if (df$big_region[row] == 'USA') {
      country <- df$big_region[row]
    } else {
      country <- df$small_region[row]
    }
    hdi <- country_df[country, 'hdi']
    hdi_vec <- c(hdi_vec, hdi)
  }
  df$hdi <- hdi_vec
  return(df)
}

# Don't run if sourcing functions into other file
if (sys.nframe() == 0) {
  mono_gdp_df <- add_hdi_col_new(mono_gdp_df)
  cat_gdp_df <- add_hdi_col_new(cat_gdp_df)

  mono_gdp_df <- apply(mono_gdp_df,2,as.character)
  cat_gdp_df <- apply(cat_gdp_df,2,as.character)

  # Save as files
  write.csv(mono_gdp_df, './part2/data/mono_data.csv', row.names=FALSE)
  write.csv(cat_gdp_df, './part2/data/cat_data.csv', row.names=FALSE)
}

