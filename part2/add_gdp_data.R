# Functions to create and load GDP data for part 2

library(tidyverse)
library(countrycode)

source('./part1/load_raw.R')
source('./part1/scrapeGDP.R')

# Read in data
load_data <- function() {
  mono_raw <- read_excel('./2023_MCM_Problem_Y_Boats.xlsx', 'Monohulled Sailboats ')
  cat_raw <- read_excel('./2023_MCM_Problem_Y_Boats.xlsx', 'Catamarans')

  mono_clean <- mono_raw %>%
    clean_raw() %>%
    remove_missing()

  cat_clean <- cat_raw %>%
    clean_raw() %>%
    remove_missing()
}

# Create df with country names, iso2c codes, and GDP
get_country_df <- function() {
  countries <- c('USA')
  for (row in 1:nrow(mono_clean)) {
    if (mono_clean$big_region[row] != 'USA') {
      if (!(mono_clean$small_region[row] %in% countries)) {
        countries <- c(countries, mono_clean$small_region[row])
      }
    }
  }
  for (row in 1:nrow(cat_clean)) {
    if (cat_clean$big_region[row] != 'USA') {
      if (!(cat_clean$small_region[row] %in% countries)) {
        countries <- c(countries, cat_clean$small_region[row])
      }
    }
  }
  country_codes <- sapply(countries, function(x) countrycode(x, origin='country.name', destination='iso2c'))
  country_df <- data.frame(country=countries, code=country_codes)

  # Manually insert missing values
  # country_df["Netherlands Antilles", 2] <- "AN"
  # country_df["Cork", 2] <- "IE"
  # country_df["West Indies", 2] <- "West Indies"
  # country_df["Saint-Martin", 2] <- "MF"
  # country_df["Lagos", 2] <- "NG"

  country_df["Guadeloupe", 2] <- 'FR'
  country_df["Netherlands Antilles", 2] <- 'NL'
  country_df["Cork", 2] <- "IE"
  country_df["West Indies", 2] <- "West Indies"
  country_df["Gibraltar", 2] <- 'GB'
  country_df["British Virgin Islands", 2] <- 'GB'
  country_df["U.S. Virgin Islands", 2] <- 'US'
  country_df["Jersey", 2] <- 'GB'
  country_df["Martinique", 2] <- 'FR'
  country_df["Saint-Martin", 2] <- 'FR'

  gdp_vec <- c()
  for (i in 1:nrow(country_df)) {
    if (country_df$code[i] == "West Indies") {
      gdp <- 12057.57
    } else {
      gdp <- get_GDP_by_code(country_df$code[i])
    }
    gdp_vec <- c(gdp_vec, gdp)
  }
  country_df$gdp <- gdp_vec

  # Manually insert missing values

  country_df["Guadeloupe", 3] <- 38816.479
  country_df["Netherlands Antilles", 3] <- 70869.4
  country_df["Gibraltar", 3] <- 47923.482
  country_df["British Virgin Islands", 3] <- 47923.482
  country_df["U.S. Virgin Islands", 3] <- 62789.128
  country_df["Jersey", 3] <- 47923.482
  country_df["Martinique", 3] <- 38816.479
  country_df["Saint-Martin", 3] <- 35873

  return(country_df)
}


get_WestIndies_gdp <- function() {
  west_indies_country_codes <- c("AG", "BS", "BB", "CU", "DM", "DO", "GD", "HT",
                                 "JM", "KN", "LC", "VC", "TT")
  west_indies_gdp <- mean(sapply(west_indies_country_codes, get_GDP_by_code))
}


get_GDP_by_code <- function(code) {
  gdp_df <- tryCatch({
    suppressWarnings(WDI(indicator='NY.GDP.PCAP.KD', country=code, start=2021, end=2022))
  }, error = function(x) {
    NULL
  })

  if (!is.null(gdp_df) && nrow(gdp_df) > 0) {
    gdp <- gdp_df[gdp_df$year==2022, 5]
  } else {
    gdp <- NA
  }

  return(gdp)
}

add_gdp_col_new <- function(df) {
  gdp_vec <- c()
  for (row in 1:nrow(df)) {
    if (df$big_region[row] == 'USA') {
      country <- df$big_region[row]
    } else {
      country <- df$small_region[row]
    }
    gdp <- country_df[country, 'gdp']
    gdp_vec <- c(gdp_vec, gdp)
  }
  df$gdp <- gdp_vec
  df <- na.omit(df)
  return(df)
}

# Don't run if sourcing functions into other file
if (sys.nframe() == 0) {
  load_data()
  country_df <- get_country_df()

  mono_gdp_df <- add_gdp_col_new(mono_clean)
  cat_gdp_df <- add_gdp_col_new(cat_clean)

  # Save as files
  write.csv(mono_gdp_df, './part2/data/mono_data.csv', row.names=FALSE)
  write.csv(cat_gdp_df, './part2/data/cat_data.csv', row.names=FALSE)
}
