library(tidyverse)
library(WDI)
library(countrycode)

get_GDP <- function(country, year) {
  # Convert country to iso2c
  cc <- countrycode(country, origin='country.name', destination='iso2c')
  #print(cc)
  print(year)
  year <- as.numeric(year)
  print(year)

  gdp_df <- tryCatch({
    suppressWarnings(WDI(indicator='NY.GDP.PCAP.KD', country=cc, start=year, end=year+1))
  }, error = function(x) {
    NULL
  })

  if (!is.null(gdp_df) && nrow(gdp_df) > 0) {
    gdp <- gdp_df[gdp_df$year==year, 5]
  } else {
    gdp <- NA
  }

  return(gdp)
}

add_gdp_col <- function(df) {
  gdp_vec <- c()
  for (row in 1:nrow(df)) {
    if (df$big_region[row] == 'USA') {
      country <- df$big_region[row]
    } else {
      country <- df$small_region[row]
    }
    gdp <- get_GDP(country=country, year=df$year[row])
    print(gdp)
    gdp_vec <- c(gdp_vec, gdp)
  }
  df$gdp <- gdp_vec
  df <- na.omit(df)
  return(df)
}

gdp_cat_df <- add_gdp_col(cleaned_catamaran_data)
gdp_mono_df <- add_gdp_col(cleaned_monohull_data)

gdp_cat_df <- na.omit(gdp_cat_df)
gdp_mono_df <- na.omit(gdp_mono_df)

write.csv(gdp_cat_df, './part1/data/clean_cat.csv', row.names=FALSE)
write.csv(gdp_mono_df, './part1/data/clean_mono.csv', row.names=FALSE)
