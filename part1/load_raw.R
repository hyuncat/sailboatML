# Functions to load and prepare raw data

library(tidyverse)
library(readxl)

# Read in raw data
monohull_data <- read_excel('./part1/2023_MCM_Problem_Y_Boats.xlsx', 'Monohulled Sailboats ')
catamaran_data <- read_excel('./part1/2023_MCM_Problem_Y_Boats.xlsx', 'Catamarans')

# Clean up raw excel data
clean_raw <- function(data) {
  # Set colnames easier to read
  colnames(data) <- c('make', 'variant', 'length', 'big_region',
                      'small_region', 'price', 'year')

  # Trim white space on all values in data
  data <- data.frame(apply(data, 2, function(x) {
    trimws(x)
    gsub("\u00A0", "", x)
  }))

  # Round each cell in make and variant cols of dataframe if numeric
  for (i in 1:nrow(data)) {
    for (j in 1:2) {
      suppressWarnings({
        data[i, j] <- as.character(data[i, j])
        # If cell is just a number (nchar when converted to numeric == nchar normally)
        if (!is.na(as.numeric(data[i, j])) && !grepl("[A-Za-z]", data[i, j])) {
          if (nchar(data[i, j]) > 10) # For weird rounded cells (excel issue)
            data[i, j] <- as.character(signif(as.numeric(data[i, j])), 2)
        }
      })
    }
  }
  data <- as.data.frame(data)
  return(data)
}


# Data Cleaning
# (1) Outlier handling
remove_outliers <- function(data) {
  n <- nrow(data)
  # Get z_scores for price
  data$price <- as.numeric(data$price)
  z_scores <- abs(scale(data$price))
  # Create logical vector indicating outliers
  outliers <- z_scores > 3
  # Filter out outlier rows
  data <- data[!outliers, ]

  print(paste("Removed", sum(outliers), "extreme values from data"))
  return(data)
}
# (2) Remove missing values
# Also select top 6 manufacturers and top 22 variants by frequency
remove_missing <- function(data) {
  # Remove NA values
  print(paste("Removing", sum(is.na(data)), "missing values in data"))
  data <- na.omit(data)

  # Ensure 'make' and 'variant' columns are character types
  data$make <- as.character(data$make)
  data$variant <- as.character(data$variant)

  # Get top 6 manufacturers
  make_freqs <- sort(table(data$make), decreasing=TRUE)
  top_make <- names(head(make_freqs, 6))
  print("Top 6 manufacturers:")
  print(top_make)

  # Get top 22 variants
  variant_freqs <- sort(table(data$variant), decreasing=TRUE)
  top_variant <- names(head(variant_freqs, 22))
  print("Top 22 variants:")
  print(top_variant)

  # Subset data by top make and variant
  n <- nrow(data)
  data <- data[data$make %in% top_make, ]
  data <- data[data$variant %in% top_variant, ]
  print(paste("Filtered", n-nrow(data), "non-NA rows from data"))
  return(data)
}

fix_white <- function(data) {
  data$year <- gsub("^\\s+", "", data$year)
  data$price <- gsub("^\\s+", "", data$price)
  return(data)
}

cleaned_monohull_data <- monohull_data %>%
  clean_raw() %>%
  remove_outliers() %>%
  remove_missing()

cleaned_catamaran_data <- catamaran_data %>%
  clean_raw() %>%
  remove_outliers() %>%
  remove_missing()

mono_names <- unique(paste(cleaned_monohull_data$make, cleaned_monohull_data$variant))
cat_names <- unique(paste(cleaned_catamaran_data$make, cleaned_catamaran_data$variant))

