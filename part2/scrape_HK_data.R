# If we had more time, we would scrape the remaining data this way...
# (Functions not used in our analysis)

library(tidyverse)
library(httr)
library(rvest)

# Returns a dataframe with sailboat attribute data
scrape_rightboat <- function() {
  # Read in html of webpage
  #url <- 'https://www.yachtworld.com/boats-for-sale/country-hong-kong/'

  headers <- c('name', 'condition', 'length', 'year', 'seller', 'price')
  big_df <- data.frame(matrix(NA, nrow=1, ncol=length(headers)))
  colnames(big_df) <- headers
  #webpage <- read_html('https://theboater.hk/sail/')

  for (i in 1:3) {
    link <- paste0('https://www.rightboat.com/boats-for-sale/HK?page=', i)
    webpage <- read_html(link)

    # Get specific html elements
    sailboats <- webpage %>%
      html_elements("div.flex.flex-col.justify-between.flex-1.h-full")

    sail_names <- sailboats %>%
      html_elements('h2') %>%
      html_text2()
    sail_details <- sailboats %>%
      html_elements('p.font-bold.text-gray-raisin-black.text-body-3') %>%
      html_text2()
    sail_prices <- sailboats %>%
      html_elements('p.py-1.font-bold.text-body-2') %>%
      html_text2()


    df <- data.frame(matrix(NA, nrow=length(sail_names), ncol=length(headers)))
    colnames(df) <- headers

    df$name <- sail_names
    count <- 1
    for (i in 1:nrow(df)) {
      for (j in 2:5) {
        df[i, j] <- sail_details[count]
        count <- count + 1
      }
    }

    df$price <- sail_prices
    big_df <- rbind(big_df, df)
  }

  big_df$length <- gsub(" ft.", "", big_df$length)
  big_df$price <- as.numeric(gsub("[^0-9]", "", big_df$price))  # Remove non-numeric characters
  big_df$price[big_df$price == 0] <- NA
  big_df <- na.omit(big_df)

  write.csv(big_df, './part2/data/hk_data.csv')

  return(big_df)
}

scrape_theboater <- function() {
  vc_gitem-zone-mini
}

# Function to generate URLs for multiple pages
generate_urls <- function(base_url, num_pages) {
  urls <- paste0(base_url, seq(1, num_pages))
  return(urls)
}

base_url <- 'https://www.sailboatlistings.com/cgi-bin/saildata/db.cgi?db=default&uid=default&view_records=1&ID=*&sb=date&so=descend&nh='

urls <- generate_urls(base_url, 5)
scraped_df <- do.call(rbind, lapply(urls, scrape_page))

mono_rows <- grepl('monohull', scraped_df$hull)
cat_rows <- grepl('catamaran', scraped_df$hull)

scraped_mono <- scraped_df[mono_rows, ]
scraped_mono <- scraped_mono[scraped_mono$name %in% mono_names, ]

scraped_cat <- scraped_df[cat_rows, ]
scraped_cat <- scraped_cat[scraped_cat$name %in% cat_names, ]



