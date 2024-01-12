library(tidyverse)
library(rvest)

# Returns a dataframe with sailboat attribute data
scrape_page <- function(url) {
  # Read in html of webpage
  webpage <- read_html(url)
  # Get specific html elements
  sail_names <- webpage %>%
    html_nodes("a.sailheader") %>%
    html_text2()
  sail_meta <- webpage %>%
    html_nodes("span.sailvk") %>%
    html_text2()
  sail_headers <- webpage %>%
    html_nodes("span.sailvb") %>%
    html_text2()

  # Extract unique headers
  unique_headers <- c('name', unique(sail_headers))

  # Initialize an empty dataframe with columns named after unique headers
  df <- data.frame(matrix(NA, nrow=length(sail_names), ncol=length(unique_headers)))
  colnames(df) <- unique_headers

  # Assign sail names to the 'name' column
  df$name <- sail_names

  # Iterate through sail_meta and populate the dataframe
  current_row <- 1
  for (j in seq_along(sail_meta)) {
    # Extract metadata and assign to the corresponding column
    header <- sail_headers[j]
    value <- sail_meta[j]
    df[current_row, header] <- value

    # Find the starting index of the next sailboat's metadata
    if (grepl("^\\$", sail_meta[j])) {
      current_row <- current_row + 1
    }
  }
  names(df) <- c('name', 'length', 'beam', 'draft', 'year', 'type', 'hull', 'engine', 'location', 'price')
  return(df)
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



