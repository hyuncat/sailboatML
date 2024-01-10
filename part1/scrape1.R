library(tidyverse)
library(rvest)

url <- 'https://www.sailboatlistings.com/cgi-bin/saildata/db.cgi?db=default&uid=default&view_records=1&ID=*&sb=date&so=descend&nh=1'

webpage <- read_html(url)
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
    #next
  }

}
