# Libraries
library(zoo)
library(dplyr)
library(purrr)

# Get already downloaded files
dir.create('data', showWarnings = FALSE)
downloaded <- list.files('data')

# Get list of dates
dates <- 
  seq(
    as.yearmon(as.character("201501"), "%Y%m"), 
    as.yearmon(Sys.Date()), 
    1/12) %>%
  format("%Y-%m")

# Wrap download.file() to handle non-existent files
downloadfile <- possibly(download.file, otherwise = -1)

result <- 
  # Which files need to be downloaded
  data.frame(list(
    file = 
      c(paste0("fhv_tripdata_", dates, ".parquet"),
        paste0("fhvhv_tripdata_", dates, ".parquet"),
        paste0("green_tripdata_", dates, ".parquet"),
        paste0("yellow_tripdata_", dates, ".parquet"))))%>% 
  filter(! file %in% downloaded) %>%
  # Download
  rowwise() %>%
  mutate(result = downloadfile(
    paste0("https://d37ci6vzurychx.cloudfront.net/trip-data/", file),
    paste0("data/", file), 
    quiet = TRUE, 
    mode = "wb"))
