# Libraries
library(yaml) # Read YAML file
library(purrr) # haha R go purrrrrrr
library(data.table) # Read in CSV files
library(dplyr) # Wrangling
library(tidyr) # Wrangling
library(readr) # Write RDS files
library(stringr) # Clean strings

# Percentage of trips to keep in sample
sample <- read_yaml('settings.yaml')$sample

# Function to read in then sample a CSV file
read_sample <- function(flnm, rename=c()) {
  fread(flnm, fill = TRUE) %>% 
    sample_frac(sample) %>%
    # Rename columns if necessary 
    rename(any_of(rename))
}

# Read in each FHV file
trips_fhv <-
  # Find files
  list.files('data', pattern = 'fhv_', full.names = T) %>% 
  # Read in and sample
  map_df(~read_sample(., 
                      c(pickup_datetime = "Pickup_DateTime",
                        dropoff_datetime = "DropOff_DateTime",
                        dropoff_datetime = "DropOff_datetime",
                        PULocationID = "PUlocationID",
                        DOLocationID = "DOlocationID"))) %>%
  # Remove unused columns
  select(-c(dispatching_base_num, SR_Flag, Affiliated_base_number, 
            Dispatching_base_number, Dispatching_base_num)) %>%
  # Store vehicle type
  mutate(vehicle = "For-Hire Vehicle")

# Read in each FHVHV file
trips_fhvhv <-
  # Find files
  list.files('data', pattern = 'fhvhv_', full.names = T) %>% 
  # Read in and sample
  map_df(~read_sample(.)) %>%
  # Remove unused columns
  select(-c(hvfhs_license_num, dispatching_base_num, SR_Flag)) %>%
  # Store vehicle type
  mutate(vehicle = "High Volume For-Hire Vehicle")

# Read in each Green Taxi file
trips_green <-
  # Find files
  list.files('data', pattern = 'green_', full.names = T) %>% 
  # Read in and sample
  map_df(~read_sample(.)) %>%
  # Remove unused columns
  select(-c(VendorID, store_and_fwd_flag, RatecodeID, total_amount)) %>%
  # Rename columns
  rename(pickup_datetime = lpep_pickup_datetime,
         dropoff_datetime = lpep_dropoff_datetime) %>%
  # Store vehicle type
  mutate(vehicle = "Green Taxi")

# Read in each Yellow Taxi file
trips_yellow <-
  # Find files
  list.files('data', pattern = 'yellow_', full.names = T) %>% 
  # Read in and sample
  map_df(~read_sample(.)) %>%
  # Remove unused columns
  select(-c(VendorID, store_and_fwd_flag, RatecodeID, total_amount)) %>%
  # Rename columns
  rename(pickup_datetime = tpep_pickup_datetime,
         dropoff_datetime = tpep_dropoff_datetime) %>%
  # Store vehicle type from file name
  mutate(vehicle = "Yellow Taxi")

payment_types <- read.csv('payment_types.csv')

trips <- 
  # Combine vehicle types
  bind_rows(
    trips_fhv,
    trips_fhvhv,
    trips_green,
    trips_yellow) %>%
  # Add id to each trip
  mutate(id = row_number()) %>%
  # Add in payment type label
  left_join(payment_types, by = "payment_type") %>%
  select(-c(payment_type))

# Free up some memory
rm(trips_fhv, trips_fhvhv, trips_green, trips_yellow, read_sample, payment_types, sample)

# Create trip fare details table
fares <- trips %>%
  select(id, fare_amount, extra, mta_tax, tip_amount, tolls_amount, 
         ehail_fee, improvement_surcharge, congestion_surcharge) %>%
  pivot_longer(-id, values_drop_na=TRUE) %>%
  mutate(name = str_replace(name, "_", " "),
         name = str_to_sentence(name))

trips <- trips %>% 
  # Sometimes the included total_amount column does not include congestion_surcharge
  # for whatever reason. Join in a calculated total_amount for fast dashboard usage
  left_join(fares %>% 
              group_by(id) %>%
              summarise(total_amount = sum(value)), by = "id") %>%
  # Remove fare details from main dataset
  select(-c(fare_amount, extra, mta_tax, tip_amount, tolls_amount, 
            ehail_fee, improvement_surcharge, congestion_surcharge))

# Write to RDS
write_rds(trips, "data_trips.RDS")
write_rds(fares, "data_fares.RDS")
