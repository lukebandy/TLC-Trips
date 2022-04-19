# Libraries
library(yaml) # Read YAML file
library(rgdal) # Read shapefile
library(rgeos) # Get area centroids
library(mapsapi) # Call Google Maps API
library(dplyr) # Basic data wrangling
library(readr) # Write data to RDS file

# Google API Key
key <- read_yaml('settings.yaml')$key

# Read in zone details file
zone_details <- read.csv('taxi+_zone_lookup.csv')

# Read in zone shapefile
zones <- readOGR("zones/taxi_zones.shp") %>%
  spTransform(CRS("+init=epsg:4326"))

# Add in service zones
zones@data$service_zone = zone_details$service_zone[1:263]

# Calculate centroid of each zone
centroids <- gCentroid(zones, byid=TRUE)@coords

# Fix centroid for Ellis Island
centroids[105, 1] <- centroids[104, 1]
centroids[105, 2] <- centroids[104, 2]

# Rename Governor's Island/Ellis Island/Liberty Island
zones@data$zone[103] = "Governor's Island"
zones@data$zone[104] = "Ellis Island"
zones@data$zone[105] = "Liberty Island"

# TODO: Fix two zones named Corona?

# Fix incorrect IDs (two with 56)
zones@data$LocationID = 1:263

# Ask Google API for directions between each area
routes <- data.frame()
for (pickup in 1:263) {
  message(pickup)
  for (dropoff in 1:263) {
    # Don't get route if going nowhere, or if found in a previous run
    if (pickup != dropoff & 
        nrow(filter(routes, PULocationID == pickup, DOLocationID == dropoff)) == 0) {
      # Send API request
      route <- mp_directions(
            origin = c(centroids[pickup, 1], centroids[pickup, 2]),
            destination = c(centroids[dropoff, 1], centroids[dropoff, 2]),
            key = key,
            quiet = TRUE) %>%
            # Extract data
            mp_get_routes() %>%
            # Add pickup and dropoff IDs
            mutate(PULocationID = pickup,
                   DOLocationID = dropoff)
      # Store
      routes <- rbind(routes, route)
    }
  }
}

# Remove unneeded columns to save some memory
routes <- routes %>%
  select(-c(alternative_id, leg_id, duration_in_traffic_s, 
            duration_in_traffic_text, departure_time, arrival_time))

# Save
write_rds(zones, "data_zones.RDS")
write_rds(routes, "data_routes.RDS")
