# TLC Dashboard

1. `settings.yaml` contains the amount to sample the trip data by, and the Google Maps API key for when finding out routes between locations.
2. `buildtrips.R` and `buildlocations.R` are run to generate the 4 `.RDS` files that can be quickly loaded before building any visualisations. The directory `/data/` should contain any monthly data `.csv` files downloaded from [the TLC website](https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page) for `buildTrips.R` to use, but just it's output has been included in the submission to keep the file size down.
3. `exploration.RMD` contains any visuals created to initially explore the data. It can be viewed at https://rpubs.com/lukebandy/TLCExploration.
4. `app.R` is a Shiny dashboard that allows for interactive filtering of the visuals deemed most useful from `exploration.RMD`. It can be viewed at https://lukebandy.shinyapps.io/TLC-Trips/.
