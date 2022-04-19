##### Setup #####

# Wrangling libraries
library(dplyr)
library(tidyr)
library(yaml)
library(readr)
library(lubridate)
library(sf)

# Visualisation libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(ggplot2)
library(DT)

options(scipen=999)

# Read in data
trips_full <- read_rds('data_trips.RDS') %>%
  # Remove extreme outliers
  filter(year(pickup_datetime) > 2010,
         year(dropoff_datetime) < 2049) %>%
  # Convert datetimes to dates
  mutate(pickup_date = as.Date(pickup_datetime),
         dropoff_date = as.Date(dropoff_datetime))

fares_full <- read_rds('data_fares.RDS') %>% 
  filter(id %in% trips_full$id)

zones <- read_rds('data_zones.RDS')

routes <- read_rds('data_routes.RDS')

sampleMax <- read_yaml('settings.yaml')$sample

##### UI Layout #####

ui <- dashboardPage(
  
  skin = "yellow",
  
  dashboardHeader(
    
    title = "TLC Trips"
    
  ),
  
  dashboardSidebar(
    
    # Trip date slider
    sliderInput(
      "date", 
      label = "Trip Date", 
      min = as.Date(min(trips_full$pickup_datetime)), 
      max = as.Date(max(trips_full$pickup_datetime)), 
      value = c(as.Date(min(trips_full$pickup_datetime)), as.Date(max(trips_full$pickup_datetime)))),
    
    # Vehicle selection
    pickerInput(
      "vehicle", 
      label = "Vehicles", 
      choices = unique(trips_full$vehicle), 
      selected = unique(trips_full$vehicle), 
      multiple = TRUE
    ),
    
    # Sample % slider
    sliderInput(
      "sample", 
      label = "Sample", 
      min = sampleMax, 
      max = sampleMax * 100, 
      value = sampleMax * 100, 
      post  = " %"),
    
    # Button to exploration markdown
    actionButton(inputId='exploration', 
                 label="View Exploration", 
                 icon = icon("chart-area"), 
                 onclick ="window.open('https://rpubs.com/lukebandy/TLCExploration', '_blank')"),
    
    # Button to open code repo
    actionButton(inputId='code', 
                 label="View Source Code", 
                 icon = icon("github"), 
                 onclick ="window.open('https://github.com/lukebandy/TLC-Trips', '_blank')")
    
  ),
  
  dashboardBody(
    
    # Info callouts
    fluidRow(
      infoBoxOutput("infoboxTrips", width=4),
      infoBoxOutput("infoboxPickup", width=4),
      infoBoxOutput("infoboxDropoff", width=4)
      ),
    fluidRow(
      infoBoxOutput("infoboxCost", width=4),
      infoBoxOutput("infoboxLength", width=4),
      infoBoxOutput("infoboxPassengers", width=4)
    ),
    
    fluidRow(
      
      # Map visual
      box(
        "Map", solidHeader = TRUE, 
        leafletOutput("map")
      ),
      
      # Routes
      
      tabBox(
        title = "Routes",
        side = "right",
        selected = "Popular",
        
        tabPanel(
          "Table",
          DTOutput("routesTable")
        ),
        
        tabPanel(
          "Popular",
          plotOutput("routesHeatmap")
        )
      )
      
    ),
    
    fluidRow(
      box(
        "Average Speed",
        plotOutput("speed"), width = 12
      )
    ),
    
    fluidRow(
      
      # Passengers
      box(
        title = "Passengers",
        plotOutput("passengers")
      ),
      
      # Trips by time/vehicle
      tabBox(
        title = "Trips",
        side = "right",
        selected = "Month/Year",
        
        tabPanel(
          "Time",
          plotOutput("tripsTime")
        ),
        
        tabPanel(
          "Month",
          plotOutput("tripsMonth")
        ),
        
        tabPanel(
          "Month/Year",
          plotOutput("tripsMonthYear")
        )
      )
      
    ),
    
    fluidRow(
      
      # Fares by payment method
      box(
        title = "Payment method",
        plotOutput("payment")
      ),
      
      # Fares by fare items
      box(
        title = "Fare items",
        plotOutput("fares")
      )
      
    )
  )
)

##### Server #####

server <- function(input, output) {
  
  # Filter and sample trips based on inputs
  trips <- reactive({
    
    # Filter for sidebar filters and sample
    tmp <- trips_full %>%
      filter(vehicle %in% input$vehicle,
             pickup_date >= input$date[1],
             dropoff_date <= input$date[2]) %>%
      sample_frac((input$sample / 100) / sampleMax)
    
    # If a map tile is selected, filter for trips with that location
    if (is.null(v$map_selected)) {
      tmp
    }
    else {
      filter(tmp, PULocationID == v$map_selected | DOLocationID == v$map_selected)
    }
  })
  
  # Further filter trips just for known pickup and dropoff locations
  trips_knownlocations <- reactive({
    trips() %>%
      filter(!is.na(PULocationID),
             !is.na(DOLocationID),
             PULocationID < 264,
             DOLocationID < 264)
  })
  
  # Filter fares based on selected trips
  fares <- reactive({
    fares_full %>% 
      filter(id %in% trips()$id)
  })
  
  # Output infobox of average daily trips
  output$infoboxTrips <- renderInfoBox({
    infoBox(
      "Average daily trips", 
      round(((100 * nrow(trips())) / input$sample) / as.numeric(input$date[2] - input$date[1])), 
      icon = icon("car"),
      color = "yellow"
    )
  })
  
  # Output infobox of most popular pickup location
  # TODO: Remove selected location as pickup point
  output$infoboxPickup <- renderInfoBox({
    infoBox(
      "Most popular pickup location", 
      zones@data %>% 
        filter(LocationID == (trips_knownlocations() %>% 
                                count(PULocationID, sort = TRUE) %>% 
                                head(1) %>%
                                pull(PULocationID))) %>%
        pull(zone), 
      icon = icon("building"),
      color = "yellow"
    )
  })
  
  # Output infobox of most popular dropoff location
  # TODO: Remove selected location as dropoff point
  output$infoboxDropoff <- renderInfoBox({
    infoBox(
      "Most popular dropoff location", 
      zones@data %>% 
        filter(LocationID == (trips_knownlocations() %>% 
                                count(DOLocationID, sort = TRUE) %>% 
                                head(1) %>%
                                pull(DOLocationID))) %>%
        pull(zone), 
      icon = icon("building"),
      color = "yellow"
    )
  })
  
  # Output infobox of average trip cost
  output$infoboxCost <- renderInfoBox({
    infoBox(
      "Average trip cost", 
      paste0(round(mean(trips()$total_amount, na.rm=TRUE), 2), "$"), 
      icon = icon("credit-card"),
      color = "yellow"
    )
  })
  
  # Output infobox of average trip length
  output$infoboxLength <- renderInfoBox({
    infoBox(
      "Average trip length", 
      paste0(round(as.numeric(mean(trips()$dropoff_datetime - trips()$pickup_datetime)) / 60), " minutes"), 
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # Output infobox of average passengers
  output$infoboxPassengers <- renderInfoBox({
    infoBox(
      "Average passengers", 
      round(mean(trips()$passenger_count, na.rm = T)), 
      icon = icon("user-plus"),
      color = "yellow"
    )
  })
  
  # Output leaflet map
  # TODO: Just update map
  output$map <- renderLeaflet({
    
    # Calculate opacity of each route
    routes_opacity <- routes %>%
      inner_join(
        trips_knownlocations() %>% 
          # Count occurrences of each trip
          count(PULocationID, DOLocationID) %>% 
          select(PULocationID, DOLocationID, n),
        by = c("PULocationID", "DOLocationID")
      ) %>%
      # Show most popular routes for performance
      top_n(1000, n) %>%
      # Calculate opacity
      mutate(opacity = n / max(n))
    
    # Plot
    leaflet() %>%
      # Background map
      addProviderTiles(providers$MtbMap) %>% 
      # Route
      addPolylines(data = routes_opacity$geometry, 
                   opacity = routes_opacity$opacity * 0.5, # Adjust scale of opacity
                   weight = 2,
                   color = "#F6A21D") %>% 
      # Area shapes
      addPolygons(data = zones,
                  layerId=zones@data$LocationID, 
                  color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.2, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = zones@data$zone,
                  popupOptions = popupOptions(closeButton = FALSE))
  })
  
  v <- reactiveValues(map_selected = NULL)
  
  observeEvent(input$map_shape_click, {
    v$map_selected <- input$map_shape_click$id
  })
  
  observeEvent(input$map_click,{
    v$map_selected <- NULL
  })
  
  # Output heatmap of most popular routes
  output$routesHeatmap <- renderPlot({
    
    # Most popular zones
    popularzones <- 
      # Calculate pickups and dropoffs for each zone
      full_join(trips_knownlocations() %>% 
                  count(PULocationID) %>% 
                  rename(LocationID = 
                           PULocationID), 
                trips_knownlocations() %>% 
                  count(DOLocationID) %>% 
                  rename(LocationID = 
                           DOLocationID), by = "LocationID") %>%
      # Rank
      mutate(n.x = replace_na(n.x, 0),
             n.y = replace_na(n.y, 0),
             n = n.x + n.y,
             rank = dense_rank(desc(n))) %>%
      # Keep most popular zones
      filter(rank <= 5)
    
    trips() %>%
      # Filter for trips to/from most popular zones
      filter(PULocationID %in% popularzones$LocationID & 
               DOLocationID %in% popularzones$LocationID) %>%
      # Count occurrences of each route
      count(PULocationID, DOLocationID) %>% 
      mutate(n = (n / input$sample) * 100) %>%
      # Join in zone names
      left_join(select(zones@data, PULocationID = LocationID, PULocation = zone), 
                by = "PULocationID") %>%
      left_join(select(zones@data, DOLocationID = LocationID, DOLocation = zone), 
                by = "DOLocationID") %>%
      # Plot
      ggplot(aes(PULocation, DOLocation, fill= n)) + 
      geom_tile() + 
      scale_fill_gradient(low="white", high="#F6A21D") +
      theme(axis.text.x = element_text(angle = 90),
            panel.background = element_blank()) + 
      coord_fixed() +
      guides(fill = guide_colourbar(title = "Trips")) +
      labs(x="Pickup Location", y = "Dropoff Location")
  })
  
  # Output table of each possible route
  
  tableLocations <- reactive({
    
    # All possible routes
    locations <- expand_grid(PULocationID = 1:263, DOLocationID = 1:263)
    
    # 
    if (is.null(v$map_selected)) {
      locations
    } 
    else {
      filter(locations, PULocationID == v$map_selected | DOLocationID == v$map_selected)
    }
  })
  
  output$routesTable <- renderDT(
    
    tableLocations() %>%
      # Calculate average duration and trips for each route
      left_join(trips_knownlocations() %>% 
                  mutate(duration = dropoff_datetime - pickup_datetime,
                         duration = as.numeric(duration)) %>%
                  group_by(PULocationID, DOLocationID) %>%
                  summarise(trips = n(),
                            duration_average = mean(duration, na.rm = TRUE),
                            cost_average = mean(total_amount, na.rm = TRUE)),
                by = c("PULocationID", "DOLocationID")) %>%
      mutate(trips = replace_na(trips, 0),
             trips = trips / input$sample) %>%
      # Get route lengths
      left_join(routes %>% 
                  select(PULocationID, DOLocationID, distance = distance_m) %>% 
                  st_drop_geometry(), 
                by = c("PULocationID", "DOLocationID")) %>%
      # Calculate average route speed and adjust values
      mutate(distance = distance / 1000,
             speed_average = round(distance / ((duration_average / 60) / 60), 1),
             duration_average = round(duration_average),
             cost_average = round(cost_average, 2)) %>%
      # Join in zone names
      left_join(select(zones@data, PULocationID = LocationID, PULocation = zone), 
                by = "PULocationID") %>%
      left_join(select(zones@data, DOLocationID = LocationID, DOLocation = zone), 
                by = "DOLocationID") %>%
      select(`Pickup Location` = PULocation,
             `Dropoff Location` = DOLocation,
             Trips = trips,
             `Average Cost ($)` = cost_average,
             `Distance (KM)` = distance,
             `Average Duration (s)` = duration_average,
             `Average Speed (KM/H)` = speed_average), 
    
    options = list(lengthChange = FALSE,
                   pageLength = 5), 
    rownames = FALSE
  )
  
  output$speed <- renderPlot({
    
    # If any routes are selected, get them
    if (length(input$routesTable_rows_selected) > 0) {
      selectedroutes <- expand_grid(PULocationID = 1:263, DOLocationID = 1:263) %>%
        mutate(id = row_number()) %>%
        filter(id %in% input$routesTable_rows_selected)
    }
    # Otherwise get top 5 routes
    else {
      selectedroutes <- trips_knownlocations() %>%
        count(PULocationID, DOLocationID) %>%
        filter(PULocationID != DOLocationID) %>%
        slice_max(n, n=5)
    }
    
    trips_knownlocations() %>%
      
      # Filter for selected routes
      inner_join(selectedroutes, by = c("PULocationID", "DOLocationID")) %>%
      
      # Calculate average trip time per route per month
      mutate(pickup_datetime = as_datetime(pickup_datetime),
             pickup_monthyear = floor_date(pickup_datetime, "month"),
             duration = dropoff_datetime - pickup_datetime,
             duration = as.numeric(duration)) %>%
      group_by(pickup_monthyear, PULocationID, DOLocationID) %>%
      summarise(duration_average = mean(duration, na.rm = TRUE),
                trips = n()) %>%
      
      # Calculate route name
      left_join(select(zones@data, PULocationID = LocationID, PULocation = zone), by = "PULocationID") %>%
      left_join(select(zones@data, DOLocationID = LocationID, DOLocation = zone), by = "DOLocationID") %>%
      mutate(route = paste(PULocation, '-', DOLocation)) %>%
      
      # Calculate average trip speed 
      inner_join(routes %>% 
                   select(PULocationID, DOLocationID, distance = distance_m) %>% 
                   st_drop_geometry(), 
                 by = c("PULocationID", "DOLocationID")) %>%
      mutate(distance = distance / 1000,
             speed_average = distance / ((duration_average / 60) / 60),
             trips = trips / input$sample) %>%
      
      # Plot
      ggplot(aes(x=pickup_monthyear, y=speed_average, group=route, color=route, size=trips)) + 
      geom_line(size = 0.5) +
      geom_point() +
      labs(x="Month/Year", y = "Average Speed (km/h)") + 
      guides(color=guide_legend(title="Route"),
             size=guide_legend(title="Trips"))
  })
  
  # Output violin chart of passengers by vehicle
  output$passengers <- renderPlot({
    trips() %>%
      filter(!is.na(passenger_count)) %>%
      # Plot
      ggplot(aes(x=passenger_count, y=vehicle, fill=vehicle)) + 
      geom_violin(trim=FALSE) + 
      coord_flip()+
      labs(x="Passenger Count", y = "Vehicle") +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("#9CA383", "#F6A21D"))
  })
  
  # Output bar chart of trips by month/year and vehicle
  output$tripsMonthYear <- renderPlot({
    trips() %>%
      # Count trips
      mutate(pickup_datetime = as_datetime(pickup_datetime),
             pickup_monthyear = floor_date(pickup_datetime, "month")) %>%
      count(vehicle, pickup_monthyear) %>%
      mutate(n = (n / input$sample) * 100) %>%
      # Plot
      ggplot(aes(x=pickup_monthyear, y=n, fill=vehicle)) + 
      geom_bar(position="stack", stat="identity") +
      labs(x="Month", y = "Trips") +
      guides(fill=guide_legend(title="Vehicle")) +
      scale_fill_manual(values = c("#974D25", "#9CA383", "#4A5356", "#F6A21D"))
  })
  
  # Output bar chart of trips by month and vehicle
  output$tripsMonth <- renderPlot({
    trips() %>%
      # Calculate average trips per month per vehicle type
      mutate(pickup_datetime = as_datetime(pickup_datetime),
             pickup_monthyear = floor_date(pickup_datetime, "month"),
             pickup_month = month(pickup_datetime, label = TRUE)) %>%
      group_by(vehicle, pickup_month) %>%
      summarise(n = n(),
                months = n_distinct(pickup_monthyear)) %>%
      mutate(n = (n / input$sample) / months) %>%
      # Plot
      ggplot(aes(x=pickup_month, y=n, fill=vehicle)) + 
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values = c("#974D25", "#9CA383", "#4A5356", "#F6A21D")) + 
      labs(title="Average Trips by Month and Vehicle Type", x="Month", y = "Average Trips") + 
      guides(fill=guide_legend(title="Vehicle"))
  })
  
  # Output bar chart of trips by time and vehicle
  output$tripsTime <- renderPlot({
    trips() %>%
      # Count how many trips for every half an hour
      mutate(pickup_datetime = as_datetime(pickup_datetime),
             pickup_datetime.hour = as.POSIXlt(pickup_datetime)$hour + as.POSIXlt(pickup_datetime)$min/60 + as.POSIXlt(pickup_datetime)$sec/3600,
             pickup_datetime.hour = round(pickup_datetime.hour / 0.5) * 0.5,
             pickup_datetime.hour = replace(pickup_datetime.hour, pickup_datetime.hour == 24, 0)) %>%
      count(pickup_datetime.hour) %>%
      mutate(n = (n / 1) / as.numeric(as.Date(max(trips$pickup_datetime)) - as.Date(min(trips$pickup_datetime)))) %>%
      # Plot
      ggplot(aes(x=pickup_datetime.hour, y=n, ymin=0)) +
      geom_line()+
      geom_point() +
      labs(title="Average Trips by Time of Day", x="Hour", y = "Average Trips")
  })
  
  output$payment <- renderPlot({
    # Bring in payment types
    d <- trips() %>% 
      filter(!is.na(total_amount),
             payment_description %in% c("Cash", "Credit card"))
    
    # Box plots with outliers removed
    ggplot(d, aes(x=payment_description, y=total_amount, color=payment_description)) + 
      geom_boxplot(outlier.shape = NA, varwidth=T) +
      scale_y_continuous(limits = quantile(d$total_amount, c(0.1, 0.9))) +
      labs(x="Payment Method", y = "Trip Fare")+ 
      theme(legend.position = "none") +
      scale_color_manual(values = c("#974D25", "#9CA383"))
  })
  
  # Output pie chart of fare breakdowns
  output$fares <- renderPlot({
    fares() %>% 
      # Sum breakdown categories
      group_by(name) %>% 
      summarise(value = sum(value)) %>%
      # Plot
      ggplot(aes(x="", y=value, fill=name)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") + 
      # Polish
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      ) +
      scale_fill_brewer("Fare Breakdown", palette = "Dark2")
  })
  
}

shinyApp(ui, server)