  library(shiny)
  library(ggplot2)
  library(shinyTime)
  library(jsonlite)
  library(ggmap)
  library(shinyWidgets)
  library(leaflet)
  library(plotly)
  library(tidyverse)
  library(DT)
  library(RColorBrewer)
  library(scales)
  library(lattice)
  library(dplyr)
  library(httr)
  library(rvest)
  library(lubridate)
  library(leaflet.extras)
  library(shinycssloaders)
  library(data.table)
  
  
  
  # datasets
  
  carparks <- read.csv("Carpark v9.csv")
  
  
  url1 <- "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2"
  my_raw_result1 <- GET(url1,add_headers(AccountKey = "UbnxVc29Qxub8XGmmXlJMw==",accept = "application/json"))
  result1 <- content(my_raw_result1, as = 'text') %>%fromJSON()
  data1 <- as.data.frame(result1)
  url2 <- "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2?$skip=500"
  my_raw_result2 <- GET(url2,add_headers(AccountKey = "UbnxVc29Qxub8XGmmXlJMw==",accept = "application/json"))
  result2 <- content(my_raw_result2, as = 'text') %>%fromJSON()
  data2 <- as.data.frame(result2)
  url3 <- "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2?$skip=1000"
  my_raw_result3 <- GET(url3,add_headers(AccountKey = "UbnxVc29Qxub8XGmmXlJMw==",accept = "application/json"))
  result3 <- content(my_raw_result3, as = 'text') %>%fromJSON()
  data3 <- as.data.frame(result3)
  url4 <- "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2?$skip=1500"
  my_raw_result4 <- GET(url4,add_headers(AccountKey = "UbnxVc29Qxub8XGmmXlJMw==",accept = "application/json"))
  result4 <- content(my_raw_result4, as = 'text') %>%fromJSON()
  data4 <- as.data.frame(result4)
  url5 <- "http://datamall2.mytransport.sg/ltaodataservice/CarParkAvailabilityv2?$skip=2000"
  my_raw_result5 <- GET(url5,add_headers(AccountKey = "UbnxVc29Qxub8XGmmXlJMw==",accept = "application/json"))
  result5 <- content(my_raw_result5, as = 'text') %>%fromJSON()
  data5 <- as.data.frame(result5)
  availability <- rbind(data1,data2,data3,data4,data5)
  
  availability <- select(availability, value.Development, value.AvailableLots,value.LotType)
  names(availability) <- c("Carpark", "AvailableLots","LotType")
  
  carparks <- full_join(availability, carparks)
  
  public_hols <- read.csv('public-holidays-2021.csv')
  public_hols$date <- as.Date(public_hols$date)
  df_hdb <- carparks[carparks$Agency == 'HDB',]
  
  
  # Backend functions
  
  #### Helper Function to obtain live weather and forecasted weather
  key <- 'cErVHo27XBa8PuyskaVVJraqFpN5G9pl'
  
  get_weather_data_curr <- function(lat, long)
  {
    lat <- lat[1]
    long <- long[1]
    location <- GET(url = paste0('http://dataservice.accuweather.com/locations/v1/cities/geoposition/search?', 
                                 'apikey=', 
                                 key, 
                                 '&q=',
                                 as.character(lat),
                                 '%2C', 
                                 as.character(long) 
    ) 
    )
    
    location <- content(location, as = 'text') %>%fromJSON()
    location_key <- location$Key
    
    weather_curr <- GET(url = paste0('http://dataservice.accuweather.com/currentconditions/v1/', 
                                     location_key, 
                                     '?apikey=', 
                                     key
    ) 
    )
    
    weather_curr <- content(weather_curr, as = 'text') %>%fromJSON()
    
    weather_curr_text <- weather_curr$WeatherText
    weather_curr_icon <- weather_curr$WeatherIcon
    weather_curr_precipitate <- weather_curr$HasPrecipitation
    weather_curr_precipitatetype <- weather_curr$PrecipitationType
    
    weather_forecast1 <- GET(url = paste0('http://dataservice.accuweather.com/forecasts/v1/hourly/1hour/', 
                                          location_key, 
                                          '?apikey=',
                                          key 
    ) 
    )
    
    weather_forecast1 <- content(weather_forecast1, as = 'text') %>% fromJSON()
    
    weather_forecast1_text <- weather_forecast1$IconPhrase
    weather_forecast1_icon <- weather_forecast1$WeatherIcon
    weather_forecast1_precipitate <- weather_forecast1$HasPrecipitation
    weather_forecast1_precipitateprob <- weather_forecast1$PrecipitationProbability
    
    weather_curr <- weather_curr
    weather_forecast1 <- weather_forecast1
    return (weather_curr)
  }
  get_weather_data_forecast1 <- function(lat, long)
  {
    lat <- lat[1]
    long <- long[1]
    location <- GET(url = paste0('http://dataservice.accuweather.com/locations/v1/cities/geoposition/search?', 
                                 'apikey=', 
                                 key, 
                                 '&q=',
                                 as.character(lat),
                                 '%2C', 
                                 as.character(long) 
    ) 
    )
    
    location <- content(location, as = 'text') %>%fromJSON()
    location_key <- location$Key
    
    weather_curr <- GET(url = paste0('http://dataservice.accuweather.com/currentconditions/v1/', 
                                     location_key, 
                                     '?apikey=', 
                                     key
    ) 
    )
    
    weather_curr <- content(weather_curr, as = 'text') %>%fromJSON()
    
    weather_curr_text <- weather_curr$WeatherText
    weather_curr_icon <- weather_curr$WeatherIcon
    weather_curr_precipitate <- weather_curr$HasPrecipitation
    weather_curr_precipitatetype <- weather_curr$PrecipitationType
    
    weather_forecast1 <- GET(url = paste0('http://dataservice.accuweather.com/forecasts/v1/hourly/1hour/', 
                                          location_key, 
                                          '?apikey=',
                                          key 
    ) 
    )
    
    weather_forecast1 <- content(weather_forecast1, as = 'text') %>% fromJSON()
    
    weather_forecast1_text <- weather_forecast1$IconPhrase
    weather_forecast1_icon <- weather_forecast1$WeatherIcon
    weather_forecast1_precipitate <- weather_forecast1$HasPrecipitation
    weather_forecast1_precipitateprob <- weather_forecast1$PrecipitationProbability
    
    weather_curr <- weather_curr
    weather_forecast1 <- weather_forecast1
    return(weather_forecast1)
  }
  
  
  ## Helper Function for calculation of parking rates
  non_sunday_cal <- function(start, end)
  {
    total <- as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M'), units = 'mins')
    return (total * 0.02)
  }
  
  non_sunday_cal_central <- function(start, end)
  {
    total <- 0
    #if start before 0700
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') < 0)
    {
      #if end before 0700
      ifelse(as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') <= 0, 
             total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M')),
             {
               total <- total - as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') 
               #if ends before 1700
               ifelse (as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('17:00', format = '%H:%M'), units = 'mins') < 0 & as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') >= 0, 
                       total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') * 2, 
                       total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('17:00', format = '%H:%M'), units = 'mins') + 1200 
               )
             }
      )
    }
    #if start before 1700 and after 0700
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('17:00', format = '%H:%M'), units = 'mins') < 0 & as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') >= 0)
    {
      #if end before 1700
      ifelse (as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('17:00', format = '%H:%M'), units = 'mins') < 0, 
              total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M')) * 2,
              total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('17:00', format = '%H:%M'), units = 'mins') + as.numeric(as.POSIXct('17:00', format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M'), units = 'mins') * 2
      )
    }
    #if start after 1700
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('17:00', format = '%H:%M'), units = 'mins') > 0)
    {
      total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M'), units = 'mins')
    }
    return (total * 0.02)
  }
  
  
  sunday_cal_7 <- function(start, end)
  {
    total <- 0
    #if start before 0700
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') < 0)
    {
      #if end before 0700
      ifelse(as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') <= 0, 
             total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M')),
             total <- total - as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') 
      )
      #if ends after 2230
      if (as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') > 0)
      {
        total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins')
      }
    }
    #if start before 2230 and after 0700
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') < 0 & as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('07:00', format = '%H:%M'), units = 'mins') >= 0)
    {
      #if end after 2230
      if (as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') > 0)
      {
        total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins')
      }
    }
    #if start after 2230
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') > 0)
    {
      total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M'))
    }
    return (total * 0.02)
  }
  
  #sunday calculation for after 1-10.30 free
  sunday_cal_13 <- function(start, end)
  {
    total <- 0
    #if start before 1300
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('13:00', format = '%H:%M'), units = 'mins') < 0)
    {
      #if end before 1300
      ifelse(as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('13:00', format = '%H:%M'), units = 'mins') <= 0, 
             total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M')),
             total <- total - as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('13:00', format = '%H:%M'), units = 'mins') 
      )
      #if ends after 2230
      if (as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') > 0)
      {
        total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins')
      }
    }
    #if start before 2230 and after 1300
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') < 0 & as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('13:00', format = '%H:%M'), units = 'mins') >= 0)
    {
      #if end after 2230
      if (as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') > 0)
      {
        total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins')
      }
    }
    #if start after 2230
    if (as.numeric(as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M') - as.POSIXct('22:30', format = '%H:%M'), units = 'mins') > 0)
    {
      total <- total + as.numeric(as.POSIXct(format(end, format = '%H:%M'), format = '%H:%M') - as.POSIXct(format(start, format = '%H:%M'), format = '%H:%M'))
    }
    return (total * 0.02)
  }
  
  #list of central car parks with special rates calculation (not just 1.20 per hour)  
  central_carpark <- c('ACB', 'BBB', 'BRB1', 'CY', 'DUXM', 'HLM', 'KAB', 'KAM', 'KAS', 'PRM', 'SLS', 'SR1', 'SR2', 'TPM', 'UCS', 'WCB')
  
  daily_rate <- function(carpark, start, end)
  {
    df_carpark <- df_hdb[df_hdb$CarparkID == carpark,]
    ifelse(wday(start) == 1 | date(start) %in% public_hols$date,
           {
             if (df_carpark$free_parking == "SUN & PH FR 7AM-10.30PM")
             {
               return (sunday_cal_7(start, end))
             }
             if (df_carpark$free_parking == "SUN & PH FR 1PM-10.30PM")
             {
               return (sunday_cal_13(start, end))
             }
             if (df_carpark$free_parking == 'NO')
             {
               return (non_sunday_cal(start, end))
             }
           },
           {
             ifelse (df_carpark$CarparkID %in% central_carpark, 
                     return (non_sunday_cal_central(start, end)),
                     return (non_sunday_cal(start,end))
             )
           }
    )
  }
  
  get_rates <- function(carpark, start, end)
  {
    ifelse (date(start) == date(end),
            {
              return (daily_rate(carpark, start, end))
            },
            {
              total <- 0
              while (as.numeric(date(end) - date(start), units = 'days') > 0)
              {
                mid <- as.POSIXct(paste(date(start), '23:59'))
                total <- total + daily_rate(carpark, start, mid) + 0.02
                start <- as.POSIXct(paste(date(start + days(1)), '00:00'))
              }
              total <- total + daily_rate(carpark, start, end)
              return (total)
            }
    )
  }
  
  
  
  # UI
  ui <- navbarPage(
    title = "Carpark Explorer",id = 'nav',
    
    tabPanel(
      title = "Explore",
      
      tags$head(
        # Include custom CSS
        includeCSS("styles.css")
      ),
      
      div(class="outer",
          leafletOutput("map", width="100%", height="100%"),
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                        draggable = FALSE, top = 60, left = 20, right = 'auto', bottom = "auto",
                        width = 400, height = "auto",
                        h2("Carpark Explorer"),

          selectInput("search", "Carpark",c("Enter Carpark"="",unique(carparks$Carpark)),  
                      selected = 'BLK 209/214 JURONG EAST ST 21',multiple=FALSE),

          radioButtons(inputId = "vehicleType",
                       label = "Type of Vehicle",
                       choices = c("Car" , "Motorbike","Heavy Vehicle")),
          dateInput(inputId = "startDate", "Start Date:", value = Sys.Date()),
          timeInput(inputId = 'startTime', value = Sys.time() + as.difftime(8, units = 'hours'),
                    label = 'Start time',
                    seconds = FALSE,
                    minute.steps = 15),
          dateInput(inputId = "endDate", "End Date:", value = Sys.Date()),
          timeInput(inputId = 'endTime',  value = Sys.time() + as.difftime(8.5, units = 'hours'),
                    label = 'End time',
                    seconds = FALSE,
                    minute.steps = 15),
          tags$h4('Parking Charges:'),
          textOutput('parkingRates'),
         
          tags$h4('Current Weather at Carpark:'),
          textOutput('weather_current'),
          tags$br(),
          tags$h4('Forecasted Weather at Carpark'), tags$h4('(1 Hourly): '),
          textOutput('weather_forecast'),
          tags$br(),
          
          plotOutput("pastData", height = 300)

          
          )
      )
    )

    
  )
  

  
  # Server
  
  server <- function(input, output,session) {
  

    # Start of map
    output$map <- renderLeaflet({
    leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
         setView(lng = 103.8198, lat =1.3521, zoom = 12)
    })  
    # End of map
    
    # For weather forecast and getting weather
    lat <- reactive({
      la <- carparks[carparks$Carpark == input$search,]
      la <- la$Latitude
      la
    })
    lon <- reactive({
      lon <- carparks[carparks$Carpark == input$search,]
      lon <- lon$Longitude
      lon
    })
    
    output$weather_current <- renderText({
      weather_curr <- get_weather_data_curr(lat(), lon())
      weather_curr_text <- weather_curr$WeatherText
      weather_curr_icon <- weather_curr$WeatherIcon
      weather_curr_precipitate <- weather_curr$HasPrecipitation
      weather_curr_precipitatetype <- weather_curr$PrecipitationType
      if (weather_curr_precipitate)
      {
        display <- paste0(weather_curr_text, ' with ', weather_curr_precipitatetype)
        return(display)
      }
      display <- paste0(weather_curr_text, ' with no precipitation')
      return(display)
    })
    output$weather_forecast <- renderText({
      weather_forecast1 <- get_weather_data_forecast1(lat(), lon())
      weather_forecast1_text <- weather_forecast1$IconPhrase
      weather_forecast1_icon <- weather_forecast1$WeatherIcon
      weather_forecast1_precipitate <- weather_forecast1$HasPrecipitation
      weather_forecast1_precipitateprob <- weather_forecast1$PrecipitationProbability
      if (weather_forecast1_precipitate)
      {
        display <- paste0(weather_forecast1_text, ' with a probability of ', weather_forecast1_precipitateprob, '%')
        return(display)
      }
      display <- paste0(weather_forecast1_text, ' with no precipitation')
      return(display)
    })
    # End of Helper Functions for weather prediction
    
    veh_type <- reactive({
      ifelse(input$vehicleType == 'Car', 'C', ifelse(input$vehicleType == 'Motorcycle', 'Y', 'H'))
      
    })
    dataVehicle <- reactive({
      veh_type <- ifelse(input$vehicleType == 'Car', 'C', ifelse(input$vehicleType == 'Motorcycle', 'Y', 'H'))
      data_vehType <- combined[combined$LotType == veh_type | is.na(combined$LotType),] 
      
      data_vehType
    })
    
    generate_data <- reactive({
      filtered_data <- carparks[carparks$LotType == veh_type(),]
      filtered_data
    })
    
    agency <- reactive({
      carparks <- generate_data()
      carpark <- carparks[carparks$Carpark == input$search,]
      carpark <- carpark$Agency
      carpark
    })
    carparkID <- reactive({
      carparks <- generate_data()
      id <- carparks[carparks$Carpark == input$search,]
      id <- id$CarparkID
      id
    })
    carparkID2 <- reactive({
      carparks <- generate_data()
      name <- carparks[carparks$Carpark == input$carparkName,]
      name <- name$CarparkID
      name
    })
    agency_tab2 <- reactive({
      carparks <- generate_data()
      carpark <- carparks[carparks$Carpark == input$carparkName,]
      carpark <- carpark$Agency
      carpark
    })
    
    # Helper Functions for plotting occupancy rate
    generate_current <- function(carpark, start_time, veh_type)
    {
      date <- format(start_time, '%Y-%m-%dT%H:%M:%S')
      date <- gsub(':', '%3A', date)
      my_raw_result <- GET(paste0("https://api.data.gov.sg/v1/transport/carpark-availability?date_time=", date), 
                           add_headers(accept = "application/json"))
      result <- content(my_raw_result, as = 'text') %>%fromJSON()
      df <- as.data.frame(result)
      df <- df$items.carpark_data
      df <- df[[1]]
      unlisted <- rbindlist(df$carpark_info, fill = T, idcol = "id")
      df$id <- seq.int(nrow(df))
      df <- left_join(df, unlisted, by = "id")
      carpark_data <- filter(df, df$carpark_number == carpark & df$lot_type == veh_type)
      available_now <- carpark_data$lots_available
      percentage <- 1- as.integer(carpark_data$lots_available) / as.integer(carpark_data$total_lots)
      return(percentage * 100)
    }
    
    past_data_plotter <- function(carpark, start_time, veh_type)
    {
      #generate past history
      days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
      day_of_week <- wday(start_time)
      day_of_week_string <- days_of_week[day_of_week]
      filename <- paste0('./Project/', day_of_week_string, '.csv')
      df <- read.csv(filename)
      df <- df[df$lot_type == veh_type,]
      columners <- c()
      for (i in 0:23)
      {
        columners <- c(columners, paste0('X', toString(i)))
      }
      
      row <- df[df$carpark_number == carpark & df$lot_type == veh_type,]
      data <- subset(row, select = columners)
      data <- as.data.frame(t(data))
      colnames(data) <- 'Occupancy Rate'
      data$`Occupancy Rate` <- as.numeric(data$`Occupancy Rate`) * 100
      data$Time <- c(0:23)
      now_time <- as.POSIXct(Sys.Date()) - as.difftime(8, units = 'hours')
      data$Time_s <- format(now_time + as.difftime(data$Time, units = 'hours'), format = '%H:%M')
      
      # Get current occupancy rate
      current_rate <- generate_current(carpark, start_time, veh_type)
      current_y <- c(rep(0, hour(start_time)), current_rate, rep(0, 23 - hour(start_time)))
      
      p <- ggplot()
      p <- p + geom_bar(data = data, aes(x = Time_s , y = `Occupancy Rate`), 
                        stat = 'identity', fill = 'lightblue') +
        ylim(0, 100) + 
        ylab('Occupancy Rate (%)') + 
        xlab('Time of Day') + 
        scale_x_discrete(breaks = data$Time_s[seq(1, 23, 3)]) + 
        geom_bar(aes(x = data$Time, y = current_y), 
                 stat = 'identity', 
                 fill = 'tomato', 
                 alpha = 0.3, 
                 width = 0.8) +
        geom_vline(xintercept = hour(start_time), linetype="dotted") +
        ggtitle('Popular Times') + 
        theme(plot.title = element_text(size = 20, face = "bold"), panel.grid.major.x = element_blank())
      
      return(p)
    }
    
    output$pastData <- renderPlot({
      carparks <- generate_data()
      tryCatch(
        if (agency() == 'HDB'){
          tryCatch({
            print(past_data_plotter(carparkID(), Sys.time(), veh_type()))}, 
            error = function(e){cat('No Available Data')})
        }
        else {print("No Data Available")},
        error = function(e){cat('No Available data')})
    })
    # End of Functions for occupancy rates

    
    # Helper Functions for calculation of parking rates
    start_time <- reactive({
      startt <- as_datetime(input$startDate) + as.difftime(hour(input$startTime), units = 'hours') + as.difftime(minute(input$startTime), units = 'mins')
      startt
    })
    end_time <- reactive({
      endd <- as_datetime(input$endDate) + as.difftime(hour(input$endTime), units = 'hours') + as.difftime(minute(input$endTime), units = 'mins')
      endd
    })
    
    
    output$parkingRates <- renderText({
      carparks <- generate_data()
      tryCatch({
        if (agency() == 'HDB')
        {
          df_hdb <- carparks[carparks$Agency == 'HDB',]
          rate <- get_rates(carparkID(), start_time(), end_time())
          return (dollar(rate))
        }
        return ('This is a non-HDB carpark, refer to map for parking rates')},
      error = function(e){cat('Please Select Carpark Name')})
    })
    
    # End of Functions for calculation of parking rates
    

    
    # This observer is responsible for maintaining the circles and legend,
    # according to the type of vehicle the user has chosen.
    observe({
      radius <- 100
      chooseVehicle <- input$vehicleType
      colorBy <- chooseVehicle
      if (chooseVehicle == "Car") {
        y <- generate_data()
        colorData <- y$AvailableLots
        bins <- c(0,50,100,200,500,2000,4000)
        rc1 <- colorRampPalette(colors = c("red", "green"), space = "Lab")(7)
        pal <- colorBin(rc1, colorData,bins = bins)
        leafletProxy("map", data = y) %>%
          clearShapes() %>%
          addCircles(~Longitude, ~Latitude, radius=radius, layerId=~Carpark,
                     stroke=FALSE, fillOpacity=0.5, fillColor=pal(colorData)) %>%
          addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                    layerId="colorLegend")
      }
      else if (chooseVehicle == "Motorbike") {
        y <- generate_data()
        colorData <- y$AvailableLots
        bins <- c(0,25,50,75,100,200,400)
        rc1 <- colorRampPalette(colors = c("red", "green"), space = "Lab")(7)
        pal <- colorBin(rc1, colorData,bins)
        leafletProxy("map", data = y) %>%
          clearShapes() %>%
          addCircles(~Longitude, ~Latitude, radius=radius, layerId=~Carpark,
                     stroke=FALSE, fillOpacity=0.5, fillColor=pal(colorData)) %>%
          addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                    layerId="colorLegend")
      } else if (chooseVehicle == "Heavy Vehicle") {
        y <- generate_data()
        colorData <- y$AvailableLots
        bins <- c(0,25,50,75,100,200)
        rc1 <- colorRampPalette(colors = c("red", "green"), space = "Lab")(6)
        pal <- colorBin(rc1, colorData,bins = bins)
        leafletProxy("map", data = y) %>%
          clearShapes() %>%
          addCircles(~Longitude, ~Latitude, radius=radius, layerId=~Carpark,
                     stroke=FALSE, fillOpacity=0.5, fillColor=pal(colorData)) %>%
          addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                    layerId="colorLegend")
      }
    })
    
    # Show a popup at the given location at lat, long.
    showCarparkPopup <- function(id, lat, lng) {
      carparks <- generate_data()
      if(length(id) > 1){id <- id[1]}
      selectedCarpark <- carparks[carparks$Carpark == id,]
      content <- as.character(tagList(
        tags$h4(paste0("Available Lots: ", ifelse(is.na(selectedCarpark$AvailableLots),"Info not available",selectedCarpark$AvailableLots))),
        tags$strong(HTML(sprintf("%s: %s (%s,  %s)",
                                 selectedCarpark$CarparkID,selectedCarpark$Carpark, selectedCarpark$Latitude, selectedCarpark$Longitude
        ))), tags$br(), 

        sprintf("Weekday Rates: %s",selectedCarpark$Weekday),tags$br(),
        sprintf("Saturday Rates: %s",selectedCarpark$Saturdays),tags$br(),
        sprintf("Sunday Rates: %s",selectedCarpark$Sundays.PublicHolidays),tags$br(),
        sprintf("Sheltered: %s",ifelse(selectedCarpark$sheltered=="Y", "Yes", "No")),tags$br(),

      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    }
    
    # When map is clicked, show a popup with carpark info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showCarparkPopup(event$id, event$lat, event$lng)
      })
    })
    
    # Zoom in after searching for Carpark Name
    observe({
      carparks <- generate_data()
      x <- carparks%>%filter(Carpark == input$search)
       new_zoom <- 12
       if(!is.null(input$search)) new_zoom <- 15
       leafletProxy('map') %>% clearPopups() %>%
         setView(lng =x$Longitude , lat = x$Latitude, zoom = new_zoom)
       isolate({
         showCarparkPopup(x$Carpark,x$Latitude,x$Longitude)
       })
  
    })

  }
  
  shinyApp(ui = ui, server = server)
  
  
