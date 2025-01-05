library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(sf)
library(shinythemes)
library(tidyr)
library(shinyjs)
library(stringr)

#Import main dataset
data_places <- read.csv("cafes-and-restaurants-with-seating-capacity-modified_1.csv")

#Adjust name of label
data_places$Industry..ANZSIC4..description <- gsub("Accommodation", "Hotel restaurants", data_places$Industry..ANZSIC4..description)
data_places$Industry..ANZSIC4..description <- gsub("Takeaway Food Services", "Takeaways", data_places$Industry..ANZSIC4..description)
data_places$Industry..ANZSIC4..description <- gsub("Bakery Product Manufacturing \\(Non-factory based\\)", "Bakeries", data_places$Industry..ANZSIC4..description)

#Filter main industry
industry_filter <- c('Cafes and Restaurants', 'Pubs, Taverns and Bars',
                     'Hotel restaurants', 'Bakeries', 'Takeaways')

data_places <- data_places[data_places$Industry..ANZSIC4..description %in% industry_filter, ]

#Create icon column
data_places$Icons <- 'cutlery'
data_places$Icons[data_places$Industry..ANZSIC4..description == 'Pubs, Taverns and Bars'] <- 'glass'
data_places$Icons[data_places$Industry..ANZSIC4..description == 'Hotel restaurants'] <- 'building'
data_places$Icons[data_places$Industry..ANZSIC4..description == 'Bakeries'] <- 'birthday-cake'
data_places$Icons[data_places$Industry..ANZSIC4..description == 'Takeaways'] <- 'archive'

#Create color column
data_places$Color <- 'lightred'
data_places$Color[data_places$Industry..ANZSIC4..description == 'Pubs, Taverns and Bars'] <- 'blue'
data_places$Color[data_places$Industry..ANZSIC4..description == 'Hotel restaurants'] <- 'purple'
data_places$Color[data_places$Industry..ANZSIC4..description == 'Bakeries'] <- 'red'
data_places$Color[data_places$Industry..ANZSIC4..description == 'Takeaways'] <- 'orange'

#Import City_Circle Dataset
data_circle_route <- st_read("city-circle-tram-route","city-circle-tram-route") #folder_based

#Import DATA_ALL_TRAM dataset
data_tram <- st_read("PTV","PTV_METRO_TRAM_STOP") #folder_based

#Top 20 places must eat filter
top20_filter <- c("Amiconi","Brunetti Classico Carlton","Conservatory",
                  "D'Penyetz & D'Cendol","Farmer's Daughters","Flower Drum",
                  "Gimlet", "Harajuku Crepes", "Higher Ground", "Hotel Windsor",
                  "Le Petit Gateau", "Matilda 159", "Pho Thin",
                  "Robata", "Section 8 Bar", "Seven Seeds", "Thai Town Melbourne", 
                  "The Heart of Carlton", "Grossi Florentino",
                  "Young & Jacksons")
data_top20places <- data_places[data_places$Trading.name %in% top20_filter, ]


#Picture in top 20
data_top20places$ImageURL[data_top20places$Trading.name == "Amiconi"] <- "https://menu.restaurantguru.com/m0/Melbourne-Amiconi-Restaurant-menu-2.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Brunetti Classico Carlton"] <- "https://whatson.melbourne.vic.gov.au/rails/active_storage/representations/proxy/eyJfcmFpbHMiOnsiZGF0YSI6ImYxODY4NTdiLWI2ZTctNDI4Ni1iODhlLWY5ZWVlM2JjYzQ5ZCIsInB1ciI6ImJsb2JfaWQifX0=--c4e7708836e92b5f65d6fc986efb4285b0053d06/eyJfcmFpbHMiOnsiZGF0YSI6eyJmb3JtYXQiOiJqcGciLCJncmF2aXR5IjoiQ2VudGVyIiwicmVzaXplX3RvX2ZpbGwiOls4ODAsNTkwXX0sInB1ciI6InZhcmlhdGlvbiJ9fQ==--8ba21a1f014d91c43eb97f346a0dc62d26bd59b2/ad6fb384-485e-4f08-99bd-4959d23c8d0e.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Conservatory"] <- "https://image-tc.galaxy.tf/wijpeg-a9veqx1uybml9tpod0wfih48t/file.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "D'Penyetz & D'Cendol"] <- "https://images.squarespace-cdn.com/content/v1/5645cfdfe4b03880a300e6e0/1617679552236-9NMENGA66T2JTJK2I0Q9/DSC04622.jpg?format=1500w"
data_top20places$ImageURL[data_top20places$Trading.name == "Farmer's Daughters"] <- "https://cdn.broadsheet.com.au/cache/51/35/5135e4b9e3ac7845844ba31538470c92.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Flower Drum"] <- "https://cdn.broadsheet.com.au/cache/54/4d/544d84a85cf3b2d73cc33e7a6ae1cae4.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Gimlet"] <- "https://cdn.concreteplayground.com/content/uploads/2020/06/SharynCairns_Gimlet_10-1-2880x1620.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Harajuku Crepes"] <- "https://media-cdn.tripadvisor.com/media/photo-s/12/13/56/02/20180213-202916-largejpg.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Higher Ground"] <- "https://highergroundmelbourne.com.au/wp-content/uploads/2023/07/BB_L_4.png"
data_top20places$ImageURL[data_top20places$Trading.name == "Hotel Windsor"] <- "https://images.otstatic.com/prod/27201608/2/large.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Le Petit Gateau"] <- "https://images.localista.com.au/eatingout/397387_lrg.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Seven Seeds"] <- "https://cdn.broadsheet.com.au/cache/65/55/6555fab98ba688f6c5636ab5a393f563.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Matilda 159"] <- "https://bestrestaurants.com.au/media/egclb4c0/4.jpg?width=1200&height=630&mode=crop&v=1d80999907c2150"
data_top20places$ImageURL[data_top20places$Trading.name == "Pho Thin"] <- "https://cdn.broadsheet.com.au/cache/44/68/4468e4b5cd8a21b1bf7beef3f803ef37.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Robata"] <- "https://img.delicious.com.au/ZdINtATR/w759-h506-cfill/del/2022/08/robata-interior-source-supplied-171900-2.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Section 8 Bar"] <- "https://cdn.broadsheet.com.au/cache/62/f1/62f1c207410f95cd631d6d403358d907.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Thai Town Melbourne"] <- "https://cdn.broadsheet.com.au/cache/f2/05/f205536370ad53cecc94ebe843435366.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "The Heart of Carlton"] <- "https://media-cdn.tripadvisor.com/media/photo-s/25/45/f8/f2/street-seating.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Grossi Florentino"] <- "https://offloadmedia.feverup.com/secretmelbourne.com/wp-content/uploads/2023/08/13224513/Grossi-Florentino_Mural-Room_Credit-Kate-Shanasy_6-1024x683.jpg"
data_top20places$ImageURL[data_top20places$Trading.name == "Young & Jacksons"] <- "https://content.api.news/v3/images/bin/e9adafe5fd18a2e1c16130b59c4c12d9"

#Top 20 places opening hour
data_top20places$Opening.hour[data_top20places$Trading.name == "Amiconi"] <- "Monday-Friday: 12-10pm, Saturday: 6-10pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Brunetti Classico Carlton"] <- "Monday-Thursday: 6am-10pm, Friday-Saturday: 6:30am-11:30pm, Sunday: 6am-10pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Conservatory"] <- "Monday-Friday: 12-2:30pm, 5:30-10pm, Saturday-Sunday: 12-3:30pm, 5:30-11pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "D'Penyetz & D'Cendol"] <- "Monday-Sunday: 11:30am-9:30pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Farmer's Daughters"] <- "Monday-Friday:: 12pm-12am, Saturday: 11:30am-12am, Sunday: 11:30am-10pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Flower Drum"] <- "Monday-Saturday: 12-3pm, 6-10:30pm, Sunday: 6-10:30pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Gimlet"] <- "Monday-Thursday: 12pm-12am, Friday-Saturday: 12pm-1am, Sunday: 12pm-12am"
data_top20places$Opening.hour[data_top20places$Trading.name == "Harajuku Crepes"] <- "Monday-Thursday: 10am-12am, Friday-Monday: 10am-1am"
data_top20places$Opening.hour[data_top20places$Trading.name == "Higher Ground"] <- "Monday-Friday: 7am-5pm, Saturday-Sunday: 7:30am-5pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Hotel Windsor"] <- "Monday-Saturday: 12-11pm, Sunday: 12-10pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Le Petit Gateau"] <- "Monday-Friday: 8am-4pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Seven Seeds"] <- "Monday-Friday: 7am-5pm, Saturday-Sunday: 8am-5pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Matilda 159"] <- "Monday-Thursday: 6-10pm, Friday: 12-2:30pm, 6-11pm, Saturday-Sunday: 8:30-2:30pm, 6-10pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Pho Thin"] <- "Monday-Sunday: 10:30am-8:30pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Robata"] <- "Tuesday-Saturday: 12-11pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Section 8 Bar"] <- "Sunday-Wednesday: 12-11pm, Thursday-Saturday: 12pm-1am"
data_top20places$Opening.hour[data_top20places$Trading.name == "Thai Town Melbourne"] <- "Sunday-Thursday: 11:30am-3pm, 5-9pm, Friday-Saturday: 11:30am-3pm, 5-9:30pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "The Heart of Carlton"] <- "Monday-Saturday: 6am-6pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "The Savoy Hotel Melbourne"] <- "Monday-Saturday: 9am-10pm"
data_top20places$Opening.hour[data_top20places$Trading.name == "Young & Jacksons"] <- "Monday-Sunday: 10am-12am"


# 
##################
# USER INTERFACE #
##################

#Map tab
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$style(HTML("
    body {
      background-color: #F5F5DC; /* Change to your desired background color */
    }
  ")),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap"),  # Link to Google Font
    tags$style(HTML("
      body, h1, h2, h3, h4, h5, h6, p, a, span, div, input, button {
        font-family: 'Roboto', sans-serif !important;
      }
    "))
  ),
  fluidRow(
    column(4, 
           h4(strong("Enter a Place", style = "color: #137c54;")), 
           fluidRow(
             column(8, textInput("place_search", NULL)),
             column(4, actionButton("search", "Search"), style = "height:5px;")
           )
    ),
    column(4, 
           h4(strong("Food Place Category", style = "color: #137c54;")), 
           selectInput('place', NULL, 
                       choices = c(sort(unique(data_places$Industry..ANZSIC4..description))),
                       selected = 'Cafes and Restaurants',
                       multiple = TRUE)
    ),
    column(4, 
           h4(strong("Transport", style = "color: #137c54;")), 
           fluidRow(
             column(6, checkboxInput('city_circle', 'City Circle Route', value = FALSE)),
             column(6, checkboxInput('all_tram', 'All Tram Stations', value = FALSE))
           )
    ),
    fluidRow(
      column(12),
      fluidRow( column(8, leafletOutput('map_eat', height = 345), uiOutput('controls')),
                column(4, h4(strong("Top 20 Food Places to Try", style = "color: #137c54;")), uiOutput("attractionList")
                )
      )
    )
  )
)  



################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  #Catergory filter
  getFilteredData <- reactive({
    req(input$place)  # Ensure input$category is not NULL
    filter( dplyr::filter(data_places, Industry..ANZSIC4..description %in% input$place))
  })
  
  
  
  
  #main map
  output$map_eat <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 16) %>%
      addAwesomeMarkers(data = getFilteredData(),
                        lng = ~Longitude, lat = ~Latitude,
                        icon = awesomeIcons(library = 'fa',
                                            icon = ~Icons,  
                                            markerColor = ~Color
                        ),
                        label = lapply(1:nrow(getFilteredData()), function(i) {
                          HTML(paste0(
                            "<b>", getFilteredData()$Trading.name[i], "</b><br>",
                            "<b>Seat type: </b>", getFilteredData()$Seating_type[i], "<br>",
                            "<b>Seat capacity: </b>", getFilteredData()$Number_of_seats[i], "<br>",
                            "<b>Category: </b>", getFilteredData()$Industry..ANZSIC4..description[i], "<br>",
                            "<b>Address: </b>", getFilteredData()$Business.address[i]
                          ))
                        }),
                        clusterOptions = markerClusterOptions(
                          maxClusterRadius = 25,
                          disableClusteringAtZoom = 50,
                          iconCreateFunction = JS("function(cluster) {
                        return new L.DivIcon({
                            html: '<div style=\"background-color: lightgrey; color: black; border-radius: 50%; text-align: center; line-height: 30px; width: 30px; height: 30px; font-size: 14px;\">' + cluster.getChildCount() + '</div>',
                            className: 'my-cluster-icon',
                            iconSize: L.point(30, 30)
                        });
                    }")
                        ), group = 'Original Map'
      )
    
  })
  
  ## For Top 20 Landmark
  output$attractionList <- renderUI({
    tagList(
      div(
        style = "display: flex; flex-wrap: wrap;",  # Use flexbox for layout
        lapply(1:min(20, nrow(data_top20places)), function(i) {  # Limit to the first 20 items
          link <- actionLink(paste0("link_", i), data_top20places$Trading.name[i],
                             title = paste("Click to view", data_top20places$Trading.name[i]))
          
          div(
            style = "width: 50%; margin-bottom: 5px",  # 100% / 2 columns = 50% width each
            link,
            tags$br()
          )
        })
      )
    )
  })
  
  ## Transport check box
  observe({leafletProxy("map_eat") %>%
      clearGroup("City Circle") %>%
      clearGroup("All Tram Station") #%>%
    
    
    input$place
    circle_image <- "https://seniorsinmelbourne.com.au/wp-content/uploads/2024/03/City_Circle_Tram_Melbourne-26.jpg"
    
    if(input$city_circle) {
      leafletProxy("map_eat") %>%
        addPolylines(data = data_circle_route,color = "darkgreen", weight = 5, 
                     label = HTML(paste0("<b>", "City Circle", "</b><br>",
                                         "<img src='", circle_image , "' style='width:100px;height:auto;'>",
                                         "<p>The City Circle (Melbourne tram route 35) is a zero-fare tram running around the Melbourne central<br>
                                       business district in Australia. Running along the city centre's outermost thoroughfares, the route passes<br>
                                       many Melbourne attractions, including Parliament House, the Old Treasury Building and the developing<br>
                                       Docklands waterfront precinct. Since October 2023, it operates in a clockwise direction only.</p>")), 
                     group = "City Circle")
    }
    
    icon <- makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/en/e/ef/Melbourne_tram_logo.svg",
      iconWidth = 20, iconHeight = 50,
      iconAnchorX = 15, iconAnchorY = 54,
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    if(input$all_tram) {
      leafletProxy("map_eat") %>%
        addMarkers(data = data_tram,
                   lng = ~LONGITUDE, lat = ~LATITUDE, 
                   icon = icon, 
                   label = lapply(1:nrow(data_tram), function(i) {
                     HTML(paste0(
                       "<b>", data_tram$STOP_NAME[i], "</b>",
                       "<p> Tram is an easily accessible form of public transport <br>
                       to see all the best attractions, venues, and shopping precints across Melbourne. </p>"
                     ))
                   }),
                   group = "All Tram Station")
    }
    
    
  })
  
  #Zoom Out Button
  #Source: https://stackoverflow.com/questions/48568256/how-to-reset-a-leaflet-click-event-in-shiny
  output$controls <- renderUI({
    req(input$map_eat_marker_click)
    
    absolutePanel(
      id = "controls", top = 10, right = 25, 
      left = "auto", bottom = "auto", width = "auto", height = "auto",
      
      actionButton(inputId = "reset", label = "Zoom Out", 
                   class = "btn-primary", 
                   style = "font-size: 10px; padding: 5px 5px; 
                          width: 70px; height: 40px;")
    )
  })  
  
  observeEvent(input$reset, {
    
    #hiding the control button
    #hide('controls')
    
    # resetting the map
    
    leafletProxy("map_eat") %>%
      clearGroup('Nearest Spots') %>%
      clearGroup('Matched Place') %>%
      clearGroup('Top 20 Places') %>%
      clearMarkerClusters() %>%
      clearGroup('Reset View') %>%
      addAwesomeMarkers(data = getFilteredData(),
                        lng = ~Longitude, lat = ~Latitude,
                        icon = awesomeIcons(library = 'fa',
                                            icon = ~Icons,  
                                            markerColor = ~Color
                        ),
                        label = lapply(1:nrow(getFilteredData()), function(i) {
                          HTML(paste0(
                            "<b>", getFilteredData()$Trading.name[i], "</b><br>",
                            "<b>Seat type: </b>", getFilteredData()$Seating_type[i], "<br>",
                            "<b>Seat capacity: </b>", getFilteredData()$Number_of_seats[i], "<br>",
                            "<b>Category: </b>", getFilteredData()$Industry..ANZSIC4..description[i], "<br>",
                            "<b>Address: </b>", getFilteredData()$Business.address[i]
                          ))
                        }),
                        clusterOptions = markerClusterOptions(
                          maxClusterRadius = 25,
                          disableClusteringAtZoom = 50,
                          iconCreateFunction = JS("function(cluster) {
                        return new L.DivIcon({
                            html: '<div style=\"background-color: lightgrey; color: black; border-radius: 50%; text-align: center; line-height: 30px; width: 30px; height: 30px; font-size: 14px;\">' + cluster.getChildCount() + '</div>',
                            className: 'my-cluster-icon',
                            iconSize: L.point(30, 30)
                        });
                    }")
                        ), group = 'Reset View'
      ) %>%
      setView("map_wheretogo", 
              lng = 144.9640, # coordinates of City of Melbourne
              lat = -37.8162, 
              zoom = 12)
  })
  
  
  
  #Showing nearest spots in radius 200m
  observeEvent(input$map_eat_marker_click, {
    click_lat <- input$map_eat_marker_click$lat
    click_lng <- input$map_eat_marker_click$lng
    
    df <- data.frame(lat = click_lat, long = click_lng)
    
    nearest_spots <- spatialrisk::points_in_circle(data_places, df$lon[1], df$lat[1], 
                                                   radius = 200, lon = Longitude, lat = Latitude)
    
    
    if(!is.null(input$map_eat_marker_click)) {
      leafletProxy("map_eat") %>%
        clearMarkerClusters() %>%
        clearGroup("Top 20 Places") %>%
        clearGroup("Matched Place") %>%
        clearGroup("Nearest Spots") %>%
        clearGroup('Reset View') %>%
        addAwesomeMarkers(data = nearest_spots,
                          lng = ~Longitude, lat = ~Latitude,
                          icon = awesomeIcons(library = 'fa',
                                              icon = ~Icons,  
                                              markerColor = ~Color
                          ),
                          label = lapply(1:nrow(nearest_spots), function(i) {
                            HTML(paste0(
                              "<b>", nearest_spots$Trading.name[i], "</b><br>",
                              "<b>Seat type: </b>", nearest_spots$Seating_type[i], "<br>",
                              "<b>Seat capacity: </b>", nearest_spots$Number_of_seats[i], "<br>",
                              "<b>Category: </b>", nearest_spots$Industry..ANZSIC4..description[i], "<br>",
                              "<b>Address: </b>", nearest_spots$Business.address[i]
                            ))
                          }),
                          clusterOptions = markerClusterOptions(
                            maxClusterRadius = 15,
                            disableClusteringAtZoom = 24,
                            iconCreateFunction = JS("function(cluster) {
                        return new L.DivIcon({
                            html: '<div style=\"background-color: lightgrey; color: black; border-radius: 50%; text-align: center; line-height: 30px; width: 30px; height: 30px; font-size: 14px;\">' + cluster.getChildCount() + '</div>',
                            className: 'my-cluster-icon',
                            iconSize: L.point(30, 30)
                        });
                    }")
                          ),
                          group = 'Nearest Spots'
        ) %>% 
        setView("map_eat", lng = input$map_eat_marker_click$lng, 
                lat = input$map_eat_marker_click$lat, zoom = 17) 
    }
  })
  
  #Top 20 places list
  observe({
    leafletProxy("map_eat") %>%
      clearGroup("Top 20 Places")
    
    top20icon <- makeIcon(
      iconUrl = "https://www.svgrepo.com/show/301687/recommended-like.svg",
      iconWidth = 47, iconHeight = 47,
      iconAnchorX = 25, iconAnchorY = 45,
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    for (i in 1:nrow(data_top20places)) {
      local({
        my_i <- i
        observeEvent(input[[paste0("link_", my_i)]], {
          leafletProxy("map_eat") %>%
            clearMarkerClusters() %>%
            clearGroup("Top 20 Places") %>%
            clearGroup("Matched Place") %>%
            clearGroup("Nearest Spots") %>%
            clearGroup('Reset View') %>%
            addMarkers(
              data = data_top20places,
              lng = ~Longitude[my_i], 
              lat = ~Latitude[my_i], 
              icon = top20icon,
              label = HTML(paste0(
                "<b>",data_top20places$Trading.name[my_i], "</b><br>",
                "<img src='", data_top20places$ImageURL[my_i], "' width='200' height='150'><br>",
                "<b>Seat type: </b>", data_top20places$Seating_type[my_i], "<br>",
                "<b>Seat capacity: </b>", data_top20places$Number_of_seats[my_i], "<br>",
                "<b>Address: </b>", data_top20places$Business.address[my_i],"<br>",
                "<b>Category: </b>", data_top20places$Industry..ANZSIC4..description[my_i], "<br>",
                "<b>Opening Hour: </b>", data_top20places$Opening.hour[my_i], "<br>"
              )),
              clusterOptions = markerClusterOptions(
                maxClusterRadius = 15,
                disableClusteringAtZoom = 24,
                iconCreateFunction = JS("function(cluster) {
                        return new L.DivIcon({
                            html: '<div style=\"background-color: lightgrey; color: black; border-radius: 50%; text-align: center; line-height: 30px; width: 30px; height: 30px; font-size: 14px;\">' + cluster.getChildCount() + '</div>',
                            className: 'my-cluster-icon',
                            iconSize: L.point(30, 30)
                        });
                    }")
              ),
              group = "Top 20 Places"
            ) %>%
            setView(lng = data_top20places$Longitude[my_i], 
                    lat = data_top20places$Latitude[my_i], zoom = 16) 
          
        })
      })
    }
  })
  
  
  observeEvent(input$search, {
    req(input$place_search)
    matched_place <- data_places %>%
      filter(str_detect(tolower(Trading.name), tolower(input$place_search)))
    
    
    if (nrow(matched_place) > 0) {
      # If more than one match, take the first one
      if (nrow(matched_place) > 1) {
        matched_place <- matched_place[1, ]
      }
      
      # Zoom into the matched place
      leafletProxy("map_eat") %>%
        setView(lng = matched_place$Longitude, lat = matched_place$Latitude, zoom = 22) %>%
        clearMarkerClusters() %>%
        clearGroup("Top 20 Places") %>%
        clearGroup("Matched Place") %>%
        clearGroup("Nearest Spots") %>%
        clearGroup('Reset View') %>%
        addAwesomeMarkers(data = matched_place,
                          lng = ~Longitude, lat = ~Latitude,
                          icon = awesomeIcons(library = 'fa',
                                              icon = ~Icons,  
                                              markerColor = ~Color,
                                              spin = TRUE
                          ),
                          label = HTML(paste0(
                            "<b>", matched_place$Trading.name, "</b><br>",
                            "<b>Seat type: </b>", matched_place$Seating_type, "<br>",
                            "<b>Seat capacity: </b>", matched_place$Number_of_seats, "<br>",
                            "<b>Category: </b>", matched_place$Industry..ANZSIC4..description, "<br>",
                            "<b>Address: </b>", matched_place$Business.address
                          )), 
                          clusterOptions = markerClusterOptions(
                            maxClusterRadius = 15,
                            disableClusteringAtZoom = 24,
                            iconCreateFunction = JS("function(cluster) {
                        return new L.DivIcon({
                            html: '<div style=\"background-color: lightgrey; color: black; border-radius: 50%; text-align: center; line-height: 30px; width: 30px; height: 30px; font-size: 14px;\">' + cluster.getChildCount() + '</div>',
                            className: 'my-cluster-icon',
                            iconSize: L.point(30, 30)
                        });
                    }")
                          ),
                          group = 'Matched Place')
      
    } else {
      # Optionally, you could add an alert or message indicating no match was found
      showNotification("No matching place found.", type = "error")
    }
    
  })
  
  
  
  
  
}

#############
# RUN SHINY #
#############

shinyApp(ui, server, options=list(port=9090))