## Add comment to explain the code

library(tidyr)
library(leaflet)
library(sf)
library(shiny)
library(dplyr)
library(shinyjs)
library(shinythemes)
library(stringr)

## Read landmark and POIs data
landmark <- read.csv('melbourne-landmarks-pois.csv')
openinghours <- read.csv('Top20openinghours.csv')

landmark <- landmark %>% 
  mutate(lat = as.numeric(sub(",.*", "", Co.ordinates)),
         lon = as.numeric(sub(".*, ", "", Co.ordinates)))

## Combining similar theme into one category
landmark$Category <- landmark$Theme
landmark$Category[landmark$Theme == 'Purpose Built'] <- 'Leisure/Recreation'
landmark$Category[landmark$Theme == 'Retail'] <- 'Mixed Use'

## Changing category name
landmark$Category[landmark$Theme == 'Office'] <- 'Institutional'

category_filter <- c('Leisure/Recreation', 'Education Centre', 'Mixed Use', 'Institutional',
                     'Place Of Assembly', 'Place of Worship', 'Transport',
                     'Community Use')

landmark <- landmark[landmark$Category %in% category_filter, ] ## Filtering the selected categories

## Source: https://rstudio.github.io/leaflet/articles/markers.html
landmarkIcons <- iconList(
  'Leisure/Recreation' = makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/427542/ecology-forest-garden-2.svg', #'https://www.svgrepo.com/show/261476/garden-tree.svg',
    iconWidth = 40, iconHeight = 60
  ),
  'Mixed Use' = makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/43150/store.svg', #'https://www.svgrepo.com/show/415684/ecommerce-market-marketplace.svg'
    iconWidth = 40, iconHeight = 50
  ),
  'Institutional' <- makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/317725/office-building.svg',
    iconWidth = 40, iconHeight = 60
  ),
  'Place Of Assembly' <- makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/180397/museum-business.svg', #'https://www.svgrepo.com/show/476902/museum.svg',
    iconWidth = 40, iconHeight = 60
  ),
  'Place of Worship' <- makeIcon(
    iconUrl = 'https://upload.wikimedia.org/wikipedia/commons/a/a8/Churchblue.svg',
    iconWidth = 40, iconHeight = 60
  ),
  'Transport' <- makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/476803/tram.svg', #'https://www.svgrepo.com/show/476798/train.svg',
    iconWidth = 40, iconHeight = 60
  ),
  'Community Use' <- makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/476902/museum.svg', #'https://www.svgrepo.com/show/476852/city-hall.svg',
    iconWidth = 40, iconHeight = 60
  ),
  'Education Centre' <- makeIcon(
    iconUrl = 'https://www.svgrepo.com/show/476809/us-capitol.svg',
    iconWidth = 40, iconHeight = 60
  )
)

names(landmarkIcons) <- c('Leisure/Recreation', 'Mixed Use', 'Institutional', 
                          'Place Of Assembly', 'Place of Worship', 'Transport', 
                          'Community Use', 'Education Centre')

## Data City Circle
city_circle_route <- st_read("city-circle-tram-route","city-circle-tram-route")


## DATA_ALL_TRAM
data_tram <- st_read("PTV","PTV_METRO_TRAM_STOP")

## Top 20 data
top20_filter <- c("Melbourne Cricket Ground (MCG)", "Federation Square", 
                  "Melbourne Museum", "Royal Exhibition Building", 
                  "Melbourne Exhibition Centre","DFO South Wharf", 
                  "NGV International", "Royal Botanic Gardens",
                  "Queen Victoria Market", "Parliament House", 
                  "Crown Entertainment Complex", "Shrine of Remembrance", 
                  "Melbourne Aquarium", "Eureka Skydeck 88", 
                  "Flinders Street Railway Station", "State Library Victoria",
                  "Fitzroy Gardens", "Victorian Arts Centre", 
                  "Melbourne Town Hall", "St Patricks Cathedral")

data_top20places <- landmark[landmark$Feature.Name %in% top20_filter, ]
data_top20places <- data_top20places %>%
  left_join(openinghours, by = "Feature.Name")

## Picture in top 20 attractions
data_top20places$ImageURL[data_top20places$Feature.Name == "Melbourne Cricket Ground (MCG)"] <- "https://www.austadiums.com/stadiums/photos/MCG-aerial-2021.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "Federation Square"] <- " https://www.melbournecb.com.au/-/media/images-mcb/blog/federation-square-the-place-to-meet.jpg?ts=20230615350946"
data_top20places$ImageURL[data_top20places$Feature.Name == "Melbourne Museum"] <- " https://whatson.melbourne.vic.gov.au/rails/active_storage/representations/proxy/eyJfcmFpbHMiOnsiZGF0YSI6IjJlNzdlMzFlLWVjOWUtNDU3NC1iZTUwLTM3NzkxM2E5NTRmNiIsInB1ciI6ImJsb2JfaWQifX0=--3fdfdc4926d9a728038cf29932b5d2709004f726/eyJfcmFpbHMiOnsiZGF0YSI6eyJmb3JtYXQiOiJqcGciLCJncmF2aXR5IjoiQ2VudGVyIiwicmVzaXplX3RvX2ZpbGwiOls4ODAsNTkwXX0sInB1ciI6InZhcmlhdGlvbiJ9fQ==--8ba21a1f014d91c43eb97f346a0dc62d26bd59b2/Melbourne-Museum-1.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "Royal Exhibition Building"] <- " https://cdn.sightseeingtoursaustralia.com.au/wp-content/uploads/2022/04/800px-Royal_Exhibition_Building_Melbourne_Australia_7-9.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "Melbourne Exhibition Centre"] <- " https://dentoncorkermarshall.com/wp-content/uploads/2022/11/Melbourne_Exhibition_Centre_03-1024x701.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "DFO South Wharf"] <- " https://a.storyblok.com/f/129601/1920x1282/99de85f49d/dfo-south-wharf_exterior-purplesky.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "NGV International"] <- " https://curateyourownadventure.com/wp-content/uploads/2024/04/436868713_972001217662722_4862347256990987730_n-1.jpg?w=1200"
data_top20places$ImageURL[data_top20places$Feature.Name == "Royal Botanic Gardens"] <- " https://www.visitvictoria.com/-/media/atdw/melbourne/things-to-do/tours/2f954df910f8e89c341a4728aee0c426_1600x1200.jpeg?ts=20210517310409"
data_top20places$ImageURL[data_top20places$Feature.Name == "Queen Victoria Market"] <- " https://live-production.wcms.abc-cdn.net.au/05778b5befdff57090d17e06fa2b7113?impolicy=wcms_crop_resize&cropH=962&cropW=1712&xPos=77&yPos=236&width=862&height=485"
data_top20places$ImageURL[data_top20places$Feature.Name == "Parliament House"] <- " https://mediaim.expedia.com/destination/9/f9c1b40c0ba2d2d2dc9d47dd7958132c.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "Crown Entertainment Complex"] <- " https://legendsinconcert.com/wp-content/uploads/2023/09/melbourne.webp"
data_top20places$ImageURL[data_top20places$Feature.Name == "Shrine of Remembrance"] <- " https://www.shrine.org.au/sites/default/files/2020-08/Glenn%20Foster%202_web%20hero.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "Melbourne Aquarium"] <- " https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgI96uTGsXTjBw146Armx2UCSihbb4ks-0zmmelVWSPiszDooiAHEkyX1gsd8HUwQFNFmHZ1NaRhbSe7goKmu4ppCZPtHzImHgX_fcbucuAYE-y0SlnG85dJ6lN6nFvq-EiPt_IeBJ8pVw/s1600/DSCF3962.JPG"
data_top20places$ImageURL[data_top20places$Feature.Name == "Eureka Skydeck 88"] <- " https://www.melbourneskydeck.com.au/wp-content/uploads/2021/12/Melbourne-Skydeck-Hero-Eureka-Tower-Skyline-1-1.jpg?w=400&h=267&dpr=2.625&q=25&auto=format"
data_top20places$ImageURL[data_top20places$Feature.Name == "Flinders Street Railway Station"] <- " https://upload.wikimedia.org/wikipedia/commons/6/63/Melbourne_Flinders_St._Station.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "State Library Victoria"] <- " https://dynamic-media-cdn.tripadvisor.com/media/photo-o/15/23/c6/c4/state-library-victoria.jpg?w=1200&h=-1&s=1"
data_top20places$ImageURL[data_top20places$Feature.Name == "Fitzroy Gardens"] <- " https://whatson.melbourne.vic.gov.au/rails/active_storage/representations/proxy/eyJfcmFpbHMiOnsiZGF0YSI6IjdhOTQ3YmU1LWNjZGMtNGE1MC1hY2U3LWUyYzEyNzkyZTU0MSIsInB1ciI6ImJsb2JfaWQifX0=--a44a71cbfeb157bb968d4a71179db68f25bb5094/eyJfcmFpbHMiOnsiZGF0YSI6eyJmb3JtYXQiOiJqcGciLCJyZXNpemVfdG9fbGltaXQiOlsxMDAwLDYwMF19LCJwdXIiOiJ2YXJpYXRpb24ifX0=--0d1dec94e96bf59e4e90ca4a7c11e516560ab297/Fitzroy-Gardens-1%20(1).jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "Victorian Arts Centre"] <- " https://www.visitvictoria.com/-/media/atdw/melbourne/see-and-do/art-and-culture/performing-arts/cf0d49bf61e0a578812b674d4605de0e_1600x1200.jpeg?ts=20240716300714"
data_top20places$ImageURL[data_top20places$Feature.Name == "Melbourne Town Hall"] <- " https://images.squarespace-cdn.com/content/v1/569c3f35a976af8774edeb54/95f8c9f8-66f2-456c-b275-25de122c4491/1.1I5A6680.jpg"
data_top20places$ImageURL[data_top20places$Feature.Name == "St Patricks Cathedral"] <- " https://d2uvk4nnl9ckw6.cloudfront.net/general/Banners/Our-cathedral.jpg"
data_top20places$description[data_top20places$Feature.Name == "Melbourne Cricket Ground (MCG)"] <- "The Melbourne Cricket Ground (MCG), is a sports stadium located in Yarra Park, Melbourne, Victoria. Founded and managed by the Melbourne Cricket Club, it is the largest stadium in the Southern Hemisphere, the 11th largest globally, and the second-largest cricket arena by capacity. The MCG is within walking distance of the Melbourne CBD and is served by Richmond and Jolimont railway stations, as well as the route 70, 75, and 48 trams."
data_top20places$description[data_top20places$Feature.Name == "Federation Square"] <- "Situated along the Yarra River (the land around Birrarung) and opposite the iconic Flinders Street Station, Federation Square is the centre of creativity in a city that's built around innovation, art, leafy green spaces, a flowing river and contemporary cultural activities and festivals. The gathering place's unique architectural shape has seen the square recognised on the Victorian Heritage Register, making it the youngest building in Australian history to gain heritage status."
data_top20places$description[data_top20places$Feature.Name == "Melbourne Museum"] <- "Melbourne Museum explores life in Victoria, from the natural environment to the culture and history. Located in Carlton Gardens, opposite the historic Royal Exhibition Building, the award-winning Melbourne Museum houses a permanent collection in eight galleries, including one just for children. Highlights include a complete skeleton of a blue whale, the Bunjilaka Aboriginal Cultural Centre, a living rainforest, the racehorse Phar Lap and an IMAX theatre on site."
data_top20places$description[data_top20places$Feature.Name == "Royal Exhibition Building"] <- "The World Heritage listed Royal Exhibition Building has been a main character in the story of Melbourne for over 140 years. It is a monumental structure that stands in the Carlton Gardens as a testimony to the Victorian spirit of enterprise and industry in late nineteenth century Australia. As the venue for many events, the building and garden surrounds represent an important link with Australia’s cultural life."
data_top20places$description[data_top20places$Feature.Name == "Melbourne Exhibition Centre"] <- "The Melbourne Exhibition Centre is a group of three adjacent buildings next to the Yarra River in South Wharf. It was opened in 1996 and is known colloquially as 'Jeff's Shed' after the then Victorian Premier, Jeff Kennett. With 40,000 sqm of pillarless exhibition space and easy loading access, the functional design, innovative solutions, and purpose-built spaces make the venue ideal for any audience type."
data_top20places$description[data_top20places$Feature.Name == "DFO South Wharf"] <- "DFO South Wharf is a multi-level Outlet Centre located on the Yarra River close to Docklands, on the south-western fringe of Melbourne's CBD. The centre comprises more than 145 outlet retailers including Armani Outlet, Polo Ralph Lauren, Calvin Klein, Tommy Hilfiger, BOSS, Coach and Michael Kors, as well as key sporting and athleisure retailers, Nike, Adidas, ASICS and PUMA."
data_top20places$description[data_top20places$Feature.Name == "NGV International"] <- "The National Gallery of Victoria is Australia’s favourite gallery. NGV International houses a whole world of international art, displaying the National Gallery of Victoria's collections of European, Asian, Oceanic and American art. A truly iconic Melbourne building, the gallery has been totally redesigned to house one of the most impressive collections in the Southern Hemisphere."
data_top20places$description[data_top20places$Feature.Name == "Royal Botanic Gardens"] <- "Royal Botanic Gardens Victoria is home to the State Botanical Collection, which is housed in the National Herbarium of Victoria. The collection, which includes 1.5 million preserved plants, algae and fungi, represents the largest herbarium collection in Australia and wider Oceania. It also includes Australia's most comprehensive botanical library."
data_top20places$description[data_top20places$Feature.Name == "Queen Victoria Market"] <- "The Queen Victoria Market (QVM) has been the heart and soul of Melbourne for more than a century. An historic landmark spread over two city blocks, it’s a vibrant and bustling inner-city market where you can shop at over 600 small businesses for everything from Australian fruit and vegetables, local and imported gourmet foods, clothing and souvenirs. The Market is open five days a week: Tuesday, Thursday, Friday, Saturday and Sunday."
data_top20places$description[data_top20places$Feature.Name == "Parliament House"] <- "The meeting place of the Victorian Government, Parliament House is one of Melbourne's best known landmarks. Facing the intersection of Spring and Bourke streets, the heritage building's facade, sweeping steps, elegant lamps and grand colonnade are an impressive sight. The inside is no less grand, with the splendid architecture of the Legislative Assembly, Legislative Council, Queen's Hall and library."
data_top20places$description[data_top20places$Feature.Name == "Crown Entertainment Complex"] <- "The Crown Entertainment Complex is the largest single building in Australia. There is always something happening at Crown from live entertainment to themed displays and riverside festivals, Crown truly is A World of Entertainment. Located on the south bank of the Yarra River, Crown Melbourne has a casino, three hotels, function rooms, award winning restaurants and shopping and entertainment facilities."
data_top20places$description[data_top20places$Feature.Name == "Shrine of Remembrance"] <- "The Shrine of Remembrance was founded in 1934 as the National War Memorial of Victoria. It was built to honour the men and women of Victoria who served in World War I, but now functions as a memorial to all Australians who have served in any war. It is a site of annual observances for Anzac Day (25 April) and Remembrance Day (11 November), and is one of the largest war memorials in Australia."
data_top20places$description[data_top20places$Feature.Name == "Melbourne Aquarium"] <- "SEA LIFE Melbourne, located on the Yarra waterfront opposite the Crown Casino, is one of Victoria's leading visitor attractions and a great outing for the whole family. Situated on the riverside, just opposite the Crown Casino, SEA LIFE Melbourne is home to thousands of animals, including sharks, turtles, stingrays, sawfish, tropical fish and much, much more."
data_top20places$description[data_top20places$Feature.Name == "Eureka Skydeck 88"] <- "The observation deck (Melbourne Skydeck) occupies the entire 88th floor of the Eureka Tower and is the highest public vantage point in a building in the Southern Hemisphere at 285 m (935 ft). The Skydeck features twenty viewfinders that help visitors to pinpoint numerous significant landmarks around all parts of Melbourne, along with several free binoculars. There is a glass cube called The Edge, which extends itself 3 metres from the building to hang over the edge of the tower and add to the viewing experience."
data_top20places$description[data_top20places$Feature.Name == "Flinders Street Railway Station"] <- "Flinders Street Station is Australia’s oldest train station, and with its prominent green copper dome, distinctive yellow facade, arched entrance, tower, and clocks, it is one of Melbourne's most recognisable landmarks. The station was completed in 1910, and the upper floors were purpose-built to house a library, gym, and lecture hall, later used as a ballroom. Its 708-metre main platform is the fourth longest railway platform in the world."
data_top20places$description[data_top20places$Feature.Name == "State Library Victoria"] <- "State Library Victoria is one of Melbourne's founding institutions, established 168 years ago in 1854. It opened as the Melbourne Public Library, and is not only Australia's oldest public library but also one of the first free public libraries in the world. The Library was created as 'the people's university' – a place of learning and discovery for all Victorians."
data_top20places$description[data_top20places$Feature.Name == "Fitzroy Gardens"] <- "Fitzroy Gardens is one of Melbourne’s most historic and beautiful gardens. The layout follows a classic Victorian era design, featuring pathways lined with magnificent elm trees, known to be some of the best grown in Victoria. There are a variety of flowers and ornamental shrubs and trees, which together with extensive lawns creates a diverse and layered landscape."
data_top20places$description[data_top20places$Feature.Name == "Victorian Arts Centre"] <- "With its iconic spire, Arts Centre Melbourne is both an architectural and cultural landmark in Melbourne. It is the flagship of the performing arts in the state of Victoria and the focal point of the cultural precinct in Melbourne. Housing an array of venues, Arts Centre Melbourne is Australia's largest and busiest performing arts venue, staging more than 4,400 performances and public events each year."
data_top20places$description[data_top20places$Feature.Name == "Melbourne Town Hall"] <- "Completed in 1870, the Melbourne Town Hall is at the heart of the city’s cultural and civic activity - many of the decisions that have helped shape Melbourne were made within this heritage-listed building. A mix of bluestone and Tasmanian freestone, the classically designed building features fine masonry, a clock tower, grand council chambers, a large auditorium with a grand organ, and an impressive portico most notable for being the spot where the Beatles waved to crowds in 1964."
data_top20places$description[data_top20places$Feature.Name == "St Patricks Cathedral"] <- "St. Patrick’s Cathedral is a cathedral church belonging to the Roman Catholic Archdiocese of Melbourne. The style of the cathedral falls into the category of Gothic-revival, and it is the tallest and overall largest church edifice in the whole of Australia."

##################
# USER INTERFACE #
##################

wheretogo_tab <- tabPanel(
  title = '',
  fluidRow(column(4, h4(strong("Enter a Place", style = "color: #137c54;")), 
                  fluidRow(column(8, textInput("place_search", NULL)), 
                           column(4, actionButton("search", "Search")))), style = "height:5px;"),
  column(4,
         h4(strong("Place Category", style = "color: #137c54;")),
         selectInput(
           inputId = 'category',
           label = NULL,
           choices = c(sort(unique(landmark$Category))),
           selected = 'Community Use',
           multiple = TRUE
         )
  ), column(4,
            h4(strong("Transport", style = "color: #137c54;")),
            fluidRow(
              column(6, checkboxInput('city_circle', 'City Circle Route', value = FALSE)),
              column(6, checkboxInput('all_tram', 'All Tram Stations', value = FALSE))
            )),
  fluidRow(
    column(12),  # Adjust the width of the attractions list as needed
    fluidRow(column(8, leafletOutput('map_wheretogo', height = 345), uiOutput('controls')),
             column(4, h4(strong("Top 20 Places to Visit", style = "color: #137c54;")),
                    uiOutput("attractionList")
             )
    )
  )
)

ui <- fluidPage(useShinyjs(),
                id='mypage2',
                title='Attractions in the City of Melbourne',
                theme = shinytheme("flatly"),
                wheretogo_tab, 
                tags$style(HTML("
    body {
      background-color: #F5F5DC; 
    }
  ")),
               tags$head(
                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500&display=swap"),  ##Link to Google Font
                  tags$style(HTML("
        body, h1, h2, h3, h4, h5, h6, p, a, span, div, input, button {
        font-family: 'Roboto', sans-serif !important;
      }
    "))
                ) 
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  getFilteredData <- reactive({
    req(input$category)  ## Ensure input$category is not NULL
    
    dplyr::filter(landmark, Category %in% input$category)
  })
  
  output$map_wheretogo <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addMarkers(data = getFilteredData(),
                 lng = ~lon, lat = ~lat,
                 icon = ~landmarkIcons[Category],
                 label = lapply(1:nrow(getFilteredData()), function(i) {
                   HTML(paste0(
                     "<b>", getFilteredData()$Feature.Name[i], "</b><br>",
                     "<b>Theme: </b>", getFilteredData()$Theme[i], "<br>",
                     "<b>Sub Theme: </b>", getFilteredData()$Sub.Theme[i]
                   ))
                 }), group = "Original Map")
  })
  
  ## For Top 20 Landmark
  output$attractionList <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between;",  ## Use flexbox to create two columns
        # Left Column (First 10 items)
        div(
          style = "width: 45%;",  # Left column width
          lapply(1:10, function(i) {  # First 10 items
            link <- actionLink(paste0("link_", i), data_top20places$Feature.Name[i],
                               title = paste("Click to view", data_top20places$Feature.Name[i]))
            
            div(
              style = "margin-bottom: 5px;",  ## Add margin for spacing between items
              link,
              tags$br()
            )
          })
        ),
        
        ## Right Column (Next 10 items)
        div(
          style = "width: 45%;",  # Right column width
          lapply(11:min(20, nrow(data_top20places)), function(i) {  ## Next 10 items
            link <- actionLink(paste0("link_", i), data_top20places$Feature.Name[i],
                               title = paste("Click to view", data_top20places$Feature.Name[i]))
            
            div(
              style = "margin-bottom: 5px;",  ## Add margin for spacing between items
              link,
              tags$br()
            )
          }))))
  })
  
  observe({leafletProxy("map_wheretogo") %>%
      clearGroup("City Circle") %>%
      clearGroup("All Tram Station")  
    
    input$place
    circle_image <- "https://seniorsinmelbourne.com.au/wp-content/uploads/2024/03/City_Circle_Tram_Melbourne-26.jpg"
    
    if(input$city_circle) {
      leafletProxy("map_wheretogo") %>%
        addPolylines(data = city_circle_route,color = "darkgreen", weight = 5, 
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
      leafletProxy("map_wheretogo") %>%
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
  
  ## Source: https://stackoverflow.com/questions/48568256/how-to-reset-a-leaflet-click-event-in-shiny
  output$controls <- renderUI({
    req(input$map_wheretogo_marker_click)
    
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
    
    ## Resetting the map
    
    leafletProxy("map_wheretogo") %>%
      clearGroup('Nearest Spots') %>%
      clearGroup('Matched Place') %>%
      clearGroup('Top 20 Places') %>%
      clearGroup('Original Place') %>%
      clearGroup('Reset View') %>%
      addMarkers(data = getFilteredData(),
                 lng = ~lon, lat = ~lat,
                 icon = ~landmarkIcons[Category],
                 label = lapply(1:nrow(getFilteredData()), function(i) {
                   HTML(paste0(
                     "<b>", getFilteredData()$Feature.Name[i], "</b><br>",
                     "<b>Theme: </b>", getFilteredData()$Theme[i], "<br>",
                     "<b>Sub Theme: </b>", getFilteredData()$Sub.Theme[i]
                   ))}
                 ), group = 'Reset View') %>%
      setView("map_wheretogo", 
              lng = 144.9640, # coordinates of City of Melbourne
              lat = -37.8162, 
              zoom = 12)
  })
  
  ## Showing nearest spots in radius 500m
  observeEvent(input$map_wheretogo_marker_click, {
    click_lat <- input$map_wheretogo_marker_click$lat
    click_lng <- input$map_wheretogo_marker_click$lng
    
    df <- data.frame(lat = click_lat, long = click_lng)
    nearest_spots <- spatialrisk::points_in_circle(landmark, df$lon[1], df$lat[1], 
                                                   radius = 500, lon = lon)
    
    if(!is.null(input$map_wheretogo_marker_click)) {
      leafletProxy("map_wheretogo") %>%
        clearGroup('Matched Place') %>%
        clearGroup('Top 20 Places') %>%
        clearGroup('Original Map') %>%
        clearGroup('Nearest Spots') %>%
        clearGroup('Reset View') %>%
        addMarkers(data = nearest_spots,
                   lng = ~lon, lat = ~lat,
                   icon = ~landmarkIcons[Category],
                   label = lapply(1:nrow(nearest_spots), function(i) {
                     HTML(paste0(
                       "<b>", nearest_spots$Feature.Name[i], "</b><br>",
                       "<b>Theme: </b>", nearest_spots$Theme[i], "<br>",
                       "<b>Sub Theme: </b>", nearest_spots$Sub.Theme[i]
                     ))}
                   ),
                   group = 'Nearest Spots'
        ) %>% 
        setView("map_wheretogo", lng = input$map_wheretogo_marker_click$lng, 
                lat = input$map_wheretogo_marker_click$lat, zoom = 15) 
    }
  })
  
  observe({
    leafletProxy("map_wheretogo") %>%
      clearGroup("Top 20 Places") 
    
    for (i in 1:nrow(data_top20places)) {
      local({
        my_i <- i
        
        observeEvent(input[[paste0("link_", my_i)]], {
          iconToUse <- landmarkIcons[[data_top20places$Theme[my_i]]]
          leafletProxy("map_wheretogo") %>%
            clearGroup('Nearest Spots') %>%
            clearGroup('Matched Place') %>%
            clearGroup('Original Map') %>%
            clearGroup('Top 20 Places') %>%
            clearGroup('Reset View') %>%
            addMarkers(
              data = data_top20places,
              icon = iconToUse,
              lng = ~lon[my_i], 
              lat = ~lat[my_i], 
              label = HTML(paste(
                "<strong>", data_top20places$Feature.Name[my_i], "</strong><br>",
                "<img src='", data_top20places$ImageURL[my_i], "' width='200'><br>",
                "<strong>Theme: </strong>", data_top20places$Theme[my_i], "<br>",
                "<strong>Sub Theme: </strong>", data_top20places$Sub.Theme[my_i], "<br>",
                "<strong>Opening Hours: </strong>", data_top20places$Opening_Hours[my_i], "<br>",
                "<u><strong>Place Information</strong></u><br>",
                
                ## Generating description. Source: https://stackoverflow.com/questions/55314502/r-paste-after-strsplit-without-duplicates 
                ## and https://stackoverflow.com/questions/12361981/sapply-paste-before-at-beginning-of-string
                
                paste(sapply(1:length(unlist(strsplit(data_top20places$description[my_i], "\\s+"))), function(j) {
                  words <- unlist(strsplit(data_top20places$description[my_i], "\\s+"))
                  if (j %% 10 == 0) {
                    return(paste(words[j], "<br>"))
                  } else {
                    return(words[j])
                  }
                }), collapse = " ")
              )),
              group = "Top 20 Places"
            ) %>%
            setView(
              lng = data_top20places$lon[my_i],  # Set the view to the longitude of the selected place
              lat = data_top20places$lat[my_i],  # Set the view to the latitude of the selected place
              zoom = 16  # Adjust zoom level to focus on the selected place
            )
        })
      })
    }
  })
  
  
  observeEvent(input$search, {
    req(input$place_search) 
    
    ## Filter the dataset based on the search input
    matched_place <- landmark %>%
      filter(str_detect(tolower(Feature.Name), tolower(input$place_search)))
    
    if (nrow(matched_place) > 0) {
      ## If there are multiple matches, take the first one
      if (nrow(matched_place) > 1) {
        matched_place <- matched_place[1, , drop = FALSE]
      }
      
      ## Zoom into the matched place and update the map
      leafletProxy("map_wheretogo") %>%
        setView(lng = matched_place$lon, lat = matched_place$lat, zoom = 15) %>%
        clearGroup('Nearest Spots') %>%
        clearGroup('Top 20 Places') %>%
        clearGroup('Original Map') %>%
        clearGroup('Matched Place') %>%
        clearGroup('Reset View') %>%
        addMarkers(data = matched_place,
                   lng = ~lon, lat = ~lat,
                   icon = ~landmarkIcons[Category],
                   label = HTML(paste(
                     "<strong>", matched_place$Feature.Name, "</strong><br>",
                     "<img src='", matched_place$ImageURL, "' width='200'><br>",
                     "<strong>", "Theme: ", "</strong>",matched_place$Theme, "<br>",
                     "<strong>", "Sub Theme: ", "</strong>", matched_place$Sub.Theme
                   )), group = 'Matched Place')
    } else {
      ## Show a notification if no matching place was found
      showNotification("No matching place found.", type = "error")
    }
  })
  
}


#############
# RUN SHINY #
#############

shinyApp(ui, server, options=list(port=9898))
