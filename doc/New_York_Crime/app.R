#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("leaflet")
library("shinyWidgets")
library("jsonlite")
library("httr")
library("rgdal")
library("geojsonio")
library("shiny")
library("geosphere")
library("dplyr")
library("ggmap")
library("stringr")
library("htmlwidgets")


Crime_Data <- read.csv("../../data/NYC_Crime_Data/NYC_crime.csv")

# Search Number can not exceed 50, if exceed 50 then the page will become bad

Search_Nearby <- function(Search_Interest, Position_Latitude, Position_Longitude, Search_Range, Search_Number){
  
  Search_Term <- Search_Interest
  
  Latitude <- Position_Latitude
  
  Longitude <- Position_Longitude
  
  range <- Search_Range
  
  Get_Data_URL <- paste("https://api.yelp.com/v3/businesses/search?", paste("term=" , Search_Term, sep = ""), "&", paste("latitude=" , Latitude, sep = ""), "&", paste("longitude=" , Longitude, sep = ""), "&", paste("radius=" , range, sep = ""), "&limit=50", sep = "")
  
  Personal_Key <- "Bearer OXlpgnVt5rYfCcaQrYKhM6e5hbs5cVnNoUitV90cFs3AMMZxwSoogkDtxJDneHgXTEwFMqQNT8kW5gyxpou3G0-W3HZdDiMjDLQfR0FoU5en-YTjxBTI23WudtKzW3Yx"
  
  Response_Content <- content(GET(Get_Data_URL, add_headers(Authorization = Personal_Key)))
  
  Total_Response <- Response_Content$total
  
  # helper function to get the info for each business
  GetData <- function(Content){
    return(data.frame(ID = Content$id,
                      Name = Content$name,
                      Is_Closed = ifelse(is.null(Content$is_closed), NA, Content$is_closed),
                      Rating = ifelse(is.null(Content$rating), NA, Content$rating),
                      Lat = Content$coordinates$latitude, Lon=Content$coordinates$longitude,
                      Price_Level = ifelse(is.null(Content$price), NA, Content$price),
                      Address = paste(Content$location$address1, Content$location$address2, Content$location$address3, sep = " "),
                      City = Content$location$city,
                      State = Content$location$state,
                      Zip_Code = Content$location$zip_code,
                      Distance = ifelse(is.null(Content$distance/1609.344), NA, Content$distance/1609.344)))
  }  
  
  Result_df <- do.call(rbind, lapply(Response_Content$businesses, GetData))
  
  Distance_Order <- order(Result_df$Distance, decreasing = FALSE)
  
  Result_df <- Result_df[Distance_Order,]
  
  if(nrow(Result_df[Result_df$State == "NY" , ]) <= Search_Number){
    return(Result_df[Result_df$State == "NY" , ])
  }
  
  else{
    return(head(Result_df[Result_df$State == "NY" , ], Search_Number))
  }
}

# Search_Nearby("hotel", Target_Lat, Target_Lon, 1700, 10)


# Calculate the distance for each row between one point

Crime_Geo_Data <- Crime_Data[, c("CMPLNT_NUM", "Latitude", "Longitude")]

colnames(Crime_Geo_Data) <- c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")

Cal_Dist_RowData_Point <- function(Geo_Data, Original_Lat, Original_Lon){
  
  Calculate_Distance <- function(RowData, lat1, lon1){
    
    return(distHaversine(c(lon1, lat1), c(RowData["Crime_Longitude"], RowData["Crime_Latitude"]))/1609.344)
    
  }
  
  Result_DF <- cbind(Geo_Data, Distance = apply(Geo_Data, 1, Calculate_Distance, lat1 = Original_Lat, lon1 = Original_Lon))

  return(Result_DF)
}

# Cal_Dist_RowData_Point(Crime_Geo_Data, Target_Lat, Target_Lon)




# note: GeoData should have same column syntax, first is id, second is lat, last is lon
# Two GeoData should have different column names for id, lat and lon
# Calculate the distance for two data each row
Cal_Dist_DF <- function(GeoData1, GeoData2){
  
  # get names for GeoData2
  GeoData1ColNames <- colnames(GeoData1)
  
  GeoData1IDNames <- GeoData1ColNames[1]
  
  GeoData1LatNames <- GeoData1ColNames[2]
  
  GeoData1LonNames <- GeoData1ColNames[3]
  
  # get names for GeoData2
  GeoData2ColNames <- colnames(GeoData2)
  
  GeoData2IDNames <- GeoData2ColNames[1]
  
  GeoData2LatNames <- GeoData2ColNames[2]
  
  GeoData2LonNames <- GeoData2ColNames[3]
  
  Merged_Geo_Data <- merge(Hotel_Geo_Data, Crime_Geo_Data)
  
  Calculate_Distance <- function(MergedData){
    
    return(distHaversine(c(as.numeric(MergedData[GeoData1LonNames]),as.numeric(MergedData[GeoData1LatNames])), c( as.numeric(MergedData[GeoData2LonNames]), as.numeric(MergedData[GeoData2LatNames])) )/1609.344)
    
  }
  
  Result_DF <- data.frame(ID1 = Merged_Geo_Data[GeoData1IDNames], ID2 = Merged_Geo_Data[GeoData2IDNames], Distance = apply(Merged_Geo_Data, 1, Calculate_Distance))
  
  colnames(Result_DF) <- c(GeoData1IDNames, GeoData2IDNames, "Distance")
  
  return(Result_DF)
  
}


 
# # This is helper function to reverse the geocode
# Get_Zip_Code <- function(GeoData){
#   
#   ZipCodeContent <- revgeocode(as.numeric(GeoData))
#   
#   while (is.na(ZipCodeContent)) {
#     
#     ZipCodeContent <- revgeocode(as.numeric(GeoData))
#     
#   }
#   
#   ZipCode <- substr(str_extract(ZipCodeContent, ' [0-9]{5}, .{0,}'),2,6)
#   
#   return(ZipCode)
#   
# }
# 
# Crime_Data <- Crime_Data %>% mutate(Zip_Code = apply(Crime_Data[,c("Longitude", "Latitude")], 1, Get_Zip_Code))
# 
# write.csv(Crime_Data, file = "Crime_Data.csv")




ui <- navbarPage("New York Crime Data",
                 tabPanel("Map", 
                          fluidPage(
                            
                            includeCSS("Crime_Map.css"),
                            
                            titlePanel("Geo Info for Crime Data"),
                            
                            checkboxInput("Restaurant", "Restaurant", value = FALSE, width = NULL),
                            
                            checkboxInput("Hotels", "Hotels", value = FALSE, width = NULL),
                            
                            sliderInput("distance", "distance: ", min = 0, max = 10, value = 0, step = 0.5),
                            
                            textOutput("Restaurant_Output"),
                            
                            leafletOutput("Map")
                            
                            
                            
                          )
                          
                          
                          ),
       
                 
                 tabPanel("Other")
   
  
   
   
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Position <- reactive({
    c(  Target_Lat = 40.808751,
        Target_Lon = -73.963228)
  })
  
  
  observeEvent(input$Restaurant,{
    
    if(input$Restaurant == TRUE){
      
      Range <- ifelse(input$Map_zoom <=5, 10, ifelse(input$Map_zoom <= 10, 20, ifelse(input$Map_zoom <= 15, 30, 40)))
      
      Restaurant_DF <- Search_Nearby("Restaurant", Position()["Target_Lat"],  Position()["Target_Lon"], 1700, Range)
      
      print(Restaurant_DF)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Restaurant_DF$Lon, lat = Restaurant_DF$Lat, clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
    }
    
    else{
      print(input$Restaurant)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
      
    }
    
    
  }, ignoreNULL = FALSE)
  

  
  
  observeEvent(input$Hotels,{
    
    greenLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
    
    
    if(input$Hotels == TRUE){
      
      Range <- ifelse(input$Map_zoom <=5, 10, ifelse(input$Map_zoom <= 10, 20, ifelse(input$Map_zoom <= 15, 30, 40)))
      
      Hotels_DF <- Search_Nearby("Hotels", Position()["Target_Lat"],  Position()["Target_Lon"], 1700, Range)
      
      print(Hotels_DF)
        
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(lng = Hotels_DF$Lon, lat = Hotels_DF$Lat, icon = greenLeafIcon, clusterId = "Hotels", clusterOptions = markerClusterOptions() )
      
    }
    
    else{

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
      
    }
    
  }, ignoreNULL = FALSE)
  
  
  
  
  observeEvent(input$distance,{
    
    
    TargetPT_CrimeGeo <- Cal_Dist_RowData_Point(Crime_Geo_Data, Position()["Target_Lat"],  Position()["Target_Lon"])
    
    
    TargetPT_CrimeGeo_Distance_Order <- order(TargetPT_CrimeGeo$Distance, decreasing = FALSE)
    
    TargetPT_CrimeGeo <- TargetPT_CrimeGeo[TargetPT_CrimeGeo_Distance_Order,]
    
    print(nrow(TargetPT_CrimeGeo[TargetPT_CrimeGeo$Distance <= input$distance,]))
    
    
  }, ignoreNULL = FALSE)
  
  
  
  # 
  # output$Restaurant_Output <- renderText({ 
  #   input$Restaurant
  # })
  # 
  # 
  
  output$Map <- renderLeaflet({


    leaflet() %>% setView(lng = as.numeric(Position()["Target_Lon"]), lat = as.numeric(Position()["Target_Lat"]), zoom = 15) %>% addMarkers(lng = Position()["Target_Lon"], lat = Position()["Target_Lat"], layerId = "Current_Address") %>% addTiles()


  })
  # 
  # 
  # observeEvent(input$Search_Term, {
  #   
  #   print(input$Search_Term)
  #   
  #   if(!is.null(input$Search_Term)){
  #     
  #     resultdf <- data.frame()
  #     
  #     for (Search_Term in input$Search_Term) {
  #       
  #       resultdf <- rbind(resultdf, Search_Nearby(Search_Term, Target_Lat, Target_Lon, 1700, 10))
  #       
  #     }
  #     
  #     
  #     leafletProxy("Map") %>% clearMarkers()  %>% addMarkers(lng = resultdf$Lon, lat = resultdf$Lat)
  #     
  #   }
  #   
  #   else{
  #     leafletProxy("Map") %>% clearMarkers()
  #   }
  #   
  #   
  # }, ignoreNULL = FALSE)
  
  
  # 
  # observeEvent(input$Map_zoom, {
  #   
  #   print(input$Search_Term)
  #   if(!is.null(input$Search_Term)){
  #     
  #     resultdf <- data.frame()
  #     
  #     for (Search_Term in input$Search_Term) {
  #       
  #       if(input$Map_zoom <= 5 && input$Map_zoom >= 1){
  #         
  #         resultdf <- rbind(resultdf, Search_Nearby(Search_Term, Target_Lat, Target_Lon, 1700, 10))
  #       
  #       }
  #       
  #       
  #       else if(input$Map_zoom <= 10 && input$Map_zoom >= 6){
  #         
  #         resultdf <- rbind(resultdf, Search_Nearby(Search_Term, Target_Lat, Target_Lon, 1700, 20))
  #         
  #       }
  #       
  #       else{
  #         resultdf <- rbind(resultdf, Search_Nearby(Search_Term, Target_Lat, Target_Lon, 1700, 40))
  #       }
  #       
  #     }
  #     
  #     leafletProxy("Map") %>% clearMarkers()  %>% addMarkers(lng = resultdf$Lon, lat = resultdf$Lat)
  #     
  #   }
  #   
  #   else{
  #     leafletProxy("Map") %>% clearMarkers()
  #   }
  #   
  # })
  
   # 
   # output$Crime_Map <- renderLeaflet({
   #   
   #   NYCGeoJson <- geojson_read('../../data/NYC_Crime_Data/NYC Street Centerline (CSCL).geojson', what = "sp")
   #   
   #   # nyc_neighborhoods <- readOGR(content(NYCGeoJson,'text'), 'OGRGeoJSON', verbose = F)
   #   # NYCGeoJson$communityDistrict <- as.character(NYCGeoJson$communityDistrict)
   #   Crime_Data <- read.csv("../../data/NYC_Crime_Data/NYPD_Complaint_Data_Current_YTD.csv")
   #   
   #   Crime_Data <- Crime_Data[1:1000,]
   #   
   #   Crime_Data_Geo_Info <- data.frame(CMPLNT_NUM = Crime_Data$CMPLNT_NUM, Lon = Crime_Data$Longitude, Lat = Crime_Data$Latitude)
   #   
   #   Map <- leaflet(data = Crime_Data_Geo_Info) %>% setView(lng = -73.9712, lat = 40.7831, zoom = 10) %>% addTiles() 
   #   
   #   Map %>% addMarkers(lng = ~Lon, lat = ~Lat) %>% addPolygons(data = NYCGeoJson,  fill = TRUE, fillOpacity = 0.1, highlightOptions = highlightOptions(
   #     color='#00ff00', opacity = 1, weight = 2, fillOpacity = 0.5,
   #     bringToFront = TRUE, sendToBack = TRUE))
   #   
   # })
   # 
   # observeEvent(input$Crime_Map_shape_click, {
   #   Polygon_Click <- input$Crime_Map_shape_click
   #   Polygon_ID <- Polygon_Click$id
   #   print(Polygon_Click)
   #   leafletProxy("Crime_Map") %>% removeShape("Layer_104" )
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

