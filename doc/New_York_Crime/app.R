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
library("plyr")
library("ggmap")
library("stringr")
library("htmlwidgets")
library("htmltools")


Crime_Data <- read.csv("../../data/NYC_Crime_Data/NYC_crime.csv")

Crime_Geo_Data <- Crime_Data[, c("CMPLNT_NUM", "Latitude", "Longitude")]

colnames(Crime_Geo_Data) <- c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")


######################## Start of Search_Nearby  ##################################################

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

######################## End of Search_Nearby  ##################################################


######################## Start of Cal_Dist_RowData_Point  ##################################################

# Calculate the distance for each row between one point

Cal_Dist_RowData_Point <- function(Geo_Data, Original_Lat, Original_Lon){
  
  Calculate_Distance <- function(RowData, lat1, lon1){
    
    return(distHaversine(c(lon1, lat1), c(RowData["Crime_Longitude"], RowData["Crime_Latitude"]))/1609.344)
    
  }
  
  Result_DF <- cbind(Geo_Data, Distance = apply(Geo_Data, 1, Calculate_Distance, lat1 = Original_Lat, lon1 = Original_Lon))

  return(Result_DF)
}

# Cal_Dist_RowData_Point(Crime_Geo_Data, Target_Lat, Target_Lon)

######################## End of Cal_Dist_RowData_Point  ##################################################



######################## Start of Cal_Dist_DF  ##################################################

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
  
  Merged_Geo_Data <- merge(GeoData1, GeoData2)
  
  Calculate_Distance <- function(MergedData){
    
    return(distHaversine(c(as.numeric(MergedData[GeoData1LonNames]),as.numeric(MergedData[GeoData1LatNames])), c( as.numeric(MergedData[GeoData2LonNames]), as.numeric(MergedData[GeoData2LatNames])) )/1609.344)
    
  }
  
  Result_DF <- data.frame(ID1 = Merged_Geo_Data[GeoData1IDNames], ID2 = Merged_Geo_Data[GeoData2IDNames], Distance = apply(Merged_Geo_Data, 1, Calculate_Distance))
  
  colnames(Result_DF) <- c(GeoData1IDNames, GeoData2IDNames, "Distance")
  
  return(Result_DF)
  
}

######################## End of Cal_Dist_DF  ##################################################



######################## Start of Selected_Crime  ##################################################

# This is function to data frame which contain all the crimes which are nearby the current position
Selected_Crime <- function(Target_Position, Time_Range, Type, Distance_to_Target, Crime_Geo_Info_Data, Original_Crime_Data){
  
  Range_Min_Time <- format(Time_Range, "%H:%M:%S", tz = "America/New_York")[1]
  
  Range_Max_Time <- format(Time_Range, "%H:%M:%S", tz = "America/New_York")[2]
  
  
  if(!is.null(Type)){
    
    print(Type)
    
    TargetPT_CrimeGeo <- Cal_Dist_RowData_Point(Crime_Geo_Info_Data, Target_Position["Target_Lat"],  Target_Position["Target_Lon"])
    
    TargetPT_CrimeGeo_Distance_Order <- order(TargetPT_CrimeGeo$Distance, decreasing = FALSE)
    
    TargetPT_CrimeGeo <- TargetPT_CrimeGeo[TargetPT_CrimeGeo_Distance_Order,]
    
    TargetPT_CrimeGeo <- merge(TargetPT_CrimeGeo[TargetPT_CrimeGeo$Distance <= Distance_to_Target,], Original_Crime_Data, by.x = "CMPLNT_NUM", by.y = "CMPLNT_NUM")[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude", "Distance", "PD_DESC", "CMPLNT_FR_TM")]
    
    # print(TargetPT_CrimeGeo)
    
    Selected_Crime <- data.frame()
    
    for (type in Type) {
      
      Selected_Crime <- rbind(Selected_Crime, TargetPT_CrimeGeo[TargetPT_CrimeGeo$PD_DESC == type , ])
      
    }
    
    Selected_Crime <- Selected_Crime[Selected_Crime$Distance <= Distance_to_Target, ]
    
    Selected_Crime_Index <- which(as.character(Selected_Crime$CMPLNT_FR_TM) >= Range_Min_Time & as.character(Selected_Crime$CMPLNT_FR_TM) <= Range_Max_Time )
    
    Selected_Crime <- Selected_Crime[Selected_Crime_Index,]
    
    return(Selected_Crime)
    
  }
  
  else{
    
    TargetPT_CrimeGeo <- Cal_Dist_RowData_Point(Crime_Geo_Info_Data, Target_Position["Target_Lat"],  Target_Position["Target_Lon"])
    
    TargetPT_CrimeGeo_Distance_Order <- order(TargetPT_CrimeGeo$Distance, decreasing = FALSE)
    
    TargetPT_CrimeGeo <- TargetPT_CrimeGeo[TargetPT_CrimeGeo_Distance_Order,]
    
    TargetPT_CrimeGeo <- merge(TargetPT_CrimeGeo[TargetPT_CrimeGeo$Distance <= Distance_to_Target,], Original_Crime_Data, by.x = "CMPLNT_NUM", by.y = "CMPLNT_NUM")[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude", "Distance", "PD_DESC", "CMPLNT_FR_TM")]
    
    # print(class(TargetPT_CrimeGeo[1,"CMPLNT_FR_TM"]))
    
    TargetPT_CrimeGeo_Index <- which(as.character(TargetPT_CrimeGeo$CMPLNT_FR_TM) >= Range_Min_Time & as.character(TargetPT_CrimeGeo$CMPLNT_FR_TM) <= Range_Max_Time )
    
    TargetPT_CrimeGeo <- TargetPT_CrimeGeo[TargetPT_CrimeGeo_Index,]
    
    return(TargetPT_CrimeGeo)
    
  }
  
}

######################## End of Selected_Crime  ##################################################
 



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


Total_Type <- c()

Type_Selection <- unique(Crime_Data$PD_DESC)

for (type in Type_Selection) {
  
  Total_Type <- c(Total_Type, type = type )
  
}

names(Total_Type)  <- Type_Selection


Min_Time <- as.POSIXlt("00:00:01", format = "%H:%M:%S", tz = "America/New_York")

Max_Time <- as.POSIXlt("23:59:59", format = "%H:%M:%S", tz = "America/New_York")



Current_Position_Content <- function(Num_Crime){
  return(paste(" ", Num_Crime))
}


###################### UI ##############################

ui <- navbarPage("New York Crime Data",
                 tabPanel("Map", 
                          fluidPage(
                            
                            includeCSS("Crime_Map.css"),
                            
                            titlePanel("Geo Info for Crime Data"),
                            
                            checkboxInput("Restaurant", "Restaurant", value = FALSE, width = NULL),
                            
                            checkboxInput("Hotels", "Hotels", value = FALSE, width = NULL),
                            
                            sliderInput("distance", "distance: ", min = 0, max = 10, value = 5, step = 0.1),
                            
                            sliderInput("restaurant_distance", "restaurant_distance: ", min = 0, max = 5, value = 1, step = 0.1),
                            
                            sliderInput("hotels_distance", "hotels_distance: ", min = 0, max = 5, value = 1, step = 0.1),
                            
                            sliderInput("time", "time: ", min = Min_Time, max = Max_Time, value =  c(Min_Time, Max_Time), timeFormat = "%H:%M:%S", step = 1),
                            
                            selectInput("type", "Type:", Total_Type, multiple = T, width = 1000),
                            
                            textOutput("Restaurant_Output"),
                            
                            leafletOutput("Map")
                            
                            
                            
                          )
                          
                          
                          ),
       
                 
                 tabPanel("Other")
   
  
   
   
   
)


###################### UI ##############################



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Position <- reactive({
    c(  Target_Lat = 40.808751,
        Target_Lon = -73.963228)
  })
  
  
  
  
######################## Start of Restaurants  ##################################################
  
############################## Restaurant ##############################
  observeEvent(input$Restaurant,{
    
    if(input$Restaurant == TRUE){
      
      Range <- ifelse(input$Map_zoom <=5, 10, ifelse(input$Map_zoom <= 10, 20, ifelse(input$Map_zoom <= 15, 30, 40)))
      
      Restaurant_DF <- Search_Nearby("Restaurant", Position()["Target_Lat"],  Position()["Target_Lon"], 1700, Range)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Restaurant_DF$Lon, lat = Restaurant_DF$Lat, clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
    }
    
    else{
      print(input$Restaurant)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
      
    }
    
    
  }, ignoreNULL = FALSE)

############################## Restaurant ############################## 
  
  
############################## Restaurant Crime distance ############################## 
  
  observeEvent(input$restaurant_distance,{
    
    if(input$Restaurant == TRUE){
      
      Range <- ifelse(input$Map_zoom <=5, 10, ifelse(input$Map_zoom <= 10, 20, ifelse(input$Map_zoom <= 15, 30, 40)))
      
      Restaurant_DF <- Search_Nearby("Restaurant", Position()["Target_Lat"],  Position()["Target_Lon"], 1700, Range)
      
      Restaurant_Geo_DF <- Restaurant_DF[, c("ID", "Lat", "Lon")]
      
      Merged_Data <- Cal_Dist_DF(Restaurant_Geo_DF, Crime_Geo_Data)
      
      Merged_Data <- Merged_Data[Merged_Data$Distance <= input$restaurant_distance, ]
      
      Restaurant_Count_DF <- ddply(Merged_Data, .(ID), nrow)
      
      colnames(Restaurant_Count_DF) <- c("ID", "Crime_Count")
      
      result_df <- merge(Restaurant_DF, Restaurant_Count_DF, by.x = "ID", by.y = "ID")
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = result_df$Lon, lat = result_df$Lat, popup = htmlEscape(Current_Position_Content(result_df$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
    }
    
    else{
      print(input$Restaurant)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
      
    }
    
    
  }, ignoreNULL = FALSE)

############################## Restaurant Crime distance ############################## 

  
######################## End of Restaurants  ##################################################



  
  
######################## Start of Hotels  ##################################################
  
  
######################################  Hotels #################################### 
  
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
  
######################################  Hotels #################################### 
  
  
######################################  Hotels Crime Distance ####################################
  
  observeEvent(input$hotels_distance,{
    
    if(input$Hotels == TRUE){
      
      Range <- ifelse(input$Map_zoom <=5, 10, ifelse(input$Map_zoom <= 10, 20, ifelse(input$Map_zoom <= 15, 30, 40)))
      
      Hotels_DF <- Search_Nearby("Hotels", Position()["Target_Lat"],  Position()["Target_Lon"], 1700, Range)
      
      Hotels_Geo_DF <- Hotels_DF[, c("ID", "Lat", "Lon")]
      
      Merged_Data <- Cal_Dist_DF(Hotels_Geo_DF, Crime_Geo_Data)
      
      Merged_Data <- Merged_Data[Merged_Data$Distance <= input$hotels_distance, ]
      
      Hotels_Count_DF <- ddply(Merged_Data, .(ID), nrow)
      
      colnames(Hotels_Count_DF) <- c("ID", "Crime_Count")
      
      result_df <- merge(Hotels_DF, Hotels_Count_DF, by.x = "ID", by.y = "ID")
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(lng = result_df$Lon, lat = result_df$Lat, popup = htmlEscape(Current_Position_Content(result_df$Crime_Count)), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
      
    }
    
    else{
      print(input$Hotels)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
      
    }
    
    
  }, ignoreNULL = FALSE)

######################################  Hotels Crime Distance ####################################


######################## End of Hotels  ##################################################
  

  
######################## Start of Observe Crime  ##################################################
  
  
  # Type
  observeEvent(input$type,{
    
    
    Result_Crime_Data <- Selected_Crime(Position(), input$time, input$type, input$distance, Crime_Geo_Data, Crime_Data)
   
     leafletProxy("Map") %>% 
      removeMarkerCluster(layerId = "Crime") %>% 
      addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
      removeMarker(layerId = "Current_Address") %>%
      addMarkers(lng = Position()["Target_Lon"], lat = Position()["Target_Lat"], layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
    
      
     
    
  }, ignoreNULL = FALSE)  
  
  
  # Time
  observeEvent(input$time,{
    
    Result_Crime_Data <- Selected_Crime(Position(), input$time, input$type, input$distance, Crime_Geo_Data, Crime_Data)
    
    leafletProxy("Map") %>% 
      removeMarkerCluster(layerId = "Crime") %>% 
      addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
      removeMarker(layerId = "Current_Address") %>%
      addMarkers(lng = Position()["Target_Lon"], lat = Position()["Target_Lat"], layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
    
    
    
  }, ignoreNULL = FALSE)
  
  
  
  # Distance
  observeEvent(input$distance,{
    
    Result_Crime_Data <- Selected_Crime(Position(), input$time, input$type, input$distance, Crime_Geo_Data, Crime_Data)
    
    leafletProxy("Map") %>% 
      removeMarkerCluster(layerId = "Crime") %>% 
      addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
      removeMarker(layerId = "Current_Address") %>%
      addMarkers(lng = Position()["Target_Lon"], lat = Position()["Target_Lat"], layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
    
    
    
    
  }, ignoreNULL = FALSE)
  
######################## End of Observe Crime  ##################################################

  
  
################################### OUTPUT ################################### 
  output$Map <- renderLeaflet({
    
    leaflet() %>% setView(lng = as.numeric(Position()["Target_Lon"]), lat = as.numeric(Position()["Target_Lat"]), zoom = 15) %>% addMarkers(lng = Position()["Target_Lon"], lat = Position()["Target_Lat"], layerId = "Current_Address") %>% addTiles()

  })
  
################################### OUTPUT ################################### 
  
  
  
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

