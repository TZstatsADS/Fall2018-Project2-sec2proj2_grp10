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
library("stringr")
library("htmlwidgets")
library("htmltools")
library("shinyjs")
library("shinydashboard")


Crime_Data <- read.csv("../../data/NYC_Crime_Data/NYC_crime.csv")

Crime_Geo_Data <- Crime_Data[, c("CMPLNT_NUM", "Latitude", "Longitude")]

colnames(Crime_Geo_Data) <- c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")


######################## Start of Get_Geo_Info ######################## 

Get_Geo_Info <- function(Address){
  
  Search_Address <- gsub(" ", "+", Address)
  
  Personal_Key <- "AIzaSyC3zuhMPkCVTVycI71lSvVHzNDHmNuCUbk"
  
  Get_Data_URL <- paste("https://maps.googleapis.com/maps/api/geocode/json?address=", Search_Address, "&key=", Personal_Key, sep = "")
  
  Response_Content <- GET(Get_Data_URL)
  
  Geo_Info <- c()
  
  if(content(Response_Content)$status == "OK"){
    
    Geo_Info <- c(lat = content(Response_Content)$results[[1]]$geometry$location$lat, lon = content(Response_Content)$results[[1]]$geometry$location$lng )
    
  }

  else{
    
    Geo_Info <- NULL

  }
  
  return(Geo_Info)
  
  
}

# Get_Geo_Info("sdfasdfasdfa asdfasdfasdf")


######################## End of Get_Geo_Info ######################## 




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
    
    TargetPT_CrimeGeo <- Cal_Dist_RowData_Point(Crime_Geo_Info_Data, Target_Position$Target_Lat,  Target_Position$Target_Lon)
    
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
    
    TargetPT_CrimeGeo <- Cal_Dist_RowData_Point(Crime_Geo_Info_Data, Target_Position$Target_Lat,  Target_Position$Target_Lon)
    
    TargetPT_CrimeGeo_Distance_Order <- order(TargetPT_CrimeGeo$Distance, decreasing = FALSE)
    
    TargetPT_CrimeGeo <- TargetPT_CrimeGeo[TargetPT_CrimeGeo_Distance_Order,]
    
    TargetPT_CrimeGeo <- merge(TargetPT_CrimeGeo[TargetPT_CrimeGeo$Distance <= Distance_to_Target,], Original_Crime_Data, by.x = "CMPLNT_NUM", by.y = "CMPLNT_NUM")[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude", "Distance", "PD_DESC", "CMPLNT_FR_TM")]
    
    TargetPT_CrimeGeo_Index <- which(as.character(TargetPT_CrimeGeo$CMPLNT_FR_TM) >= Range_Min_Time & as.character(TargetPT_CrimeGeo$CMPLNT_FR_TM) <= Range_Max_Time )
    
    TargetPT_CrimeGeo <- TargetPT_CrimeGeo[TargetPT_CrimeGeo_Index,]
    
    return(TargetPT_CrimeGeo)
    
  }
  
}

######################## End of Selected_Crime  ##################################################
 


####################### Start of Selected_Business ###########################################

Selected_Business <- function(Target_Position, Business_Type, Zoom_Size, Business_Crime_Distance, Business_Rating, Business_Price_Level, Crime_Geo_Info_Data){
  
  Max_Rating <- Business_Rating[2]
  
  Min_Rating <- Business_Rating[1]
  
  Range <- ifelse(Zoom_Size <=5, 10, ifelse(Zoom_Size <= 10, 20, ifelse(Zoom_Size <= 15, 30, 40)))
  
  Business_Original_DF <- Search_Nearby(Business_Type, Target_Position$Target_Lat,  Target_Position$Target_Lon, 1700, Range)
  
  Business_Original_Geo_DF <- Business_Original_DF[, c("ID", "Lat", "Lon")]
  
  Business_Crime_Data <- Cal_Dist_DF(Business_Original_Geo_DF, Crime_Geo_Info_Data)
  
  Business_Crime_Data <- Business_Crime_Data[Business_Crime_Data$Distance <= Business_Crime_Distance, ]
  
  Business_Crime_Count_DF <- ddply(Business_Crime_Data, .(ID), nrow)
  
  colnames(Business_Crime_Count_DF) <- c("ID", "Crime_Count")
  
  Result_Business_df <- merge(Business_Original_DF, Business_Crime_Count_DF, by.x = "ID", by.y = "ID")
  
  Result_Business_df <- Result_Business_df[Result_Business_df$Rating <= Max_Rating & Result_Business_df$Rating >= Min_Rating,]
  
  if(!is.null(Business_Price_Level)){
    
    Final_Result_Business_df <- data.frame()
    
    for (price_level in Business_Price_Level) {
      
      Final_Result_Business_df <- rbind(Final_Result_Business_df, Result_Business_df[Result_Business_df$Price_Level == price_level,] )
      
    }
    
    return(Final_Result_Business_df)
    
  }
  
  else{
    
    return(Result_Business_df)
    
  }
  
}

####################### End of Selected_Business ###########################################





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

price_level <- c("$", "$$", "$$$", "$$$$")

names(price_level) <- c("$", "$$", "$$$", "$$$$")


Min_Time <- as.POSIXlt("00:00:01", format = "%H:%M:%S", tz = "America/New_York")

Max_Time <- as.POSIXlt("23:59:59", format = "%H:%M:%S", tz = "America/New_York")



Current_Position_Content <- function(Num_Crime){
  return(paste(" ", Num_Crime))
}


###################### UI ##############################

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "NYC Crime"),
  dashboardSidebar(
    sidebarMenu(
      id = "Sider_Menu",
      menuItem("Intro", tabName = "intro", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("fal fa-map")),
      menuItem("Statistic", tabName = "statistic", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Crime_Map.css")
    ),
    
    tabItems(
      

      tabItem(tabName = "intro",

              fluidRow(
                
                column(class = "Intro-board" ,width = 6, offset = 3, 
                       
                       h1(class = "Intro-Header" , "Welcome to New York"),
                       
                       searchInput(inputId = "Intro_Search_Address", 
                                   placeholder = "Enter Address eg. 1 West End Ave, New York, New York", 
                                   btnSearch = icon("fas fa-search")
                       
                       
                       )
                       
                       )
                

              )
      ),

      
      tabItem(tabName = "map",
              fluidRow(
                useShinyjs(),
                column(width = 8,
                       
                       
                       box(
                         box(class="Crime-Time", width = 4, sliderInput("Crime_Time", "Crime Time: ", min = Min_Time, max = Max_Time, value =  c(Min_Time, Max_Time), timeFormat = "%H:%M:%S", step = 1, width = ) ),
                         box(width = 4, pickerInput("Crime_Type", "Crime Type:", Total_Type, multiple = T) ),
                         box(width = 4, sliderInput("Crime_Distance", "Crime Distance: ", min = 0, max = 5, value = 2.5, step = 0.1) ),
                         width = 12,
                         status = "primary",
                         leafletOutput("Map", height = 500)
                       )
                ),
                column(width = 4,
                       box(
                         title = "Search Address:", width = NULL, status = "warning",
                         searchInput(inputId = "Map_Search_Address", 
                                     placeholder = "Enter Address", 
                                     btnSearch = icon("fas fa-search"))
                       ),
                       
                       box(
                         title = "Category", width = NULL, status = "warning",
                         prettyCheckbox("Hotels", "Hotels", value = FALSE, status = "warning",
                                        shape = "square", animation = "smooth", inline = TRUE),
                         prettyCheckbox("Restaurant", "Restaurant", value = FALSE, status = "warning",
                                        shape = "square", animation = "smooth", inline = TRUE),
                         prettyCheckbox("Entertainment", "Entertainment", value = FALSE, status = "warning",
                                        shape = "square", animation = "smooth", inline = TRUE),
                         prettyCheckbox("Crime", "Crime", value = FALSE, status = "warning",
                                        shape = "square", animation = "smooth", inline = TRUE)
                       ),
                       
                       tabBox(
                         side = "left", width = NULL,
                         selected = "Hotels",
                         tabPanel("Hotels", 
                                sliderInput("Hotels_Crime_Distance", "Hotels Crime Distance: ", min = 0, max = 5, value = 1, step = 0.1),
                                sliderInput("Hotels_Rating", "Hotels Rating: ", min = 0, max = 5, value = c(3, 5), step = 0.5),
                                pickerInput("Hotels_Price_Level", "Hotels Price Level:", price_level, multiple = TRUE)
                                  
                         ),
                         tabPanel("Restaruant", 
                                  sliderInput("Restaurant_Crime_Distance", "Restaruant Crime Distance: ", min = 0, max = 5, value = 1, step = 0.1),
                                  sliderInput("Restaurant_Rating", "Restaurant Rating: ", min = 0, max = 5, value = c(3, 5), step = 0.5),
                                  pickerInput("Restaurant_Price_Level", "Restaurant Price Level:", price_level, multiple = TRUE)
                        ),
                         tabPanel("Entertainment", 
                                  sliderInput("Entertainment_Crime_Distance", "Entertainment Crime Distance: ", min = 0, max = 5, value = 1, step = 0.1),
                                  sliderInput("Entertainment_Rating", "Entertainment Rating: ", min = 0, max = 5, value = c(3, 5), step = 0.5),
                                  pickerInput("Entertainment_Price_Level", "Entertainment Price Level:", price_level, multiple = TRUE)
                                  
                                  
                        )
                    )

                )
              )
      ),
      
      
      tabItem(tabName = "statistic",
              h2("statistic")
      )
    )
  )
)




###################### UI ##############################



# Define server logic required to draw a histogram
server <- function(input, output, session) {

  Position <- reactiveValues(Target_Lat = 40.7128,  Target_Lon = -74.0060)
  
#   
# ######################## Start of Restaurants  ##################################################

############################## Restaurant ##############################
  observeEvent(input$Restaurant,{

    if(input$Restaurant == TRUE){

      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Restaurant_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )


    }

    else{

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")

    }


  }, ignoreNULL = FALSE)

############################## Restaurant ##############################

  

############################## Restaurant Crime distance ##############################

  observeEvent(input$Restaurant_Crime_Distance,{

    if(input$Restaurant == TRUE){

      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Restaurant_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      

    }

    else{

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")

    }


  }, ignoreNULL = FALSE)

############################## Restaurant Crime distance ##############################


############################## Restaurant Rating ##############################

  observeEvent(input$Restaurant_Rating,{

    if(input$Restaurant == TRUE){

      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Restaurant_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
    }

    else{

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")

    }


  }, ignoreNULL = FALSE)


############################## Restaurant Rating ##############################



############################## Restaurant Price Leve ##############################

  observeEvent(input$Restaurant_Price_Level,{


    if(input$Restaurant == TRUE){

      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Restaurant_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
    }

    else{

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")

    }


  }, ignoreNULL = FALSE)


############################## Restaurant Price Level ##############################


######################## End of Restaurants  ##################################################

  
  

######################## Start of Observe Crime  ##################################################


########################### Crime Type ########################
  observeEvent(input$Crime_Type,{

    if(input$Crime){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      
    }
    

  }, ignoreNULL = FALSE)

########################### Crime Type ########################
  
  
########################### Crime Time ########################
  observeEvent(input$Crime_Time,{

    if(input$Crime){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
    }


  }, ignoreNULL = FALSE)
  
########################### Crime Time ########################
  
  
########################### Show Crime CheckBox ########################  
  
  observeEvent(input$Crime,{
    
    
    if(input$Crime){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
    }

    
  }, ignoreNULL = FALSE)
  
########################### Show Crime CheckBox ########################
    


########################### Crime Distance ########################
  observeEvent(input$Crime_Distance,{
    
    
    if(input$Crime){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
      
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(lng = Selected_Restaurant$Lon, lat = Selected_Restaurant$Lat, popup = htmlEscape(Current_Position_Content(Selected_Restaurant$Crime_Count)), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      
    }
    

  }, ignoreNULL = FALSE)


  
  
  
  
######################## End of Observe Crime  ##################################################

  
  
############################ Observe Event for Map Search button ###################################  
  
  observeEvent(input$Map_Search_Address_search,{
    
    print(input$Map_Search_Address_search)
    
    if(input$Intro_Search_Address_search >= 1){
      
      Geo_Info <- Get_Geo_Info(input$Map_Search_Address)
      
      if(!is.null(Geo_Info) && !is.null(Geo_Info["lat"]) && !is.null(Geo_Info["lon"]) ){
        
        updateTabItems(session, "Sider_Menu", "map")
        
        Position$Target_Lat <- Geo_Info["lat"]
        Position$Target_Lon <- Geo_Info["lon"]
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        output$Map <- renderLeaflet({
          
          leaflet() %>% setView(lng = Position$Target_Lon, lat = Position$Target_Lat, zoom = 15) %>% addTiles() %>% removeMarker(layerId = "Current_Address") %>%
            addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
          
        })
        
        
      }
      
      else{
        
        showModal(modalDialog(
          title = "Address Error!",
          "There is no such address, please check the address again and input again! ",
          easyClose = TRUE
        ))
        
      }
      
      
    }
    
    
  }, ignoreNULL = FALSE)
  
############################ Observe Event for Intro Search button ###################################
  
  
  
############################ Observe Event for Intro Search button ###################################  
  
    observeEvent(input$Intro_Search_Address_search,{
      
      print(input$Intro_Search_Address_search)
      
      if(input$Intro_Search_Address_search >= 1){
        
              Geo_Info <- Get_Geo_Info(input$Intro_Search_Address)

              if(!is.null(Geo_Info) && !is.null(Geo_Info["lat"]) && !is.null(Geo_Info["lon"]) ){
                
                updateTabItems(session, "Sider_Menu", "map")

                Position$Target_Lat <- Geo_Info["lat"]
                Position$Target_Lon <- Geo_Info["lon"]

                Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)

                output$Map <- renderLeaflet({
                  
                  leaflet() %>% setView(lng = Position$Target_Lon, lat = Position$Target_Lat, zoom = 15) %>% addTiles() %>% removeMarker(layerId = "Current_Address") %>%
                    addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", popup = htmlEscape(Current_Position_Content(nrow(Result_Crime_Data))))
                  
                })
                  

              }

              else{

                showModal(modalDialog(
                  title = "Address Error!",
                  "There is no such address, please check the address again and input again! ",
                  easyClose = TRUE
                ))

              }
        
        
      }


    }, ignoreNULL = FALSE)
  
############################ Observe Event for Intro Search button ###################################  
  

  
################################### OUTPUT ################################### 
  
  output$Map <- renderLeaflet({
    
    leaflet() %>% setView(lng = Position$Target_Lon, lat = Position$Target_Lat, zoom = 15) %>% addTiles()
    
  })

  
################################### OUTPUT ################################### 
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

