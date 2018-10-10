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
library("httr")
library("rgdal")
library("shiny")
library("geosphere")
library("dplyr")
library("plyr")
library("stringr")
library("htmlwidgets")
library("htmltools")
library("shinyjs")
library("shinydashboard")
library("ggplot2")
library("reshape2")




###########################  Start from here is crime value for map ########################### 
Crime_Data <- read.csv("../data/NYC_Crime_Data/NYPD_Complaint_Data_Current_YTD.csv")

crime <- Crime_Data

Get_Year <- function(Date){
  return(substring(Date, 7, 10))
}

Get_Month <- function(Date){
  return(substring(Date, 1, 2))
}

Crime_Data <- cbind(Crime_Data, year = sapply(Crime_Data$CMPLNT_FR_DT, Get_Year), month = sapply(Crime_Data$CMPLNT_FR_DT, Get_Month))

Crime_Data <- Crime_Data[Crime_Data$year == "2018" & Crime_Data$month == "03",]

Crime_Geo_Data <- Crime_Data[, c("CMPLNT_NUM", "Latitude", "Longitude")]

colnames(Crime_Geo_Data) <- c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")
###########################  End here is crime value for map ########################### 




########################### Start from here is crime for statistics ########################### 
# Work on date format
date <- crime$CMPLNT_FR_DT
hours <- crime$CMPLNT_FR_TM
crime$Year <- substring(date, 7, 10)
crime$Month <- substring(date, 1, 2)
crime$Day <- substring(date, 4, 5)
crime$Time <- substring(hours, 1, 5)
###########################  end here is crime for statistics ########################### 




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
                      Image_Url = Content$image_url,
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

# Search_Nearby("hotel", 40.7128, -74.0060, 1700, 10)

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
 

######################## Start of Count Crime Numbers ######################## 

Crime_Business_Count <- function(Business_Crime_Data, Business_Crime_Distance){
  
  if(nrow(Business_Crime_Data[Business_Crime_Data$Distance <= Business_Crime_Distance,]) == 0){
    return(0)
  }
  
  else{
    return(nrow(Business_Crime_Data[Business_Crime_Data$Distance <= Business_Crime_Distance,]))
  }
  
}

######################## End of Count Crime Numbers ######################## 


####################### Start of Selected_Business ###########################################

Selected_Business <- function(Target_Position, Business_Type, Zoom_Size, Business_Crime_Distance, Business_Rating, Business_Price_Level, Crime_Geo_Info_Data){
  
  Max_Rating <- Business_Rating[2]
  
  Min_Rating <- Business_Rating[1]
  
  Range <- ifelse(Zoom_Size <=5, 10, ifelse(Zoom_Size <= 10, 20, ifelse(Zoom_Size <= 15, 30, 40)))
  
  Business_Original_DF <- Search_Nearby(Business_Type, Target_Position$Target_Lat,  Target_Position$Target_Lon, 1700, Range)
  
  Business_Original_Geo_DF <- Business_Original_DF[, c("ID", "Lat", "Lon")]
  
  Business_Crime_Data <- Cal_Dist_DF(Business_Original_Geo_DF, Crime_Geo_Info_Data)
  
  Business_Crime_Count_DF <- ddply(Business_Crime_Data, .(ID), Crime_Business_Count, Business_Crime_Distance = Business_Crime_Distance)
  
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


####################### Start of Hotel_Icon_Maker ###########################################
Hotel_Icon_Maker <- function(Crime_Number){
  
  if(Crime_Number>=0 && Crime_Number <= 10){
    return(makeIcon(
      iconUrl = "./Hotel_Green.png",
      iconWidth = 25, iconHeight = 25,
      className = "Hotel_Icon_Green"
    ))
  }
  
  else if(Crime_Number>=11 && Crime_Number <= 20){
    return(makeIcon(
      iconUrl = "./Hotel_Blue.png",
      iconWidth = 25, iconHeight = 25,
      className = "Hotel_Icon_Blue"
    ))
  }
  
  else{
    return(makeIcon(
      iconUrl = "./Hotel_Red.png",
      iconWidth = 25, iconHeight = 25,
      className = "Hotel_Icon_Red"
    ))
    
  }
  
}
####################### End of Hotel_Icon_Maker ###########################################

####################### Start of Restaurant_Icon_Maker ###########################################
Restaurant_Icon_Maker <- function(Crime_Number){
  
  if(Crime_Number>=0 && Crime_Number <= 10){
    return(makeIcon(
      iconUrl = "./Restaurant_Green.png",
      iconWidth = 25, iconHeight = 25,
      className = "Restaurant_Icon_Green"
    ))
  }
  
  else if(Crime_Number>=11 && Crime_Number <= 20){
    return(makeIcon(
      iconUrl = "./Restaurant_Blue.png",
      iconWidth = 25, iconHeight = 25,
      className = "Restaurant_Icon_Blue"
    ))
  }
  
  else{
    return(makeIcon(
      iconUrl = "./Restaurant_Red.png",
      iconWidth = 25, iconHeight = 25,
      className = "Restaurant_Icon_Red"
    ))
    
  }
  
}
####################### End of Restaurant_Icon_Maker ###########################################

####################### Start of Entertainment_Icon_Maker ###########################################
Entertainment_Icon_Maker <- function(Crime_Number){
  
  if(Crime_Number>=0 && Crime_Number <= 10){
    return(makeIcon(
      iconUrl = "./Entertainment_Green.png",
      iconWidth = 25, iconHeight = 25,
      className = "Entertainment_Icon_Green"
    ))
  }
  
  else if(Crime_Number>=11 && Crime_Number <= 20){
    return(makeIcon(
      iconUrl = "./Entertainment_Blue.png",
      iconWidth = 25, iconHeight = 25,
      className = "Entertainment_Icon_Blue"
    ))
  }
  
  else{
    return(makeIcon(
      iconUrl = "./Entertainment_Red.png",
      iconWidth = 25, iconHeight = 25,
      className = "Entertainment_Icon_Red"
    ))
    
  }
  
}
####################### End of Entertainment_Icon_Maker ###########################################

####################### Start of Current_Position_Icon_Maker ###########################################
Current_Position_Icon_Maker <- function(Crime_Number){
  
  if(Crime_Number>=0 && Crime_Number <= 10){
    return(makeIcon(
      iconUrl = "./Current_Position_Green.png",
      iconWidth = 25, iconHeight = 25,
      className = "Current_Position_Icon_Green"
    ))
  }
  
  else if(Crime_Number>=11 && Crime_Number <= 20){
    return(makeIcon(
      iconUrl = "./Current_Position_Yellow.png",
      iconWidth = 25, iconHeight = 25,
      className = "Current_Position_Icon_Yellow"
    ))
  }
  
  else{
    return(makeIcon(
      iconUrl = "./Current_Position_Red.png",
      iconWidth = 25, iconHeight = 25,
      className = "Current_Position_Icon_Red"
    ))
    
  }
  
}
####################### End of Current_Position_Icon_Maker ###########################################

####################### Start of Business_Pop_Up_Maker ####################### 

Business_Pop_Up_Maker <- function(Name, Pic = "", Rating = 0, Price_Level = "", Address = "", City = "", State = "", Zip_Code = "", Distance = 0, Num_Crime = 0){
  
  Complete_Name <- paste("<b>", Name, "</b>", sep = "")
  
  Complete_Rating <- paste("Rating: ", Rating, sep = "")
  
  Complete_Price_Level <- paste("Price Level: ", Price_Level, sep = "")
  
  Complete_Pic <- paste("<img src='", Pic, "'", "width='200' height='150'>", sep = "")
  
  Complete_Address <- paste(Address, City, State, Zip_Code, sep = ", ")
  
  Complete_Crime <- paste("Crime Number: ", Num_Crime, sep = "")
  
  Complete_Distance <- paste("Distance: ", round(Distance, 3), sep = "")
  
  return(paste(Complete_Pic, Complete_Name, Complete_Address, Complete_Rating, Complete_Price_Level, Complete_Crime, Complete_Distance, sep = "<br/>"))
}

####################### End of Business_Pop_Up_Maker ####################### 

####################### Start of Current_Position_Pop_Up_Maker #######################
Current_Position_Pop_Up_Maker <- function(Num_Crime = 0){
  
  if(Num_Crime>=0 && Num_Crime <= 10){
    
    Title <- "<center><b> Relax !</b></center>"
    
    Message <- paste("There are only ", Num_Crime, " happened before at your location, But still be careful when you walk on street or take cab", sep = "")
    
    return(paste(Title, Message, sep = "<br/>"))
  }
  
  else if(Num_Crime>=11 && Num_Crime <= 20){
    Title <- "<center><b> Be Carefull !</b></center>"
    
    Message <- paste("WOW! There are ", Num_Crime, " happened before at your location, Please be careful when you walk on street or take cab", sep = "")
    
    return(paste(Title, Message, sep = "<br/>"))
  }
  
  else{
    Title <- "<center><b> Warning !</b></center>"
    
    Message <- paste("Oh on! There are ", Num_Crime, " happened before at your location, Please ask your friends or parents come to pick you up!", sep = "")
    
    return(paste(Title, Message, sep = "<br/>"))
    
  }
  
  
}


####################### End of Current_Position_Pop_Up_Maker #######################

####################### Start of Crime_Icon #######################

Crime_Icon <- makeIcon(
  iconUrl = "./Crime.png",
  iconWidth = 30, iconHeight = 30,
  className = "Crime_Icon"
)

####################### End of Crime_Icon #######################


Total_Type <- c()

Type_Selection <- unique(Crime_Data$LAW_CAT_CD)

for (type in Type_Selection) {
  
  Total_Type <- c(Total_Type, type = type )
  
}

names(Total_Type)  <- Type_Selection

price_level <- c("$", "$$", "$$$", "$$$$")

names(price_level) <- c("$", "$$", "$$$", "$$$$")


Min_Time <- as.POSIXlt("00:00:01", format = "%H:%M:%S", tz = "America/New_York")

Max_Time <- as.POSIXlt("23:59:59", format = "%H:%M:%S", tz = "America/New_York")



###################### UI ##############################

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "NYC Crime"),
  dashboardSidebar(
    sidebarMenu(
      id = "Sider_Menu",
      menuItem("Intro", tabName = "intro", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("fal fa-map")),
      menuItem("Crime Statistic", icon = icon("th"), 
               sidebarMenu(id = "Statistic_Sub_Side_Bar",
                           menuItem("Crime Types", tabName = "Crime_Types" ),
                           menuItem("Victim Sex", tabName = "Victim_Sex"),
                           menuItem("Age Group and Race", tabName = "Crime_Age_Group_Race"),
                           menuItem("Crime During Day", tabName = "Crime_During_Day")
                           )
              )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Crime_Map.css")
    ),
 
########################## UI for map and Intro ########################## 
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
                         box(class="Crime-Time", width = 4, sliderInput("Crime_Time", "Crime Time: ", min = Min_Time, max = Max_Time, value =  c(Min_Time, Max_Time), timeFormat = "%H:%M:%S", step = 1 ) ),
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
########################## UI for map and Intro ##########################    
      
      tabItem(tabName = "Crime_Types",
              fluidRow(
                column(width = 9,
                       tabBox(
                         side = "left", width = 12,
                         selected = "Crime Count in Boroughs",
                         tabPanel("Crime Count in Boroughs", 
                                  plotOutput("Crime_Count_Boro")
                                  ),
                         tabPanel("Monthly Crime", 
                                  plotOutput("Monthly_Crime")                      
                                  )

                       )
                ),
                column(width = 3,
                       box(width = 12,
                           pickerInput("Statistic_Year", "Year:", list("2016", "2017", "2018"), selected = "2016")
                           )
                )
              )
      ),      

      tabItem(tabName = "Victim_Sex",
              fluidRow(
                column(width = 9,
                       tabBox(
                         side = "left", width = 12,
                         selected = "Victim Sex Crime Type",
                         tabPanel("Victim Sex Crime Type", 
                                  plotOutput("Victim_Sex_Crime_Type")
                                  ),
                         tabPanel("Victim Sex Boroughs", 
                                  plotOutput("Victim_Sex_Boroughs")                      
                                  )

                       )
                ),
                column(width = 3,
                       tabBox(
                         side = "left", width = 12,
                         selected = "Crime Type",
                         tabPanel("Crime Type", 
                                  pickerInput("Statistic_Crime_Type", "Crime Type:", list("VIOLATION", "MISDEMEANOR", "FELONY") , selected = "VIOLATION" )
                         ),
                         tabPanel("NYC Borough", 
                                  pickerInput("Statistic_NYC_Borough", "NYC Borough:", list("BROOKLYN", "STATEN ISLAND", "QUEENS", "BRONX", "MANHATTAN"), selected = "BROOKLY")
                         )
                         
                       )
                )
              )
      ),

      tabItem(tabName = "Crime_Age_Group_Race",
              fluidRow(
                column(width = 10, offset = 1,
                       tabBox(
                         side = "left", width = 12,
                         selected = "Crime Count Vs Avg Group",
                         tabPanel("Crime Count Vs Avg Group", 
                                  plotOutput("Crime_Count_Avg_Group")
                                  ),
                         tabPanel("Crime Count Vs Race", 
                                  plotOutput("Crime_Count_Race")                      
                                  )

                       )
                )
              )
      ),

      tabItem(tabName = "Crime_During_Day",
              fluidRow(
                column(width = 10, offset = 1,
                       tabBox(
                         side = "left", width = 12,
                         selected = "Crime During Day",
                         tabPanel("Crime During Day", 
                                  plotOutput("Crime_During_Day")
                                  )

                       )
                )
              )
      )

    
      
      
      
      
      
    )
  )
)




###################### UI ##############################



# Define server logic required to draw a histogram
server <- function(input, output, session) {

  Position <- reactiveValues(Target_Lat = 40.7128,  Target_Lon = -74.0060)
  

######################## Start of Restaurants  ##################################################

############################## Restaurant ##############################
  observeEvent(input$Restaurant,{

    if(input$Restaurant == TRUE){

      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Restaurant_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )


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
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      

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
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
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
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
      
    }

    else{

      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")

    }


  }, ignoreNULL = FALSE)


############################## Restaurant Price Level ##############################


######################## End of Restaurants  ##################################################

  
   
  
  
######################## Start of Hotels  ##################################################
  
  ############################## Hotels ##############################
  observeEvent(input$Hotels,{
    
    if(input$Hotels == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  ############################## Hotels ##############################
  
  
  
  ############################## Hotels Crime distance ##############################
  
  observeEvent(input$Hotels_Crime_Distance,{
    
    if(input$Hotels == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  ############################## Hotels Crime distance ##############################
  
  
  ############################## Hotels Rating ##############################
  
  observeEvent(input$Hotels_Rating,{
    
    if(input$Hotels == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  
  ############################## Hotels Rating ##############################
  
  
  
  ############################## Hotels Price Leve ##############################
  
  observeEvent(input$Hotels_Price_Level,{
    
    
    if(input$Hotels == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  
  ############################## Hotels Price Level ##############################
  
  
  ######################## End of Hotels  ##################################################
  
  
  
  
  
  ######################## Start of Entertainment  ##################################################
  
  ############################## Entertainment ##############################
  observeEvent(input$Entertainment,{
    
    if(input$Entertainment == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  ############################## Entertainment ##############################
  
  
  
  ############################## Entertainment Crime distance ##############################
  
  observeEvent(input$Entertainment_Crime_Distance,{
    
    if(input$Entertainment == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  ############################## Entertainment Crime distance ##############################
  
  
  ############################## Entertainment Rating ##############################
  
  observeEvent(input$Entertainment_Rating,{
    
    if(input$Entertainment == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  
  ############################## Entertainment Rating ##############################
  
  
  
  ############################## Entertainment Price Leve ##############################
  
  observeEvent(input$Entertainment_Price_Level,{
    
    
    if(input$Entertainment == TRUE){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
      
      Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url , Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
      
      
    }
    
    else{
      
      leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
      
    }
    
    
  }, ignoreNULL = FALSE)
  
  
  ############################## Entertainment Price Level ##############################
  
  
  ######################## End of Entertainment  ##################################################
  
  

  

######################## Start of Observe Crime  ##################################################


########################### Crime Type ########################
  observeEvent(input$Crime_Type,{

    if(input$Crime){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, icon = Crime_Icon, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      # Hotels
      if(input$Hotels == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
        
      }
      
      
      # Entertainment
      if(input$Entertainment == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
        
      }
      
      
      
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      
      # Hotels
      if(input$Hotels == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
        
      }
      
      
      # Entertainment
      if(input$Entertainment == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
        
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
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, icon = Crime_Icon, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address",icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      # Hotels
      if(input$Hotels == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
        
      }
      
      # Entertainment
      if(input$Entertainment == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
        
      }
      
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address",icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      # Hotels
      if(input$Hotels == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
        
      }
      
      # Entertainment
      if(input$Entertainment == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
        
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
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, icon = Crime_Icon, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
    }

    
  }, ignoreNULL = FALSE)
  
########################### Show Crime CheckBox ########################
    


########################### Crime Distance ########################
  observeEvent(input$Crime_Distance,{
    
    
    if(input$Crime){
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        addMarkers(lng = Result_Crime_Data$Crime_Longitude, lat = Result_Crime_Data$Crime_Latitude, icon = Crime_Icon, clusterId = "Crime", clusterOptions = markerClusterOptions() ) %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      # Hotels
      if(input$Hotels == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
        
      }
      
      # Entertainment
      if(input$Entertainment == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
        
      }
      
      
    }
    
    else{
      
      Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
      
      leafletProxy("Map") %>%
        removeMarkerCluster(layerId = "Crime") %>%
        removeMarker(layerId = "Current_Address") %>%
        addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
      
      
      # Restaurant
      if(input$Restaurant == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Restaurant <- Selected_Business(Position, "Restaurant", input$Map_zoom, input$Hotels_Crime_Distance, input$Restaurant_Rating, input$Restaurant_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")  %>% addMarkers(data = Selected_Restaurant, lng = ~Lon, lat = ~Lat, icon = ~Restaurant_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Restaurant", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Restaurant")
        
      }
      
      
      # Hotels
      if(input$Hotels == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Hotels <- Selected_Business(Position, "Hotels", input$Map_zoom, input$Hotels_Crime_Distance, input$Hotels_Rating, input$Hotels_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")  %>% addMarkers(data = Selected_Hotels, lng = ~Lon, lat = ~Lat, icon = ~Hotel_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Hotels", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Hotels")
        
      }
      
      # Entertainment
      if(input$Entertainment == TRUE){
        
        Result_Crime_Data <- Selected_Crime(Position, input$Crime_Time, input$Crime_Type, input$Crime_Distance, Crime_Geo_Data, Crime_Data)
        
        Selected_Crime_Geo_Data <- Result_Crime_Data[, c("CMPLNT_NUM", "Crime_Latitude", "Crime_Longitude")]
        
        Selected_Entertainment <- Selected_Business(Position, "Entertainment", input$Map_zoom, input$Entertainment_Crime_Distance, input$Entertainment_Rating, input$Entertainment_Price_Level, Selected_Crime_Geo_Data)
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")  %>% addMarkers(data = Selected_Entertainment,  lng = ~Lon, lat = ~Lat, icon = ~Entertainment_Icon_Maker(Crime_Count), popup = ~Business_Pop_Up_Maker(Name, Image_Url, Rating, Price_Level, Address, City, State, Zip_Code, Distance, Crime_Count), clusterId = "Entertainment", clusterOptions = markerClusterOptions() )
        
        
      }
      
      else{
        
        leafletProxy("Map") %>% removeMarkerCluster(layerId = "Entertainment")
        
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
            addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
          
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
                    addMarkers(lng = Position$Target_Lon, lat = Position$Target_Lat, layerId = "Current_Address", icon = Current_Position_Icon_Maker(nrow(Result_Crime_Data)) , popup = Current_Position_Pop_Up_Maker(nrow(Result_Crime_Data)))
                  
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
  

  
################################### OUTPUT for map ################################### 
  
  output$Map <- renderLeaflet({
    
    leaflet() %>% setView(lng = Position$Target_Lon, lat = Position$Target_Lat, zoom = 15) %>% addTiles()
    
  })

  
################################### OUTPUT for map ################################### 
  
  
################################### Statistics Crime Count in Boroughs ################
  output$Crime_Count_Boro <- renderPlot({
    if(input$Statistic_Year == "2018")
    {
      data <- crime[which(crime$Year == input$Statistic_Year),  ]
      data <- data[!(is.na(data$BORO_NM) | data$BORO_NM == ""), ]
    }
    if(input$Statistic_Year == "2017")
    {
      data <- crime[which(crime$Year == input$Statistic_Year),  ]
      data <- data[!(is.na(data$BORO_NM) | data$BORO_NM == ""), ]
    }
    if(input$Statistic_Year == "2016")
    {
      data <- crime[which(crime$Year == "2016"),  ]
      data <- data[!(is.na(data$BORO_NM) | data$BORO_NM == ""), ]
    }
    
    g <- reactive({ 
      ggplot(data, aes(BORO_NM)) + geom_bar(aes(fill = LAW_CAT_CD), width = 0.5, position=position_dodge(), colour="black") + 
        theme(text = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle=65, vjust=0.6)) +
        labs(title = "Bar Chart: Compare Crime count across 5 Boroughs",
             x = "5 Boroughs", y = "Crime Count")
    })
    g()
  })
  
################################### Statistics Crime Count in Boroughs ################  

  
################################### Statistics Monthly_Crime ################
  output$Monthly_Crime <- renderPlot({ 
    if(input$Statistic_Year == "2018")
    {
      data <- crime[which(crime$Year == input$Statistic_Year),  ]
    }
    if(input$Statistic_Year == "2017")
    {
      data <- crime[which(crime$Year == input$Statistic_Year),  ]
    }
    if(input$Statistic_Year == "2016")
    {
      data <- crime[which(crime$Year == "2016"),  ]
    }
    data <- data[!(is.na(data$BORO_NM) | data$BORO_NM == ""), ]
    
    data <- aggregate(data["LAW_CAT_CD"], by = data[c("Month", "LAW_CAT_CD")], FUN = length)
    colnames(data)[3] <- "Total"
    
    monthNames <- c("January","February","March","April","May","June","July",
                    "August","September","October","November","December")
    
    ggplot(data, aes(Month)) +
      geom_line(aes(y = Total, group = LAW_CAT_CD, colour = LAW_CAT_CD), size = 2) +
      scale_x_discrete(labels = monthNames) +
      scale_colour_manual(values = rainbow(14)) +
      theme(text = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(title = "Monthly crime totals per month",
           colour = "Crime type",
           y = "Total Crime")
  })
  
################################### Statistics Monthly_Crime ################
    
################################### Statistics Victim_Sex_Crime_Type ################
  output$Victim_Sex_Crime_Type <- renderPlot({
    if(input$Statistic_Crime_Type == "FELONY")
    {
      data <- crime[which(crime$LAW_CAT_CD == "FELONY"),  ]
    }
    if(input$Statistic_Crime_Type == "MISDEMEANOR")
    {
      data <- crime[which(crime$LAW_CAT_CD == "MISDEMEANOR"),  ]
    }
    if(input$Statistic_Crime_Type == "VIOLATION")
    {
      data <- crime[which(crime$LAW_CAT_CD == "VIOLATION"),  ]
    }
    
    data <- data[!(is.na(data$LAW_CAT_CD) | data$LAW_CAT_CD == ""), ]
    data <- data[!(data$VIC_SEX == "D" | data$VIC_SEX == "E"), ]
    
    pie <- ggplot(data, aes(x = "", fill = factor(VIC_SEX))) + 
      geom_bar(width = 1) +
      theme(text = element_text(size = 14, face = "bold"), 
            axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="VIC_SEX", 
           x = NULL, 
           y = NULL, 
           title="Pie Chart of Victim Sex for 3 different types of crime")
    
    pie + coord_polar(theta = "y", start=0)
  })
  
################################### Statistics Victim_Sex_Crime_Type ################
      
  
################################### Statistics Victim_Sex_Boroughs ################
  output$Victim_Sex_Boroughs <- renderPlot({
    if(input$Statistic_NYC_Borough == "MANHATTAN")
    {
      data <- crime[which(crime$BORO_NM == "MANHATTAN"),  ]
    }
    if(input$Statistic_NYC_Borough == "BRONX")
    {
      data <- crime[which(crime$BORO_NM == "BRONX"),  ]
    }
    if(input$Statistic_NYC_Borough == "QUEENS")
    {
      data <- crime[which(crime$BORO_NM == "QUEENS"),  ]
    }
    if(input$Statistic_NYC_Borough == "STATEN ISLAND")
    {
      data <- crime[which(crime$BORO_NM == "STATEN ISLAND"),  ]
    }
    if(input$Statistic_NYC_Borough == "BROOKLYN")
    {
      data <- crime[which(crime$BORO_NM == "BROOKLYN"),  ]
    }
    data <- data[!(is.na(data$BORO_NM) | data$BORO_NM == ""), ]
    data <- data[!(data$VIC_SEX == "D" | data$VIC_SEX == "E"), ]
    
    pie <- ggplot(data, aes(x = "", fill = factor(VIC_SEX))) + 
      geom_bar(width = 1) +
      theme(text = element_text(size = 14, face = "bold"), 
            axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="VIC_SEX", 
           x = NULL, 
           y = NULL, 
           title="Pie Chart of Victim Sex in 5 different Boroughs")
    
    pie + coord_polar(theta = "y", start=0)
  })
  
################################### Statistics Victim_Sex_Boroughs ################
  
        
################################### Statistics Crime_Count_Avg_Group ################
  output$Crime_Count_Avg_Group <- renderPlot({
    data <- crime[which(crime$VIC_AGE_GROUP == "<18" | crime$VIC_AGE_GROUP == "18-24" | crime$VIC_AGE_GROUP == "25-44" |
                          crime$VIC_AGE_GROUP == "45-64"| crime$VIC_AGE_GROUP == "65+"),  ]
    data <- aggregate(data["VIC_AGE_GROUP"], by = data[c("BORO_NM", "VIC_AGE_GROUP")], FUN = length)
    colnames(data)[3] <- "Total"
    
    ggplot(data, aes(x = VIC_AGE_GROUP, y = Total)) + 
      geom_bar(stat="identity", width=.5, fill="tomato3") + 
      labs(title="Crime Count Vs Avg Group") + 
      theme(text = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
################################### Statistics Crime_Count_Avg_Group ################
          
  
################################### Statistics Crime_Count_Race ################
  output$Crime_Count_Race <- renderPlot({
    data <- crime[which(crime$VIC_RACE == "BLACK" | crime$VIC_RACE == "WHITE" | crime$VIC_RACE == "WHITE HISPANIC" |
                          crime$VIC_RACE == "BLACK HISPANIC"| crime$VIC_RACE == "ASIAN/PAC.ISL" 
                        | crime$VIC_RACE == "AMER IND"),  ]
    data <- aggregate(data["VIC_RACE"], by = data[c("BORO_NM", "VIC_RACE")], FUN = length)
    colnames(data)[3] <- "Total"
    
    ggplot(data, aes(x = VIC_RACE, y = Total)) + 
      geom_bar(stat="identity", width=.5) + 
      labs(title="Crime Count Vs Race") + 
      theme(text = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle=65, vjust=0.6))
  })
################################### Statistics Crime_Count_Race ################
           
  
################################### Statistics During Day ################
  output$Crime_During_Day <- renderPlot({
    data <- aggregate(crime["Time"], by = crime[c("BORO_NM", "LAW_CAT_CD", "Time")], FUN = length)
    colnames(data)[4] <- "Total"
    data <- data[!(is.na(data$BORO_NM) | data$BORO_NM == ""), ]
    str(data) 
    
    ggplot(data, aes(x = Time, y = Total, color = LAW_CAT_CD)) + 
      geom_point() +
      geom_line() + 
      facet_wrap(~ BORO_NM, ncol =2) +
      ylab("Number of Crime")
  })

################################### Statistics Crime_Count_Race ################
  

}

# Run the application 
shinyApp(ui = ui, server = server)

