
library(shiny)
library(shiny.semantic)
library(shinyjs)
library(dplyr)
library(lubridate)
library(geosphere)
library(feather)
library(leaflet)
library(readr)
# put a red star
labelMandatory <- function(label) {
  tagList(
    label,
    shiny::span("*", class = "mandatory_star")
  )
}
# read data from feather file 
path <- "my_data.feather"
#write_feather(by_cyl, path)
df2 <- read_feather(path)
source("modules/shipuimodule.R")
introspiel= "Use this tool to find out the maximum distance travelled by various ships amongst various categories.Use the
dropdown to select and the map below shows the locations reported between maximum travel distance. More details available 
on clicking the marker to see a popup"

# call this function to recreate the max distance file from raw csv
Csv_ship_read<- function(){
  
  id <- "1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV" # google file ID
  df<-read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
   
  by_cyl <- df %>%
  filter(is_parked ==0) %>%
  group_by(ship_type) %>%
  group_modify(~ maxlonglatShiptype(.))


  maxlonglatShipname <- function(b){
 
  ad <- b %>%
    mutate(
      newdatetime = ymd_hms(DATETIME)) %>%
    distinct(newdatetime, .keep_all = TRUE) %>%
    arrange(newdatetime)
  shipval2 <- ad %>%
    select(LON,LAT) %>%
    data.matrix(rownames.force = NA)
#browser()
  ad2<-ad %>%
    mutate(
      distns = distGeo(shipval2, a=6378137, f=1/298.257223563)) %>%
    mutate(distns=lag(distns)) %>%
    na.omit() %>%
    slice(which.max(distns))
  if(nrow(ad2) > 1 ){
    time_to_use <- max(ad2$DATETIME)

  }
  else{
    time_to_use <- unique(ad2$DATETIME)

  }
  #print(length(ad2))
  index2<- which(ad$DATETIME ==time_to_use)
  ad[c(index2-1,index2),] %>%
    mutate(distance= ad2$distns)

}
  maxlonglatShiptype<- function(a){

  a %>%
    group_by(SHIPNAME) %>%
    group_modify(~ maxlonglatShipname(.))

}
  path <- "my_data.feather"
  write_feather(by_cyl, path)



}
