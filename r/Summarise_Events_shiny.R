library(rvest)
library(magrittr)
library(dplyr)
library(sp)
library(maps)
library(ggmap)
library(leaflet)

#Get the Events details from the CSV
dfe <- read.csv("Events 2014-2017.csv", header=TRUE) 
#Tempfilename required for getGeoDetails
tempfilename <- "Events 2014-2017 geo.raw"

#Quick check of the events df
summary(dfe)

#Summrise the Events dataframe by event_name, event_location_name, event_postcode, event_street, event_locality
df3 <- data.frame(count(dfe,dfe$event_name,dfe$event_location_name, dfe$event_postcode, dfe$event_street, dfe$event_locality))


#define a function that will process googles server responses
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}


#### Event Lat Long processing #####
#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
#Create a vector of address
addresses  <- paste(df3$dfe.event_location_name,df3$dfe.event_street, df3$dfe.event_locality, df3$dfe.event_postcode , sep = ",")

# Loop through the Event locations and get lat long details
for (ii in seq(1, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(enc2utf8(addresses[ii])) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(result, geocoded)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#Created sorted version of geocoded by index
geoSort <- geocoded[order(geocoded$index),]

#Add the new Lat Long details to the Events DF
dfjoined <-cbind(df3,geoSort)

#Write the process Event DF to save re-processing lat/long request 
write.csv(dfjoined, file = "EventsSummary.csv" )



## Process Car Park Data ####
library("dplyr")
#1 Data Collection and preparation 
BANEScarparking <- read.csv("BANES_Historic_Car_Park_Occupancy.csv", header=TRUE) 
BANEScarparking$Name[1:10] # takes the column called Name

#Summrise the Parking details by Parking name and location
dfgroup <- BANEScarparking %>%
  group_by(Name, Location) %>%
  dplyr::summarise(n=n())

df <- data.frame(dfgroup)


# Regular expression matches all groups within the string which are at least 1
# character long (that's what the + does) and which are made up of digits 0-9
# and the characters "." and "-" (i.e. the things within the brackets) ONLY
#   * regexpr() finds the first match; gregexpr() finds all matches
#   * regmatches() uses the locations returned by gregexpr() to extract the
#     substrings
split <- regmatches(df$Location , gregexpr("[0-9.-]+", df$Location))
# Write new columns Lat and Long: use the [[ "subsetting" function to take the
# first, and then second, element of each item in the list
#   * You can write a list directly to a new dataframe column!
#df$Lat <- lapply(split, "[[", 1)
dfgroup$Lat <- lapply(split, "[", 1)
dfgroup$Long <- lapply(split, "[", 2)

dfgroup$Long <- as.numeric(as.character(unlist(dfgroup$Long)))
dfgroup$Lat <- as.numeric(as.character(unlist(dfgroup$Lat)))

dfgroupsub <- subset( dfgroup, is.na(dfgroup$Lat) == 0)

write.csv(dfgroupsub, file = "CarParkSummary.csv" )


###### Event & Parking Plotting #########
dfmap <- read.csv(file = "EventsSummary.csv", header=TRUE)
dfgroupsub <- read.csv(file = "CarParkSummary.csv", header=TRUE)

MapIcons <- iconList(
  event = makeIcon("event.png", "event.png", 18, 18),
  carpark = makeIcon("carpark.png", "carpark.png", 18, 18)
)

m<- leaflet(data = dfmap) %>% addTiles() %>%
  addMarkers(~dfmap$long, ~dfmap$lat, popup = ~as.character(dfmap$dfe.event_location_name), 
             label = ~as.character(dfmap$dfe.event_location_name),
             icon = ~MapIcons["event"] )
m
addMarkers(m, dfgroupsub$Long, dfgroupsub$Lat, data = dfgroupsub, 
           icon = ~MapIcons["carpark"], label = ~as.character(dfgroupsub$Name))

#Output the map and plotted points



### Translate Event Data ####

# Pivot Event summary to create matrix framework for all locations
library(reshape)
dfmap
cast(dfmap, dfmap$dfe.event_location_name)


# 







