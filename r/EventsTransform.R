library(reader)
library(dplyr)
library(lubridate)
library(stringr)
library(statnet.common)

<<<<<<< HEAD


#trans_events <- function(x) {
DateFormats <- c("%Y%m%d %H%M%S", "dBY HMS", "dbY HMS", "dmyHMS", "BdY H", "%Y %b %d  %I:%M %p", "%Y%m%d  %I:%M:%p", "%Y %Om-%d", "%B %d, %Y", "%B %d %Y %I:%M %p", "%d/%Om/%Y")
#load events csv
df_events <- reader::reader("Events 2014-2017.csv")
=======
library(anytime)
#Load the events


#trans_events <- function(x) {
  df_events <- reader::reader("Events 2014-2017.csv")
>>>>>>> 79adf39af81050196f6e7cb565f7b28512f94d3d
#remove any events with a NA event location
df_events <- dplyr::filter(df_events ,!is.na(df_events$event_location_name))

#Convert the Char Event date to real Date
<<<<<<< HEAD
df_events$event_date <- lubridate::parse_date_time(df_events$year_month_day_seq.ymd.,DateFormats)
=======
lubridate::guess_formats(df_events$year_month_day_seq.ymd.,"%d/%Om/%Y")
df_events$event_date <- lubridate::parse_date_time(df_events$year_month_day_seq.ymd.,"%d/%Om/%Y")
>>>>>>> 79adf39af81050196f6e7cb565f7b28512f94d3d
max(df_events$event_date)
#lubridate::guess_formats(df_events$year_month_day_seq.ymd.,c("%d/%b/%y", "y-b", "dmyHMS", "BdY H"))
#Filter out out Eventsdf_events
#df_events <- df_events %>%
# filter(df_events$event_date>"2016-11-01")

#======= Clean Time details ============
#Create the 24 hour DF
DF24 <- data.frame("Time" = seq(
  from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
  to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
  by="hour"
) %>%
  strftime( format="%H:%M:%S"))

<<<<<<< HEAD
#======== Start Date & Time Cleaning Process

#Remove the @ value from the start date
DatetimecleanS <- df_events$event_start %>% str_replace("@", "")

#Phase 1 try and parse start date
Datetimeclean <- lubridate::parse_date_time(DatetimecleanS , DateFormats) 
#Get the indexs of the not parse dates
NotParsed <- which(is.na(Datetimeclean))

#Phase 2 add the year prefix and try to parse again
DatetimecleanS[NotParsed] <- paste0(substr(df_events$year_month_day_seq.ymd.[NotParsed], 7, 11), "-", DatetimecleanS[NotParsed])
Datetimeclean <- lubridate::parse_date_time(DatetimecleanS, DateFormats)

#Get the indexs of the not parse dates
NotParsed <- which(is.na(Datetimeclean)) #This is a check
NotParsed


#Find all the date which do not have times and add a default of 8am
notime <- which(lubridate::hour(Datetimeclean)==0)
DatetimecleanS[notime] <- paste0(lubridate::as_date(Datetimeclean[notime]), " ", "08:00:00")
Datetimeclean <- lubridate::parse_date_time(DatetimecleanS, DateFormats)


#Final Check
NotParsed <- which(is.na(Datetimeclean)) #This is a check
NotParsed

df_events$event_start_clean <- Datetimeclean

paste0("Max Start Date in range : ",max(df_events$event_start_clean) )# Return Max and min of 
paste0("Min Start Date in range : ",min(df_events$event_start_clean)) # Return Max and min of 

#========== END OF START DATE TIME CLEANING =============
#========== START OF END DATE TIME CLEANING =============

#Remove the @ value from the start date
DatetimecleanS <- df_events$event_end %>% str_replace("@", "")

#Find all the NA end dates
EndDateNa <- which(is.na(DatetimecleanS))
#replade the NA dates with the Start date & 10pm finish time
DatetimecleanS[EndDateNa] <- paste0(lubridate::as_date(df_events$event_start_clean[EndDateNa]), " ", "22:00:00")

#Phase 1 try and parse start date
Datetimeclean <- lubridate::parse_date_time(DatetimecleanS, DateFormats)
#Get the indexs of the not parse dates
NotParsed <- which(is.na(Datetimeclean))

DatetimecleanS[NotParsed] #DEBUG

#First Deal with the NA

#Phase 2 add the year prefix and try to parse again
DatetimecleanS[NotParsed] <- paste0(substr(df_events$year_month_day_seq.ymd.[NotParsed], 7, 11), "-", DatetimecleanS[NotParsed])
Datetimeclean <- lubridate::parse_date_time(DatetimecleanS, DateFormats)

#Get the indexs of the not parse dates
NotParsed <- which(is.na(Datetimeclean)) #This is a check
NotParsed


#Find all the date which do not have times and add a default of 8am
notime <- which(lubridate::hour(Datetimeclean) == 0)
DatetimecleanS[notime] <- paste0(lubridate::as_date(Datetimeclean[notime]), " ", "22:00:00")
Datetimeclean <- lubridate::parse_date_time(DatetimecleanS, DateFormats)


#Final Check
NotParsed <- which(is.na(Datetimeclean)) #This is a check
NotParsed

df_events$event_end_clean <- Datetimeclean

paste0("Max Start Date in range : ", max(df_events$event_end_clean)) # Return Max and min of 
paste0("Min Start Date in range : ", min(df_events$event_end_clean)) # Return Max and min of 

#========== END OF END DATE TIME CLEANING =============
=======


#Clean and extract the time event start and event end
# Note : added the year value to get the time value in the correct year
timeclean <-  paste0(substr(df_events$year_month_day_seq.ymd.,7,11)," ",df_events$event_start) %>% str_replace("@", "")
df_events$time_startc <- as.POSIXct(timeclean,format = "%Y %b %d  %I:%M %p") 

#========= Date Start & End Cleaning ====================
#Extract the Start Dates from the Time start
library(anytime)
# Add the trailling function

# Create a new Date from the Year + The start date
dateclean <- paste0(substr(df_events$year_month_day_seq.ymd.,7,11),"-",df_events$event_start)  %>% 
  substr(1,11)
#Trim the trailling spaces from the field
dateclean <- trimws(dateclean, which= "right")
#Replace the remaining spaces with "-" 
dateclean <- gsub(" ","-",dateclean)
head(dateclean,10)

#Fantastic lubridate function parse_date_time to process almost any format of date!
dateclean <- lubridate::parse_date_time(dateclean,c("y-b-d", "y-b", "dmyHMS", "BdY H") )

df_events$date_startc <- dateclean
df_events$datetime_startc <- paste0(dateclean ," ", "08:00:00") # Add default start time of 8am
# ========= End of Date Start C ============

timeclean <- paste0(substr(df_events$year_month_day_seq.ymd.,7,11)," ",df_events$event_end)%>% str_replace("@", "")
df_events$time_endc <- as.POSIXct(timeclean,format = "%Y %b %d  %I:%M %p")
df_events$time_endc <-  lubridate::parse_date_time(timeclean,c("%Y %b %d  %I:%M %p","%Y %Om-%d"))

head(lubridate::guess_formats(timeclean,"%Y %b %d"),10)
head(timeclean,10)
head(df_events$time_endc,10)


## 06/06 Error happens before here !!!!!!!!!!!!!!
head(df_events$time_endc[order(df_events$time_endc , decreasing = TRUE)],40)

#Add any missing end event times by defaulting to 5pm on the start date
head(df_events$event_end,10)
df_events <- df_events %>% 
  mutate(event_end2 = coalesce(as.character(time_endc, paste0(as.character(date_startc,format="%b-%d")," ","17:00:00"))))
#--------> TODO : Really not happy with the above line but gets things sorted 




# Create a new Date from the Year + The start date
dateclean <- paste0(substr(df_events$year_month_day_seq.ymd.,7,11),"-",df_events$event_end2)  %>% 
  substr(1,11)
head(dateclean,10)
#Trim the trailling spaces from the field
dateclean <- trimws(dateclean, which= "right")
#Replace the remaining spaces with "-" 
dateclean <- gsub(" ","-",dateclean)
head(dateclean,10)

#Fantastic lubridate function parse_date_time to process almost any format of date!
dateclean <- lubridate::parse_date_time(dateclean,c("y-b-d", "y-b", "dmyHMS", "BdY H") )

df_events$date_endc <- dateclean
df_events$datetime_endc <- paste0(dateclean ," ", "22:00:00")  # Add default end time of 8pm
head(df_events$datetime_endc)

# === End of Date End C ================


#Fill any unspecified Start Date/Times with created data from steps above
df_events <- df_events %>% 
  mutate(clean_start_dateS = coalesce(as.character(time_startc),as.character(datetime_startc))
#Convert string values to dates
df_events$clean_start_date <- lubridate::parse_date_time(df_events$clean_start_dateS,"%Y-%Om-%d %H:%M:%S")
#Fill any unspecified Start Date/Times with created data from steps above
df_events <- df_events %>% 
  mutate(clean_end_dateS = coalesce(as.character(time_endc),as.character(datetime_endc)))
#Convert string values to dates
df_events$clean_end_date <- lubridate::parse_date_time(df_events$clean_end_dateS,"%Y-%Om-%d %H:%M:%S")

#== End of Date Cleaning ==========
>>>>>>> 79adf39af81050196f6e7cb565f7b28512f94d3d



#Remove all unwanted data leaving only the cleaned start and end dates, the event location &
<<<<<<< HEAD
df_events_trim <- dplyr::transmute( df_events, year_month_day_seq.ymd.,event_name, event_location_name, event_start_clean, event_end_clean)
=======
df_events_trim <- dplyr::transmute( df_events, year_month_day_seq.ymd.,event_name, event_location_name, clean_start_date, clean_end_date)
>>>>>>> 79adf39af81050196f6e7cb565f7b28512f94d3d

#Cross join the 24hour generation DF with the Processed Events
df_event_merge <- data.frame(df_events_trim %>% mutate(temp=1) %>%
  dplyr::inner_join(DF24 %>% mutate(temp=1) , BY = "temp" ))

#Create a formmated date version of the crossed joined time
<<<<<<< HEAD
df_event_merge$timedateS <- paste0(as.character(df_event_merge$event_start_clean,format="%d/%Om/%Y"), " ",df_event_merge$Time)
=======
df_event_merge$timedateS <- paste0(as.character(df_event_merge$clean_start_date,format="%d/%Om/%Y"), " ",df_event_merge$Time)
>>>>>>> 79adf39af81050196f6e7cb565f7b28512f94d3d

#Add a datetime version of the new joined 24 hour time for use in filter below
df_event_merge$timedate <-lubridate::parse_date_time(df_event_merge$timedateS,c("%d/%Om/%Y %H:%M:%S"))

#Remove all lines where the generated time interval is outside the event start and end times
<<<<<<< HEAD
df_event_merge <- dplyr::filter(df_event_merge,  df_event_merge$timedate >= df_event_merge$event_start_clean & df_event_merge$timedate <= df_event_merge$event_start_clean) 
=======
df_event_merge <- dplyr::filter(df_event_merge,  df_event_merge$timedate >= df_event_merge$clean_start_date & df_event_merge$timedate <= df_event_merge$clean_end_date) 
>>>>>>> 79adf39af81050196f6e7cb565f7b28512f94d3d
df_event_time <- dplyr::transmute(df_event_merge, df_event_merge$event_location_name, df_event_merge$timedate) # Remove all non-required vars

library(dummies)
#Generate dummy flags for event times 
event_dummy <- dummies::dummy.data.frame(df_event_time,sep=".")
#Drop the time Col
event_dummy <- subset(event_dummy, select = -c(`df_event_merge$timedate`))

#remove the event locations and summrise the date times
df_event_time2 <- data.frame((df_event_time$`df_event_merge$timedate`))
colnames(df_event_time2) <- c("DateTime")

#Join the event times dummy back to the event times 
df_final <- dplyr::bind_cols(df_event_time2, event_dummy) 

#Summrise per every event per hour
df_final2 <- df_final %>% group_by(DateTime) %>% summarise_all(funs(max))


#df_final #Return the final dataset
#}
