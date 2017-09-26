library(reader)
library(dplyr)
library(lubridate)
library(stringr)
library(statnet.common)



#trans_events <- function(x) {
DateFormats <- c("%Y%m%d %H%M%S", "dBY HMS", "dbY HMS", "dmyHMS", "BdY H", "%Y %b %d  %I:%M %p", "%Y%m%d  %I:%M:%p", "%Y %Om-%d", "%B %d, %Y", "%B %d %Y %I:%M %p", "%d/%Om/%Y")
#load events csv
df_events <- reader::reader("Events 2014-2017.csv")
#remove any events with a NA event location
df_events <- dplyr::filter(df_events ,!is.na(df_events$event_location_name))

#Convert the Char Event date to real Date
df_events$event_date <- lubridate::parse_date_time(df_events$year_month_day_seq.ymd.,DateFormats)
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



#Remove all unwanted data leaving only the cleaned start and end dates, the event location &
df_events_trim <- dplyr::transmute( df_events, year_month_day_seq.ymd.,event_name, event_location_name, event_start_clean, event_end_clean)

#Cross join the 24hour generation DF with the Processed Events
df_event_merge <- data.frame(df_events_trim %>% mutate(temp=1) %>%
  dplyr::inner_join(DF24 %>% mutate(temp=1) , BY = "temp" ))

#Create a formmated date version of the crossed joined time
df_event_merge$timedateS <- paste0(as.character(df_event_merge$event_start_clean,format="%d/%Om/%Y"), " ",df_event_merge$Time)

#Add a datetime version of the new joined 24 hour time for use in filter below
df_event_merge$timedate <-lubridate::parse_date_time(df_event_merge$timedateS,c("%d/%Om/%Y %H:%M:%S"))

#Remove all lines where the generated time interval is outside the event start and end times
df_event_merge <- dplyr::filter(df_event_merge,  df_event_merge$timedate >= df_event_merge$event_start_clean & df_event_merge$timedate <= df_event_merge$event_start_clean) 
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
