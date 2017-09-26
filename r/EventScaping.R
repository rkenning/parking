library(rvest)
library(magrittr)

from <- "2016-10-01"
to <- "2016-12-01"

year_month_day_seq <- seq(as.Date(from), as.Date(to), by = "months") %>%
  substr(., 0, nchar(.)+6)

year_month_day_seq

df <- get_events_detail(year_month_day_seq[1], year_month_day_seq[2])
df2 <- NA

year_month_day_seq[1]

for (ymd in 2:length(year_month_day_seq)-1) {
  paste0(c("Processing : ", year_month_day_seq[ymd],"-" ,  year_month_day_seq[ymd+1])) %>%
  print()
  flush.console() 
  df2 <- get_events_detail(year_month_day_seq[ymd], year_month_day_seq[ymd+1])
  df <- rbind(df,df2)
  Sys.sleep(200)
  
}

write.csv(df, file = "Events 2017-01-09.csv")



