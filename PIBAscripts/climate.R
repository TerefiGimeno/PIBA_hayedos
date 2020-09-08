library(lubridate)
library(dplyr)
source('PIBAscripts/basicFunTEG.R')
Part <- read.csv('PIBAdata/euskalmet/Pday_Artikutza.csv')
Part$Date <- ymd(Part$YYYYMMDD)
Part$Year <- year(Part$Date)
Part$Month <- month(Part$Date, label = T)
Part_month  <- summarise(group_by(Part, Month, Year),
                         Pmonth = sum(Pday_mm, na.rm = T),
                         len = lengthWithoutNA(Pday_mm))
Part_month_summ <- summarise(group_by(Part_month, Month),
                             Pmonth_mean = mean(Pmonth, na.rm =T), Pmonth_se = s.err(Pmonth))
Part_month <- left_join(Part_month, Part_month_summ, by = 'Month')
Part_month$Pmonth <- ifelse(Part_month$len < 24, Part_month$Pmonth_mean, Part_month$Pmonth)
Part_year <- summarise(group_by(Part_month, Year),
                       Pann = sum(Pmonth))

