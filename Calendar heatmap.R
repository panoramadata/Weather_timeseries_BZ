library(plyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

setwd("~/Git/Weatherdata")
source("calendarHeat_10000.R")

bz <- read.csv("CDO6355766284667.csv")

# Parse dates
bz$date <- ymd(bz$YEARMODA)

alldays <- seq(bz$date[1],length=10000,by="+1 day")
tail(alldays)

# Merge
bz.i <- merge(as.data.frame(alldays), bz, by.x = "alldays", by.y = "date", all.x = TRUE)
bz.i <- transform(bz.i, week = week(alldays),
                        wday = wday(alldays),
                        year = year(alldays) )
# Convert to Celsius
bz.i$Temperature <- (bz.i$TEMP - 32) * 5 / 9
head(bz.i)

bz.short <- bz.i[which(bz.i$year > 2010),]

calendarHeat(bz.short$alldays, bz.short$Temperature, varname="10000 days", color="r2b")

pdf('calendar_10000_days.pdf')
calendarHeat(bz.i$alldays, bz.i$Temperature, date.form = "%Y%m%d", varname="10000 days", color="r2b")
dev.off()
