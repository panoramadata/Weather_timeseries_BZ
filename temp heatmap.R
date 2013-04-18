library(plyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(imputation)
library(reshape2)

setwd("~/Git/Weatherdata")

source("calendarHeat.R")

bz <- read.csv("CDO6355766284667.csv")

# Parse dates
bz$date <- ymd(bz$YEARMODA)
daycount <- table(bz$date)   




alldays <- seq(bz$date[1],length=10000,by="+1 day")
tail(alldays)

bz.all <- as.data.frame(alldays)

# Merge
bz.i <- merge(bz.all, bz, by.x = "alldays", by.y = "date", all.x = TRUE)

bz.temp <- as.vector(bz.i$TEMP)
temp.m <- matrix(bz.temp ,nrow = 100,ncol = 100)

temp.m <- lmImpute(temp.m)
bz.temp <- temp.m[[1]]

# Convert to Celsius
bz.temp.c <- (bz.temp - 32) * 5 / 9
hist(bz.temp.c)

# Replace extreme values
bz.temp.c[bz.temp.c < -12] <- 0


l <- ggplot(melt(bz.temp.c), aes(Var1,Var2, fill=value)) + geom_raster() + theme_minimal() +  labs(x = "", y = "") +
  theme(legend.position = "none",  axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks = element_blank(),  panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank() ) +
  scale_fill_gradient2(low = "blue", mid = "#FFCC33", high = "red", midpoint = 12)
l
ggsave(l, file="heatmap 10'000 days.jpeg", width=4, height=4)


# Convert to Celsius
bz.i$TEMP <- (bz.i$TEMP - 32) * 5 / 9

pdf('10000days.pdf')
calendarHeat(bz.i$alldays, bz.i$TEMP, varname="10000 days", color="r2b")
dev.off()

###

bz.i <- transform(bz.i, week = week(alldays),
                        wday = wday(alldays, label = TRUE, abbr = FALSE),
                        year = year(alldays) )



table(bz.i$wday)
correct.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

lll <- ggplot(bz.i, aes(week, wday, fill = TEMP)) +   theme_minimal(base_size = 8) +
  geom_tile(colour = "white") + 
  scale_fill_gradientn(colours = c("#99CCFF","#FFFFBD","#FFAE63","#FF6600")) + 
  facet_wrap(~ year, ncol = 1) +
  theme(strip.background = element_blank(), panel.margin = unit(0, "lines"), 
          axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y=element_text(size=6) ) +
  ylab("Weekdays") + xlab("Weeks") +
  scale_y_discrete(limits= rev(correct.week)) +
  ggtitle("Last 10'000 days of Bozen-Bolzano's temperature in °C")

ggsave(lll, file="cal 10000 days.pdf", width=6, height=20)


