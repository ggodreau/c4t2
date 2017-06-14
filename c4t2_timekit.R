# imports
library(tidyquant)
library(timekit)
library(ggplot2)
library(forecast)

data <- read.table(
  './household_power_consumption.txt',
    sep=";",
  header=TRUE,
  as.is=TRUE)

#remove NAs
data <- na.omit(data)

# create dateTime column
data <- cbind(
  data,paste(data$Date,data$Time), 
  stringsAsFactors=FALSE)
colnames(data)[10] <-"dateTime"

# move dateTime col to col1 of table
data <- data[,c(ncol(data), 1:(ncol(data)-1))]

# remove the old date and time cols
data <- data[,-c(2:3)]

# convert dateTime to POSIXlt
data$dateTime <- as.POSIXct(strptime(
  data[,1],
  format="%d/%m/%Y %H:%M:%S",
  tz=""
))

# convert data types
data$Global_active_power <- as.double(data$Global_active_power)
data$Global_reactive_power <- as.double(data$Global_reactive_power)
data$Voltage <- as.double(data$Voltage)
data$Global_intensity <- as.double(data$Global_intensity)
data$Sub_metering_1 <- as.integer(data$Sub_metering_1)
data$Sub_metering_2 <- as.integer(data$Sub_metering_2)
data$Sub_metering_3 <- as.integer(data$Sub_metering_3)

# create detail metric data, group by year/week
dataTsYW <- 
  cbind(tk_get_timeseries_signature(data[,1]),data) %>%
  group_by(year, week) %>%
  summarise(SM1 = sum(Sub_metering_1)/1000,
            SM2 = sum(Sub_metering_2)/1000,
            SM3 = sum(Sub_metering_3)/1000) %>% 
  arrange(year, week)

# cull out the last 4 weeks of 2006 and throw out 53rd weeks
# this makes a nice (2007, 1) index for the ts() object
dataTsYW <- subset(dataTsYW, ((year > 2006)&(week != 53)))

# plot SM1,2,3 with no Holt-Winters smoothing
ts.plot(
  ts(dataTsYW[,3], start=(c(2007, 1)), frequency=52),
  ts(dataTsYW[,4], start=(c(2007, 1)), frequency=52),
  ts(dataTsYW[,5], start=(c(2007, 1)), frequency=52)
)

# plot SM1 with HW smoothing (can only do one submeter at a time)
plot(
  HoltWinters(
    ts(dataTsYW[,3], start=(c(2007, 1)), frequency=52), 
    beta=FALSE, gamma=FALSE
  )
)

# plot SM1 HW forecast for 1 year (52 weeks)
forecast::plot.forecast(
  forecast::forecast.HoltWinters(
    HoltWinters(
      ts(dataTsYW[,3], start=(c(2007, 1)), frequency=52), 
      beta=FALSE, gamma=FALSE
    ), 
    h=52
  )
)
