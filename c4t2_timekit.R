# imports
library(tidyquant)
library(timekit)
library(ggplot2)

data <- read.table(
  './house.txt',
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

# back up of raw data
dataBak <- data
# restore
#data <- dataBak

### generate three timeseries objects, one for each submeter

# create detail metric data, group by year/week
dataTsYW <- 
  cbind(tk_get_timeseries_signature(data[,1]),data) %>%
  group_by(year, week) %>%
  summarise(SM1 = sum(Sub_metering_1)/1000,
            SM2 = sum(Sub_metering_2)/1000,
            SM3 = sum(Sub_metering_3)/1000) %>% 
  arrange(year, week)

# create timeseries objects for the 3 submeters from dataTsYW

tsSM1 <- ts(dataTsYW[,c(1:3)])
tsSM2 <- ts(dataTsYW[,c(1:2,4)])
tsSM3 <- ts(dataTsYW[,c(1:2,5)])