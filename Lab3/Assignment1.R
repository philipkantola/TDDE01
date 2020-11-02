# somehow my working directory changed, so had to redirect it
setwd("kod/Skola/R/Lab3")


set.seed(12345)
library(geosphere)
stations <- read.csv('stations.csv', fileEncoding="latin1")
temps <- read.csv('temps50k.csv')
stFull <- merge(stations,temps,by="station_number")

times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")



# coordinates for Abisko
latitude = 59.4037
longitude = 18.3506



# date to predict the weather for
myDate <- "2019-12-17"

st = subset(stFull, as.Date(stFull$date) <= as.Date(myDate))

help(subset)

coordinates = c(longitude, latitude)

# converts date from string to Date type
dateTemp <- as.Date(myDate)
# take out only month and day from date
dateFormatted = substr(dateTemp, 6, 10)


# width constant for distance kernel, needs to be big since the distance is measured in meters
h_distance = 200000
# width constant for date kernel
h_date = 5
# width constant for hour kernel
h_time = 2

distanceVector = 1:length(st[,1])

# create kernel vector which transposes data points to higher dimension depending 
# on how close they are to abisko (the chosen location)
distanceKernel = 1:length(st[,1])
for (i in 1:length(st[,1])) {
  # distHaversine is function which calculates shortest distance between
  # two coordinates taking in account the curvature of earth.
  # x is distance from chosen location to data point, divided by a width constant 
  x = distHaversine(coordinates,c(st$longitude[i], st$latitude[i]))/h_distance
  # gaussian kernel function 
  distanceKernel[i] = exp(-x^2)
}
# plot to see if h is reasonable, should be able to diffirentiate between data points
plot(distanceKernel)

# kernel vector transposing data points depending on the difference in days 
dateKernel = 1:length(st[,1])
for (i in 1:length(st[,1])) {
  # difftime gives difference in time between the two dates, independent of year
  diffDays = abs(difftime(strptime(substr(st$date[i], 6, 10), format = "%m-%d"),
                  strptime(dateFormatted, format = "%m-%d"), units = "days"))
  # if days are past half a year away, the counting reverts
  if(isTRUE(diffDays > 183)) {
    diffDays = 365 - diffDays
  }
  x = diffDays/h_date
  # gaussian kernel function 
  dateKernel[i] = exp(-as.numeric(x)^2)
}
plot(dateKernel)


hourKernel <- function(st, h, hour) {
# kernel vector transposing data points depending on the difference in hours of the day 
hourKernelVector = 1:length(st[,1])
# converts time from string to time type
hourFormatted = as.POSIXct(hour, format = "%H:%M:%S")
for (i in 1:length(st[,1])) {
  x = difftime(as.POSIXct(st$time[i], format = "%H:%M:%S"),hourFormatted, units="hours")/h
  # gaussian kernel function 
  hourKernelVector[i] = exp(-as.numeric(x)^2)
}
return(hourKernelVector)
}
# vector to store the sum kernel for each hour
tempSum <- vector(length=length(times))
tempProduct <- vector(length=length(times))
# sum the kernels to one
finalKernelSum = 1:length(st[,1])
finalKernelProduct = 1:length(st[,1])
for (i in 1:length(times)) {
  
  finalKernelSum = distanceKernel + dateKernel + hourKernel(st,h_time,times[i])
  finalKernelProduct = distanceKernel * dateKernel * hourKernel(st, h_time, times[i])
  #na.rm=T is used, seems like some value(s) in data set is NA , this will avoid the problem
  tempSum[i] = sum(finalKernelSum*st$air_temperature, na.rm=T)/sum(finalKernelSum, na.rm=T)
  tempProduct[i] = sum(finalKernelProduct*st$air_temperature, na.rm=T)/sum(finalKernelProduct, na.rm=T)
}


#xaxt removes labels from x axis
plot(tempSum,  type="o", xlab = "hour",xaxt = "n", ylab = "temperature", main = "Sum of kernels")
# axis puts labels to axis
axis(1, at=1:length(times), labels=times)
plot(tempProduct,  type = "o",  xlab = "hour",xaxt = "n", ylab="temperature", main = "Product of kernels")
# axis puts labels to axis
axis(1, at=1:length(times), labels=times)

