#Activity 3: GEOG 331: Luke Wang
#Testing your codes

#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")
#evaluation of 1 == 1 is true, so nothing is printed
assert(1 == 1)

#evaluate of anothing true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#Data from Bewkes site
#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:/zwang2/github_lw/GEOG331/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
print(datW[1,])
#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:/zwang2/github_lw/GEOG331/data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])


#use install.packages to install lubridate
install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again.

library(lubridate)

assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }
}
#the following segment incorporates converting the times of the data

#convert to standardized format <- standard: new york
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#the following segment incorporates how to remove false anomalous datas.
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#checking for realistic values:
#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]

#chapter: measurements outside of sensor capabilities


#The following segment describes using lightning sensors and precipitation sensors to detect thunderstorms. In turn, this points out data collected during thunderstorms that might be unreliable due to inaccuracy from heavy rain and strong wind

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


###############################################

#For Question 5
#Using assert to see if lightscale is in fact incorporating the lightning.activity values. Assert functions checks this by testing to see if data length is same for both group of values.
assert(length(lightscale) == length(datW$lightning.acvitivy))

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

################################################

#Question 6
datW$wind.speedQ6<- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

assert(length(datW$air.tempQ2) == length(datW$wind.speedQ6))

plot(datW$DD , datW$wind.speedQ6, xlab = "Day of Year", ylab = "Wind Speed (suspect measurements removed)",
     type="n")
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)   
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


plot(datW$DD, datW$wind.speedQ6, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind Speed (suspect measurements removed)")

###############################################

#Question 7
par(mar=c(5,4,4,4)+0.1)
plot(datW$DD , datW$precipitation, type = "n", xlab = "Day of year", ylab = "Precipitation(mm)")
points(datW$DD, datW$precipitation, 
       col= "blue")
par(new = TRUE)
plot(datW$DD , datW$soil.moisture, type = "l", col="orange", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
'''
points(datW$DD[datW$soil.moisture > 0], datW$soil.moisture[datW$soil.moisture > 0], 
       col= "orange", 
      pch=15)
'''
axis(side=4)
mtext("Soil Moisture Level(meters cubed per meter cubed)", side = 4, line = 3)
title(main="Precipitation compared w. Soil Moisture")
legend("topright", legend = c("Precipitation(mm)", "Soil Moisture Level(meters cubed per meter cubed)"), col = c("blue","orange"), lty = c(2), pch=c(1))

'''
plot(datW$DD , datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temperature & Soil Temperature",
     type="n")
points(datW$DD, datW$air.tempQ2, 
       col= "blue")
lines(datW$DD[datW$soil.temp > 0], datW$soil.temp[datW$soil.temp > 0],
      col= "orange", pch=200)
title(main="Air Temperature compared w. Soil Temperature")
'''
par(mar=c(5,4,4,4)+0.1)
plot(datW$DD , datW$air.tempQ2, type = "n", xlab = "Day of year", ylab = "Air Temperature (Celsius)")
points(datW$DD, datW$air.tempQ2, 
       col= "blue")
par(new = TRUE)
plot(datW$DD , datW$soil.temp, type = "l", col="orange", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
'''
points(datW$DD[datW$soil.moisture > 0], datW$soil.moisture[datW$soil.moisture > 0], 
       col= "orange", 
      pch=15)
'''
axis(side=4)
mtext("Soil Temperature (Celsius)", side = 4, line = 3)
title(main="Air Temperature compared w. Soil Temeprature")
legend("topright", legend = c("Air Temperature (Celsius)", "Soil Temperature (Celsius)"), col = c("blue","orange"), lty = c(2), pch=c(1))

##################################################

#Question 8

num_valid_values1 <- sum(!is.na(datW$wind.speedQ6))
print(num_valid_values1)

num_valid_values2 <- sum(!is.na(datW$DD))
print(num_valid_values2)

num_valid_values3 <- sum(!is.na(datW$air.temperature))
print(num_valid_values3)

mean_airtemp <- mean(datW$air.tempQ2, na.rm=TRUE)
rmean_airtemp <- round(mean_airtemp, digits = 1)
print(rmean_airtemp)

mean_windspeed <- mean(datW$wind.speedQ6, na.rm=TRUE)
rmean_ws <- round(mean_windspeed, digits = 1)
print(rmean_ws)

mean_st <- mean(datW$soil.temp, na.rm=TRUE)
rmean_st <- round(mean_st)
print(rmean_st)

mean_prcp <- mean(datW$precipitation, na.rm=TRUE)
#how to round this??????
print(mean_prcp)
'''
totalmeanairtemp <- mean(datW$air.temperature, na.rm=TRUE)
print(totalmeanairtemp)
'''

##################################################

#Question 9
par(mfrow=c(2,2))
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation",
     type="n")
points(datW$DD, datW$precipitation, 
       col= "blue")
title(main="Precipitation")
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="n")
points(datW$DD, datW$soil.moisture, 
       col= "tomato")
title(main="Soil Moisture")
plot(datW$DD , datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temperature",
     type="n")
points(datW$DD, datW$air.tempQ2, 
       col= "orange")
title(main="Air Temperature")
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Precipitation",
     type="n")
points(datW$DD, datW$soil.temp, 
       col= "brown")
title(main="Soil Temperature")

'''
probably useless codes
identical(datW$soil.moisture, datW$soil.moistureQ2)
all.equal(datW$soil.moisture, datW$soil.moistureQ2)
environment(datW$soil.moisture)
'''
