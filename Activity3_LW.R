#Activity 3: GEOG 331: Luke Wang
#Testing your codes


##### Start of basic codes that will be used for exercise #####

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
#install.packages(c("lubridate"))
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

##### Start of Codes that links to answers ######

###############################################

#For Question 5
#Using assert to see if lightscale is in fact incorporating the lightning.activity values. Assert functions checks this by testing to see if data length is same for both group of values.
assert(length(lightscale) == length(datW$lightning.acvitivy), "error: unequal length")

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

################################################

#Question 6
#utilization of the previous filter to create a filtered wind speed data.
datW$wind.speedQ6<- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#utilization of assert to test if length of both filtered wind speed and air temp is the same. Same suggests successful filtering
assert(length(datW$air.tempQ2) == length(datW$wind.speedQ6), "error: unequal length")

plot(datW$DD , datW$wind.speedQ6, xlab = "Day of Year", ylab = "Wind Speed (suspect measurements removed)",
     type="n")
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)   
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)


plot(datW$DD, datW$wind.speedQ6, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind Speed (suspect measurements removed)")
title(main="Filtered Wind Speed Plot")
###############################################

#Question 7
#Utilization of plotting of graphs to check if soil sensor was reliable up to date of sabotage.
#The first part involves comparison between precipitatin and soil moisture to see if soil moisture measurements are accurate.

#Utilization of par function to alter parameters and allow plotting of 2nd y-axis on right side of the graph.
par(mar=c(5,4,4,4)+0.1)

#Using of plotting functions and points function to plot precipitation data onto a scatter plot
plot(datW$DD , datW$precipitation, type = "n", xlab = "Day of year", ylab = "Precipitation(mm)")
points(datW$DD, datW$precipitation, 
       col= "blue")
#par(new = TRUE) allows plotting of new group of data
par(new = TRUE)
#plotting of soil moisture points into graph
plot(datW$DD , datW$soil.moisture, type = "l", col="orange", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
#utilization of axis and mtext to incorporate a 2nd y-axis that corresponds to the 2nd set of data, soil moisture.
axis(side=4)
mtext("Soil Moisture Level(meters cubed per meter cubed)", side = 4, line = 3)
#incorporating a title and legend to help presentation of the datas
title(main="Precipitation compared w. Soil Moisture")
legend("topright", legend = c("Precipitation(mm)", "Soil Moisture Level(meters cubed per meter cubed)"), col = c("blue","orange"), lty = c(2), pch=c(1))

#repeat of previous steps for comparison of air temperature to soil temperature
par(mar=c(5,4,4,4)+0.1)
plot(datW$DD , datW$air.tempQ2, type = "n", xlab = "Day of year", ylab = "Air Temperature (Celsius)")
points(datW$DD, datW$air.tempQ2, 
       col= "blue")
par(new = TRUE)
plot(datW$DD , datW$soil.temp, type = "l", col="orange", xaxt = "n", yaxt = "n", ylab = "", xlab = "")

axis(side=4)
mtext("Soil Temperature (Celsius)", side = 4, line = 3)

title(main="Air Temperature compared w. Soil Temeprature")
legend("topright", legend = c("Air Temperature (Celsius)", "Soil Temperature (Celsius)"), col = c("blue","orange"), lty = c(2), pch=c(1))

##################################################

#Question 8

#showcase of finding required values of air temperature.
#Utilization of mean function to calculate mean of specified measurement. Utilization of na.rm=TRUE to remove missing values from calculation.
mean_airtemp <- mean(datW$air.tempQ2, na.rm=TRUE)
#utilization of sum function and !is.na function to determine how many observations went into this calculation.
num_valid_airtemp <- sum(!is.na(datW$air.tempQ2))
#printing of mean value and number of observations went in.
print(mean_airtemp)
print(num_valid_airtemp)

#repeat of previous steps for wind speed
mean_windspeed <- mean(datW$wind.speedQ6, na.rm=TRUE)
num_valid_ws <- sum(!is.na(datW$wind.speedQ6))
print(mean_windspeed)
print(num_valid_ws)

#repeat of steps for soil temperature
mean_st <- mean(datW$soil.temp, na.rm=TRUE)
num_valid_st <- sum(!is.na(datW$soil.temp))
print(mean_st)
print(num_valid_st)

#repeat of steps for soil moisture
mean_sm <- mean(datW$soil.moisture, na.rm=TRUE)
num_sm <- sum(!is.na(datW$soil.moisture))
print(mean_sm)
print(num_sm)

#repeat of steps for precipitation
#small alterations to calculate summed up precipitation observations instead of calculating mean.
t_prcp <- sum(datW$precipitation)
num_prcp <- sum(!is.na(datW$precipitation))
print(t_prcp)
print(num_prcp)

##################################################

#Question 9
#Utilization of parameters to create a 2x2 digram depicting the 4 plots to be plotted for Q9.
par(mfrow=c(2,2))

#Plotting of the precipitation data with title
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation (mm)",
     type="n")
points(datW$DD, datW$precipitation, 
       col= "blue")
title(main="Precipitation Data")

#Plotting of the soil moisture data with title
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture (Meters Cubed per Meter Cube)",
     type="n")
points(datW$DD, datW$soil.moisture, 
       col= "tomato")
title(main="Soil Moisture Data")

#Plotting of air temperature data with title
plot(datW$DD , datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temperature (Celsius)",
     type="n")
points(datW$DD, datW$air.tempQ2, 
       col= "orange")
title(main="Air Temperature Data")

#Plotting of soil temperature data with title
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature (Celsius)",
     type="n")
points(datW$DD, datW$soil.temp, 
       col= "brown")
title(main="Soil Temperature Data")

