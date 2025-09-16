#New filed for activity/hw2
#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#c is a concotanate
#covert heights to cm
h_cm <- heights*100
h_cm

#matric\es
help (matrix)

#function inside function -> matrices
#by row
mat <- matrix (c(1,2,3,4,5,6), ncol=2, byrow = TRUE)
mat
#by column
mat_col <- matrix (c(1,2,3,4,5,6), ncol=2, byrow = FALSE)
mat_col

#start of NOAA exercise
#extraction of data to computer with file path
datW <- read.csv("Z:\\zwang2\\github_lw\\GEOG331\\data\\NOAA\\noaa_weather\\2011124.csv", 
                 stringsAsFactors = T)
#to obtain more info abt dataframe:
str(datW)
#9 variables are seen suggesting there are 9 columns
#according to data, there are 157849 objcts. This suggests there are 157849 rows.

#specify a column with a proper date format <- copied from instructions
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#find out all unique site names
unique(datW$NAME)
#To find mean maximum temp for Aberdeen, WA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
#NA suggests data is missing, we have to ignore the NA data here
#Use argument to ignore -> Look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature <- "Since this is the average across many decades of observations, this value can help tell us about the climate of the site. Before we move on, average daily temperature is often more helpful to evaulate temperature. The average is always halfway between the minimum and maximum.
#This temperature will be halfway between the minimum and maximum temperature" (Prof Loranty, activity 2)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp