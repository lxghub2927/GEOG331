#New file for activity/hw2
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

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Q2 vector creations:
numeric_vector <- c(1.1, 2.2, 3.14157, -2.25, 100.00)
int_vector <- c(1,2,0,-3,-5)
character_vector <- c("Hello", "114514", "1919810", "demerits","meow")
logic_vector <- c(TRUE, FALSE, NA, TRUE, FALSE)

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
datW$TAVE
#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)
datW$siteN

#histogram creation
#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#q3 answers
help(hist)
help(paste)

#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
par(mfrow=c(2,2))
plot(1:2, main = "Plot 1")
#livermore histogram representation (2nd city)
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
