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
par(mfrow=c(2,2))
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

#livermore histogram representation (2nd city)
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="yellow",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Mandan Experiment Station data histogram representation (3rd city)
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="dodgerblue1",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Mormon Flat Station data histogram representation (4th city)
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="green3",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 4)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Morrisville Station data histogram representation (5th city)
hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="sienna1",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3",
       lwd = 5)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#dnorm function showcase from exercise:
#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h_aberdeen <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#moving to calculating probability of temp occuring
help(dnorm)
#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)-> To calculate probability of getting a freezing temp at this site
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 so taking away possiblity of below 0 temp gives me prob of obtaining temp between 0-5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#creation of current extreme temperature threshold
extreme_temp <- qnorm(0.95, 
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#calculation of probability for Q6 with modification to mean according to question
1 - pnorm(extreme_temp,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Question 7: Precipitation of Aberdeen
#To create a histogram depicting precipitation in Aberdeen
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipitation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Question 8: Calculating sum annual precipitation:
#aggregating and creating datasets pf average annual precipitation organized by year.
#construction is completed through the utilization of aggregate feature. modification of parameters/argument required for calculation of annual precipitation.
annual_prcp_abr <- aggregate(datW$PRCP[datW$siteN == 1], by=list(datW$year[datW$siteN == 1]), FUN="sum",na.rm=TRUE)
annual_prcp_abr


#reorganization of data into 
colnames(annual_prcp_abr) <- c("Year","Total Annual Prcp (mm)")
annual_prcp_abr


annual_prcp_abr$Year <- as.numeric(annual_prcp_abr$Year)
annual_prcp_abr

hist(annual_prcp_abr$`Avg Prcp (mm)`,
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation (mm)", 
     ylab="Relative frequency",
     col="cadetblue2",
     border="white")

#repeat of steps for site 2 (Livermore, CA)
annual_prcp_lvr <- aggregate(datW$PRCP[datW$siteN == 2], by=list(datW$year[datW$siteN == 2]), FUN="sum",na.rm=TRUE)
annual_prcp_lvr

colnames(annual_prcp_lvr) <- c("Year","Total Annual Prcp (mm)")
annual_prcp_lvr

annual_prcp_lvr$Year <- as.numeric(annual_prcp_lvr$Year)
annual_prcp_lvr

#repeat of steps for site 3 (Mandan Experiment Station, ND)
annual_prcp_mes <- aggregate(datW$PRCP[datW$siteN == 3], by=list(datW$year[datW$siteN == 3]), FUN="sum",na.rm=TRUE)
annual_prcp_mes

colnames(annual_prcp_mes) <- c("Year","Total Annual Prcp (mm)")
annual_prcp_mes

annual_prcp_mes$Year <- as.numeric(annual_prcp_mes$Year)
annual_prcp_mes

#repeat of steps for site 4 (Mormon Flat, AZ)
annual_prcp_mf <- aggregate(datW$PRCP[datW$siteN == 4], by=list(datW$year[datW$siteN == 4]), FUN="sum",na.rm=TRUE)
annual_prcp_mf

colnames(annual_prcp_mf) <- c("Year","Total Annual Prcp (mm)")
annual_prcp_mf

annual_prcp_mf$Year <- as.numeric(annual_prcp_mf$Year)
annual_prcp_mf

#repeat of steps for site 5 (Morrisville, NY)
annual_prcp_ms <- aggregate(datW$PRCP[datW$siteN == 5], by=list(datW$year[datW$siteN == 5]), FUN="sum",na.rm=TRUE)
annual_prcp_ms

colnames(annual_prcp_ms) <- c("Year","Total Annual Prcp (mm)")
annual_prcp_ms

annual_prcp_ms$Year <- as.numeric(annual_prcp_ms$Year)
annual_prcp_ms

#question 9: calculating average annual precipitation
#Usage of mean function to determine mean of the annual precipitation for all sites <- obtained earlier from Q8.
#e.g. mean of aberdeen obtained here
mean_prcp_abr <- mean(annual_prcp_abr$`Total Annual Prcp (mm)`)
mean_prcp_abr

#repeat steps for Livermore
mean_prcp_lvr <- mean(annual_prcp_lvr$`Total Annual Prcp (mm)`)
mean_prcp_lvr

#repeat steps for M.E.S.
mean_prcp_mes <- mean(annual_prcp_mes$`Total Annual Prcp (mm)`)
mean_prcp_mes

#repeat steps for Mormon Flats
mean_prcp_mf <- mean(annual_prcp_mf$`Total Annual Prcp (mm)`)
mean_prcp_mf

#repeat steps for Morrisville
mean_prcp_ms <- mean(annual_prcp_ms$`Total Annual Prcp (mm)`)
mean_prcp_ms
