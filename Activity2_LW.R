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