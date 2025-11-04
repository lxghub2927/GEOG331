#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("//geogsv02/class/GEOG331_F25/zwang2/github_lw/GEOG331/data/hw5_data/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH) 

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("//geogsv02/class/GEOG331_F25/zwang2/github_lw/GEOG331/data/hw5_data/2049867.csv")                            
head(datP)

#only use most reliable measurements <- symbolized by "A"
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))          

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

###############
#Q3: Checking number of observations
nrow(datD)
nrow(datP)

###############
#Q4:
?expression
?paste


##############
###Plotting the average daily discharge across all years with s.d.###
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
    #using polygon feature to highlight standard deviation around the mean values on the graph
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes  
    axes = FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #adding legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border



##################
#Question 5 & Question 6

#Isolating and aggregating 2017 data
day_2017 <- subset(datD, year == 2017)
average_2017 <- aggregate(discharge ~ doy, day_2017, mean, na.rm = TRUE)

#Incorporate proper data for ymax and ymin and round to more 
ymin <- 0
ymax <- max(c(aveF$dailyAve + sdF$dailySD, average_2017$discharge), na.rm = TRUE)
ymax <- ceiling(ymax/10)*10 

#Isolate and highlight month data to be placed onto x axis + 2017 not an leap year
month_starts <- yday(ymd(paste(2017, 1:12, 1, sep = "-")))  # 1,32,60,...
#Use .abb function to incorporate built in month abbrevations
month_labs   <- month.abb

#Plot graph with average annual data across all years with s.d.
par(mai = c(1,1,1,1))
plot(aveF$doy, aveF$dailyAve,
     type = "l",
     xlim = c(1, 365), ylim = c(ymin, ymax),
     xaxs = "i", yaxs = "i",
     xlab = "Month",
     ylab = expression(paste("Discharge (ft"^3, " sec"^-1, ")")),
     lwd = 2, axes = FALSE)

polygon(c(aveF$doy, rev(aveF$doy)),
        c(aveF$dailyAve - sdF$dailySD, rev(aveF$dailyAve + sdF$dailySD)),
        col = rgb(0.392, 0.584, 0.929, .2),
        border = NA)

axis(1, at = month_starts, labels = month_labs)                 # months on x-axis
axis(2, las = 2)                                                # y-axis
box()

#Plot 2017 layer that overlays the average annual data
lines(average_2017$doy, average_2017$discharge, lwd = 2, col = "orange") # orange
#Incorporate legend on top right according to requirements.
legend("topright",
       c("Mean Discharge Across All Years", "±1 Standard Deviation", "Mean Discharge in 2017"),
       lwd = c(2, NA, 2),
       fill = c(NA, rgb(0.392, 0.584, 0.929, .2), NA),
       col  = c("black", NA, "orange"),
       border = NA,
       bty = "n")
###################


###################
#Question 7
#call dplyer for data manipulation and creation of dataframe
library(dplyr)

#Set correct time zone for creation of dataframe
TZ <- "America/New_York"  

#Setting datatime for dataframe
datP$dt   <- ymd_hm(datP$DATE, tz = TZ)
datP$date <- as.Date(datP$dt)
datP$year <- year(datP$dt)
datP$doy  <- yday(datP$dt)


#checking 4 data that would fail to parse
which(is.na(datP$dt))
datP[is.na(datP$dt), "DATE"]
#attempt to debug....
datP <- datP %>% 
  mutate(
    dt_local = ymd_hm(DATE, tz = TZ, quiet = TRUE),
    date = as.Date(ymd(substr(DATE, 1, 8)))
  )

p_counts <- datP %>%
  group_by(date) %>%
  summarise(n_obs = sum(!is.na(dt_local)), .groups = "drop") %>%
  mutate(full_day = n_obs %in% c(23L, 24L, 25L))


datD$date <- as.Date(datD$date, format = "%m/%d/%Y")
datD_24days <- merge(datD, p_counts[, c("date", "full_day")], by = "date", all.x = TRUE)


datD_24days
datD_24days$dt
names(datD_24days)
grep("^dt", names(datD_24days), value = TRUE)  # look for dt, dt.x, dt.y, etc.
datD_24days$dt <- as_datetime(as.Date(datD_24days$date, format="%m/%d/%Y"), tz=TZ) + hm(datD_24days$time)

plot(datD_24days$dt, datD_24days$discharge, type = "l",
     xlab = "Date",
     ylab = expression(paste("Discharge (ft"^3, " sec"^-1, ")")),
     main = "Discharge with Full Precipitation Days",
     col = "black")

###!!!!! Add points or markers for full precip days!!!!
points(datD_24days$dt[datD_24days$full_day],
       datD_24days$discharge[datD_24days$full_day],
       col = "orange", pch = 16)

###################

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
min(hydroD$discharge)


#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
yl
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
yh
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP, na.rm = TRUE))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

###################
###Test Q9
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#####Q9#####


