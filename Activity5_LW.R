#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("Z:/zwang2/github_lw/GEOG331/data/hw5_data/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH) 

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:/zwang2/github_lw/GEOG331/data/hw5_data/2049867.csv")                            
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
#basic formatting
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
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
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


#adding legend
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
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
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border


##################
#Question 5 & Question 6
#GOTTA EDIT THIS
## 1) 2017 daily means (by DOY)
d2017 <- subset(datD, year == 2017)
ave2017 <- aggregate(discharge ~ doy, d2017, mean, na.rm = TRUE)

## 2) Nice y-limits that include mean±SD and 2017
ymin <- 0
ymax <- max(c(aveF$dailyAve + sdF$dailySD, ave2017$discharge), na.rm = TRUE)
ymax <- ceiling(ymax/10)*10  # round up a bit

## 3) Month ticks (use a non-leap template, 2017)
month_starts <- yday(ymd(paste(2017, 1:12, 1, sep = "-")))  # 1,32,60,...

month_labs   <- month.abb

## 4) Plot mean, ribbon (±1 SD), custom axes, then overlay 2017 line
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

## 5) 2017 overlay (distinct color)
lines(ave2017$doy, ave2017$discharge, lwd = 2, col = "#D55E00") # orange

legend("topright",
       c("mean", "±1 SD", "2017"),
       lwd = c(2, NA, 2),
       fill = c(NA, rgb(0.392, 0.584, 0.929, .2), NA),
       col  = c("black", NA, "#D55E00"),
       border = NA,
       bty = "n")



###################
#Question 7
###################

library(dplyr)

TZ <- "America/New_York"   # set your timezone as appropriate

# --- 1) Ensure datetime columns exist ---
# Precip (assumes datP$DATE is "YYYY-mm-dd HH:MM" or similar)
datP <- datP %>%
  mutate(dt = ymd_hm(DATE, tz = TZ),
         date = as.Date(dt))

# Discharge (use your existing dt column if you already built it)
if (!("dt" %in% names(datD))) {
  # Example if you have separate date ("m/d/Y") and time ("HH:MM")
  datD <- datD %>%
    mutate(dt = as_datetime(as.Date(date, "%m/%d/%Y"), tz = TZ) + hm(time))
}
datD$date <- as.Date(datD$dt)

# --- 2) Auto-detect precip sampling interval & expected count per day ---
# Uses the median interval across the whole series (robust if a few gaps exist)
int_min <- as.numeric(median(diff(sort(unique(datP$dt)))), units = "mins")
expected_per_day <- round(1440 / int_min)  # e.g., 24 for hourly, 96 for 15-min

# If you know it's hourly and want to be strict, you could set: expected_per_day <- 24

# --- 3) Build the "complete precip days" dataframe ---
# Change 'precip' below to your precipitation column name if different
value_col <- "precip"                            # <- adjust if needed (e.g., "Precip", "P")
has_value <- if (value_col %in% names(datP)) !is.na(datP[[value_col]]) else rep(TRUE, nrow(datP))

precip_daily <- datP %>%
  mutate(has_value = has_value) %>%
  group_by(date) %>%
  summarise(
    n_obs = sum(has_value),                      # non-NA measurements counted
    expected = expected_per_day,
    full24 = n_obs >= expected,                  # TRUE if day is "complete"
    .groups = "drop"
  )

# This is the dataframe you asked for:
# precip_daily has columns: date, n_obs, expected, full24
# View the complete days:
# subset(precip_daily, full24)

# --- 4) Plot discharge and symbolize complete-precip days ---
# choose y-limits
ylim_max <- max(datD$discharge, na.rm = TRUE)
ylim_max <- ceiling(ylim_max/10)*10

# dates to highlight
full_dates <- precip_daily$date[precip_daily$full24]
idx_full <- datD$date %in% full_dates

# Make the plot
plot(datD$dt, datD$discharge, type = "l",
     xlab = "Date",
     ylab = expression(paste("Discharge (ft"^3, " sec"^-1, ")")),
     main = "Discharge with Days of Complete Precipitation Coverage",
     ylim = c(0, ylim_max))

# Add points for times that fall on complete precip days (distinct color/symbol)
points(datD$dt[idx_full], datD$discharge[idx_full],
       pch = 16, col = "#D55E00", cex = 0.6)

legend("topleft",
       legend = c("Discharge", "Complete precip day"),
       lty    = c(1, NA),
       lwd    = c(2, NA),
       pch    = c(NA, 16),
       col    = c("black", "#D55E00"),
       bty    = "n")

