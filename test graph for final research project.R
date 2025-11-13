datco2_columbia_aug <- read.csv("//geogsv02/class/GEOG331_F25/zwang2/github_lw/GEOG331/data(lw)/ASRC1hAugust(test2).csv")
head(datco2_columbia_aug)                               

datco2_columbia_aug$ddtime <- as.POSIXct(datco2_columbia_aug$UTC_time, format = "%m/%d/%Y %H:%M", tz= "America/New_York")
datco2_columbia_aug$CO2_ppm_num <- as.numeric(datco2_columbia_aug$CO2_ppm)


plot(datco2_columbia_aug$ddtime, datco2_columbia_aug$CO2_ppm,
     type = "l",
     xlab = "Days in August 2025",
     ylab = expression(CO[2]~"(ppm)"),
     main = "Sample Data: CO2 Concentration at Columbia University (ASRC) Site - August 2025",
     lwd = 2,
     xaxt = "n")  

# Add custom x-axis ticks every 5 days
axis.POSIXct(1,
             at = seq(from = min(datco2_columbia_aug$ddtime, na.rm = TRUE),
                      to   = max(datco2_columbia_aug$ddtime, na.rm = TRUE),
                      by   = "5 days"),
             format = "%b %d")

