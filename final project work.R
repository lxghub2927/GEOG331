datco2_columbia_aug <- read.csv("Z:/zwang2/data(lw)/asrc_data/ASRC1hAugust(test2).csv")
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

datco2_columbia_sep_oct <- read.csv("Z:/zwang2/data(lw)/asrc_data/ASRC_1h_merge_sep to oct_attempt_csv.csv")
head(datco2_columbia_sep_oct)
datco2_columbia_sep_oct$UTC_time

datco2_columbia_sep_oct$ddtime <- as.POSIXct(datco2_columbia_sep_oct$UTC_time, format = "%m/%d/%Y %H:%M", tz= "America/New_York")
datco2_columbia_sep_oct$CO2_ppm_num <- as.numeric(datco2_columbia_sep_oct$CO2_ppm)

plot(datco2_columbia_sep_oct$ddtime, datco2_columbia_sep_oct$CO2_ppm,
     type = "l",
     xlab = "Days in September & October 2025",
     ylab = expression(CO[2]~"(ppm)"),
     main = "Sample Data: CO2 Concentration at Columbia University (ASRC) Site - September & October 2025",
     lwd = 2,
     xaxt = "n")  


datco2_columbia_jul <- read.csv("Z:/zwang2/data(lw)/asrc_data/ASRC_jul_no_Table_name.csv")
head(datco2_columbia_jul)
datco2_columbia_jul$UTC_time



#########
#Attempt to plot all together
#########


data_dir <- "Z:/zwang2/data(lw)/asrc_data_2425"
dir.exists(data_dir)
list.files(data_dir)

csv_paths <- list.files(data_dir,
                        pattern = "\\.csv$",  
                        full.names = TRUE)

length(csv_paths)
csv_paths

for (f in csv_paths) {
  cat("\n=== File:", basename(f), "===\n")
  tmp <- read.csv(f, stringsAsFactors = FALSE)
  print(names(tmp))                     
  print(head(tmp$UTC_time, 3))         
}

per_month <- function(file_path) {
  monthly_data <- read.csv(file_path, stringsAsFactors = FALSE)
  

  monthly_data$ddtime <- as.POSIXct(
    monthly_data$UTC_time,
    format = "%m/%d/%Y %H:%M",          
    tz     = "America/New_York"         
  )
  
  cat("File:", basename(file_path), 
      "- parsed", sum(!is.na(monthly_data$ddtime)), 
      "of", length(monthly_data$ddtime), "timestamps\n")
  
  monthly_data$CO2_ppm <- as.numeric(monthly_data$CO2_ppm)
  
  monthly_data[, c("ddtime", "CO2_ppm")]
}

monthly_list <- lapply(csv_paths, per_month)
annual_data  <- do.call(rbind, monthly_list)

annual_data <- annual_data[order(annual_data$ddtime), ]
annual_data <- annual_data[!duplicated(annual_data$ddtime), ]

good <- with(annual_data, !is.na(ddtime) & !is.na(CO2_ppm))
annual_plot <- annual_data[good, ]

t_min <- min(annual_plot$ddtime)
t_max <- max(annual_plot$ddtime)

tz_used <- attr(t_min, "tzone")
tick_start <- as.POSIXct(strftime(t_min, "%Y-%m-01 00:00:00"), tz = tz_used)
tick_end   <- as.POSIXct(strftime(t_max, "%Y-%m-01 00:00:00"), tz = tz_used)

ticks <- seq(from = tick_start, to = tick_end, by = "1 month")

plot(annual_plot$ddtime, annual_plot$CO2_ppm,
     type = "l",
     xlab = "Date",
     ylab = expression(CO[2]~"(ppm)"),
     main = "CO2 at Columbia Site - Merged from January 2024 to Eary October 2025",
     lwd = 1.5,
     xaxt = "n")

axis.POSIXct(side = 1, at = ticks, format = "%b")  

