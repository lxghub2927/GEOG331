
library(readr)
library(dplyr)
library(lubridate)


#####
#Creating data directory for 2025 data#
data_dir_25 <- "Z:/zwang2/data(lw)/asrc_data_2025"
datdir_list_25 <- list.files(data_dir_25, pattern = "\\.csv$", full.names = TRUE)

dir.exists(data_dir_25)
list.files(data_dir_25)

utc_time_col_25 <- "UTC_time"
co2_col_25 <- "CO2_ppm"

raw_df_25 <- datdir_list_25 |>
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE)
  }) |>
  bind_rows()

co2_df_25 <- raw_df_25 |>
  select(all_of(c(utc_time_col_25, co2_col_25))) |>
  rename(
    time_utc = !!utc_time_col_25,
    co2_ppm  = !!co2_col_25
  )

co2_df_25 <- co2_df_25 |>
  mutate(
    time_utc = mdy_hm(time_utc, tz="UTC")
  )

daily_co2_25 <- co2_df_25 |>
  mutate(date_utc = as.Date(time_utc, tz = "UTC")) |>
  group_by(date_utc) |>
  summarise(
    mean_co2_25 = mean(co2_ppm, na.rm=TRUE),
    n_measurements = n(),
    .groups = "drop"
  )

#Is data normally distributed?
hist(daily_co2_25$mean_co2_25,
     breaks = 30,
     probability=TRUE,
     main = "Distribution of CO2 ppm(2025)",
     xlab = "CO2 PPM",
     col = "blue")

shapiro.test(daily_co2_25$mean_co2_25)
#Nah

###'24 data###
data_dir_24 <- "Z:/zwang2/data(lw)/asrc_data_2024"
datdir_list_24 <- list.files(data_dir_24, pattern = "\\.csv$", full.names = TRUE)

dir.exists(data_dir_24)
list.files(data_dir_24)

utc_time_col_24 <- "UTC_time"
co2_col_24 <- "CO2_ppm"

raw_df_24 <- datdir_list_24 |>
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE)
  }) |>
  bind_rows()

co2_df_24 <- raw_df_24 |>
  select(all_of(c(utc_time_col_24, co2_col_24))) |>
  rename(
    time_utc = !!utc_time_col_24,
    co2_ppm  = !!co2_col_24
  )

co2_df_24 <- co2_df_24 |>
  mutate(
    time_utc = mdy_hm(time_utc, tz="UTC")
  )

daily_co2_24 <- co2_df_24 |>
  mutate(date_utc = as.Date(time_utc, tz = "UTC")) |>
  group_by(date_utc) |>
  summarise(
    mean_co2_24 = mean(co2_ppm, na.rm=TRUE),
    n_measurements = n(),
    .groups = "drop"
  )

#Is data normally distributed?
hist(daily_co2_24$mean_co2_24,
     breaks = 30,
     probability=TRUE,
     main = "Distribution of CO2 ppm (2024)",
     xlab = "CO2 PPM",
     col = "red")

shapiro.test(daily_co2_24$mean_co2_24)
#Nah

#########
#Wilcoxon test & data analysis#
#########

daily_co2_24_mutated <- daily_co2_24 |>
  mutate(month_day = format(date_utc, "%m-%d"))

daily_co2_25_mutated <- daily_co2_25 |>
  mutate(month_day = format(date_utc, "%m-%d"))

matched_data <- inner_join(
  daily_co2_24_mutated |> select(month_day, mean_co2_24, date_utc_2024 = date_utc),
  daily_co2_25_mutated |> select(month_day, mean_co2_25, date_utc_2025 = date_utc),
  by = "month_day"
)

cat("Number of matched pairs:", nrow(matched_data), "\n")
head(matched_data)

wilcox_result <- wilcox.test(matched_data$mean_co2_24,
                             matched_data$mean_co2_25,
                             paired = TRUE,
                             alternative = "two.sided")

print(wilcox_result)

median_2024 <- median(matched_data$mean_co2_24, na.rm = TRUE)
median_2025 <- median(matched_data$mean_co2_25, na.rm = TRUE)
median_diff <- median(matched_data$mean_co2_25 - matched_data$mean_co2_24, na.rm = TRUE)
percent_change <- ((median_2025 - median_2024) / median_2024) * 100

cat("\n--- Summary Statistics ---\n")
cat("2024 Median CO2:", round(median_2024, 2), "ppm\n")
cat("2025 Median CO2:", round(median_2025, 2), "ppm\n")
cat("Median difference:", round(median_diff, 2), "ppm\n")
cat("Percent change:", round(percent_change, 2), "%\n")
cat("P-value:", wilcox_result$p.value, "\n")

###Visualizing the comparison###
par(mfrow = c(1, 2))

###Boxplot###
boxplot(matched_data$mean_co2_24, matched_data$mean_co2_25,
        names = c("2024", "2025"),
        main = "CO2 Emissions Comparison",
        ylab = "CO2 (ppm)",
        col = c("lightblue", "lightgreen"))


###Distribution of Differences###
differences <- matched_data$mean_co2_25 - matched_data$mean_co2_24
hist(differences,
     main = "Daily Differences (2025 - 2024)",
     xlab = "CO2 Difference (ppm)",
     col = "coral",
     breaks = 30)
abline(v = 0, col = "red", lwd = 2, lty = 2)
abline(v = median(differences), col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Zero", "Median"),
       col = c("red", "blue"), lty = 2, lwd = 2)

#########
#Attempt to plot all together
#########
csv_paths <- list.files(data_dir_25,
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
     main = "CO2 at Columbia Site - January to Eary October 2025",
     lwd = 1.5,
     xaxt = "n")

axis.POSIXct(side = 1, at = ticks, format = "%b")  


'''


###
Preliminary Data Plotting
###


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
'''
