
# install packages 

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("forecast",  quietly = TRUE)) install.packages("forecast")
if (!requireNamespace("ggridges",  quietly = TRUE)) install.packages("ggridges")

# tidyverse for data cleaniang and all ggplot2 plotting
# lubridate for data parsing with dmy(), year(), month(), yday()
# forecast for ma() function for the 30 day long term moving average
# ggridges for ridgelines density plots for monthly temperature distributions 

library(tidyverse)
library(lubridate)
library(forecast)
library(ggridges)

# forecast masks dplyr::filter when loaded. This line restorres the correct version
filter <- dplyr::filter

# skip = 13 skips the station metadat heaader rows before the actual data begins
raw <- read.csv("Data Sherkin Island.csv",
                skip        = 13,
                header      = TRUE,
                na.strings  = c("", " "),
                stringsAsFactors = FALSE)

df <- raw %>%
  select(date, rain = rain, maxt = maxt, mint = mint) %>% # keep only the four columns needed 
  mutate(
    date      = dmy(date), # parse "01-jan-2000" string to a Date
    rain      = suppressWarnings(as.numeric(rain)), # coerce to numeric; blanks become NA
    maxt      = suppressWarnings(as.numeric(maxt)), # coerce max temp to numeric
    mint      = suppressWarnings(as.numeric(mint)), # coerce min temp to numeric
    meant     = (maxt + mint) / 2, # derive daily mean as midpoint of max and min
    year      = year(date), # extract four digit year
    month     = month(date, label = TRUE, abbr = TRUE), # month as ordered factor: jan, feb etc
    month_num = month(date), # month as integer 1-12 for axis ordering
    doy       = yday(date) # day of year for LTA matching 
  ) %>%
  filter(!is.na(date)) # remove any rows where the date string failed to parse

# step 1 is each day of year, compute the average temperature across all years
# this forms the raw climatological baseline before smoothing 

doy_avg <- df %>%
  filter(!is.na(meant)) %>%
  group_by(doy) %>%
  summarise(
    lta_meant = mean(meant, na.rm = TRUE), # multi year mean for this calendar day
    lta_maxt  = mean(maxt,  na.rm = TRUE),
    lta_mint  = mean(mint,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(doy) %>%
  mutate(
    # apply 30 day centred moving average to smooth the seasonal cycle 
    # formula: LTA_j = (1/m) * sum_{i=j-m+1}^{j} x_i  with m = 30
    # big note: the 30 NA warnings on plot 1 are expected, edges of the smoothing window
    lta_meant = as.numeric(ma(lta_meant, order = 30, centre = TRUE)),
    lta_maxt  = as.numeric(ma(lta_maxt,  order = 30, centre = TRUE)),
    lta_mint  = as.numeric(ma(lta_mint,  order = 30, centre = TRUE))
  )
  # step 2 is computing percentile bands per day of year across the full record
  # these form the shaded envelope on plot 1 showing typical temperature spread
doy_percentiles <- df %>%
  filter(!is.na(meant)) %>%
  group_by(doy) %>%
  summarise(
    p10 = quantile(meant, 0.10, na.rm = TRUE), # 10th percentile - -  unusally cold threshold
    p25 = quantile(meant, 0.25, na.rm = TRUE), # lower quartile 
    p75 = quantile(meant, 0.75, na.rm = TRUE), # upper quartile
    p90 = quantile(meant, 0.90, na.rm = TRUE), # 90th percentile - - unusually warm threshold
    .groups = "drop"
  )

  # step 3 is joining LTA and percentiles back onto every row of the main data frame by day of year
  # then calculate the daily departure for how far each day deviatedfrom its historical avg
df <- df %>%
  left_join(doy_avg, by = "doy") %>%
  left_join(doy_percentiles, by = "doy") %>%
  mutate(
    dep_meant = meant - lta_meant, # positive = warmer than the historical avg for that day
    dep_maxt  = maxt  - lta_maxt,
    dep_mint  = mint  - lta_mint
  )
  # print the five number summary for each temperatire variable
  # q1 to q3 is the IQR the range containing the middle 50% of all daily values

cat("Temperature quartiles \n")
for (v in c("meant", "maxt", "mint")) {
  q <- quantile(df[[v]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  cat(sprintf("%s -> Min: %.1f  Q1: %.1f  Median: %.1f  Q3: %.1f  Max: %.1f\n",
              v, q[1], q[2], q[3], q[4], q[5]))
}

cat(" Rainfall analysis ")

# a rainy day is defined as > 0.1mm which is the standard met eireann wet day threshold

df <- df %>% mutate(rainy_day = !is.na(rain) & rain > 0.1)

# empirical probability = number of rainy days divided by total days with valid rain records

p_rain <- mean(df$rainy_day[!is.na(df$rain)])
cat(sprintf("P(rainy day) = %.4f  (%.1f%% of days)\n", p_rain, p_rain * 100))

# first sum rain within each month of each year, then average those monthly totals across all years
# this gives the typical total rainfall expected in each calendar month

monthly_avg <- df %>%
  filter(!is.na(rain)) %>%
  group_by(year, month_num, month) %>%
  summarise(total = sum(rain, na.rm = TRUE), .groups = "drop") %>% # monthly total per year 
  group_by(month_num, month) %>%
  summarise(avg_rain = mean(total, na.rm = TRUE), .groups = "drop") %>% # mean across all years
  arrange(month_num)

cat("\nAverage monthly rainfall in mm:\n")
print(as.data.frame(monthly_avg[, c("month", "avg_rain")]))

# compute annual mean departure then standardise to z-scores
# z = (x - mean) / sd — a z score above 2 or below -2 marks a statistically great year

yearly_anomaly <- df %>%
  filter(!is.na(dep_meant)) %>%
  group_by(year) %>%
  summarise(mean_dep = mean(dep_meant, na.rm = TRUE), .groups = "drop") %>%
  mutate(z_score = as.numeric(scale(mean_dep))) # scale() centres and divides by SD

cat("Top five warmest years z-score \n")
print(yearly_anomaly %>% arrange(desc(z_score)) %>% head(5))
cat("Top five coldest years z-score \n")
print(yearly_anomaly %>% arrange(z_score) %>% head(5))

# shared minimal theme applied to every plot for visual consistency
theme_clean <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(10, 15, 10, 10)
  )

# filter the full dataset down to the chosen year for plot 1
chosen_year <- 2024
temp_year <- df %>% filter(year == chosen_year)

# plot 1 is daily temperature vs LTA with percentile shading for 2024
# shows whether each day in 2024 was above or below its long term average
p1 <- ggplot(temp_year, aes(x = date)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), fill = "#BDD7EE", alpha = 0.4) + # outer band is 10th-90th percentile
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#5B9BD5", alpha = 0.4) + # inner band: IQR (25th-75th)
  geom_line(aes(y = lta_meant), colour = "black", linewidth = 0.9) + # smoothed LTA curve
  geom_line(aes(y = meant), colour = "#C00000", linewidth = 0.5, alpha = 0.8) + # actual daily mean temp
  labs(
    title    = "Daily mean temperature vs long term average at Sherkin Island 2024",
    subtitle = "Red = daily mean | Black = LTA | Dark band = 25th to 75th percentile | Light band = 10th to 90th percentile",
    x = "Date", y = "Mean temperature (°C)"
  ) +
  theme_clean

print(p1)
ggsave("plot1_temp_vs_lta.png", p1, width = 10, height = 5, dpi = 180)

# plot 2 is ridgeline density plot of daily mean temperature by month across all years
# each ridge is the full distribution of temperatures for that month
# quantile lines mark Q1 median Q3. colour gradient shows temperature value
p2 <- df %>%
  filter(!is.na(meant)) %>%
  ggplot(aes(x = meant, y = fct_rev(month), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01,
                               quantile_lines = TRUE, quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_gradientn(
    colours = c("#313695", "#74ADD1", "#FEE090", "#F46D43", "#A50026"), # blue (cold) to red (warm)
    name = "Temp (°C)"
  ) +
  labs(
    title    = "Monthly Distribution of Mean Daily Temperature",
    subtitle = "Vertical lines mark Q1, median, and Q3 for each month",
    x = "Mean Daily Temperature (°C)", y = NULL
  ) +
  theme_clean

print(p2)
ggsave("plot2_temp_ridgeline.png", p2, width = 8, height = 7, dpi = 180)

# plot 3 is bar chart of average monthly rainfall, "the required "histogram on monthly rainfall"
# bar height is the average total rainfall for each calendar month across all years
# colour gradient from light to dark blue reinforces the rainfall amount

p3 <- monthly_avg %>%
  ggplot(aes(x = fct_reorder(month, month_num), y = avg_rain, fill = avg_rain)) +
  geom_col(width = 0.75, colour = "white") +
  geom_text(aes(label = round(avg_rain, 0)), vjust = -0.4, size = 3, colour = "grey30") + # label each bar with its value
  scale_fill_gradient(low = "#C6DBEF", high = "#08519C", name = "mm") +
  labs(
    title    = "Average monthly rainfall at Sherkin Island",
    subtitle = "Mean total precipitation in mm per calendar month across full record from 1972 to 2025",
    x = NULL, y = "Average Rainfall in mm"
  ) +
  theme_clean + theme(legend.position = "none")

print(p3)
ggsave("plot3_monthly_rain_bar.png", p3, width = 8, height = 5, dpi = 180)

# computing monthly rainfall totals per year for the heatmap
monthly_rain_ts <- df %>%
  filter(!is.na(rain), year >= 1975) %>%
  group_by(year, month_num, month) %>%
  summarise(total = sum(rain, na.rm = TRUE), .groups = "drop")

# plot 4 is the heatmap of monthly rainfall over time, the required "plot of monthly rainfall over time"
# each tile is one month in one year. darker = more rainfall
# reveals wet and dry years and the dominant winter rainfall pattern

p4 <- ggplot(monthly_rain_ts, aes(x = year, y = fct_rev(month), fill = total)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"), # light to dark blue
    name = "Rain (mm)", na.value = "grey90"
  ) +
  labs(
    title    = "Monthly rainfall over time",
    subtitle = "Each cell = total rainfall in that month/year | Darker = wetter",
    x = "Year", y = NULL
  ) +
  theme_clean + theme(axis.text.y = element_text(size = 8))

print(p4)
ggsave("plot4_rain_heatmap.png", p4, width = 12, height = 5, dpi = 180)

# Plot 5 is standardised annual temperature anomaly, extra analysis beyond requirements
# Red bars = years warmer than the long term mean, blue = cooler
# Dashed lines at +/-2 SD highlight statistcally good years
p5 <- yearly_anomaly %>%
  mutate(direction = ifelse(z_score >= 0, "Warmer", "Cooler")) %>%
  ggplot(aes(x = year, y = z_score, fill = direction)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("Warmer" = "#C00000", "Cooler" = "#2171B5"), name = NULL) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", colour = "grey50") + # +/-2 SD threshold lines
  labs(
    title    = "Standardised annual temperature anomaly",
    subtitle = "Z scores relative to long term mean | Dashed lines = +/-2 standard deviations",
    x = "Year", y = "Z score"
  ) +
  theme_clean

print(p5)
ggsave("plot5_anomaly_zscore.png", p5, width = 10, height = 4, dpi = 180)

cat("\nSaved plots.\n") #gg

