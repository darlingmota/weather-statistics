if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("forecast",  quietly = TRUE)) install.packages("forecast")
if (!requireNamespace("ggridges",  quietly = TRUE)) install.packages("ggridges")

library(tidyverse)
library(lubridate)
library(forecast)
library(ggridges)

filter <- dplyr::filter

raw <- read.csv("Data Sherkin Island.csv",
                skip        = 13,
                header      = TRUE,
                na.strings  = c("", " "),
                stringsAsFactors = FALSE)

df <- raw %>%
  select(date, rain = rain, maxt = maxt, mint = mint) %>%
  mutate(
    date      = dmy(date),
    rain      = suppressWarnings(as.numeric(rain)),
    maxt      = suppressWarnings(as.numeric(maxt)),
    mint      = suppressWarnings(as.numeric(mint)),
    meant     = (maxt + mint) / 2,
    year      = year(date),
    month     = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date),
    doy       = yday(date)
  ) %>%
  filter(!is.na(date))

doy_avg <- df %>%
  filter(!is.na(meant)) %>%
  group_by(doy) %>%
  summarise(
    lta_meant = mean(meant, na.rm = TRUE),
    lta_maxt  = mean(maxt,  na.rm = TRUE),
    lta_mint  = mean(mint,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(doy) %>%
  mutate(
    lta_meant = as.numeric(ma(lta_meant, order = 30, centre = TRUE)),
    lta_maxt  = as.numeric(ma(lta_maxt,  order = 30, centre = TRUE)),
    lta_mint  = as.numeric(ma(lta_mint,  order = 30, centre = TRUE))
  )

doy_percentiles <- df %>%
  filter(!is.na(meant)) %>%
  group_by(doy) %>%
  summarise(
    p10 = quantile(meant, 0.10, na.rm = TRUE),
    p25 = quantile(meant, 0.25, na.rm = TRUE),
    p75 = quantile(meant, 0.75, na.rm = TRUE),
    p90 = quantile(meant, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

df <- df %>%
  left_join(doy_avg, by = "doy") %>%
  left_join(doy_percentiles, by = "doy") %>%
  mutate(
    dep_meant = meant - lta_meant,
    dep_maxt  = maxt  - lta_maxt,
    dep_mint  = mint  - lta_mint
  )

cat("Temperature quartiles \n")
for (v in c("meant", "maxt", "mint")) {
  q <- quantile(df[[v]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  cat(sprintf("%s -> Min: %.1f  Q1: %.1f  Median: %.1f  Q3: %.1f  Max: %.1f\n",
              v, q[1], q[2], q[3], q[4], q[5]))
}

cat(" Rainfall analysis ")

df <- df %>% mutate(rainy_day = !is.na(rain) & rain > 0.1)

p_rain <- mean(df$rainy_day[!is.na(df$rain)])
cat(sprintf("P(rainy day) = %.4f  (%.1f%% of days)\n", p_rain, p_rain * 100))

monthly_avg <- df %>%
  filter(!is.na(rain)) %>%
  group_by(year, month_num, month) %>%
  summarise(total = sum(rain, na.rm = TRUE), .groups = "drop") %>%
  group_by(month_num, month) %>%
  summarise(avg_rain = mean(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(month_num)

cat("\nAverage monthly rainfall in mm:\n")
print(as.data.frame(monthly_avg[, c("month", "avg_rain")]))

yearly_anomaly <- df %>%
  filter(!is.na(dep_meant)) %>%
  group_by(year) %>%
  summarise(mean_dep = mean(dep_meant, na.rm = TRUE), .groups = "drop") %>%
  mutate(z_score = as.numeric(scale(mean_dep)))

cat("Top five warmest years z-score \n")
print(yearly_anomaly %>% arrange(desc(z_score)) %>% head(5))
cat("Top five coldest years z-score \n")
print(yearly_anomaly %>% arrange(z_score) %>% head(5))

theme_clean <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(10, 15, 10, 10)
  )

chosen_year <- 2024
temp_year <- df %>% filter(year == chosen_year)

p1 <- ggplot(temp_year, aes(x = date)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), fill = "#BDD7EE", alpha = 0.4) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#5B9BD5", alpha = 0.4) +
  geom_line(aes(y = lta_meant), colour = "black", linewidth = 0.9) +
  geom_line(aes(y = meant), colour = "#C00000", linewidth = 0.5, alpha = 0.8) +
  labs(
    title    = "Daily mean temperature vs long term average at Sherkin Island 2024",
    subtitle = "Red = daily mean | Black = LTA | Dark band = 25th to 75th percentile | Light band = 10th to 90th percentile",
    x = "Date", y = "Mean temperature (°C)"
  ) +
  theme_clean

print(p1)
ggsave("plot1_temp_vs_lta.png", p1, width = 10, height = 5, dpi = 180)

p2 <- df %>%
  filter(!is.na(meant)) %>%
  ggplot(aes(x = meant, y = fct_rev(month), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01,
                               quantile_lines = TRUE, quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_gradientn(
    colours = c("#313695", "#74ADD1", "#FEE090", "#F46D43", "#A50026"),
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

p3 <- monthly_avg %>%
  ggplot(aes(x = fct_reorder(month, month_num), y = avg_rain, fill = avg_rain)) +
  geom_col(width = 0.75, colour = "white") +
  geom_text(aes(label = round(avg_rain, 0)), vjust = -0.4, size = 3, colour = "grey30") +
  scale_fill_gradient(low = "#C6DBEF", high = "#08519C", name = "mm") +
  labs(
    title    = "Average monthly rainfall at Sherkin Island",
    subtitle = "Mean total precipitation in mm per calendar month across full record from 1972 to 2025",
    x = NULL, y = "Average Rainfall in mm"
  ) +
  theme_clean + theme(legend.position = "none")

print(p3)
ggsave("plot3_monthly_rain_bar.png", p3, width = 8, height = 5, dpi = 180)

monthly_rain_ts <- df %>%
  filter(!is.na(rain), year >= 1975) %>%
  group_by(year, month_num, month) %>%
  summarise(total = sum(rain, na.rm = TRUE), .groups = "drop")

