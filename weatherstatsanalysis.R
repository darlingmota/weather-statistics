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

