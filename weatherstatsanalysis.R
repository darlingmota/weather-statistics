for (pkg in c("tidyverse", "forecast", "ggridges")) {
  if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
}

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("forecast",  quietly = TRUE)) install.packages("forecast")
if (!requireNamespace("ggridges",  quietly = TRUE)) install.packages("ggridges")

library(tidyverse)
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
