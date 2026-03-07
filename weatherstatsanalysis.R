for (pkg in c("tidyverse", "forecast", "ggridges")) {
  if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
}

library(tidyverse)
library(forecast)
library(ggridges)

raw <- read.csv("Data Sherkin Island.csv",
                skip        = 13,
                header      = TRUE,
                na.strings  = c("", " "),
                stringsAsFactors = FALSE)
