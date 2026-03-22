# 53 Years of Irish Weather — Sherkin Island Climate Analysis

Statistical analysis of daily weather data from the Sherkin Island synoptic station, Co. Cork, Ireland. Completed as part of STAT 2012 Statistics.

---

## Station

| | |
|---|---|
| **Location** | Sherkin Island, Co. Cork, Ireland |
| **Coordinates** | 51.476°N, 9.428°W |
| **Elevation** | 20 m |
| **Record** | July 1972 – September 2025 |
| **Observations** | 19,392 daily records |
| **Source** | [Met Éireann Historical Data](https://www.met.ie/climate/available-data/historical-data) |

---

## What the analysis covers

**Temperature**
- Five-number summary (min, Q1, median, Q3, max) for mean, max, and min daily temperature
- Long-term average (LTA) computed per day-of-year using a 30-day centred moving average
- Daily departures from LTA with 10th/25th/75th/90th percentile shading (Met Éireann style)
- Standardised annual temperature anomalies (z-scores) to identify warming trend

**Rainfall**
- Empirical probability of a rainy day using the Met Éireann threshold of > 0.1 mm
- Average monthly rainfall across the full record
- Monthly rainfall heatmap over time (1975–2025)

---

## Plots

| File | Description |
|------|-------------|
| `plot1_temp_vs_lta.png` | Daily mean temp vs 30-day smoothed LTA — Sherkin Island 2024 |
| `plot2_temp_ridgeline.png` | Monthly temperature distributions with Q1/median/Q3 lines |
| `plot3_monthly_rain_bar.png` | Average monthly rainfall (mm) across full record |
| `plot4_rain_heatmap.png` | Monthly rainfall totals heatmap 1975–2025 |
| `plot5_anomaly_zscore.png` | Standardised annual temperature anomaly (z-scores) |

---

## Key findings

- **Median mean daily temperature:** 11.0°C — narrow IQR of 5.6°C reflects Atlantic climate stability
- **P(rainy day) = 0.59** — rain falls on roughly 3 in every 5 days
- **Wettest months:** December (138.9 mm), January (137.1 mm), October (126.7 mm)
- **Driest month:** April (66.6 mm)
- **4 of the 5 warmest years on record have occurred since 2022** — consistent with long-term warming across Ireland's western seaboard

---

## Files

```
├── sherkin_analysis.R          # Full analysis script
├── plot1_temp_vs_lta.png
├── plot2_temp_ridgeline.png
├── plot3_monthly_rain_bar.png
├── plot4_rain_heatmap.png
├── plot5_anomaly_zscore.png
└── README.md
```

> **Note:** The data file `Data Sherkin Island.csv` is not included in this repository. Download it directly from [Met Éireann](https://www.met.ie/climate/available-data/historical-data) and place it in the same folder as the R script before running.

---

## How to run

1. Download `Data Sherkin Island.csv` from Met Éireann and place it in the project folder
2. Open `sherkin_analysis.R` in RStudio
3. Set working directory to the source file location: **Session → Set Working Directory → To Source File Location**
4. Click **Source**

The script will install any missing packages automatically and save all five plots to the working directory.

**R packages used:** `tidyverse`, `lubridate`, `forecast`, `ggridges`

---

## Course

STAT 2012 Statistics — Dr Nicole Beisiegel
