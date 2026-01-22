Module: IJC437 – Introduction to Data Science
Project: The Urban Breath: A Statistical Analysis of Meteorological Dispersion and Anthropogenic Cycles of Nitrogen Dioxide (NO2) in Sheffield (2024)
Author: Dharun Kanagasabai
Approach: Question → Gathering → Structuring → Exploring → Communicating
--------------------------------------------------------------------------------------------------------------
1. Project Overview
This project investigates **to what extent meteorological conditions modulate nitrogen dioxide (NO₂) concentrations in Sheffield** during the year 2024. NO₂ is a key urban air pollutant primarily associated with road traffic and combustion processes. Meteorological factors such as wind speed, temperature, and humidity influence pollutant dispersion, accumulation, and chemical transformation, making them critical to understanding observed air-quality patterns.
The analysis follows a reproducible data science workflow using **open, real‑world environmental datasets** accessed via public APIs.

2. Research Question
To what extent does meteorology modulate NO₂ levels in Sheffield?
Sub‑questions explored:
  •	How do NO₂ concentrations vary seasonally?
  •	What is the relationship between wind speed and NO₂ (dispersion effect)?
  •	Are diurnal and weekday/weekend patterns visible, indicating anthropogenic sources?
  •	How strongly are meteorological variables correlated with NO₂ and PM2.5?

3. Data Sources & Provenance
3.1 Air Quality Data:
Source:  OpenAQ V3 API
Location:  Sheffield – Devonshire Green (Location ID: 2508)
Pollutants: 
  •	Nitrogen Dioxide (NO₂)
  •	Particulate Matter (PM2.5)
  •	Temporal Resolution:  Hourly
  •	Coverage:  1 January 2024 – 31 December 2024
OpenAQ aggregates data from regulatory-grade monitoring stations and provides standardised access with clear provenance.

3.2 Meteorological Data
  •	Source:  Open‑Meteo Historical Archive API
  •	Variables Used:
    o	Temperature at 2 m (°C)
    o	Relative Humidity at 2 m (%)
    o	Wind Speed at 10 m (m/s)
    o	Location:  Sheffield city centre (lat 53.3811, lon −1.4701)
    o	Temporal Resolution:  Hourly

3.3 Ethics & Reproducibility
  •	All data are open-access and used in accordance with API terms.
  •	API keys are stored as environment variables to avoid hard‑coding credentials.

4. Data Processing & Structuring
Key steps:
  1.	Retrieval of hourly sensor data using paginated API calls.
  2.	Parsing and standardisation of timestamps to UTC.
  3.	Reshaping air‑quality data into wide format (NO₂ and PM2.5 columns).
  4.	Joining meteorological and air‑quality data by timestamp.
  5.	Removal of incomplete observations to ensure statistical validity.
  6.	Feature engineering:
        o	Month (seasonality)
        o	Hour of day (diurnal cycle)
        o	Weekday vs Weekend classification
The final dataset contains **hourly, co‑located meteorological and pollution measurements** suitable for exploratory and statistical analysis.


5. Exploratory Analysis & Key Observations
5.1 Seasonal Variation in NO₂
  •	NO₂ concentrations are **highest during winter months** and lowest during summer.
  •	This pattern is consistent with:
    o	Reduced atmospheric mixing in colder months
    o	Increased emissions from heating and traffic
Interpretation: Seasonal meteorology strongly modulates background NO₂ levels.

5.2 Wind Speed and the Dispersion Effect
  •	A clear negative association exists between wind speed and NO₂ concentration.
  •	Higher wind speeds correspond to lower observed NO₂ values.
Interpretation: Increased wind enhances pollutant dispersion, reducing local accumulation. These supports established atmospheric dispersion theory.

5.3 Diurnal and Weekly Patterns (Human Fingerprint)
  •	Pronounced NO₂ peaks during weekday morning rush hours (07:00–09:00).
  •	Reduced NO₂ levels on weekends, particularly Sundays.
Interpretation: These patterns strongly indicate a traffic-related anthropogenic source, with meteorology modulating but not generating emissions.

5.4 Correlation Structure
  •	Wind speed shows the strongest negative correlation with NO₂.
  •	Temperature exhibits a weaker negative relationship.
  •	PM2.5 shows a different correlation structure, suggesting partially distinct emission sources and atmospheric     behaviour.

6. Statistical Analysis
A multiple linear regression model was fitted:
NO₂ ~ Wind Speed + Temperature + Relative Humidity
Key Findings:
  •	Wind speed is the strongest predictor, with a statistically significant negative coefficient.
  •	Temperature and humidity contribute additional explanatory power but with smaller effects.
Model Interpretation
The results indicate that meteorological variables, particularly wind speed, significantly explain variation in NO₂ concentrations. However, the model is **associative rather than causal** due to the observational nature of the data.
Basic diagnostic checks suggest approximate linearity and homoscedasticity, though some deviation is expected at extreme pollution levels.

7. Anomaly Detection: Peak Pollution Event
The highest recorded NO₂ concentration in 2024 occurred during:
  •	Low wind conditions
  •	A weekday
  •	Likely morning hours
Interpretation: This event is consistent with stagnant meteorological conditions combined with peak traffic emissions, illustrating how meteorology amplifies anthropogenic pollution episodes.

8. Limitations & Future Work
Limitations:
  •	Single monitoring location limits spatial generalisability.
  •	Observational data restricts causal inference.
  •	No lagged meteorological effects considered.

Future Improvements:
  •	Incorporate multiple Sheffield monitoring stations.
  •	Test lagged meteorological variables.
  •	Aggregate to daily means for noise reduction.
  •	Extend analysis to additional pollutants.

9. Technologies Used
  •	Language: R
  •	Key Packages:
      o	httr, jsonlite (API access)
      o	tidyverse (data wrangling & visualisation)
      o	lubridate (time handling)
      o	viridis (colour scales)
      o	reshape2 (correlation visualisation)

10. Reproducibility Instructions
  1. Clone this repository
  2. Install required R packages:
      install.packages(c("httr", "jsonlite", "tidyverse", "lubridate", "viridis", "reshape2"))
  3. Set your OpenAQ API key as an environment variable:
      Sys.setenv(OPENAQ_API_KEY = "your_api_key_here")
  4. Run the analysis script end‑to‑end

11. Analysis Code
################################################################################
# MODULE: IJC437 - Introduction to Data Science
# PROJECT: Impact of Meteorology on Air Quality in Sheffield (2024)
# AUTHOR: Dharun Kanagasabai
# PROCESS: Question -> Gathering -> Structuring -> Exploring -> Communicating
################################################################################

# --- 0. REPRODUCIBILITY & ENVIRONMENT SETUP ---
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(viridis)
library(reshape2)

api_key <- Sys.getenv("OPENAQ_API_KEY")
location_id <- 2508 # Sheffield Devonshire Green

sensor_url <- paste0("https://api.openaq.org/v3/locations/", location_id, "/sensors")
sensor_res <- GET(sensor_url, add_headers("X-API-Key" = api_key))
sensors_list <- fromJSON(content(sensor_res, "text"))$results

no2_id  <- sensors_list %>% filter(parameter$name == "no2") %>% pull(id) %>% .[1]
pm25_id <- sensors_list %>% filter(parameter$name == "pm25") %>% pull(id) %>% .[1]

fetch_air_data <- function(s_id, pollutant_label) {
  batches <- list()
  for(page in 1:10) {
    url <- paste0("https://api.openaq.org/v3/sensors/", s_id, "/hours")
    params <- list(datetimeFrom = "2024-01-01T00:00:00Z",
                   datetimeTo = "2024-12-31T23:59:59Z",
                   limit = 1000, page = page)
    res <- GET(url, query = params, add_headers("X-API-Key" = api_key))
    if(status_code(res) != 200) break
    data <- fromJSON(content(res, "text"))$results
    if(length(data) == 0) break
    batches[[page]] <- data
  }
  bind_rows(batches) %>%
    mutate(Timestamp = as.POSIXct(period$datetimeFrom$utc,
                                  format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
           Value = as.numeric(value), Pollutant = pollutant_label) %>%
    select(Timestamp, Value, Pollutant)
}

no2_raw  <- fetch_air_data(no2_id, "NO2")
pm25_raw <- fetch_air_data(pm25_id, "PM2.5")

weather_res <- GET("https://archive-api.open-meteo.com/v1/archive", query = list(
  latitude = 53.3811, longitude = -1.4701,
  start_date = "2024-01-01", end_date = "2024-12-31",
  hourly = "temperature_2m,relative_humidity_2m,wind_speed_10m",
  windspeed_unit = "ms", timezone = "GMT"
))

weather_df <- as.data.frame(fromJSON(content(weather_res, "text"))$hourly) %>%
  mutate(Timestamp = as.POSIXct(time, format="%Y-%m-%dT%H:%M", tz="UTC")) %>%
  select(Timestamp,
         Temp_C = temperature_2m,
         Humidity_Pct = relative_humidity_2m,
         Wind_Speed_ms = wind_speed_10m)
aq_wide <- bind_rows(no2_raw, pm25_raw) %>%
  pivot_wider(names_from = Pollutant, values_from = Value)
final_data <- weather_df %>%
  left_join(aq_wide, by = "Timestamp") %>%
  drop_na(NO2) %>%
  mutate(Month = factor(month(Timestamp, label = TRUE, abbr = TRUE)),
         Hour = hour(Timestamp),
         DayType = ifelse(wday(Timestamp) %in% c(1, 7), "Weekend", "Weekday"))
# Linear regression model
aq_model <- lm(NO2 ~ Wind_Speed_ms + Temp_C + Humidity_Pct, data = final_data)
summary(aq_model)

12. Conclusion
This project demonstrates that meteorology—particularly wind speed—plays a significant role in shaping observed NO₂ concentrations in Sheffield. While emissions are primarily anthropogenic, atmospheric conditions strongly modulate pollution intensity and persistence. The study highlights the value of integrating environmental data sources within a reproducible data science framework.
