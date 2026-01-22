################################################################################
# MODULE: IJC437 - Introduction to Data Science
# PROJECT: The Urban Breath: A Statistical Analysis of Meteorological Dispersion and Anthropogenic Cycles of Nitrogen Dioxide (NO2) in Sheffield (2024)
# AUTHOR: Dharun Kanagasabai
# PROCESS: Question -> Gathering -> Structuring -> Exploring -> Communicating
################################################################################

### REPRODUCIBILITY & ENVIRONMENT SETUP
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(viridis)
library(reshape2)

### 1) DATA GATHERING
api_key <- "d1da0330280b205215dbaed57e2ae6d728ec6cfcf71871f4bce43a6e10a49fbd"
location_id <- 2508 # Sheffield Devonshire Green

# Methodology: Using OpenAQ V3 API for hourly sensor data.
sensor_url <- paste0("https://api.openaq.org/v3/locations/", location_id, "/sensors")
sensor_res <- GET(sensor_url, add_headers("X-API-Key" = api_key))
sensors_list <- fromJSON(content(sensor_res, "text"))$results

# Identifying specific hardware IDs for NO2 & PM25
no2_id  <- sensors_list %>% filter(parameter$name == "no2") %>% pull(id) %>% .[1]
pm25_id <- sensors_list %>% filter(parameter$name == "pm25") %>% pull(id) %>% .[1]

# Fetching Data -> Function with Pagination
fetch_air_data <- function(s_id, pollutant_label) {
  batches <- list()
  for(page in 1:10) { # Fetching ~10,000 rows for full 2024 coverage
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

# Fetching Weather Data from Open-Meteo Archive
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

### 2) DATA STRUCTURING & CLEANING
# Handling Missing Data and Joins.
aq_wide <- bind_rows(no2_raw, pm25_raw) %>%
  pivot_wider(names_from = Pollutant, values_from = Value)

final_data <- weather_df %>%
  inner_join(aq_wide, by = "Timestamp") %>%
  drop_na() %>% # Removing 127 incomplete rows after EDA to ensure statistical validity
  mutate(Month = factor(month(Timestamp, label = TRUE, abbr = TRUE)),
         Hour = hour(Timestamp),
         DayType = ifelse(wday(Timestamp) %in% c(1, 7), "Weekend", "Weekday"))

### 3) EXPLORATION & VISUALISATION

# Seasonal Boxplot: Quantifying Winter vs Summer
ggplot(final_data, aes(x = Month, y = NO2, fill = Month)) +
  geom_boxplot(outlier.alpha = 0.05) +
  scale_fill_viridis_d(option = "mako") +
  labs(title = "Seasonal NO2 Variation in Sheffield (2024)",
       subtitle = "Pollution peaks observed during stagnant winter months",
       x = "Calendar Month", y = expression(NO[2]~Concentration~(mu*g/m^3))) +
  theme_minimal()

# The 'Dispersion Effect': Wind vs. Pollutants
ggplot(final_data, aes(x = Wind_Speed_ms, y = NO2)) +
  geom_point(alpha = 0.15, color = "#34495e") +
  geom_smooth(method = "lm", color = "#e67e22", size = 1.2) +
  labs(title = "Dispersion Analysis: Impact of Wind Speed",
       subtitle = "Negative correlation confirms wind acts as an urban pollutant 'broom'",
       x = "Wind Speed (m/s)", y = expression(NO[2]~Concentration~(mu*g/m^3))) +
  theme_minimal()

# The Human Fingerprint: Rush Hour & Weekend Effect
ggplot(final_data, aes(x = Hour, y = NO2, color = DayType)) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  annotate("rect", xmin = 7, xmax = 9, ymin = 0, ymax = 50, alpha = .1, fill = "red") +
  annotate("text", x = 8, y = 48, label = "AM Rush", color = "darkred", fontface = "bold") +
  scale_color_manual(values = c("Weekday" = "#c0392b", "Weekend" = "#2980b9")) +
  labs(title = "Diurnal Cycles: Weekday Traffic vs. Weekend Relaxation",
       subtitle = "Significant peak reduction on Sundays confirms transport origin",
       x = "Hour of Day (24h)", y = expression(Mean~NO[2]~(mu*g/m^3)),
       color = "Classification") +
  theme_minimal()

### 4) STATISTICAL ANALYSIS

# Linear Regression: Predicting NO2 via Meteorological Drivers
aq_model <- lm(NO2 ~ Wind_Speed_ms + Temp_C + Humidity_Pct, data = final_data)
summary(aq_model)

# Correlation Matrix: Inter-variable relationships
cor_mat <- cor(final_data %>% select(Temp_C, Wind_Speed_ms, Humidity_Pct, NO2, PM2.5))
melted_cor <- melt(cor_mat)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) +
  labs(title = "Variable Correlation Matrix", x = "", y = "", fill = "Pearson R") +
  theme_minimal()

### 5) ANOMALY DETECTION
# Identifying the 'Worst Day' for the Report Case Study
worst_event <- final_data %>% arrange(desc(NO2)) %>% slice(1)
print(paste("Peak NO2 event occurred on:", worst_event$Timestamp))