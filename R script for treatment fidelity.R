# Packages needed
library(readr)
library(ggplot2)

# Read the file
raw_data <- read_csv("https://raw.githubusercontent.com/Obernique/Extra-Urchin/main/Experimental_conditions.csv")

# Clean and prepare the file
library(dplyr)

df <- raw_data %>%
  mutate(
    measured_pH = as.numeric(measured_pH),
    salinity = as.numeric(salinity),
    pH_deviation = measured_pH - target_pH,
    temp_deviation = measured_temperature - target_temperature
  )

df <- df %>%
  mutate(
    pH_deviation = measured_pH - target_pH,
    temp_deviation = measured_temperature - target_temperature
  )

# Summarise fidelity
fidelity_summary <- df %>%
  group_by(treatment) %>%
  summarise(
    mean_pH_dev = mean(pH_deviation, na.rm = TRUE),
    sd_pH_dev = sd(pH_deviation, na.rm = TRUE),
    mean_temp_dev = mean(temp_deviation, na.rm = TRUE),
    sd_temp_dev = sd(temp_deviation, na.rm = TRUE),
    n = n()
  )
print(fidelity_summary)

# Flag outliers
df <- df %>%
  mutate(
    pH_outlier = abs(pH_deviation - mean(pH_deviation, na.rm = TRUE)) > 3 * sd(pH_deviation, na.rm = TRUE),
    temp_outlier = abs(temp_deviation - mean(temp_deviation, na.rm = TRUE)) > 3 * sd(temp_deviation, na.rm = TRUE)
  )

# Visualise results
ggplot(df, aes(x = treatment, y = pH_deviation)) +
  geom_boxplot(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "pH Fidelity by Treatment", y = "Measured - Target pH")

ggplot(df, aes(x = treatment, y = temp_deviation)) +
  geom_boxplot(fill = "tomato") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Temperature Fidelity by Treatment", y = "Measured - Target Temp (°C)")