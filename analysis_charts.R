# County Health Dashboard
library(tidyverse)

places <- read_csv("data/places_county_2025.csv", show_col_types = FALSE)

glimpse(places)

# Small clean table with important columns only
places_small <- places %>%
  select(
    Year,
    StateAbbr,
    StateDesc,
    LocationName,
    Category,
    Measure,
    Data_Value,
    Data_Value_Type,
    Low_Confidence_Limit,
    High_Confidence_Limit,
    Geolocation
  )

glimpse(places_small)
head(places_small)

# Check what obesity value types exist
places_small %>%
  filter(Measure == "Obesity among adults") %>%
  count(Data_Value_Type)


# See the choices inside the data
sort(unique(places_small$Year))
sort(unique(places_small$Category))

length(unique(places_small$Measure))
sort(unique(places_small$Measure))

# Choose only the best 12 measures
chosen_measures <- c(
  "Coronary heart disease among adults",
  "Stroke among adults",
  "Diagnosed diabetes among adults",
  "High blood pressure among adults",
  "Obesity among adults",
  "No leisure-time physical activity among adults",
  "Short sleep duration among adults",
  "High cholesterol among adults who have ever been screened",
  "Food insecurity in the past 12 months among adults",
  "Housing insecurity in the past 12 months among adults",
  "Lack of reliable transportation in the past 12 months among adults",
  "Lack of social and emotional support among adults"
)

places_best <- places_small %>%
  filter(Measure %in% chosen_measures,
         Data_Value_Type == "Age-adjusted prevalence"
  )

glimpse(places_best)
sort(unique(places_best$Measure))

# Check top 10 obesity counties again after keeping only age-adjusted prevalence
obesity_check2 <- places_best %>%
  filter(
    Year == 2023,
    Measure == "Obesity among adults"
  ) %>%
  arrange(desc(Data_Value)) %>%
  select(
    Year,
    StateAbbr,
    StateDesc,
    LocationName,
    Data_Value
  ) %>%
  slice_head(n = 10)

obesity_check2

# top 10 counties for age-adjusted obesity in 2023

obesity_2023 <- places_best %>%
  filter(
    Year == 2023,
    Measure == "Obesity among adults"
  ) %>%
  mutate(County_State = paste0(LocationName, ", ", StateAbbr)) %>%
  arrange(desc(Data_Value)) %>%
  slice_head(n = 10)

print(obesity_2023)

ggplot(obesity_2023, aes(x = reorder(County_State, Data_Value), y = Data_Value)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = sprintf("%.1f", Data_Value)),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Top 10 Counties for Age-Adjusted Adult Obesity Prevalence (2023)",
    subtitle = "Age-adjusted county-level estimates from the CDC PLACES dataset",
    x = "County, State",
    y = "Age-adjusted obesity prevalence (%)",
    caption = "Source: CDC PLACES county-level dataset, 2023"
  ) +
  ylim(0, max(obesity_2023$Data_Value) + 3) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 10)
  )

# Save the chart as a professional PNG file
ggsave(
  filename = "top10_age_adjusted_obesity_2023.png",
  width = 12,
  height = 8,
  dpi = 300
)

# Professional chart: top 10 counties for age-adjusted diagnosed diabetes in 2023

diabetes_2023 <- places_best %>%
  filter(
    Year == 2023,
    Measure == "Diagnosed diabetes among adults"
  ) %>%
  mutate(County_State = paste0(LocationName, ", ", StateAbbr)) %>%
  arrange(desc(Data_Value)) %>%
  slice_head(n = 10)

print(diabetes_2023)

ggplot(diabetes_2023, aes(x = reorder(County_State, Data_Value), y = Data_Value)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = sprintf("%.1f", Data_Value)),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Top 10 Counties for Age-Adjusted Diagnosed Diabetes Prevalence (2023)",
    subtitle = "Age-adjusted county-level estimates from the CDC PLACES dataset",
    x = "County, State",
    y = "Age-adjusted diagnosed diabetes prevalence (%)",
    caption = "Source: CDC PLACES county-level dataset, 2023"
  ) +
  ylim(0, max(diabetes_2023$Data_Value) + 3) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 10)
  )

ggsave(
  filename = "top10_age_adjusted_diabetes_2023.png",
  width = 12,
  height = 8,
  dpi = 300
)

# Relationship chart: obesity vs diagnosed diabetes in 2023


# Relationship chart: obesity vs diagnosed diabetes in 2023

obesity_diabetes_2023 <- places_best %>%
  filter(
    Year == 2023,
    Measure %in% c("Obesity among adults", "Diagnosed diabetes among adults")
  ) %>%
  group_by(StateAbbr, StateDesc, LocationName, Measure) %>%
  summarise(Data_Value = mean(Data_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value
  ) %>%
  drop_na()

print(obesity_diabetes_2023)

# Correlation between obesity and diabetes
correlation_value <- cor(
  obesity_diabetes_2023$`Obesity among adults`,
  obesity_diabetes_2023$`Diagnosed diabetes among adults`,
  use = "complete.obs"
)

round(correlation_value, 3)

ggplot(
  obesity_diabetes_2023,
  aes(
    x = `Obesity among adults`,
    y = `Diagnosed diabetes among adults`
  )
) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
  labs(
    title = "Relationship Between Age-Adjusted Obesity and Diagnosed Diabetes (2023)",
    subtitle = paste0(
      "Each point represents one county from the CDC PLACES dataset (r = ",
      round(correlation_value, 3),
      ")"
    ),
    x = "Age-adjusted obesity prevalence (%)",
    y = "Age-adjusted diagnosed diabetes prevalence (%)",
    caption = "Source: CDC PLACES county-level dataset, 2023"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    plot.caption = element_text(size = 10)
  )

ggsave(
  filename = "obesity_vs_diabetes_2023.png",
  width = 12,
  height = 8,
  dpi = 300
)

# Correlation between age-adjusted obesity and diagnosed diabetes in 2023

correlation_value <- cor(
  obesity_diabetes_2023$`Obesity among adults`,
  obesity_diabetes_2023$`Diagnosed diabetes among adults`,
  use = "complete.obs"
)

correlation_value

# Rounded version for reporting
round(correlation_value, 3)








