# library(readxl)
# data <- read_excel("data.xlsx")
# write.csv(data, "immunization.csv", row.names = FALSE)
immunization = read.csv("immunization.csv")


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate) # Though date is already numeric year
library(tidyr)
library(knitr)
library(viridis) # For color palettes

# Assuming 'immunization' dataframe is already loaded.
# For reproducibility, let's alias it
df <- immunization

# --- 1. Data Overview and Structure ---
cat("1. Data Overview and Structure\n")
cat("Dimensions of the dataset:\n")
print(dim(df))
cat("\nColumn names:\n")
print(names(df))
cat("\nStructure of the dataset (glimpse):\n")
glimpse(df)
head(df)
summary(df)


# EDA 1: Distribution of immunization estimates
ggplot(df, aes(x = estimate)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "EDA 1: Distribution of Childhood Immunization Estimates",
       x = "Immunization Coverage (%)", y = "Frequency") +
  theme_minimal()


# EDA 2: Immunization estimates by WHO region

ggplot(df, aes(x = whoreg6, y = estimate, fill = whoreg6)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "EDA 2: Immunization Coverage by WHO Region",
       x = "WHO Region", y = "Immunization Coverage (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# EDA 3: Immunization coverage by income group
ggplot(df, aes(x = wbincome2024, y = estimate, fill = wbincome2024)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  labs(title = "EDA 3: Immunization Coverage by World Bank Income Group",
       x = "Income Group", y = "Immunization Coverage (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# EDA 4: Global trend of immunization over time
df_time <- df %>%
  group_by(date) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE))

ggplot(df_time, aes(x = date, y = mean_estimate)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "black") +
  labs(title = "EDA 4: Global Average Immunization Coverage Over Time",
       x = "Year", y = "Average Immunization Coverage (%)") +
  theme_minimal()




# EDA 5: Immunization estimates by wealth deciles
library(stringr) # For str_replace and other string functions if needed

df_wealth <- df %>%
  filter(grepl("Decile", subgroup, ignore.case = TRUE)) %>% # Keep your initial filter
  mutate(
    # Standardize the subgroup names
    subgroup_standardized = case_when(
      subgroup == "Decile 1 (poorest)"  ~ "Decile 1",
      subgroup == "Decile 10 (richest)" ~ "Decile 10",
      TRUE                             ~ subgroup # Keep others as they are (e.g., "Decile 2")
    ),
    # Now create the factor using the standardized names and the desired order
    subgroup_factor = factor(subgroup_standardized, levels = paste("Decile", 1:10))
  )

# Check if it worked:
table(df_wealth$subgroup_factor, useNA = "ifany") # useNA shows if any NAs are left

# Now plot using the new 'subgroup_factor' column for x and fill
ggplot(df_wealth, aes(x = subgroup_factor, y = estimate, fill = subgroup_factor)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, option = "B") + # Or your preferred color scale
  labs(title = "EDA 5: Immunization Coverage by Wealth Decile",
       x = "Wealth Decile", y = "Immunization Coverage (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))





# EDA 6: Trend in inequality over time (Decile 1 vs Decile 10)
df_gap <- df %>%
  filter(subgroup %in% c("Decile 1 (poorest)", "Decile 10 (richest)")) %>%
  group_by(date, subgroup) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = subgroup, values_from = mean_estimate) %>%
  mutate(gap = `Decile 10 (richest)` - `Decile 1 (poorest)`)

ggplot(df_gap, aes(x = date, y = gap)) +
  geom_line(color = "darkred", size = 1) +
  geom_point() +
  labs(title = "EDA 6: Immunization Inequality Over Time (Richest - Poorest)",
       x = "Year", y = "Coverage Gap (%)") +
  theme_minimal()



# EDA 7: Immunization by maternal education level
df_edu <- df %>%
  filter(dimension %in% c("Education (3 groups)", "Education (4 groups)"),
         !is.na(estimate))

ggplot(df_edu, aes(x = subgroup, y = estimate, fill = subgroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "EDA 7: Immunization Coverage by Maternal Education Level",
       x = "Maternal Education Level", y = "Immunization Coverage (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))



# EDA 8: Urban vs. Rural Immunization
df_place <- df %>%
  filter(dimension == "Place of residence", !is.na(estimate))

ggplot(df_place, aes(x = subgroup, y = estimate, fill = subgroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "EDA 8: Immunization Coverage by Place of Residence",
       x = "Residence Type", y = "Immunization Coverage (%)") +
  theme_minimal() +
  theme(legend.position = "none")


