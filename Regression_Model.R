# Load necessary libraries
library(dplyr)
library(tidyr)
library(lme4)     # For mixed-effects models (lmer)
library(lmerTest) # Provides p-values for lmer output
library(ggplot2)
library(broom.mixed) # For tidying model output

# --- 1. Load and Prepare Data ---
cat("--- 1. Loading and Preparing Data ---\n")
df <- read.csv("immunization.csv")

# Glimpse and summary if needed for a quick check
# glimpse(df)
# summary(df)

# --- 2. Select Data for a Specific Indicator and Relevant Dimensions ---
# Let's choose DTP3 coverage for one-year-olds ('vdpt') as the outcome.
# And focus on wealth, education, and place of residence as primary determinants.
cat("\n--- 2. Selecting Data for Modeling ---\n")
df_model_prep <- df %>%
  filter(indicator_abbr == "vdpt",  # DTP3 coverage for one-year-olds
         dimension %in% c("Economic status (wealth decile)", # Using deciles for more granularity
                          "Education (3 groups)",            # Standard education groups
                          "Place of residence"),             # Urban/Rural
         !is.na(estimate)) %>%
  select(setting, date, dimension, subgroup, estimate, whoreg6, wbincome2024)

# Check the structure of the prepared data
print(paste("Number of rows in df_model_prep:", nrow(df_model_prep)))
if (nrow(df_model_prep) == 0) {
  stop("No data selected for modeling. Check filter conditions for indicator_abbr and dimension.")
}
# print(head(df_model_prep))
# print(table(df_model_prep$dimension, df_model_prep$subgroup))


# --- 3. Data Cleaning and Transformation for Modeling ---
cat("\n--- 3. Data Cleaning and Transformation ---\n")

# Standardize subgroup names for easier modeling, especially for wealth deciles
df_model_data <- df_model_prep %>%
  mutate(
    # Standardize wealth deciles
    subgroup_clean = case_when(
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 1 (poorest)"  ~ "Wealth_Decile1",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 10 (richest)" ~ "Wealth_Decile10",
      dimension == "Economic status (wealth decile)" & grepl("Decile", subgroup) ~ paste0("Wealth_", gsub(" ", "", subgroup)), # "Wealth_Decile2", etc.
      # Standardize education
      dimension == "Education (3 groups)" & subgroup == "No education" ~ "Edu_None",
      dimension == "Education (3 groups)" & subgroup == "Primary education" ~ "Edu_Primary",
      dimension == "Education (3 groups)" & subgroup == "Secondary or higher education" ~ "Edu_SecondaryOrHigher",
      # Standardize place of residence
      dimension == "Place of residence" & subgroup == "Rural" ~ "Residence_Rural",
      dimension == "Place of residence" & subgroup == "Urban" ~ "Residence_Urban",
      TRUE ~ NA_character_ # If any subgroup doesn't match, mark as NA (should be filtered later)
    )
  ) %>%
  filter(!is.na(subgroup_clean)) # Remove rows where subgroup_clean couldn't be assigned

# Convert relevant columns to factors and set reference levels
# Reference levels are important for interpreting model coefficients.
# The coefficient for a factor level will be relative to the reference level.
df_model_data$subgroup_clean <- factor(df_model_data$subgroup_clean)
# Set reference levels (e.g., poorest, no education, rural)
# Check current levels first: levels(df_model_data$subgroup_clean)
# Example: if "Wealth_Decile1" exists and you want it as ref for wealth
if ("Wealth_Decile1" %in% levels(df_model_data$subgroup_clean)) {
  df_model_data$subgroup_clean <- relevel(df_model_data$subgroup_clean, ref = "Wealth_Decile1")
}
if ("Edu_None" %in% levels(df_model_data$subgroup_clean)) {
  df_model_data$subgroup_clean <- relevel(df_model_data$subgroup_clean, ref = "Edu_None") # This might not work as intended if multiple factors are in one column
}
if ("Residence_Rural" %in% levels(df_model_data$subgroup_clean)) {
  df_model_data$subgroup_clean <- relevel(df_model_data$subgroup_clean, ref = "Residence_Rural")
}
# For modeling, it's better to have separate columns for each dimension of inequality
# Let's reshape the data so wealth, education, residence are distinct factor columns

# Reshape data: Create separate columns for each inequality dimension's subgroup
# This is a more robust way to model multiple dimensions
df_model_reshaped <- df_model_prep %>%
  mutate(
    wealth_level = case_when(
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 1 (poorest)"  ~ "Decile1_Poorest",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 2"            ~ "Decile2",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 3"            ~ "Decile3",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 4"            ~ "Decile4",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 5"            ~ "Decile5",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 6"            ~ "Decile6",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 7"            ~ "Decile7",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 8"            ~ "Decile8",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 9"            ~ "Decile9",
      dimension == "Economic status (wealth decile)" & subgroup == "Decile 10 (richest)" ~ "Decile10_Richest",
      TRUE ~ NA_character_
    ),
    education_level = case_when(
      dimension == "Education (3 groups)" & subgroup == "No education"                  ~ "Edu_None",
      dimension == "Education (3 groups)" & subgroup == "Primary education"             ~ "Edu_Primary",
      dimension == "Education (3 groups)" & subgroup == "Secondary or higher education" ~ "Edu_SecondaryOrHigher",
      TRUE ~ NA_character_
    ),
    residence_type = case_when(
      dimension == "Place of residence" & subgroup == "Rural" ~ "Rural",
      dimension == "Place of residence" & subgroup == "Urban" ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  # We need to ensure each row has values for these predictors.
  # The current structure means a row for wealth won't have education/residence values.
  # This requires a more complex pivot or joining national averages of predictors.
  
  # For a direct model on subgroup estimates, we need to pick ONE dimension
  # or model interactions carefully.
  # Let's simplify and model based on ONE dimension at a time for clarity,
  # or create a dataset where each row is a country-year average estimate
  # and merge predictors.
  
  # --- SIMPLIFIED APPROACH: Focusing on Wealth Disparities for DTP3 ---
  # We will filter for one dimension to model its subgroups.
  # This is to keep the first modeling step clear.
  # A more complex model would handle multiple dimensions simultaneously (e.g., via interactions).
  filter(dimension == "Economic status (wealth decile)") %>%
  select(setting, date, subgroup, estimate, whoreg6, wbincome2024) %>%
  mutate(
    wealth_decile = factor(case_when(
      subgroup == "Decile 1 (poorest)"  ~ "D01_Poorest",
      subgroup == "Decile 2"            ~ "D02",
      subgroup == "Decile 3"            ~ "D03",
      subgroup == "Decile 4"            ~ "D04",
      subgroup == "Decile 5"            ~ "D05",
      subgroup == "Decile 6"            ~ "D06",
      subgroup == "Decile 7"            ~ "D07",
      subgroup == "Decile 8"            ~ "D08",
      subgroup == "Decile 9"            ~ "D09",
      subgroup == "Decile 10 (richest)" ~ "D10_Richest",
      TRUE ~ NA_character_
    ),
    levels = c("D01_Poorest", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10_Richest"),
    ordered = TRUE # Treat as ordered for potential trend interpretation
    )
  ) %>%
  filter(!is.na(wealth_decile)) # Ensure only valid deciles remain

# Convert other predictors to factors
df_model_final <- df_model_reshaped # Renaming for clarity
df_model_final$setting <- factor(df_model_final$setting)
df_model_final$whoreg6 <- factor(df_model_final$whoreg6)
df_model_final$wbincome2024 <- factor(df_model_final$wbincome2024,
                                      levels = c("Low income", "Lower-middle income", "Upper-middle income", "High income"),
                                      ordered = TRUE) # Assuming this order is meaningful
# df_model_final$date can be treated as numeric (year) for trend

print(paste("Number of rows in df_model_final (wealth deciles for vdpt):", nrow(df_model_final)))
if (nrow(df_model_final) < 20) { # Arbitrary small number check
  stop("Too few rows remaining after filtering for wealth deciles. Check data.")
}
# print(head(df_model_final))
# summary(df_model_final)


# --- 4. Build the Mixed-Effects Model ---
# We want to see how 'wealth_decile' (a specific subgroup type) affects 'estimate',
# while accounting for general country differences and year trends.
# Random intercept for 'setting' allows each country to have its own baseline coverage level.
cat("\n--- 4. Building Mixed-Effects Model for DTP3 by Wealth Decile ---\n")

# Ensure there's enough data and factor levels
if (length(unique(df_model_final$wealth_decile)) < 2 || length(unique(df_model_final$setting)) < 2) {
  stop("Not enough distinct levels for wealth_decile or setting to fit the mixed model.")
}

# Model 1: DTP3 estimate ~ wealth_decile + year + country_income_group + (random intercept for country)
# We use 'date' as a numeric representation of year.
# We use 'wbincome2024' as a country-level characteristic.
# 'whoreg6' can also be added as a fixed effect if desired.
model1 <- lmer(estimate ~ wealth_decile + date + wbincome2024 + (1 | setting),
               data = df_model_final)

# Get a summary of the model (includes fixed effects, random effects, etc.)
cat("\n--- Model Summary ---\n")
print(summary(model1))

# To get p-values for fixed effects using lmerTest (Satterthwaite's method is default)
cat("\n--- Model Summary with P-values (from lmerTest) ---\n")
print(summary(lmerTest::as_lmerModLmerTest(model1)))


# --- 5. Interpreting the Model (Brief Guide) ---
cat("\n--- 5. Brief Interpretation Guide ---\n")
cat(" - Fixed Effects: \n")
cat("   - Intercept: Estimated DTP3 coverage for the reference group (poorest decile, earliest year in data, reference income group) in an 'average' country.\n")
cat("   - wealth_decile[D02...D10_Richest]: The estimated difference in DTP3 coverage for that decile compared to the poorest decile (D01_Poorest), holding other variables constant.\n")
cat("   - date: Estimated average change in DTP3 coverage per year, holding other variables constant.\n")
cat("   - wbincome2024[Lower-middle income...]: Estimated difference in DTP3 coverage for that income group compared to 'Low income' (if that's the reference), holding other variables constant.\n")
cat("   - P-values (Pr(>|t|)): Indicate statistical significance. Small p-values (e.g., < 0.05) suggest the predictor has a significant association with the outcome.\n")
cat(" - Random Effects: \n")
cat("   - (1 | setting): Shows the variance between countries in their baseline DTP3 coverage. A larger variance means countries differ substantially.\n")


# --- 6. Visualizing Fixed Effects (Optional, for specific predictors) ---
# Example: Plotting estimated marginal means for wealth deciles
# library(effects) # For effect plots or use ggeffects
# library(ggeffects)
#
# cat("\n--- 6. Visualizing Model Effects (Example for Wealth Decile) ---\n")
# if (requireNamespace("ggeffects", quietly = TRUE)) {
#   gg_effects_wealth <- ggeffects::ggpredict(model1, terms = "wealth_decile")
#   print(plot(gg_effects_wealth) +
#     labs(title = "Predicted DTP3 Coverage by Wealth Decile",
#          x = "Wealth Decile", y = "Predicted DTP3 Estimate (%)") +
#     theme_minimal())
# }


# --- 7. Model Diagnostics (Important step for robust modeling - brief mention) ---
cat("\n--- 7. Model Diagnostics (Brief Mention - further checks needed for robust analysis) ---\n")
cat(" - Check residuals for normality and homogeneity of variance (e.g., plot(model1), qqnorm(resid(model1))).\n")
cat(" - Check for influential observations.\n")
cat(" - This script provides a starting point; thorough model validation is crucial.\n")

cat("\n--- Modeling Script Complete ---\n")











# --- 0. Load Necessary Libraries ---
library(dplyr)
library(tidyr)
library(lme4)     # For mixed-effects models (lmer)
library(lmerTest) # Provides p-values for lmer output
library(ggplot2)
library(ggeffects) # For plotting model predictions

# --- 1. Load Data ---
cat("--- 1. Loading Data ---\n")
df <- read.csv("immunization.csv")

# --- 2. Prepare Data for Model 2 (DTP3 by Wealth Decile) ---
cat("\n--- 2. Preparing Data for DTP3 vs Wealth Decile Model ---\n")

df_model2_data <- df %>%
  filter(indicator_abbr == "vdpt",  # DTP3 coverage for one-year-olds
         dimension == "Economic status (wealth decile)", # Focus on wealth deciles
         !is.na(estimate)) %>%
  select(setting, date, subgroup, estimate) %>% # Keep only essential columns for this model
  mutate(
    wealth_decile = factor(case_when(
      subgroup == "Decile 1 (poorest)"  ~ "D01_Poorest",
      subgroup == "Decile 2"            ~ "D02",
      subgroup == "Decile 3"            ~ "D03",
      subgroup == "Decile 4"            ~ "D04",
      subgroup == "Decile 5"            ~ "D05",
      subgroup == "Decile 6"            ~ "D06",
      subgroup == "Decile 7"            ~ "D07",
      subgroup == "Decile 8"            ~ "D08",
      subgroup == "Decile 9"            ~ "D09",
      subgroup == "Decile 10 (richest)" ~ "D10_Richest",
      TRUE ~ NA_character_ # Should not happen if filter is correct
    ),
    # Define levels for correct order and reference (D01_Poorest as reference)
    levels = c("D01_Poorest", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10_Richest"),
    ordered = TRUE # Treat as ordered for polynomial contrasts if desired by lm/lmer
    ),
    setting = factor(setting), # Ensure 'setting' is a factor for random effects
    # Optional: Center date for more interpretable intercept
    # date_centered = date - mean(date, na.rm = TRUE)
  ) %>%
  filter(!is.na(wealth_decile)) # Remove any rows where wealth_decile mapping failed

# Check final data for the model
print(paste("Number of rows for Model 2:", nrow(df_model2_data)))
print(paste("Number of unique countries for Model 2:", length(unique(df_model2_data$setting))))
# summary(df_model2_data) # Optional: for a quick check of the prepared data

if (nrow(df_model2_data) < 20 || length(unique(df_model2_data$setting)) < 2 || length(unique(df_model2_data$wealth_decile)) < 2) {
  stop("Insufficient data or distinct factor levels to fit Model 2. Check data preparation steps.")
}

# --- 3. Build and Summarize Mixed-Effects Model (Model 2) ---
cat("\n--- 3. Building and Summarizing Model 2 ---\n")

model2 <- lmer(estimate ~ wealth_decile + date + (1 | setting),
               data = df_model2_data)

# Get model summary with p-values
cat("\n--- Model 2 Summary (with p-values) ---\n")
print(summary(lmerTest::as_lmerModLmerTest(model2)))

# --- 4. Visualize the Effect of Wealth Decile from Model 2 ---
cat("\n--- 4. Visualizing Predicted DTP3 Coverage by Wealth Decile (Model 2) ---\n")

# Check if model converged and can produce predictions
if (inherits(model2, "lmerMod")) { # A simple check
  gg_effects_wealth_m2 <- ggpredict(model2, terms = "wealth_decile")
  plot_model2_effects <- plot(gg_effects_wealth_m2) +
    labs(title = "Predicted DTP3 Coverage by Wealth Decile (Model 2)",
         x = "Wealth Decile", y = "Predicted DTP3 Estimate (%)") +
    theme_minimal()
  print(plot_model2_effects)
} else {
  cat("Model 2 did not converge or is not a valid lmerMod object. Cannot plot effects.\n")
}

cat("\n--- Modeling Script for Model 2 Complete ---\n")


if (inherits(model2, "lmerMod")) {
  gg_effects_date_m2 <- ggpredict(model2, terms = "date [all]") # Show the full range of date
  plot_model2_date_effects <- plot(gg_effects_date_m2) +
    labs(title = "Predicted DTP3 Coverage Over Time (Model 2)",
         x = "Year", y = "Predicted DTP3 Estimate (%)") +
    theme_minimal()
  print(plot_model2_date_effects)
}

