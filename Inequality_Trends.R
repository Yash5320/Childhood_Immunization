# --- 0. Load Necessary Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis) # For color palettes

# --- 1. Load Data ---
# This script assumes 'df' is already loaded from "immunization.csv"
# If running this script standalone, uncomment the next line:
# df <- read.csv("immunization.csv")

# Create df_no_na_est as it's used in the original friend's code
# This ensures we are working with rows that have an 'estimate' value.
df_no_na_est <- df %>% filter(!is.na(estimate))

if (nrow(df_no_na_est) == 0) {
  stop("The dataframe 'df' is empty or all 'estimate' values are NA. Please load data correctly.")
}

# --- 2. Visualization 1: Temporal Trends in Inequality GAPS ---
cat("\n--- 2. Generating Plots for Temporal Trends in Inequality GAPS ---\n")

# --- 2.1 DTP3 Wealth Gap (Quintile 5 - Quintile 1) Over Time ---
cat("\nCalculating and plotting DTP3 Wealth Gap (Q5-Q1) over time...\n")
dtp3_wealth_gap_trends <- df_no_na_est %>%
  filter(indicator_name == "DTP3 immunization coverage among one-year-olds (%)",
         dimension == "Economic status (wealth quintile)",
         subgroup %in% c("Quintile 1 (poorest)", "Quintile 5 (richest)")) %>%
  dplyr::select(setting, date, subgroup, estimate, whoreg6) %>% # Kept whoreg6 for potential future faceting
  pivot_wider(names_from = subgroup, values_from = estimate) %>%
  # Ensure column names are safe for direct use if they contain special characters
  # For quintiles, they are usually "Quintile 1 (poorest)" etc.
  rename(q1_estimate = `Quintile 1 (poorest)`, q5_estimate = `Quintile 5 (richest)`) %>%
  mutate(wealth_gap_abs = q5_estimate - q1_estimate) %>%
  filter(!is.na(wealth_gap_abs))

if(nrow(dtp3_wealth_gap_trends) > 0) {
  # Average wealth gap over time globally
  avg_dtp3_wealth_gap_by_year <- dtp3_wealth_gap_trends %>%
    group_by(date) %>%
    summarise(mean_gap = mean(wealth_gap_abs, na.rm = TRUE),
              median_gap = median(wealth_gap_abs, na.rm=TRUE), # Median can be more robust to outliers
              .groups = 'drop')
  
  plot_dtp3_wealth_gap_global <- ggplot(avg_dtp3_wealth_gap_by_year, aes(x = date)) +
    geom_line(aes(y = mean_gap, color="Mean Gap"), linewidth=1) +
    geom_point(aes(y = mean_gap, color="Mean Gap")) +
    # Optionally plot median gap as well
    # geom_line(aes(y = median_gap, color="Median Gap"), linetype="dashed", linewidth=1) +
    # geom_point(aes(y = median_gap, color="Median Gap")) +
    labs(title = "Global Trend: DTP3 Wealth Gap (Richest Q5 - Poorest Q1)",
         x = "Year", y = "Average Wealth Gap (% points)", color="Metric") +
    theme_minimal() +
    scale_color_viridis_d(option = "viridis") # Or choose a specific color
  print(plot_dtp3_wealth_gap_global)
  
} else {
  cat("Not enough data to calculate DTP3 wealth gap trends.\n")
}


# --- 2.2 DTP3 Residence Gap (Urban - Rural) Over Time ---
cat("\nCalculating and plotting DTP3 Residence Gap (Urban-Rural) over time...\n")
dtp3_residence_gap_trends <- df_no_na_est %>%
  filter(indicator_name == "DTP3 immunization coverage among one-year-olds (%)",
         dimension == "Place of residence",
         subgroup %in% c("Rural", "Urban")) %>%
  dplyr::select(setting, date, subgroup, estimate, whoreg6) %>%
  pivot_wider(names_from = subgroup, values_from = estimate) %>%
  # Ensure column names are safe ('Rural' and 'Urban' are usually fine)
  mutate(residence_gap_abs = Urban - Rural) %>%
  filter(!is.na(residence_gap_abs))

if(nrow(dtp3_residence_gap_trends) > 0) {
  avg_dtp3_residence_gap_by_year <- dtp3_residence_gap_trends %>%
    group_by(date) %>%
    summarise(mean_gap = mean(residence_gap_abs, na.rm = TRUE),
              median_gap = median(residence_gap_abs, na.rm=TRUE),
              .groups = 'drop')
  
  plot_dtp3_residence_gap_global <- ggplot(avg_dtp3_residence_gap_by_year, aes(x = date)) +
    geom_line(aes(y = mean_gap, color="Mean Gap"), linewidth=1) +
    geom_point(aes(y = mean_gap, color="Mean Gap")) +
    # Optionally plot median gap
    # geom_line(aes(y = median_gap, color="Median Gap"), linetype="dashed", linewidth=1) +
    # geom_point(aes(y = median_gap, color="Median Gap")) +
    labs(title = "Global Trend: DTP3 Residence Gap (Urban - Rural)",
         x = "Year", y = "Average Residence Gap (% points)", color="Metric") +
    theme_minimal() +
    scale_color_viridis_d(option = "plasma") # Different color scheme
  print(plot_dtp3_residence_gap_global)
  
} else {
  cat("Not enough data to calculate DTP3 residence gap trends.\n")
}


# --- 3. Visualization 2: Temporal Trends in DTP3 Coverage BY WEALTH QUINTILE ---
cat("\n\n--- 3. Generating Plot for DTP3 Coverage Trends BY Wealth Quintile ---\n")
cat("Plotting DTP3 coverage trends by Wealth Quintile (Global Average)...\n")

dtp3_wealth_subgroup_trends <- df_no_na_est %>%
  filter(indicator_name == "DTP3 immunization coverage among one-year-olds (%)",
         dimension == "Economic status (wealth quintile)") %>%
  group_by(date, subgroup) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(mean_estimate)) # Ensure mean_estimate could be calculated

if(nrow(dtp3_wealth_subgroup_trends) > 0) {
  # Ensure subgroup is an ordered factor for correct legend and plotting order
  wealth_levels_ordered <- c("Quintile 1 (poorest)", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 (richest)")
  # Filter for levels actually present in the data to avoid warnings/errors if some are missing
  present_wealth_levels <- intersect(wealth_levels_ordered, unique(dtp3_wealth_subgroup_trends$subgroup))
  
  if (length(present_wealth_levels) > 0) {
    dtp3_wealth_subgroup_trends$subgroup <- factor(dtp3_wealth_subgroup_trends$subgroup, levels = present_wealth_levels, ordered = TRUE)
    
    plot_dtp3_by_wealth_quintile_time <- ggplot(dtp3_wealth_subgroup_trends, aes(x = date, y = mean_estimate, color = subgroup, group = subgroup)) +
      geom_line(linewidth = 1) +
      geom_point(size=1.5) +
      labs(title = "Global Average DTP3 Coverage Over Time by Wealth Quintile",
           x = "Year", y = "Mean DTP3 Coverage (%)", color = "Wealth Quintile") +
      theme_minimal() +
      scale_color_viridis_d(option = "magma") + # Using Viridis for distinct colors
      theme(legend.position = "bottom",
            legend.title = element_text(size=9),
            legend.text = element_text(size=8)) +
      guides(color = guide_legend(nrow=2)) # Adjust legend layout if needed
    print(plot_dtp3_by_wealth_quintile_time)
  } else {
    cat("No valid wealth quintile subgroups found after filtering for trends.\n")
  }
} else {
  cat("Not enough data for DTP3 wealth subgroup trends.\n")
}

cat("\n--- Additional Visualizations Script Complete ---\n")