# --- 0. Load Necessary Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)          # For spatial data operations
library(rnaturalearth) # For world map data
library(rnaturalearthdata) # For world map data
library(viridis)     # For color scales

# --- 1. Load Data ---
cat("--- 1. Loading Data ---\n")
df <- read.csv("immunization.csv")

# --- 2. Calculate DTP3 Inequality Gap per Country ---
cat("\n--- 2. Calculating DTP3 Inequality Gap (Richest - Poorest Decile) per Country ---\n")

country_inequality_gaps <- df %>%
  filter(indicator_abbr == "vdpt",
         dimension == "Economic status (wealth decile)",
         subgroup %in% c("Decile 1 (poorest)", "Decile 10 (richest)"),
         !is.na(estimate)) %>%
  group_by(setting, subgroup) %>%
  summarise(mean_decile_estimate = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = subgroup, values_from = mean_decile_estimate) %>%
  filter(!is.na(`Decile 1 (poorest)`) & !is.na(`Decile 10 (richest)`)) %>%
  mutate(dtp3_inequality_gap = `Decile 10 (richest)` - `Decile 1 (poorest)`) %>%
  # Be explicit with dplyr::select to avoid masking issues
  dplyr::select(setting, dtp3_inequality_gap)

# Check the result
print(paste("Number of countries with calculated DTP3 inequality gaps:", nrow(country_inequality_gaps)))
if (nrow(country_inequality_gaps) > 0) {
  print(head(country_inequality_gaps %>% arrange(-dtp3_inequality_gap)))
} else {
  warning("No countries had data for both poorest and richest deciles for DTP3. Cannot create inequality map.")
}


# --- 3. Prepare for Merging with Map Data (Add ISO codes if available and useful) ---
# It's often more reliable to join spatial data using ISO codes.
# Let's assume your original 'df' has an 'iso3' column.
cat("\n--- 3. Adding ISO codes to inequality data (if available) ---\n")

if ("iso3" %in% names(df)) {
  iso_codes_lookup <- df %>%
    distinct(setting, iso3) %>% # Get unique country name to iso3 mapping
    filter(!is.na(iso3) & iso3 != "") # Ensure iso3 codes are valid
  
  country_inequality_gaps_with_iso <- country_inequality_gaps %>%
    left_join(iso_codes_lookup, by = "setting") %>%
    filter(!is.na(iso3)) # Keep only countries for which we found an ISO code
  
  print(paste("Number of countries with inequality gaps AND ISO codes:", nrow(country_inequality_gaps_with_iso)))
  join_key_map <- "iso_a3_eh" # rnaturalearth often uses 'iso_a3' or 'iso_a3_eh'
  join_key_data <- "iso3"
  data_to_join <- country_inequality_gaps_with_iso
  
} else {
  warning("No 'iso3' column found in the original 'df'. Will attempt to join map by country name ('setting'). This can be less reliable.")
  join_key_map <- "name_long" # or "name", "admin", "sovereignt"
  join_key_data <- "setting"
  data_to_join <- country_inequality_gaps
}


# --- 4. Get World Map Data and Merge ---
cat("\n--- 4. Merging Inequality Gaps with World Map Data ---\n")
world_map_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Dynamically create the join condition
join_condition <- setNames(join_key_data, join_key_map) # e.g., c("iso_a3_eh" = "iso3") or c("name_long" = "setting")

world_inequality_map_data <- world_map_sf %>%
  left_join(data_to_join, by = join_condition)

# Check how many countries were matched
matched_countries_on_map <- sum(!is.na(world_inequality_map_data$dtp3_inequality_gap))
print(paste("Number of map features successfully joined with inequality data:", matched_countries_on_map))
if(matched_countries_on_map == 0 && nrow(data_to_join) > 0) {
  warning("No countries were matched between map data and inequality data. Check join keys and country names/ISO codes carefully!")
  print("Example country names from map data (column used for join):")
  print(head(world_map_sf[[join_key_map]]))
  print("Example country names from your data (column used for join):")
  print(head(data_to_join[[join_key_data]]))
}


# --- 5. Plot the World Map of DTP3 Inequality ---
cat("\n--- 5. Plotting World Map of DTP3 Inequality ---\n")

if (matched_countries_on_map > 0) {
  map_inequality <- ggplot(data = world_inequality_map_data) +
    geom_sf(aes(fill = dtp3_inequality_gap), color = "gray50", linewidth = 0.1) + # Changed size to linewidth
    scale_fill_viridis_c(option = "plasma", na.value = "lightgray",
                         name = "DTP3 Coverage Gap\n(Richest - Poorest Decile, % points)") +
    labs(title = "Global DTP3 Immunization Inequality: Richest vs. Poorest",
         caption = paste(nrow(data_to_join), "countries with DTP3 decile data; ", matched_countries_on_map, "shown on map.")) +
    theme_void() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.key.width = unit(1.5, "cm"))
  print(map_inequality)
} else {
  cat("Cannot plot map as no countries were successfully merged with inequality data.\n")
}

cat("\n--- Inequality Mapping Script Complete ---\n")