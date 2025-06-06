# Load necessary libraries (ensure these are installed)
library(dplyr)
library(tidyr)      # For pivot_wider, pivot_longer
library(viridis)    # For color palettes (can be optional, ggplot has defaults)
library(cluster)    # For daisy (if using for Gower distance with mixed types - not needed here)
library(factoextra) # For fviz_cluster
library(reshape2)   # For melt (alternative to pivot_longer, but pivot_longer is more current)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate) # Though date is already numeric year
library(knitr)

df = read.csv("immunization.csv")





df_temp_before_dropna <- df %>%
  filter(!is.na(estimate)) %>%
  group_by(setting, indicator_abbr) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator_abbr, values_from = mean_estimate)

missing_counts_per_indicator <- colSums(is.na(df_temp_before_dropna))
# print(sort(missing_counts_per_indicator))


# Step 0: Prepare the data with selected core indicators
core_indicators_selected <- names(missing_counts_per_indicator[missing_counts_per_indicator <= 8])
if (!"setting" %in% core_indicators_selected) {
  core_indicators_selected <- c("setting", core_indicators_selected)
}
print("Selected core indicators for clustering:")
print(core_indicators_selected)

df_cluster_final <- df_temp_before_dropna %>%
  dplyr::select(all_of(core_indicators_selected)) %>%
  drop_na()

print(paste("Number of countries in df_cluster_final for clustering:", nrow(df_cluster_final)))

# Step 1: Prepare data for clustering
row_labels <- df_cluster_final$setting  # store country names
df_scaled <- scale(df_cluster_final[, -1])  # remove 'setting' column and scale the data
# Check for zero variance columns if errors occur:
# if(any(apply(df_cluster_final[, -1], 2, var, na.rm = TRUE) == 0)){
#   print("Warning: Some columns have zero variance after filtering. This might affect scaling or clustering.")
#   # Consider removing them or handling them before scaling
# }


# Step 2: Compute distance matrix (already done if you are proceeding with hc)
# dist_matrix <- dist(df_scaled, method = "euclidean") # Not strictly needed again if hc is based on it


# Step 3: Hierarchical clustering using Ward's method (already done)
# hc <- hclust(dist_matrix, method = "ward.D2")


# --- NEW: Determine Optimal Number of Clusters using Elbow Method ---
cat("\n--- Determining Optimal k using Elbow Method ---\n")
elbow_plot <- fviz_nbclust(df_scaled, # Use the scaled data
                           FUNcluster = function(x, k) hcut(dist(x, method="euclidean"), k=k, hc_method="ward.D2"),
                           method = "wss", # WSS is the Elbow method
                           k.max = 10, # Check up to 10 clusters (adjust as needed)
                           verbose = FALSE) # Suppress progress messages
print(elbow_plot + labs(title = "Elbow Method for Optimal k (Hierarchical Clustering)"))


# --- Step 4: Plot Dendrogram (using pre-calculated hc) ---
# Hierarchical clustering (Ward's method) - if not run above or if you want to ensure it's available
dist_matrix <- dist(df_scaled, method = "euclidean") # Ensure dist_matrix is available
hc <- hclust(dist_matrix, method = "ward.D2")

plot(hc, labels = row_labels, cex = 0.6, hang = -1,
     main = "Cluster Dendrogram of Countries by Immunization Profiles (Core Indicators)",
     xlab = "Countries", ylab = "Height")


# --- Step 5: Cut dendrogram into k clusters ---
# Visually inspect the dendrogram AND the elbow plot to choose k.
# Let's say the elbow plot also suggests k=3 or it's still your preferred choice:
k_chosen_from_elbow <- 3 # <--- UPDATE THIS based on the elbow plot, or keep as 3 if it's reasonable
clusters <- cutree(hc, k = k_chosen_from_elbow)
df_cluster_final$cluster <- factor(clusters)


# --- Step 6: Visualize clusters in 2D (PCA) ---
fviz_cluster(list(data = df_scaled, cluster = clusters),
             geom = "point",
             ellipse.type = "convex",
             palette = "Set1",
             ggtheme = theme_minimal(),
             main = paste("Country Clusters (k=", k_chosen_from_elbow, ") Based on Core Immunization Profiles", sep=""))


# --- Step 7: Analyze cluster characteristics - Bar chart ---
df_long_final <- df_cluster_final %>%
  pivot_longer(cols = -c(setting, cluster),
               names_to = "indicator", values_to = "value")

df_summary_final <- df_long_final %>%
  group_by(cluster, indicator) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# Ensure indicators are factors for consistent plotting order
indicator_order <- setdiff(core_indicators_selected, "setting") # Get indicator names without 'setting'
df_summary_final$indicator <- factor(df_summary_final$indicator, levels = indicator_order)

ggplot(df_summary_final, aes(x = indicator, y = mean_value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "D") +
  labs(title = paste("Average Immunization Coverage by Cluster (k=", k_chosen_from_elbow, ") and Core Indicator", sep=""),
       x = "Immunization Indicator", y = "Mean Coverage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --- Step 8: Plot clusters on a World Map ---
world <- ne_countries(scale = "medium", returnclass = "sf")

world_clustered_final <- world %>%
  left_join(df_cluster_final %>% dplyr::select(setting, cluster), by = c("name_long" = "setting"))
# Consider other join keys if name_long doesn't work well:
# by = c("name" = "setting") or by = c("iso_a2_eh" = "iso_code_column_if_you_had_one")

ggplot(data = world_clustered_final) +
  geom_sf(aes(fill = cluster), color = "gray80", size = 0.1) +
  scale_fill_viridis_d(option = "D", na.value = "lightgray", name = "Cluster") +
  labs(title = paste("World Map Colored by Immunization Cluster (k=", k_chosen_from_elbow, ", Core Indicators)", sep=""),
       caption = paste(nrow(df_cluster_final), "countries included in clustering")) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

print(elbow_plot + labs(title = "Elbow Method for Optimal k (Hierarchical Clustering)"))


library(klaR)
library(psych)
library(MASS)
#library(ggord)
library(devtools)

# --- Step 9: Deeper Dive into Cluster Profiles (MANOVA & LDA) ---
cat("\n\n--- Step 9: Characterizing Clusters with MANOVA and LDA ---\n")

# Prepare data for MANOVA/LDA: We need the cluster assignment and the original (or scaled) indicator values
# It's generally better to use the original (unscaled but numeric) indicator values for MANOVA/LDA
# if you want to interpret coefficients in original units, but scaled can also be used.
# Let's use the unscaled data from df_cluster_final (excluding 'setting' and the 'cluster' column itself for the dependent vars).

indicator_columns_for_analysis <- setdiff(colnames(df_cluster_final), c("setting", "cluster"))
manova_lda_data <- df_cluster_final # Contains 'cluster' and the indicator columns

if (nrow(manova_lda_data) > 0 && length(unique(manova_lda_data$cluster)) > 1 && ncol(manova_lda_data[, indicator_columns_for_analysis]) > 0) {
  
  # 9.1: MANOVA (Multivariate Analysis of Variance)
  cat("\n--- 9.1: MANOVA Results ---\n")
  # Formula: dependent variables matrix ~ grouping variable
  # Dependent variables are all indicator columns
  manova_formula_string <- paste(paste0("`", indicator_columns_for_analysis, "`", collapse = " + "), "~ cluster")
  manova_formula <- as.formula(paste("cbind(", paste0("`", indicator_columns_for_analysis, "`", collapse = ", "), ") ~ cluster"))
  
  # Check if there are enough observations per group for MANOVA
  min_group_size <- min(table(manova_lda_data$cluster))
  num_dependent_vars <- length(indicator_columns_for_analysis)
  if(min_group_size > num_dependent_vars) {
    manova_result <- manova(manova_formula, data = manova_lda_data)
    print(summary(manova_result, test = "Wilks")) # Popular test statistic
    # For individual ANOVAs (which indicators differ significantly across clusters)
    cat("\n--- Follow-up Univariate ANOVAs ---\n")
    print(summary.aov(manova_result))
  } else {
    cat("Skipping MANOVA: Not enough observations per cluster group relative to the number of indicators.\n")
    cat(paste("Minimum group size:", min_group_size, "Number of indicators:", num_dependent_vars, "\n"))
  }
  
  
  # 9.2: LDA (Linear Discriminant Analysis)
  cat("\n\n--- 9.2: LDA Results ---\n")
  # Formula: grouping variable ~ predictor variables
  lda_formula <- as.formula(paste("cluster ~", paste0("`", indicator_columns_for_analysis, "`", collapse = " + ")))
  
  # LDA requires group sizes to be larger than number of predictors for some calculations,
  # or at least not too small.
  if(min_group_size > 1 && nrow(manova_lda_data) > num_dependent_vars + length(unique(manova_lda_data$cluster)) ) { # Basic check
    lda_model <- lda(lda_formula, data = manova_lda_data)
    print(lda_model) # Shows prior probabilities, group means, coefficients of linear discriminants
    
    # Proportion of trace (variance explained by each discriminant function)
    prop_trace <- lda_model$svd^2 / sum(lda_model$svd^2)
    cat("\nProportion of Trace for LDA Functions:\n")
    print(prop_trace)
    
    # You can also get predictions and create a confusion matrix
    # lda_pred <- predict(lda_model, manova_lda_data)
    # cat("\nLDA Classification Table:\n")
    # print(table(Observed = manova_lda_data$cluster, Predicted = lda_pred$class))
    # cat(paste("LDA Accuracy:", mean(lda_pred$class == manova_lda_data$cluster), "\n"))
    
    # Visualize LDA (if 2 or more discriminant functions exist, i.e., k_chosen_from_elbow >= 3)
    if (length(prop_trace) >= 1) { # Requires at least one LD
      lda_scores_df <- as.data.frame(predict(lda_model, manova_lda_data)$x)
      lda_plot_data <- cbind(manova_lda_data, lda_scores_df)
      
      # Plot first two Linear Discriminants
      if (ncol(lda_scores_df) >= 2) {
        lda_gg <- ggplot(lda_plot_data, aes(x = LD1, y = LD2, color = cluster, shape = cluster)) +
          geom_point(alpha = 0.7, size = 2) +
          stat_ellipse(aes(group = cluster), type = "t", level = 0.95) + # Ellipses around clusters
          labs(title = "LDA Plot: Countries by First Two Discriminant Functions",
               x = paste0("LD1 (", round(prop_trace[1]*100, 1), "%)"),
               y = paste0("LD2 (", round(prop_trace[2]*100, 1), "%)")) +
          theme_minimal() +
          scale_color_viridis_d(option = "D") +
          scale_shape_discrete()
        print(lda_gg)
      } else if (ncol(lda_scores_df) == 1) {
        # Plot first Linear Discriminant (e.g., density plot)
        lda_gg_1d <- ggplot(lda_plot_data, aes(x = LD1, fill = cluster, color = cluster)) +
          geom_density(alpha = 0.5) +
          labs(title = "LDA Plot: Density of First Discriminant Function by Cluster",
               x = paste0("LD1 (", round(prop_trace[1]*100, 1), "%)")) +
          theme_minimal() +
          scale_fill_viridis_d(option = "D") +
          scale_color_viridis_d(option = "D")
        print(lda_gg_1d)
      }
    }
  } else {
    cat("Skipping LDA: Issues with group sizes or number of variables for LDA.\n")
    cat(paste("Minimum group size:", min_group_size, "Number of indicators:", num_dependent_vars, "\n"))
  }
} else {
  cat("Skipping MANOVA/LDA: Not enough data after final preparation for clustering or insufficient cluster variation.\n")
}

cat("\n\n--- Cluster Analysis Script Complete (including MANOVA/LDA) ---\n")

