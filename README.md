# Global Childhood Immunization: Inequality Patterns and Determinants

👉 [Click here to the presentation]([https://yashdeole.me](https://prezi.com/view/z7tPVH25avFMDa5GJLQi/))

## Project Overview

This project investigates global patterns of childhood immunization coverage, focusing on identifying inequalities and understanding their socio-demographic and spatio-temporal determinants. Utilizing WHO global health data, this analysis employs multivariate statistical techniques including Exploratory Data Analysis (EDA), Hierarchical Clustering, and Mixed-Effects Regression Modeling to uncover distinct national immunization profiles and the factors influencing them.

**Research Objective:** To investigate the socio-demographic and spatio-temporal determinants of childhood immunization coverage and identify distinct national patterns of inequality using multivariate statistical analysis of WHO global health data.

## Data Source

The primary dataset used is `immunization.csv`, derived from the WHO Health Inequality Data Repository. It contains immunization coverage estimates for various vaccines and zero-dose indicators, disaggregated by several dimensions of inequality (e.g., wealth, maternal education, place of residence) across numerous countries and years.

*   **Source:** WHO Health Inequality Data Repository
*   **Key Variables Used:** `setting` (country), `date` (year), `indicator_abbr` (vaccine/indicator code), `estimate` (coverage %), `dimension` (inequality dimension), `subgroup` (specific group within dimension), `whoreg6` (WHO Region), `wbincome2024` (World Bank Income Group).

## Methodology & Analysis Workflow

The project follows a multi-stage analytical approach:

1.  **Data Loading and Initial Exploration:** Understanding the structure, variables, and initial distributions.
2.  **Exploratory Data Analysis (EDA):** Visualizing distributions and relationships to identify preliminary patterns and inequalities related to:
    *   Overall distribution of immunization estimates.
    *   WHO Region.
    *   World Bank Income Group.
    *   Wealth Deciles.
    *   Maternal Education Level.
    *   Place of Residence (Urban/Rural).
    *   Temporal trends in overall coverage and inequality gaps.
3.  **Hierarchical Clustering:** Grouping countries based on their immunization profiles across a set of core indicators to identify distinct national patterns.
    *   Data Preparation: Creating a country-indicator matrix, selecting core indicators to maximize country inclusion.
    *   Distance Calculation: Euclidean distance on scaled data.
    *   Clustering Algorithm: Ward's D2 method.
    *   Optimal Cluster Number: Assessed using the Elbow method (WCSS).
4.  **Cluster Profile Analysis:** Characterizing the identified clusters based on their average performance across different immunization indicators.
5.  **Mixed-Effects Regression Modeling:** Investigating the determinants of a key immunization indicator (DTP3 coverage for one-year-olds - `vdpt`) considering wealth deciles and time trends, while accounting for country-level variations.

## Key Findings & Visualizations

### 1. Exploratory Data Analysis (EDA) Insights

**a. Distribution of Immunization Estimates:**
The overall distribution of immunization estimates is U-shaped, indicating that many reported coverage figures are either very high (good coverage or high "zero-dose" for poorly performing vaccines) or very low (poor coverage or low "zero-dose" for well-performing vaccines).

![EDA 1: Distribution of Childhood Immunization Estimates](images/EDA1_Distribution.png)
*(Replace `images/EDA1_Distribution.png` with the actual path to your image)*

**b. Coverage by WHO Region:**
The European region shows the highest median estimates, while the Eastern Mediterranean and African regions show lower medians and wider variability.

![EDA 2: Immunization Coverage by WHO Region](images/EDA2_WHO_Region.png)
*(Replace `images/EDA2_WHO_Region.png` with the actual path to your image)*

**c. Coverage by World Bank Income Group:**
A general trend of higher median estimates is observed with increasing national income levels, though "High income" countries show wide variability due to the mix of "coverage" and "zero-dose" indicators.

![EDA 3: Immunization Coverage by World Bank Income Group](images/EDA3_Income_Group.png)
*(Replace `images/EDA3_Income_Group.png` with the actual path to your image)*

**d. Wealth-Related Inequality:**
There's a clear positive gradient: as wealth decile increases, immunization coverage estimates tend to increase.

*(This plot was updated after addressing the missing Decile 1 & 10 issue)*
![EDA 5: Immunization Coverage by Wealth Decile (Corrected)](images/EDA5_Wealth_Decile_Corrected.png)
*(Replace `images/EDA5_Wealth_Decile_Corrected.png` with the actual path to your image)*

**e. Trend in Wealth Inequality Gap (Richest - Poorest Decile):**
The gap between the richest and poorest deciles has fluctuated over time, showing periods of larger and smaller inequality. A notable dip and subsequent rise occurred around the COVID-19 pandemic era.

![EDA 6: Immunization Inequality Over Time (Richest - Poorest)](images/EDA6_Inequality_Gap.png)
*(Replace `images/EDA6_Inequality_Gap.png` with the actual path to your image)*

**f. Maternal Education & Coverage:**
Higher maternal education levels are associated with higher immunization coverage estimates.

![EDA 7: Immunization Coverage by Maternal Education Level](images/EDA7_Maternal_Education.png)
*(Replace `images/EDA7_Maternal_Education.png` with the actual path to your image)*

**g. Urban vs. Rural Coverage:**
Urban areas generally show higher median immunization estimates than rural areas, though urban areas also exhibit greater variability.

![EDA 8: Immunization Coverage by Place of Residence](images/EDA8_Urban_Rural.png)
*(Replace `images/EDA8_Urban_Rural.png` with the actual path to your image)*

### 2. Country Clustering Based on Immunization Profiles (Core Indicators)

After selecting core indicators to include 108 countries, hierarchical clustering identified 3 distinct national profiles:

**a. Dendrogram:**
Visual representation of the hierarchical clustering process.

![Cluster Dendrogram (Core Indicators)](images/Cluster_Dendrogram_Core.png)
*(Replace `images/Cluster_Dendrogram_Core.png` with the actual path to your image)*

**b. Optimal Number of Clusters (Elbow Method):**
The elbow method suggested k=2 or k=3 as potential optimal numbers of clusters. k=3 was chosen for a more nuanced interpretation.

![Elbow Method for Optimal k](images/Elbow_Plot.png)
*(Replace `images/Elbow_Plot.png` with the actual path to your image)*

**c. Cluster Visualization (PCA-based):**
The 3 clusters are relatively well-separated in a 2D representation.

![Country Clusters (k=3) Based on Core Immunization Profiles](images/Fviz_Cluster_Core.png)
*(Replace `images/Fviz_Cluster_Core.png` with the actual path to your image)*

**d. Cluster Profiles - Average Coverage by Indicator:**
*   **Cluster 1 (Purple):** "Lower Overall Coverage & Higher Zero-Dose" - Consistently lowest coverage and highest zero-dose rates.
*   **Cluster 2 (Teal/Green):** "High Core Vaccine Coverage, Moderate Elsewhere" - Highest coverage for many core vaccines, very low DTP zero-dose.
*   **Cluster 3 (Yellow):** "Good to Very Good Coverage, but Some Gaps" - Generally good performance, often between Cluster 1 and 2.

![Average Immunization Coverage by Cluster and Core Indicator](images/Cluster_Bar_Chart_Core.png)
*(Replace `images/Cluster_Bar_Chart_Core.png` with the actual path to your image)*

**e. Geographical Distribution of Clusters:**
The clusters show distinct geographical patterns, with Cluster 1 predominantly in Sub-Saharan Africa.

![World Map Colored by Immunization Cluster (Core Indicators)](images/World_Map_Clusters_Core.png)
*(Replace `images/World_Map_Clusters_Core.png` with the actual path to your image)*

### 3. Determinants of DTP3 Immunization Coverage (Mixed-Effects Model)

A mixed-effects model (`model2`) was used to assess the impact of wealth decile and year on DTP3 coverage for one-year-olds, accounting for country-level variations (109 countries included).

**Model Equation Concept:**
`DTP3 Coverage % ≈ Overall Average (adj. for year) + Effect of Wealth Decile + Country's Own Baseline`

**a. Effect of Wealth Decile:**
A strong positive gradient exists: predicted DTP3 coverage significantly increases with household wealth. The richest 10% have ~21-22 percentage points higher predicted coverage than the poorest 10%.

![Predicted DTP3 Coverage by Wealth Decile (Model 2)](images/Model2_Wealth_Effect.png)
*(Replace `images/Model2_Wealth_Effect.png` with the actual path to your image)*

**b. Effect of Time (Year):**
There's a significant positive trend, with DTP3 coverage predicted to increase by approximately 0.6 percentage points per year on average, holding other factors constant.

![Predicted DTP3 Coverage Over Time (Model 2)](images/Model2_Time_Effect.png)
*(Replace `images/Model2_Time_Effect.png` with the actual path to your image)*

**Model Summary (Key Fixed Effects from `model2`):**
*   **Wealth Decile (Linear Trend):** Estimate = 18.40, p < 0.001 (Strong positive effect)
*   **Wealth Decile (Cubic Trend):** Estimate = 1.99, p < 0.001 (Indicates some non-linearity)
*   **Date (Year):** Estimate = 0.60, p < 0.001 (Positive time trend)
*   **Random Effect (Country Variance):** Substantial variation exists between countries (Std.Dev. of random intercept ≈ 18.16).

## Conclusions

1.  **Significant Inequalities:** Childhood immunization coverage is significantly influenced by socio-demographic factors, with notable disparities related to household wealth, maternal education, and urban/rural residence. Wealth appears to be a particularly strong determinant.
2.  **Distinct National Patterns:** Countries can be grouped into distinct clusters based on their immunization profiles, highlighting varying levels of overall performance and challenges (e.g., high zero-dose burden in one cluster vs. high core coverage in another). These clusters often show geographical coherence.
3.  **Positive Temporal Trends:** Overall, there has been an average improvement in DTP3 coverage over time, though this progress coexists with persistent inequalities.
4.  **Country Context Matters:** Even after accounting for specific determinants, significant variation in immunization performance exists between countries, underscoring the importance of national health system strength, policies, and local context.

## Future Work & Potential Next Steps

*   **Deeper Cluster Characterization:** Employ MANOVA/LDA to statistically validate and further describe cluster differences.
*   **Advanced Determinants Modeling:**
    *   Incorporate multiple dimensions of inequality (wealth, education, residence) simultaneously in mixed models, including interaction terms.
    *   Model specific inequality gaps (e.g., richest vs. poorest) as outcome variables.
    *   Explore more explicit spatio-temporal modeling techniques.
*   **Principal Component Analysis (PCA):** Reduce the dimensionality of immunization indicators to identify underlying "immunization performance constructs" for further analysis.
*   **Indicator-Specific Modeling:** Conduct detailed determinant analyses for other key immunization indicators beyond DTP3.
*   **Robust Model Diagnostics:** Perform thorough diagnostic checks for all regression models.

## Repository Structure
