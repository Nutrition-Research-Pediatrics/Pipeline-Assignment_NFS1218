# Pipeline-Assignment_NFS1218
Group project: Building an A-Z Reproducible Pipeline for Precision Nutrition Analysis

# Part 1: Exploring Missingness & Data Cleaning for Growth Dataset in R 

## 1 Packages
```
packages <- c("tidyverse", "naniar", "skimr", "ggplot2")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
```

## 2  Data loading 

```
data <- read_csv("~/Desktop/NFS 2026/NFS1218/Data Analysis Pipeline/mock_precision_growth_dataset(updated).csv")
```

## Inspect structure

```
glimpse(data)
summary(data)
```

## 3  Missingness exploration 

## 3.1 Percent missing per variable

```
missing_summary <- data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary)
```

## 3.2 Visual inspection of missing data

```
vis_miss(data)

# Missingness combinations
gg_miss_upset(data)
```
![Missingness](https://github.com/Nutrition-Research-Pediatrics/Pipeline-Assignment_NFS1218/blob/68117ba717fc3bcad035e45062531f2866f1b524/Graphs/Missingness_03.22.png)

All the variables were missing at random, variables of interest for our research question were not missing

## 4  Distribution inspection 

## 4.1 zBMI distribution at birth

```
if ("WHO_zBMI_birth" %in% names(data)) {
  
  ggplot(data, aes(x = WHO_zBMI_birth)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of zBMI at birth")
}
```
![zBMI Distribution](https://github.com/Nutrition-Research-Pediatrics/Pipeline-Assignment_NFS1218/blob/e7f8f1234c97dac41d90579d1e8080e9d502780d/Graphs/zBMI%20at%20birth_Distribution_03.22.png)
WHO_zBMI_birth was normally distributed and this histrogram points out any potential outliers that we will investigate in the next step

## 4.2 Maternal BMI distribution

```
if ("Maternal_BMI" %in% names(data)) {
  
  ggplot(data, aes(x = Maternal_BMI)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of maternal BMI")
}
```
![Maternal BMI Distribution](https://github.com/Nutrition-Research-Pediatrics/Pipeline-Assignment_NFS1218/blob/e7f8f1234c97dac41d90579d1e8080e9d502780d/Graphs/Maternal%20BMI_Distribution_03.22.png)
Maternal_BMI was normally distributed and this histogram points out any potential outliers that we will investigate in the next step

## 4.3 Maternal Fasting glucose distribution

```
if ("Fasting_glucose" %in% names(data)) {
  
  ggplot(data, aes(x = Fasting_glucose)) +
    geom_histogram(bins = 30) +
    theme_minimal() +
    labs(title = "Distribution of maternal fasting glucose")
}
```
![Maternal Fasting Glucose Distribution](https://github.com/Nutrition-Research-Pediatrics/Pipeline-Assignment_NFS1218/blob/e7f8f1234c97dac41d90579d1e8080e9d502780d/Graphs/Fasting%20glucose_Distribution_03.22.png)
Fasting_glucose was normally distributed and this histogram points out any potential outliers that we will investigate in the next step

## 5  Identifying implausible values 

## 5.1 Implausible gestational age values

```
if ("Gestational_age_weeks" %in% names(data)) {
  
  implausible_gest_age <- data %>%
    filter(Gestational_age_weeks < 22 | Gestational_age_weeks > 44)
  
  print(implausible_gest_age)
}
```

Cut-offs were selected based off of criteria and the results indicated 1 participant as an outlier/implausible value (participant ID 96 with a gestational age of 44.8)

## 5.2 Implausible maternal BMI values

```
if ("Maternal_BMI" %in% names(data)) {
  
  implausible_mBMI <- data %>%
    filter(Maternal_BMI < 15 | Maternal_BMI > 40)
  
  print(implausible_mBMI)
}
```

Cut-offs were selected based off of criteria and the results indicated 6 participants as an outlier/implausible value (participant ID 58 = 14.5 kg/m^2; participant ID 65 = 41.4 kg/m^2; participant ID 131 = 13.6 kg/m^2; participant ID 162 = 14.9 kg/m^2; participant ID 233 = 12.5 kg/m^2; participant ID 255 = 12.7 kg/m^2

## 5.3 Implausible maternal fasting glucose values

```
if ("Fasting_glucose" %in% names(data)) {
  
  implausible_fglucose <- data %>%
    filter(Fasting_glucose < 2.5 | Fasting_glucose > 12)
  
  print(implausible_fglucose)
}
```

Cut-offs were selected based off of criteria and the results indicated zero potential participants as outliers/implausible values

## 5.4 WHO-style plausibility cut-offs for zBMI at birth

WHO commonly flags z-scores < -5 or > +5 as implausible

```
if ("WHO_zBMI_birth" %in% names(data)) {
  
  implausible_atbirth <- data %>%
    filter(WHO_zBMI_birth < -5 | WHO_zBMI_birth > 5)
  
  print(implausible_atbirth)
}
```

Cut-offs were selected based off of criteria and the results indicated 12 participants as an outlier/implausible value (participnat ID 2 = -6.4; participant ID 13 = -5.59; participnat ID 33 = -5.2; participant ID 42 = -5.4; participant ID 112 = -5.6; participant ID 140 = +5.3; participant ID 142 = -5,4; participant ID 200 = -5.1; participant ID 226 = -6.1; participant ID 249 = -6.5; participant ID 278 = -6.5; participant ID 298 = -6.1

## 6  Data cleaning 

```
clean_data <- data

# 6.1 Remove implausible gestational age
if ("Gestational_age_weeks" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Gestational_age_weeks >= 22 & Gestational_age_weeks <= 44)
}
```

From this a total of 1 participant was removed from the dataset, new sample size is N=299

## 6.2 Remove implausible maternal BMI

```
if ("Maternal_BMI" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(Maternal_BMI >= 15 & Maternal_BMI <= 40)
}
```

From this a total of 6 participants were removed from the dataset, new sample size is N=293

## 6.3 Remove implausible zBMI at birth

```
if ("WHO_zBMI_birth" %in% names(clean_data)) {
  clean_data <- clean_data %>%
    filter(WHO_zBMI_birth >= -5 & WHO_zBMI_birth <= 5)
}
```

From this a total of 12 participants were removed from the dataset, new sample size is N=281

Since in section 5.3 there were no potential outliers/implausible values for Fasting_glucose we did not have to remove any implausible values, therefore the cleaned dataset sample size is N = 281

## 7  Post-cleaning diagnostics  

## 7.1 Compare sample size

```
cat("Original N:", nrow(data), "\n")
cat("Cleaned N:", nrow(clean_data), "\n")
```

## 7.2 Recalculate missingness after cleaning

```
missing_summary_clean <- clean_data %>%
  summarise(across(everything(),
                   ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "percent_missing") %>%
  arrange(desc(percent_missing))

print(missing_summary_clean)
```

## 7.3 Save cleaned dataset

```
write_csv(clean_data, "clean_precision_growth_dataset_03.22.csv")
```

# Part 2 Assessing Clustering Tendency

## 1 Install and load required packages

```
packages <- c("NbClust", "factoextra", "ggplot2", "gridExtra", "cluster", 
              "RColorBrewer", "reshape2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
```

## 2 Reading Data

```
data_clean <- read_csv("~/Desktop/NFS 2026/NFS1218/Data Analysis Pipeline/clean_precision_growth_dataset_03.22_2variables.csv")
```

This dataset contains the cleaned variables specific variables for our clustering analysis, in the dataset there is a total of 3 variables consisting of Participant ID, Maternal BMI, and Fasting Blood Glucose

## 2.1 Renamed the dataset

```
data_mBMI_glucose <- data_clean
```

## 2.2 Scale the data

```
data_scaled <- scale(data_mBMI_glucose)
```

## 3. NBClust analysis to determine optimal number of clusters

```
nbclust_result <- NbClust(data_scaled,
                          distance = "euclidean",
                          min.nc = 2,
                          max.nc = 8,
                          method = "kmeans",
                          index ="all"
)
```

## 3.1 Extract optimal k

```
optimal_k <- as.numeric(names(which.max(table(nbclust_result$Best.nc[1,]))))
cat(sprintf("\nOptimal number of clusters: %d\n", optimal_k))
```

## 3.2 PLOT 1: VOTING RESULTS

```
votes <- table(nbclust_result$Best.nc[1,])
vote_df <- as.data.frame(votes)
names(vote_df) <- c("k", "Votes")
vote_df$k <- as.numeric(as.character(vote_df$k))

plot1 <- ggplot(vote_df, aes(x = k, y = Votes, fill = k == optimal_k)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.8) +
  geom_text(aes(label = Votes), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("grey70", "#E74C3C"), guide = "none") +
  scale_x_continuous(breaks = vote_df$k) +
  labs(
    title = "A. NbClust Voting Results",
    subtitle = sprintf("Optimal k = %d (most votes)", optimal_k),
    x = "Number of Clusters (k)",
    y = "Number of Indices"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )
# 3.3 PLOT 2: ELBOW METHOD

wss <- sapply(1:8, function(k) {
  if (k == 1) {
    sum(scale(data_scaled, scale = FALSE)^2)
  } else {
    kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
  }
})

elbow_df <- data.frame(k = 1:8, WSS = wss)

plot2 <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(color = "#3498DB", linewidth = 1.5) +
  geom_point(color = "#3498DB", size = 4) +
  geom_point(data = elbow_df[elbow_df$k == optimal_k, ], 
             aes(x = k, y = WSS), color = "#E74C3C", size = 6) +
  annotate("text", x = optimal_k, y = max(wss) * 0.85, 
           label = sprintf("Elbow at k = %d", optimal_k), 
           color = "#E74C3C", fontface = "bold", size = 5.5) +
  annotate("curve", x = optimal_k + 0.3, y = max(wss) * 0.83, 
           xend = optimal_k + 0.05, yend = wss[optimal_k] + 5,
           arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
           color = "#E74C3C", linewidth = 1) +
  scale_x_continuous(breaks = 1:8) +
  labs(
    title = "B. Elbow Method",
    subtitle = "Look for the bend (elbow) in the curve",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )
plot2
```

## 3.4 PLOT 3: SILHOUETTE ANALYSIS

```
install.packages("cluster")

library(cluster)

install.packages("factoextra")

library(factoextra)

km_optimal <- kmeans(data_scaled, centers = optimal_k, nstart = 25)
sil <- silhouette(km_optimal$cluster, dist(data_scaled))
avg_sil_width <- mean(sil[, 3])

plot3 <- fviz_silhouette(sil, print.summary = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = sprintf("C. Silhouette Analysis (k = %d)", optimal_k),
    subtitle = sprintf("Average silhouette width = %.3f (higher is better)", avg_sil_width),
    x = "Observations",
    y = "Silhouette Width"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 13),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("tutorial_plot3_silhouette.png", plot3, width = 11, height = 7, dpi = 300, bg = "white")
ggsave("tutorial_plot3_silhouette.svg", plot3, width = 11, height = 7, bg = "white")
plot3

# Silhouette Analysis: works with any distance metric, detects cluster separation,and cohesion 

# 3.5 PLOT 4: PCA CLUSTER VISUALIZATION

pca_result <- prcomp(data_scaled)
pca_df <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Cluster = as.factor(km_optimal$cluster)
)

variance_explained <- round(100 * summary(pca_result)$importance[2, 1:2], 1)

plot4 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, shape = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  stat_ellipse(aes(fill = Cluster), alpha = 0.2, geom = "polygon", level = 0.95) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 9, 10, 11)[1:optimal_k]) +
  labs(
    title = sprintf("D. Cluster Visualization (k = %d)", optimal_k),
    subtitle = "PCA projection with 95% confidence ellipses",
    x = sprintf("PC1 (%s%% variance)", variance_explained[1]),
    y = sprintf("PC2 (%s%% variance)", variance_explained[2])
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 13),
    panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
    plot.margin = margin(20, 20, 20, 20)
  )
plot4

# 3.6 PLOT 5: COMPARISON OF MULTIPLE K VALUES

k_values <- 2:6
comparison_plots <- list()

for (k in k_values) {
  km_temp <- kmeans(data_scaled, centers = k, nstart = 25)
  temp_df <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Cluster = as.factor(km_temp$cluster)
  )
  
  p <- ggplot(temp_df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    stat_ellipse(level = 0.95, linewidth = 1) +
    scale_color_brewer(palette = "Set2") +
    labs(title = sprintf("k = %d", k)) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      legend.position = "none",
      panel.grid.major = element_line(color = "grey95")
    )
  
  comparison_plots[[length(comparison_plots) + 1]] <- p
}

plot5 <- grid.arrange(
  grobs = comparison_plots,
  ncol = 3,
  top = grid::textGrob(
    "E. Comparing Different Numbers of Clusters",
    gp = grid::gpar(fontsize = 20, fontface = "bold")
  )
)

# Strong separation (non‑overlapping ellipses) between the three groups which supports k = 3 

# 3.7 PLOT 6: INDEX HEATMAP

# Create matrix showing which k each index voted for
index_votes <- nbclust_result$Best.nc[1, ]
index_votes <- index_votes[!is.na(index_votes)]

index_matrix <- data.frame(
  Index = names(index_votes),
  Optimal_k = as.numeric(index_votes)
)

# Create categorical heatmap
index_matrix$Index_num <- 1:nrow(index_matrix)

plot6 <- ggplot(index_matrix, aes(x = Optimal_k, y = reorder(Index, Index_num))) +
  geom_tile(aes(fill = as.factor(Optimal_k)), color = "white", linewidth = 1) +
  geom_text(aes(label = Optimal_k), color = "white", fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Spectral", name = "Optimal k") +
  scale_x_continuous(breaks = 2:8, expand = c(0, 0)) +
  labs(
    title = "F. Individual Index Recommendations",
    subtitle = "Each row shows one index's vote for optimal k",
    x = "Recommended Number of Clusters",
    y = "Statistical Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

plot6

# 3.8 CREATE COMBINED FIGURE


combined <- grid.arrange(
  plot1, plot2, plot3, plot4,
  ncol = 2, nrow = 2,
  top = grid::textGrob(
    "NbClust Tutorial: Complete Analysis",
    gp = grid::gpar(fontsize = 22, fontface = "bold"),
    vjust = 1
  )
)

# GENERATE SUMMARY REPORT

sink("nbclust_summary.txt")

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("NbClust - ANALYSIS SUMMARY\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("DATASET INFORMATION:\n")
cat(sprintf("  • Number of observations: %d\n", nrow(data_scaled)))
cat(sprintf("  • Number of features: %d\n", ncol(data_scaled)))
cat(sprintf("  • True number of clusters: 3\n\n"))

cat("NBCLUST RECOMMENDATION:\n")
cat(sprintf("  • Optimal number of clusters: %d\n", optimal_k))
cat(sprintf("  • Number of indices tested: %d\n", length(index_votes)))
cat("\n")

cat("VOTING BREAKDOWN:\n")
for (i in 1:nrow(vote_df)) {
  cat(sprintf("  • k = %d: %d votes\n", vote_df$k[i], vote_df$Votes[i]))
}
cat("\n")

cat("CLUSTER QUALITY METRICS:\n")
cat(sprintf("  • Average Silhouette Width: %.3f\n", avg_sil_width))
cat(sprintf("  • Total Within-Cluster SS: %.2f\n", km_optimal$tot.withinss))
cat(sprintf("  • Between-Cluster SS / Total SS: %.1f%%\n", 
            100 * km_optimal$betweenss / km_optimal$totss))
cat("\n")

cat("INTERPRETATION GUIDE:\n")
cat("  • Silhouette width > 0.50: Good cluster structure\n")
cat("  • Silhouette width 0.25-0.50: Weak cluster structure\n")
cat("  • Silhouette width < 0.25: No substantial cluster structure\n\n")

cat("FILES GENERATED:\n")
cat("  1. plot1_voting.png/svg - Voting results\n")
cat("  2. plot2_elbow.png/svg - Elbow method\n")
cat("  3. plot3_silhouette.png/svg - Silhouette analysis\n")
cat("  4. plot4_pca.png/svg - PCA visualization\n")
cat("  5. plot5_comparison.png/svg - Multiple k comparison\n")
cat("  6. plot6_index_heatmap.png/svg - Index recommendations\n")
cat("  7. combined_all.png/pdf - Combined figure\n")
cat("  8. nbclust_summary.txt - This summary\n\n")

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")

sink()
```

## Define helper for pipe

```
`%>%` <- function(x, f) f(x)
```


