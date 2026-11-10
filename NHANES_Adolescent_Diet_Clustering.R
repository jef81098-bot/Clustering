# ============================================================
# NHANES 2021-2023: Adolescent dietary pattern clustering
# Data: DR1TOT_L.xpt + DEMO_L.xpt
# Age: 12-18 years
# Outputs: ./output/
# ============================================================

# 0. Packages -------------------------------------------------
required_packages <- c("haven", "cluster")
new_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}
library(haven)
library(cluster)

# 1. File paths ----------------------------------------------
# Recommended: save this R script in the same folder as DR1TOT_L.xpt
# and set the R working directory to that folder in RStudio.
data_dir <- "C:/Users/jef81/Desktop/Health_Big_Data/Cluster"
diet_file <- file.path(data_dir, "DR1TOT_L.xpt")
demo_file <- file.path(data_dir, "DEMO_L.xpt")
output_dir <- file.path(data_dir, "output")

if (!dir.exists(data_dir)) {
  stop("資料資料夾不存在，請確認 data_dir 路徑。")
}
if (!file.exists(diet_file)) {
  stop("找不到 DR1TOT_L.xpt：", diet_file)
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# DEMO_L.xpt contains RIDAGEYR; download it if it is not already present.
if (!file.exists(demo_file)) {
  demo_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DEMO_L.xpt"
  message("正在下載 DEMO_L.xpt（用來取得年齡 RIDAGEYR）...")
  try(
    download.file(demo_url, demo_file, mode = "wb", quiet = FALSE),
    silent = TRUE
  )
}
if (!file.exists(demo_file)) {
  stop(
    "找不到 DEMO_L.xpt，且自動下載失敗。請從 NHANES 下載 DEMO_L.xpt，",
    "並放到：", data_dir
  )
}

# 2. Read and merge data -------------------------------------
diet <- read_xpt(diet_file)
demo <- read_xpt(demo_file)

if (!all(c("SEQN", "DR1DRSTZ") %in% names(diet))) {
  stop("DR1TOT_L.xpt 缺少 SEQN 或 DR1DRSTZ，請確認資料版本。")
}
if (!all(c("SEQN", "RIDAGEYR") %in% names(demo))) {
  stop("DEMO_L.xpt 缺少 SEQN 或 RIDAGEYR。")
}

dat <- merge(
  diet,
  demo[, c("SEQN", "RIDAGEYR")],
  by = "SEQN",
  all = FALSE
)

# Keep adolescents aged 12 through 18 and reliable Day 1 recalls.
dat <- dat[
  !is.na(dat$RIDAGEYR) &
    dat$RIDAGEYR >= 12 &
    dat$RIDAGEYR <= 18 &
    dat$DR1DRSTZ == 1,
]

write.csv(
  data.frame(
    Step = c("Merged participants", "Age 12-18 with reliable Day 1 recall"),
    N = c(nrow(merge(diet, demo[, c("SEQN", "RIDAGEYR")],
                     by = "SEQN", all = FALSE)), nrow(dat))
  ),
  file.path(output_dir, "sample_flow.csv"),
  row.names = FALSE
)

# 3. Variables ------------------------------------------------
# Variables below are the requested nutrient variables in DR1TOT_L.
nutrient_vars <- c(
  "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TSUGR", "DR1TFIBE",
  "DR1TTFAT", "DR1TCHOL", "DR1TATOC", "DR1TVARA", "DR1TVB1",
  "DR1TVB2", "DR1TVB6", "DR1TVB12", "DR1TVC", "DR1TVD",
  "DR1TCAFF", "DR1TALCO", "DR1_320Z"
)

missing_vars <- setdiff(nutrient_vars, names(dat))
if (length(missing_vars) > 0) {
  stop(
    "資料中找不到以下變數：",
    paste(missing_vars, collapse = ", "),
    "\n請確認下載的 DR1TOT_L.xpt 是否為 2021-2023 版本。"
  )
}

# Use clear English labels in exported tables and plots.
pretty_names <- c(
  DR1TKCAL = "Energy_kcal",
  DR1TPROT = "Protein_g",
  DR1TCARB = "Carbohydrate_g",
  DR1TSUGR = "Total_sugars_g",
  DR1TFIBE = "Dietary_fiber_g",
  DR1TTFAT = "Total_fat_g",
  DR1TCHOL = "Cholesterol_mg",
  DR1TATOC = "Vitamin_E_mg",
  DR1TVARA = "Vitamin_A_RAE_mcg",
  DR1TVB1 = "Thiamin_B1_mg",
  DR1TVB2 = "Riboflavin_B2_mg",
  DR1TVB6 = "Vitamin_B6_mg",
  DR1TVB12 = "Vitamin_B12_mcg",
  DR1TVC = "Vitamin_C_mg",
  DR1TVD = "Vitamin_D_mcg",
  DR1TCAFF = "Caffeine_mg",
  DR1TALCO = "Alcohol_g",
  DR1_320Z = "Plain_water_g"
)

# 4. Clean data and descriptive statistics -------------------
# Convert labelled columns to numeric values.
for (v in nutrient_vars) {
  dat[[v]] <- as.numeric(dat[[v]])
}

# Nutrient totals should be non-negative. Treat impossible negative values
# as missing; do not replace genuine zero intake values.
for (v in nutrient_vars) {
  dat[[v]][!is.na(dat[[v]]) & dat[[v]] < 0] <- NA
}

# Save a basic sample description.
sample_summary <- data.frame(
  N = nrow(dat),
  Mean_age = mean(dat$RIDAGEYR, na.rm = TRUE),
  SD_age = sd(dat$RIDAGEYR, na.rm = TRUE),
  Age_min = min(dat$RIDAGEYR, na.rm = TRUE),
  Age_max = max(dat$RIDAGEYR, na.rm = TRUE)
)
write.csv(sample_summary, file.path(output_dir, "sample_summary.csv"),
          row.names = FALSE)

# Keep complete cases for all selected nutrient variables so that all
# clustering methods use the same participants.
complete_rows <- complete.cases(dat[, nutrient_vars])
analysis_dat <- dat[complete_rows, ]
if (nrow(analysis_dat) < 30) {
  stop("完整資料筆數過少，請檢查缺失值及變數。")
}

# 5. Construct clustering features ---------------------------
# Main analysis: nutrient density per 1,000 kcal, plus total energy.
# This reduces the tendency to cluster only by how much a person ate.
# Energy remains in the feature set to retain information on total intake.
energy <- analysis_dat$DR1TKCAL
if (any(energy <= 0)) {
  keep <- energy > 0
  analysis_dat <- analysis_dat[keep, ]
  energy <- analysis_dat$DR1TKCAL
}

density_vars <- setdiff(nutrient_vars, "DR1TKCAL")
x_density <- as.data.frame(
  lapply(density_vars, function(v) analysis_dat[[v]] / energy * 1000)
)
names(x_density) <- density_vars

# Include total energy (log scale) with nutrient density measures.
x_main <- cbind(
  Energy_kcal = log1p(analysis_dat$DR1TKCAL),
  as.data.frame(lapply(x_density, log1p))
)

# log1p reduces right-skewness; z-scores put variables on comparable scales.
x_main <- as.data.frame(scale(x_main))
x_main <- as.matrix(x_main)

# Remove any zero-variance columns, if present.
nonzero_sd <- apply(x_main, 2, function(z) sd(z, na.rm = TRUE) > 0)
x_main <- x_main[, nonzero_sd, drop = FALSE]

# 6. Determine a reasonable number of clusters ----------------
set.seed(2026)
k_candidates <- 2:8
n <- nrow(x_main)

# Elbow method: within-cluster sum of squares.
wss <- sapply(k_candidates, function(k) {
  kmeans(x_main, centers = k, nstart = 30, iter.max = 100)$tot.withinss
})
png(file.path(output_dir, "01_elbow_method.png"),
    width = 1000, height = 700, res = 120)
plot(k_candidates, wss, type = "b", pch = 19,
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method")
dev.off()

# Average silhouette width for k-means solutions.
sil_width <- sapply(k_candidates, function(k) {
  km <- kmeans(x_main, centers = k, nstart = 30, iter.max = 100)
  sil <- silhouette(km$cluster, dist(x_main))
  mean(sil[, "sil_width"])
})
silhouette_results <- data.frame(k = k_candidates,
                                 average_silhouette = sil_width)
write.csv(silhouette_results,
          file.path(output_dir, "silhouette_by_k.csv"),
          row.names = FALSE)

png(file.path(output_dir, "02_silhouette_by_k.png"),
    width = 1000, height = 700, res = 120)
plot(k_candidates, sil_width, type = "b", pch = 19,
     xlab = "Number of clusters (k)",
     ylab = "Average silhouette width",
     main = "Silhouette Statistic")
dev.off()

# Gap statistic. B=50 balances computation time and stability.
# cluster::clusGap uses a reference distribution to compare observed
# clustering dispersion with that expected under a no-cluster reference.
set.seed(2026)
gap <- clusGap(
  x_main,
  FUN = function(x, k) {
    list(cluster = kmeans(x, centers = k, nstart = 20,
                          iter.max = 100)$cluster)
  },
  K.max = 8,
  B = 50
)
png(file.path(output_dir, "03_gap_statistic.png"),
    width = 1000, height = 700, res = 120)
plot(gap, main = "Gap Statistic")
dev.off()

gap_table <- as.data.frame(gap$Tab)
gap_table$k <- as.integer(rownames(gap_table))
write.csv(gap_table, file.path(output_dir, "gap_statistic_values.csv"),
          row.names = FALSE)

# Recommended k from the silhouette maximum; compare with elbow and gap plots.
selected_k <- k_candidates[which.max(sil_width)]
writeLines(
  paste0("Selected k by maximum average silhouette = ", selected_k),
  file.path(output_dir, "selected_k.txt")
)

# 7. K-means clustering --------------------------------------
set.seed(2026)
km_final <- kmeans(x_main, centers = selected_k, nstart = 100,
                   iter.max = 200)
analysis_dat$cluster <- factor(km_final$cluster)

cluster_sizes <- as.data.frame(table(analysis_dat$cluster))
names(cluster_sizes) <- c("Cluster", "N")
cluster_sizes$Percent <- round(cluster_sizes$N / sum(cluster_sizes$N) * 100, 1)
write.csv(cluster_sizes, file.path(output_dir, "cluster_sizes.csv"),
          row.names = FALSE)

# 8. Hierarchical clustering ---------------------------------
# Complete linkage with Euclidean distance, as suggested in the assignment.
# A random subset is used if the sample is large, to keep the dendrogram readable.
set.seed(2026)
hc_n <- min(nrow(x_main), 500)
hc_index <- sample(seq_len(nrow(x_main)), hc_n)
hc_data <- x_main[hc_index, , drop = FALSE]
hc_dist <- dist(hc_data, method = "euclidean")
hc <- hclust(hc_dist, method = "complete")

png(file.path(output_dir, "04_hierarchical_dendrogram.png"),
    width = 1400, height = 900, res = 120)
plot(hc, labels = FALSE, hang = -1,
     main = "Hierarchical Clustering Dendrogram (Complete Linkage)",
     xlab = "Adolescent participants", ylab = "Euclidean distance")
rect.hclust(hc, k = selected_k, border = 2:(selected_k + 1))
dev.off()

# 9. Cluster profiles in original units ----------------------
# Present means and medians in the original nutrient units for interpretation.
profile_mean <- aggregate(
  analysis_dat[, nutrient_vars],
  by = list(Cluster = analysis_dat$cluster),
  FUN = function(z) mean(z, na.rm = TRUE)
)
profile_median <- aggregate(
  analysis_dat[, nutrient_vars],
  by = list(Cluster = analysis_dat$cluster),
  FUN = function(z) median(z, na.rm = TRUE)
)
names(profile_mean)[-1] <- unname(pretty_names[names(profile_mean)[-1]])
names(profile_median)[-1] <- unname(pretty_names[names(profile_median)[-1]])

write.csv(profile_mean, file.path(output_dir, "cluster_profile_means.csv"),
          row.names = FALSE)
write.csv(profile_median, file.path(output_dir, "cluster_profile_medians.csv"),
          row.names = FALSE)

# Standardized cluster centers: positive values are above the overall mean.
centers <- aggregate(
  as.data.frame(x_main),
  by = list(Cluster = analysis_dat$cluster),
  FUN = mean
)
write.csv(centers, file.path(output_dir, "cluster_standardized_centers.csv"),
          row.names = FALSE)

# Heatmap-like base R image of standardized cluster centers.
center_matrix <- as.matrix(centers[, -1, drop = FALSE])
rownames(center_matrix) <- paste("Cluster", centers$Cluster)
png(file.path(output_dir, "05_cluster_profile_heatmap.png"),
    width = 1500, height = 900, res = 130)
par(mar = c(10, 8, 4, 2))
image(x = seq_len(ncol(center_matrix)),
      y = seq_len(nrow(center_matrix)),
      z = t(center_matrix[nrow(center_matrix):1, , drop = FALSE]),
      axes = FALSE, xlab = "", ylab = "",
      main = "Cluster Profiles (Standardized Feature Means)")
axis(1, at = seq_len(ncol(center_matrix)),
     labels = colnames(center_matrix), las = 2, cex.axis = 0.7)
axis(2, at = seq_len(nrow(center_matrix)),
     labels = rev(rownames(center_matrix)), las = 1)
box()
dev.off()

# 10. Demographic summaries by cluster -----------------------
age_profile <- aggregate(
  analysis_dat$RIDAGEYR,
  by = list(Cluster = analysis_dat$cluster),
  FUN = mean
)
names(age_profile)[2] <- "Mean_age"
write.csv(age_profile, file.path(output_dir, "cluster_age_profile.csv"),
          row.names = FALSE)

# Save participant-level cluster assignments.
assignment <- data.frame(
  SEQN = analysis_dat$SEQN,
  Age = analysis_dat$RIDAGEYR,
  Cluster = analysis_dat$cluster
)
write.csv(assignment, file.path(output_dir, "participant_cluster_assignment.csv"),
          row.names = FALSE)

# 11. Sensitivity analysis: raw total nutrient amounts --------
# This checks whether the clustering changes when using total daily amounts
# rather than nutrient density. It is not the primary result because raw
# amounts may mostly reflect total food/energy intake.
x_raw <- as.data.frame(lapply(analysis_dat[, nutrient_vars], log1p))
x_raw <- scale(x_raw)
set.seed(2026)
km_raw <- kmeans(x_raw, centers = selected_k, nstart = 100,
                 iter.max = 200)
raw_assignment <- data.frame(
  SEQN = analysis_dat$SEQN,
  Cluster_density = analysis_dat$cluster,
  Cluster_raw_totals = factor(km_raw$cluster)
)
write.csv(raw_assignment,
          file.path(output_dir, "sensitivity_raw_total_clusters.csv"),
          row.names = FALSE)
write.csv(
  as.data.frame(table(raw_assignment$Cluster_density,
                      raw_assignment$Cluster_raw_totals)),
  file.path(output_dir, "sensitivity_cluster_cross_tab.csv"),
  row.names = FALSE
)

# 12. Save session information and final notes ----------------
capture.output(sessionInfo(),
               file = file.path(output_dir, "session_info.txt"))

cat(
  "\nAnalysis completed.\n",
  "Number of adolescents included: ", nrow(analysis_dat), "\n",
  "Selected number of clusters (silhouette): ", selected_k, "\n",
  "Output folder: ", output_dir, "\n",
  sep = ""
)

# IMPORTANT INTERPRETATION NOTES:
# 1. DR1TOT_L is a single 24-hour recall, not a measure of usual long-term diet.
# 2. Cluster labels are exploratory patterns, not clinical diagnoses.
# 3. The primary clustering uses nutrient density per 1,000 kcal plus total energy.
# 4. Day 1 dietary sample weights (WTDRD1) are not used in k-means/hclust here.
#    Therefore, do not describe these cluster percentages as nationally
#    representative population estimates.
# 5. The selected k is guided by silhouette; inspect elbow and gap plots too.
# 6. Name clusters only after reviewing cluster_profile_means.csv,
#    cluster_profile_medians.csv, and cluster_standardized_centers.csv.
