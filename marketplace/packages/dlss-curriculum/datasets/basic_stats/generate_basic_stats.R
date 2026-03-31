#!/usr/bin/env Rscript
# =============================================================================
# DfLSS Black Belt - Module M10: Basic Statistics Exercises
# Descriptive Statistics and Normality Testing
# =============================================================================
# This script accompanies the thermostat dimensions and hospital wait time
# datasets. It demonstrates core descriptive statistics and normality testing
# techniques used in Design for Lean Six Sigma.
#
# Prerequisites: R with base stats package (no additional packages required
# for core analysis; 'ggplot2' is optional for enhanced visualizations).
#
# Usage:
#   Rscript generate_basic_stats.R
#
# Author: DfLSS Curriculum Team
# =============================================================================

set.seed(42)  # Reproducibility -- ensures identical results on every run

# ---------------------------------------------------------------------------
# SECTION 1: Load Data
# ---------------------------------------------------------------------------
# Load both datasets from CSV files. These represent real-world measurement
# scenarios common in DfLSS projects:
#   - Thermostat dimensions: continuous manufacturing measurement (Product Development)
#   - Hospital wait times: process cycle time (New Process Design)

cat("=== DfLSS Module M10: Basic Statistics Analysis ===\n\n")

thermo <- read.csv("thermostat_dimensions.csv", stringsAsFactors = FALSE)
wait   <- read.csv("hospital_wait_times.csv", stringsAsFactors = FALSE)

cat(sprintf("Thermostat dataset: %d observations, %d columns\n",
            nrow(thermo), ncol(thermo)))
cat(sprintf("Hospital wait dataset: %d observations, %d columns\n\n",
            nrow(wait), ncol(wait)))

# ---------------------------------------------------------------------------
# SECTION 2: Descriptive Statistics Function
# ---------------------------------------------------------------------------
# Define a reusable function that calculates all key descriptive statistics.
# This is the foundation of any DfLSS data analysis -- you MUST understand
# what each measure tells you about your process.

calculate_descriptives <- function(data, value_col, dataset_name) {

  x <- data[[value_col]]
  n <- length(x)

  cat(sprintf("--- %s (n = %d) ---\n", dataset_name, n))

  # Central tendency measures
  # Mean: arithmetic average; sensitive to outliers
  mean_val <- mean(x)
  cat(sprintf("  Mean:            %.4f\n", mean_val))

  # Median: middle value; robust to outliers
  median_val <- median(x)
  cat(sprintf("  Median:          %.4f\n", median_val))

  # Mode: most frequent value (requires custom function since R has no built-in)
  get_mode <- function(v) {
    tab <- table(v)
    as.numeric(names(tab)[which.max(tab)])
  }
  mode_val <- get_mode(x)
  cat(sprintf("  Mode:            %.4f\n", mode_val))

  # Dispersion measures
  # Sample standard deviation: average distance from the mean (s)
  sd_val <- sd(x)
  cat(sprintf("  Std Dev (s):     %.4f\n", sd_val))

  # Sample variance: squared standard deviation (s^2)
  var_val <- var(x)
  cat(sprintf("  Variance (s^2):  %.4f\n", var_val))

  # Range: max - min; simplest dispersion measure
  range_val <- max(x) - min(x)
  cat(sprintf("  Range:           %.4f\n", range_val))

  # Interquartile Range (IQR): spread of the middle 50% of data
  # Q1 (25th percentile) to Q3 (75th percentile)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr_val <- IQR(x)
  cat(sprintf("  Q1 (25th %%):     %.4f\n", q1))
  cat(sprintf("  Q3 (75th %%):     %.4f\n", q3))
  cat(sprintf("  IQR:             %.4f\n", iqr_val))

  # Additional percentiles useful for process capability analysis
  cat(sprintf("  Min:             %.4f\n", min(x)))
  cat(sprintf("  Max:             %.4f\n", max(x)))
  cat(sprintf("  5th percentile:  %.4f\n", quantile(x, 0.05)))
  cat(sprintf("  95th percentile: %.4f\n", quantile(x, 0.95)))

  # Skewness: measures asymmetry of the distribution
  #   < 0 = left-skewed, > 0 = right-skewed, ~0 = symmetric
  skew <- sum((x - mean_val)^3) / (n * sd_val^3)
  cat(sprintf("  Skewness:        %.4f\n", skew))

  # Kurtosis: measures "tailedness" (heaviness of tails)
  #   > 0 = heavy tails (leptokurtic), < 0 = light tails (platykurtic)
  kurt <- sum((x - mean_val)^4) / (n * sd_val^4) - 3
  cat(sprintf("  Excess Kurtosis: %.4f\n\n", kurt))

  # Return as list for downstream use
  list(mean = mean_val, median = median_val, mode = mode_val,
       sd = sd_val, var = var_val, range = range_val,
       q1 = q1, q3 = q3, iqr = iqr_val, n = n, data = x,
       skew = skew, kurt = kurt)
}

# ---------------------------------------------------------------------------
# SECTION 3: Calculate Descriptive Statistics for Both Datasets
# ---------------------------------------------------------------------------
cat("========================================\n")
cat("  DESCRIPTIVE STATISTICS\n")
cat("========================================\n\n")

thermo_stats <- calculate_descriptives(thermo, "measurement_mm",
                                       "Thermostat Dimensions (mm)")
wait_stats   <- calculate_descriptives(wait, "wait_min",
                                       "Hospital Wait Times (min)")

# ---------------------------------------------------------------------------
# SECTION 4: Normality Testing
# ---------------------------------------------------------------------------
# The Shapiro-Wilk test is the gold standard for normality testing when
# n < 5000. It tests:
#   H0: The data are normally distributed
#   H1: The data are NOT normally distributed
#
# Interpretation:
#   p-value > 0.05: Fail to reject H0 (data may be normal)
#   p-value <= 0.05: Reject H0 (data are likely NOT normal)
#
# In DfLSS, normality matters because many statistical tools (t-tests,
# control charts, process capability indices) ASSUME normality.

cat("========================================\n")
cat("  NORMALITY TESTING (Shapiro-Wilk)\n")
cat("========================================\n\n")

# Shapiro-Wilk test for thermostat dimensions
sw_thermo <- shapiro.test(thermo_stats$data)
cat(sprintf("Thermostat Dimensions:\n"))
cat(sprintf("  W-statistic: %.6f\n", sw_thermo$statistic))
cat(sprintf("  p-value:     %.6f\n", sw_thermo$p.value))
cat(sprintf("  Conclusion:  %s\n\n",
            ifelse(sw_thermo$p.value > 0.05,
                   "Fail to reject H0 -- data consistent with normality",
                   "Reject H0 -- data likely NOT normal")))

# Shapiro-Wilk test for hospital wait times
sw_wait <- shapiro.test(wait_stats$data)
cat(sprintf("Hospital Wait Times:\n"))
cat(sprintf("  W-statistic: %.6f\n", sw_wait$statistic))
cat(sprintf("  p-value:     %.6f\n", sw_wait$p.value))
cat(sprintf("  Conclusion:  %s\n\n",
            ifelse(sw_wait$p.value > 0.05,
                   "Fail to reject H0 -- data consistent with normality",
                   "Reject H0 -- data likely NOT normal")))

# ---------------------------------------------------------------------------
# SECTION 5: Outlier Detection Using IQR Method
# ---------------------------------------------------------------------------
# The IQR method identifies outliers as values beyond:
#   Lower fence = Q1 - 1.5 * IQR
#   Upper fence = Q3 + 1.5 * IQR
#
# This is a robust method (not affected by the outliers themselves).
# In DfLSS, outlier identification is critical for Measurement System
# Analysis (MSA) and process capability studies.

cat("========================================\n")
cat("  OUTLIER DETECTION (IQR Method)\n")
cat("========================================\n\n")

detect_outliers_iqr <- function(data, value_col, dataset_name) {
  x <- data[[value_col]]
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr

  outliers <- x[x < lower_fence | x > upper_fence]
  outlier_indices <- which(x < lower_fence | x > upper_fence)

  cat(sprintf("%s:\n", dataset_name))
  cat(sprintf("  Q1:           %.4f\n", q1))
  cat(sprintf("  Q3:           %.4f\n", q3))
  cat(sprintf("  IQR:          %.4f\n", iqr))
  cat(sprintf("  Lower fence:  %.4f\n", lower_fence))
  cat(sprintf("  Upper fence:  %.4f\n", upper_fence))

  if (length(outliers) == 0) {
    cat("  Outliers:     None detected\n\n")
  } else {
    cat(sprintf("  Outliers (%d): %s\n\n", length(outliers),
                paste(sprintf("%.2f", outliers), collapse = ", ")))
  }

  list(lower = lower_fence, upper = upper_fence,
       outliers = outliers, indices = outlier_indices)
}

detect_outliers_iqr(thermo, "measurement_mm", "Thermostat Dimensions")
detect_outliers_iqr(wait, "wait_min", "Hospital Wait Times")

# ---------------------------------------------------------------------------
# SECTION 6: Histograms with Normal Curve Overlay
# ---------------------------------------------------------------------------
# Visual inspection is a critical complement to formal normality tests.
# Overlay the theoretical normal distribution curve (using the sample mean
# and SD) to visually assess how well the data fit a normal distribution.

cat("========================================\n")
cat("  GENERATING HISTOGRAMS\n")
cat("========================================\n\n")

# --- Thermostat Dimensions Histogram ---
# Histogram: shows frequency distribution of continuous data
# Breaks: controls number of bins; Sturges' formula is default
hist(thermo_stats$data,
     breaks = "Sturges",
     main = "Thermostat Housing Dimensions (mm)\nHistogram with Normal Curve Overlay",
     xlab = "Measurement (mm)",
     ylab = "Frequency",
     col  = "steelblue",
     border = "white",
     freq = FALSE)  # density scale for curve overlay

# Overlay theoretical normal curve
curve(dnorm(x, mean = thermo_stats$mean, sd = thermo_stats$sd),
      add = TRUE, col = "red", lwd = 2)

# Add vertical lines for mean and +/- 3 sigma
abline(v = thermo_stats$mean, col = "darkgreen", lwd = 2, lty = 2)
abline(v = thermo_stats$mean + 3 * thermo_stats$sd, col = "orange", lwd = 1, lty = 3)
abline(v = thermo_stats$mean - 3 * thermo_stats$sd, col = "orange", lwd = 1, lty = 3)

legend("topright",
       legend = c("Normal curve", "Mean", "+/- 3 sigma"),
       col = c("red", "darkgreen", "orange"),
       lty = c(1, 2, 3),
       lwd = c(2, 2, 1),
       cex = 0.8)

# Save thermostat histogram
dev.copy(png, "thermostat_histogram.png", width = 800, height = 600, res = 120)
dev.off()
cat("Saved: thermostat_histogram.png\n")

# --- Hospital Wait Times Histogram ---
hist(wait_stats$data,
     breaks = "Sturges",
     main = "ED Triage Wait Times (min)\nHistogram with Normal Curve Overlay",
     xlab = "Wait Time (minutes)",
     ylab = "Frequency",
     col  = "coral",
     border = "white",
     freq = FALSE)

# Overlay theoretical normal curve
curve(dnorm(x, mean = wait_stats$mean, sd = wait_stats$sd),
      add = TRUE, col = "blue", lwd = 2)

# Add vertical lines for mean and +/- 3 sigma
abline(v = wait_stats$mean, col = "darkgreen", lwd = 2, lty = 2)
abline(v = wait_stats$mean + 3 * wait_stats$sd, col = "orange", lwd = 1, lty = 3)
abline(v = wait_stats$mean - 3 * wait_stats$sd, col = "orange", lwd = 1, lty = 3)

legend("topright",
       legend = c("Normal curve", "Mean", "+/- 3 sigma"),
       col = c("blue", "darkgreen", "orange"),
       lty = c(1, 2, 3),
       lwd = c(2, 2, 1),
       cex = 0.8)

# Save wait time histogram
dev.copy(png, "hospital_wait_histogram.png", width = 800, height = 600, res = 120)
dev.off()
cat("Saved: hospital_wait_histogram.png\n\n")

# ---------------------------------------------------------------------------
# SECTION 7: Q-Q Plot for Normality Assessment
# ---------------------------------------------------------------------------
# Q-Q (Quantile-Quantile) plots compare sample quantiles against theoretical
# normal quantiles. If data are normal, points fall approximately on the
# diagonal reference line. Deviations from the line indicate departures
# from normality (especially in the tails).

cat("========================================\n")
cat("  GENERATING Q-Q PLOTS\n")
cat("========================================\n\n")

# Thermostat Q-Q Plot
qqnorm(thermo_stats$data,
       main = "Q-Q Plot: Thermostat Dimensions",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19, col = "steelblue")
qqline(thermo_stats$data, col = "red", lwd = 2)
dev.copy(png, "thermostat_qq_plot.png", width = 800, height = 600, res = 120)
dev.off()
cat("Saved: thermostat_qq_plot.png\n")

# Hospital Wait Times Q-Q Plot
qqnorm(wait_stats$data,
       main = "Q-Q Plot: Hospital Wait Times",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19, col = "coral")
qqline(wait_stats$data, col = "blue", lwd = 2)
dev.copy(png, "hospital_wait_qq_plot.png", width = 800, height = 600, res = 120)
dev.off()
cat("Saved: hospital_wait_qq_plot.png\n\n")

# ---------------------------------------------------------------------------
# SECTION 8: Summary Table
# ---------------------------------------------------------------------------
# Print a side-by-side comparison table for easy reference.
# This format is useful for DfLSS project documentation and tollgate reviews.

cat("========================================\n")
cat("  COMPARATIVE SUMMARY TABLE\n")
cat("========================================\n\n")

comparison <- data.frame(
  Statistic = c("n", "Mean", "Median", "Mode", "Std Dev", "Variance",
                "Range", "IQR", "Skewness", "Kurtosis",
                "Shapiro-Wilk W", "Shapiro-Wilk p"),
  Thermostat = c(
    thermo_stats$n,
    sprintf("%.4f", thermo_stats$mean),
    sprintf("%.4f", thermo_stats$median),
    sprintf("%.4f", thermo_stats$mode),
    sprintf("%.4f", thermo_stats$sd),
    sprintf("%.4f", thermo_stats$var),
    sprintf("%.4f", thermo_stats$range),
    sprintf("%.4f", thermo_stats$iqr),
    sprintf("%.4f", thermo_stats$skew),
    sprintf("%.4f", thermo_stats$kurt),
    sprintf("%.4f", sw_thermo$statistic),
    sprintf("%.6f", sw_thermo$p.value)
  ),
  Hospital_Wait = c(
    wait_stats$n,
    sprintf("%.4f", wait_stats$mean),
    sprintf("%.4f", wait_stats$median),
    sprintf("%.4f", wait_stats$mode),
    sprintf("%.4f", wait_stats$sd),
    sprintf("%.4f", wait_stats$var),
    sprintf("%.4f", wait_stats$range),
    sprintf("%.4f", wait_stats$iqr),
    sprintf("%.4f", wait_stats$skew),
    sprintf("%.4f", wait_stats$kurt),
    sprintf("%.4f", sw_wait$statistic),
    sprintf("%.6f", sw_wait$p.value)
  )
)

print(comparison, row.names = FALSE)

cat("\n========================================\n")
cat("  ANALYSIS COMPLETE\n")
cat("========================================\n")
cat("Output files generated:\n")
cat("  - thermostat_histogram.png\n")
cat("  - hospital_wait_histogram.png\n")
cat("  - thermostat_qq_plot.png\n")
cat("  - hospital_wait_qq_plot.png\n")
cat("\nKey takeaways for students:\n")
cat("  1. The thermostat data is approximately normal (as expected for\n")
cat("     manufacturing measurements) -- central limit theorem at work.\n")
cat("  2. The hospital wait time data has outliers and may show departure\n")
cat("     from normality -- common in service/healthcare processes.\n")
cat("  3. Always use BOTH statistical tests AND visual methods for\n")
cat("     normality assessment -- neither alone is sufficient.\n")
cat("  4. Outlier detection using the IQR method is robust and preferred\n")
cat("     over z-score methods when normality is in question.\n")
cat("========================================\n")
