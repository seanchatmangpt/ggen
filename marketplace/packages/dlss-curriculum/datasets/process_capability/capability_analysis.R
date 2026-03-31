#!/usr/bin/env Rscript
# =============================================================================
# DfLSS Black Belt - Module 17: Process Capability Analysis
# =============================================================================
# This script performs comprehensive process capability analysis on two
# datasets: battery capacity (bilateral spec) and loan processing time
# (one-sided spec).
#
# Key Learning Objectives:
#   1. Calculate and interpret Cp, Cpk, Cpl, Cpu, Pp, Ppk
#   2. Understand the difference between capability (Cp/Cpk) and
#      performance (Pp/Ppk) indices
#   3. Perform normality testing (Anderson-Darling)
#   4. Create and interpret capability six-pack plots
#   5. Identify when a process is NOT capable and recommend improvements
#
# Required packages: tidyverse, ggplot2, qcc, nortest
# Install with: install.packages(c("tidyverse", "ggplot2", "qcc", "nortest"))
# =============================================================================

set.seed(42)

# --- Install missing packages if needed ---
required_packages <- c("ggplot2", "qcc", "nortest")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(ggplot2)
library(qcc)
library(nortest)

# =============================================================================
# DATA LOADING
# =============================================================================

battery_data <- read.csv("battery_capacity.csv", stringsAsFactors = FALSE)
loan_data    <- read.csv("loan_processing_time.csv", stringsAsFactors = FALSE)

cat("=== Data Loaded ===\n")
cat("Battery capacity: n =", nrow(battery_data), "samples\n")
cat("Loan processing:   n =", nrow(loan_data), "samples\n\n")

# =============================================================================
# CAPABILITY INDEX CALCULATION FUNCTION
# =============================================================================
# This function calculates ALL capability indices for a given dataset.
#
# STUDENT NOTE - Understanding Each Index:
#
# Cp  (Process Capability):
#   Cp = (USL - LSL) / (6 * sigma_within)
#   Measures the POTENTIAL capability if the process were perfectly centered.
#   Cp does NOT consider process centering.
#   Cp >= 1.33 is generally considered capable.
#
# Cpk (Process Capability Index):
#   Cpk = min(Cpl, Cpu)
#   Measures ACTUAL capability accounting for process centering.
#   Cpk = Cp only when the process is perfectly centered at the target.
#   Cpk < Cp always indicates the process is off-center.
#
# Cpl (Lower Capability):
#   Cpl = (mean - LSL) / (3 * sigma_within)
#   How well the process fits between the mean and the LOWER spec limit.
#
# Cpu (Upper Capability):
#   Cpu = (USL - mean) / (3 * sigma_within)
#   How well the process fits between the UPPER spec limit and the mean.
#
# Pp  (Process Performance):
#   Pp = (USL - LSL) / (6 * sigma_overall)
#   Like Cp but uses the OVERALL (total) standard deviation.
#   Captures long-term variation including shifts and drifts.
#
# Ppk (Process Performance Index):
#   Ppk = min(Ppl, Ppu)
#   Like Cpk but uses overall sigma. Measures long-term performance.
#
# KEY RELATIONSHIPS:
#   - If Cpk < Cp: process is not centered (shifted from target)
#   - If Ppk < Cpk: process has more variation over time (not stable)
#   - If Pp < Cp: between-subgroup variation is larger than within
# =============================================================================

calculate_capability_indices <- function(data, value_col, USL, LSL = NA,
                                          subgroup_size = 1) {
  x <- data[[value_col]]
  x <- x[!is.na(x)]
  n <- length(x)

  # Sample statistics
  x_bar <- mean(x)

  # Within-subgroup standard deviation (using average moving range for I-MR)
  # For subgrouped data, use pooled within-subgroup std dev
  if (subgroup_size > 1) {
    # Pooled within-subgroup standard deviation
    groups <- cut(seq_along(x),
                  breaks = ceiling(n / subgroup_size),
                  labels = FALSE)
    pooled_var <- tapply(x, groups, var, na.rm = TRUE)
    sigma_within <- sqrt(mean(pooled_var, na.rm = TRUE))
  } else {
    # Individual data: use average moving range method (d2 = 1.128 for n=2)
    mr <- abs(diff(x))
    sigma_within <- mean(mr, na.rm = TRUE) / 1.128
  }

  # Overall standard deviation (sample standard deviation)
  sigma_overall <- sd(x)

  # Short-term capability indices (Cp, Cpk)
  if (!is.na(LSL) && !is.na(USL)) {
    # Bilateral specification
    Cp <- (USL - LSL) / (6 * sigma_within)
    Cpl <- (x_bar - LSL) / (3 * sigma_within)
    Cpu <- (USL - x_bar) / (3 * sigma_within)
    Cpk <- min(Cpl, Cpu)
  } else if (!is.na(USL)) {
    # Upper spec only (no LSL)
    Cp <- (USL - x_bar) / (3 * sigma_within)
    Cpl <- NA
    Cpu <- (USL - x_bar) / (3 * sigma_within)
    Cpk <- Cpu
  } else if (!is.na(LSL)) {
    # Lower spec only (no USL)
    Cp <- (x_bar - LSL) / (3 * sigma_within)
    Cpl <- (x_bar - LSL) / (3 * sigma_within)
    Cpu <- NA
    Cpk <- Cpl
  } else {
    stop("At least one specification limit (USL or LSL) must be provided.")
  }

  # Long-term performance indices (Pp, Ppk)
  if (!is.na(LSL) && !is.na(USL)) {
    Pp <- (USL - LSL) / (6 * sigma_overall)
    Ppl <- (x_bar - LSL) / (3 * sigma_overall)
    Ppu <- (USL - x_bar) / (3 * sigma_overall)
    Ppk <- min(Ppl, Ppu)
  } else if (!is.na(USL)) {
    Pp <- (USL - x_bar) / (3 * sigma_overall)
    Ppl <- NA
    Ppu <- (USL - x_bar) / (3 * sigma_overall)
    Ppk <- Ppu
  } else {
    Pp <- (x_bar - LSL) / (3 * sigma_overall)
    Ppl <- (x_bar - LSL) / (3 * sigma_overall)
    Ppu <- NA
    Ppk <- Ppl
  }

  # Expected defect rates (ppm - parts per million)
  if (!is.na(LSL) && !is.na(USL)) {
    ppm_below <- pnorm(LSL, mean = x_bar, sd = sigma_overall) * 1e6
    ppm_above <- (1 - pnorm(USL, mean = x_bar, sd = sigma_overall)) * 1e6
  } else if (!is.na(USL)) {
    ppm_below <- 0
    ppm_above <- (1 - pnorm(USL, mean = x_bar, sd = sigma_overall)) * 1e6
  } else {
    ppm_below <- pnorm(LSL, mean = x_bar, sd = sigma_overall) * 1e6
    ppm_above <- 0
  }
  ppm_total <- ppm_below + ppm_above

  # Percentage within spec
  pct_within <- (1 - ppm_total / 1e6) * 100

  results <- data.frame(
    Statistic = c("n", "Mean", "Sigma_Within", "Sigma_Overall",
                  "Cp", "Cpk", "Cpl", "Cpu",
                  "Pp", "Ppk", "Ppl", "Ppu",
                  "PPM_Below", "PPM_Above", "PPM_Total", "Pct_Within_Spec"),
    Value = c(n, round(x_bar, 4), round(sigma_within, 4), round(sigma_overall, 4),
              round(Cp, 4), round(Cpk, 4), round(Cpl, 4), round(Cpu, 4),
              round(Pp, 4), round(Ppk, 4), round(Ppl, 4), round(Ppu, 4),
              round(ppm_below, 1), round(ppm_above, 1), round(ppm_total, 1),
              round(pct_within, 2))
  )

  attr(results, "x_bar") <- x_bar
  attr(results, "sigma_within") <- sigma_within
  attr(results, "sigma_overall") <- sigma_overall

  return(results)
}

# =============================================================================
# NORMALITY TEST FUNCTION
# =============================================================================
# The Anderson-Darling test checks if data follows a normal distribution.
#
# STUDENT NOTE:
#   H0: Data is normally distributed
#   H1: Data is NOT normally distributed
#
#   If p-value < 0.05: reject H0 (data is NOT normal)
#   If p-value >= 0.05: fail to reject H0 (data may be normal)
#
# IMPORTANT: Capability indices (Cp, Cpk) assume normality.
# If data is not normal, consider:
#   1. Transforming the data (Box-Cox, Johnson)
#   2. Using non-parametric capability indices
#   3. Fitting an alternative distribution (Weibull, Lognormal)
# =============================================================================

test_normality <- function(x, var_name = "Variable") {
  x <- x[!is.na(x)]
  ad_result <- ad.test(x)

  cat(sprintf("  Normality Test (Anderson-Darling) for %s:\n", var_name))
  cat(sprintf("    A-statistic: %.4f\n", ad_result$statistic))
  cat(sprintf("    p-value:     %.4f\n", ad_result$p.value))

  if (ad_result$p.value >= 0.05) {
    cat("    Conclusion:  FAIL TO REJECT H0 - Data appears normally distributed\n")
  } else {
    cat("    Conclusion:  REJECT H0 - Data is NOT normally distributed\n")
    cat("    Action:      Consider Box-Cox transformation or non-parametric indices\n")
  }
  cat("\n")

  return(ad_result)
}

# =============================================================================
# HISTOGRAM WITH SPEC LIMITS
# =============================================================================

plot_histogram_with_specs <- function(data, value_col, USL, LSL = NA,
                                       target = NA, title = "Process Histogram") {
  x <- data[[value_col]]
  x_bar <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)

  p <- ggplot(data, aes(x = !!sym(value_col))) +
    geom_histogram(aes(y = after_stat(density)), bins = 25,
                   fill = "steelblue", color = "white", alpha = 0.8) +
    stat_function(fun = dnorm, args = list(mean = x_bar, sd = s),
                  color = "red", linewidth = 1, linetype = "solid") +
    geom_vline(xintercept = x_bar, color = "red", linewidth = 0.8,
               linetype = "dashed") +
    labs(
      title = title,
      subtitle = sprintf("Mean = %.1f | SD = %.1f | n = %d",
                         x_bar, s, sum(!is.na(x))),
      x = value_col, y = "Density"
    )

  # Add spec limit lines
  if (!is.na(USL)) {
    p <- p + geom_vline(xintercept = USL, color = "red", linewidth = 1.2,
                        linetype = "solid") +
      annotate("text", x = USL, y = Inf, label = sprintf("USL = %g", USL),
               vjust = 2, hjust = -0.1, color = "red", fontface = "bold")
  }
  if (!is.na(LSL)) {
    p <- p + geom_vline(xintercept = LSL, color = "red", linewidth = 1.2,
                        linetype = "solid") +
      annotate("text", x = LSL, y = Inf, label = sprintf("LSL = %g", LSL),
               vjust = 2, hjust = 1.1, color = "red", fontface = "bold")
  }
  if (!is.na(target)) {
    p <- p + geom_vline(xintercept = target, color = "darkgreen", linewidth = 0.8,
                        linetype = "dotted") +
      annotate("text", x = target, y = Inf, label = sprintf("Target = %g", target),
               vjust = 4, hjust = 0.5, color = "darkgreen")
  }

  p <- p + theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))

  return(p)
}

# =============================================================================
# CAPABILITY SIX-PACK PLOT
# =============================================================================
# A capability six-pack contains 6 subplots:
#   1. Individual value plot (I-chart)
#   2. Moving range chart (MR-chart)
#   3. Histogram with spec limits
#   4. Normal probability plot
#   5. Capability indices summary
#   6. Process data summary
#
# STUDENT NOTE: The six-pack gives a complete picture of process behavior.
# - I-chart: Detect shifts in process level (look for runs, trends, outliers)
# - MR-chart: Detect changes in process variation
# - Histogram: Visual check of distribution shape and centering
# - Probability plot: Formal check of normality (points near line = normal)
# - Indices: Quantitative capability assessment
# =============================================================================

plot_capability_sixpack <- function(data, value_col, USL, LSL = NA,
                                      target = NA, title = "Capability Six-Pack") {

  x <- data[[value_col]]
  x <- x[!is.na(x)]
  n <- length(x)
  x_bar <- mean(x)
  s <- sd(x)

  # Create individual and moving range data
  df <- data.frame(obs = 1:n, value = x)
  df$mr <- c(NA, abs(diff(x)))

  # --- 1. Individuals Control Chart (I-chart) ---
  mr_bar <- mean(df$mr, na.rm = TRUE)
  sigma_est <- mr_bar / 1.128
  ucl_i <- x_bar + 3 * sigma_est
  lcl_i <- x_bar - 3 * sigma_est

  p_ichart <- ggplot(df, aes(x = obs, y = value)) +
    geom_point(color = "steelblue", size = 1.5) +
    geom_line(color = "steelblue", alpha = 0.5) +
    geom_hline(yintercept = x_bar, color = "green", linewidth = 0.8) +
    geom_hline(yintercept = ucl_i, color = "red", linewidth = 0.8, linetype = "dashed") +
    geom_hline(yintercept = lcl_i, color = "red", linewidth = 0.8, linetype = "dashed") +
    labs(title = "Individuals Chart (I-Chart)", x = "Observation", y = value_col) +
    theme_minimal() +
    annotate("text", x = n, y = ucl_i, label = "UCL", vjust = -0.5, hjust = 1.1,
             color = "red", size = 3)

  # --- 2. Moving Range Chart (MR-chart) ---
  ucl_mr <- mr_bar * 3.267
  lcl_mr <- 0

  p_mrchart <- ggplot(df, aes(x = obs, y = mr)) +
    geom_point(color = "steelblue", size = 1.5) +
    geom_line(color = "steelblue", alpha = 0.5) +
    geom_hline(yintercept = mr_bar, color = "green", linewidth = 0.8) +
    geom_hline(yintercept = ucl_mr, color = "red", linewidth = 0.8, linetype = "dashed") +
    geom_hline(yintercept = lcl_mr, color = "red", linewidth = 0.8, linetype = "dashed") +
    labs(title = "Moving Range Chart (MR-Chart)", x = "Observation", y = "Moving Range") +
    theme_minimal()

  # --- 3. Histogram ---
  p_hist <- plot_histogram_with_specs(
    data, value_col, USL, LSL, target,
    title = paste0("Histogram with Normal Curve")
  )

  # --- 4. Normal Probability Plot ---
  # Using Q-Q plot approach
  sorted_x <- sort(x)
  theoretical <- qnorm(ppoints(n))
  qq_df <- data.frame(sample = sorted_x, theoretical = theoretical)

  p_qq <- ggplot(qq_df, aes(x = theoretical, y = sample)) +
    geom_point(color = "steelblue", size = 1.5) +
    geom_abline(intercept = x_bar, slope = s, color = "red", linewidth = 0.8) +
    labs(title = "Normal Probability Plot",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()

  # --- 5. Capability Indices Text ---
  cap_results <- calculate_capability_indices(
    data.frame(val = x), "val", USL, LSL
  )

  cap_text <- sprintf(
    "Capability Indices\n\n
     Cp  = %.3f\n
     Cpk = %.3f\n
     Cpl = %.3f\n
     Cpu = %.3f\n\n
     Pp  = %.3f\n
     Ppk = %.3f\n\n
     Mean = %.1f\n
     Sigma_within = %.2f\n
     Sigma_overall = %.2f",
    cap_results$Value[cap_results$Statistic == "Cp"],
    cap_results$Value[cap_results$Statistic == "Cpk"],
    cap_results$Value[cap_results$Statistic == "Cpl"],
    cap_results$Value[cap_results$Statistic == "Cpu"],
    cap_results$Value[cap_results$Statistic == "Pp"],
    cap_results$Value[cap_results$Statistic == "Ppk"],
    cap_results$Value[cap_results$Statistic == "Mean"],
    cap_results$Value[cap_results$Statistic == "Sigma_Within"],
    cap_results$Value[cap_results$Statistic == "Sigma_Overall"]
  )

  p_indices <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = cap_text,
             hjust = 0.5, vjust = 0.5, size = 3.5, family = "monospace") +
    labs(title = "Capability Indices") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  # --- 6. Summary Statistics ---
  summary_text <- sprintf(
    "Process Summary\n\n
     n = %d\n
     Mean = %.2f\n
     Median = %.2f\n
     Std Dev = %.2f\n
     Min = %.1f\n
     Max = %.1f\n\n
     USL = %s\n
     LSL = %s\n
     Target = %s",
    n, x_bar, median(x), s, min(x), max(x),
    ifelse(is.na(USL), "None", as.character(USL)),
    ifelse(is.na(LSL), "None", as.character(LSL)),
    ifelse(is.na(target), "None", as.character(target))
  )

  p_summary <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = summary_text,
             hjust = 0.5, vjust = 0.5, size = 3.5, family = "monospace") +
    labs(title = "Process Summary") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))

  # Combine all six plots
  combined <- gridExtra::grid.arrange(
    p_ichart, p_mrchart, p_hist,
    p_qq, p_indices, p_summary,
    ncol = 3,
    top = grid::textGrob(title, gp = grid::gpar(fontsize = 16, fontface = "bold"))
  )

  return(combined)
}

# =============================================================================
# ANALYSIS 1: BATTERY CAPACITY
# =============================================================================
# Context: Product Development - Lithium-ion battery cell capacity
# Spec limits: LSL = 3400 mAh, USL = 3600 mAh, Target = 3500 mAh
# This is a BILATERAL specification (both LSL and USL exist).
# =============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("ANALYSIS 1: Battery Capacity (Bilateral Specification)\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Specification limits
battery_USL <- 3600
battery_LSL <- 3400
battery_target <- 3500

# Calculate capability indices
cat("--- Capability Indices ---\n")
battery_indices <- calculate_capability_indices(
  battery_data, "capacity_mah",
  USL = battery_USL, LSL = battery_LSL
)
print(battery_indices, row.names = FALSE)
cat("\n")

# Interpretation
cat("--- Interpretation ---\n")
Cp_val  <- battery_indices$Value[battery_indices$Statistic == "Cp"]
Cpk_val <- battery_indices$Value[battery_indices$Statistic == "Cpk"]
Pp_val  <- battery_indices$Value[battery_indices$Statistic == "Pp"]
Ppk_val <- battery_indices$Value[battery_indices$Statistic == "Ppk"]

cat(sprintf("  Cp  = %.3f: Process spread uses %.1f%% of tolerance width\n",
            Cp_val, (1 / Cp_val) * 100))
cat(sprintf("  Cpk = %.3f: Actual capability (accounts for centering)\n", Cpk_val))

if (Cpk_val >= 1.33) {
  cat("  VERDICT: Process is CAPABLE (Cpk >= 1.33)\n")
} else if (Cpk_val >= 1.0) {
  cat("  VERDICT: Process is MARGINALLY capable (1.0 <= Cpk < 1.33)\n")
} else {
  cat("  VERDICT: Process is NOT CAPABLE (Cpk < 1.0) - IMPROVEMENT REQUIRED\n")
}

# Cp vs Cpk gap analysis
gap <- Cp_val - Cpk_val
if (gap > 0.05) {
  cat(sprintf("\n  WARNING: Cp - Cpk = %.3f indicates the process is OFF-CENTER.\n", gap))
  cat("  Action: Shift the process mean toward the target (3500 mAh).\n")
}
cat("\n")

# Normality test
cat("--- Normality Assessment ---\n")
test_normality(battery_data$capacity_mah, "Battery Capacity (mAh)")

# Create plots
cat("--- Generating Plots ---\n")

# Histogram
battery_hist <- plot_histogram_with_specs(
  battery_data, "capacity_mah",
  USL = battery_USL, LSL = battery_LSL, target = battery_target,
  title = "Battery Capacity Distribution"
)
ggsave("battery_capacity_histogram.png", battery_hist, width = 8, height = 5, dpi = 150)
cat("  Saved: battery_capacity_histogram.png\n")

# Capability six-pack
# Note: gridExtra is needed for the six-pack. Load if available.
if (requireNamespace("gridExtra", quietly = TRUE)) {
  library(gridExtra)
  plot_capability_sixpack(
    battery_data, "capacity_mah",
    USL = battery_USL, LSL = battery_LSL, target = battery_target,
    title = "Battery Capacity - Capability Six-Pack"
  )
  cat("  Capability six-pack displayed.\n")
} else {
  cat("  [Skip] Install gridExtra for six-pack plots: install.packages('gridExtra')\n")
}

cat("\n")

# =============================================================================
# ANALYSIS 2: LOAN PROCESSING TIME
# =============================================================================
# Context: New Process Design - Loan application processing time
# Spec limits: USL = 72 hours, No LSL, Target = 24 hours
# This is a ONE-SIDED (upper) specification - only the USL matters.
#
# STUDENT NOTE: For one-sided specs, Cp and Cpk are calculated differently:
#   - There is no LSL, so we only care about Cpu (distance to USL)
#   - Cp = Cpk = Cpu for one-sided upper specs
#   - The goal is to have the mean FAR below the USL
# =============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("ANALYSIS 2: Loan Processing Time (One-Sided Upper Specification)\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Specification limits
loan_USL <- 72
loan_target <- 24

# Calculate capability indices
cat("--- Capability Indices ---\n")
loan_indices <- calculate_capability_indices(
  loan_data, "processing_hours",
  USL = loan_USL, LSL = 0  # LSL = 0 (physical lower bound, not a spec)
)
print(loan_indices, row.names = FALSE)
cat("\n")

# Interpretation
cat("--- Interpretation ---\n")
loan_Cp  <- loan_indices$Value[loan_indices$Statistic == "Cp"]
loan_Cpk <- loan_indices$Value[loan_indices$Statistic == "Cpk"]
loan_Pp  <- loan_indices$Value[loan_indices$Statistic == "Pp"]
loan_Ppk <- loan_indices$Value[loan_indices$Statistic == "Ppk"]

cat(sprintf("  Cpk = %.3f: Upper capability index\n", loan_Cpk))
cat(sprintf("  Mean = %.1f hours (Target = %d hours)\n",
            loan_indices$Value[loan_indices$Statistic == "Mean"], loan_target))

if (loan_Cpk >= 1.33) {
  cat("  VERDICT: Process is CAPABLE (Cpk >= 1.33)\n")
} else if (loan_Cpk >= 1.0) {
  cat("  VERDICT: Process is MARGINALLY capable (1.0 <= Cpk < 1.33)\n")
  cat("  RISK: Some applications may exceed the 72-hour USL.\n")
} else {
  cat("  VERDICT: Process is NOT CAPABLE (Cpk < 1.0) - IMPROVEMENT REQUIRED\n")
}

cat(sprintf("\n  Distance from mean to USL: %.1f hours\n",
            loan_USL - loan_indices$Value[loan_indices$Statistic == "Mean"]))
cat("  Recommendation: Reduce processing time by simplifying workflow,\n")
cat("  improving document completeness, or increasing officer experience.\n")
cat("\n")

# Normality test
cat("--- Normality Assessment ---\n")
test_normality(loan_data$processing_hours, "Loan Processing Time (hours)")

# Stratified analysis by loan type
cat("--- Stratified Analysis by Loan Type ---\n")
strat_results <- aggregate(processing_hours ~ loan_type, data = loan_data,
                           FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
for (i in 1:nrow(strat_results)) {
  lt <- strat_results$loan_type[i]
  vals <- strat_results$processing_hours[[i]]
  cat(sprintf("  %-15s: Mean = %6.1f h, SD = %4.1f h, n = %d\n",
              lt, vals["mean"], vals["sd"], vals["n"]))
}
cat("\n")

# Create plots
cat("--- Generating Plots ---\n")

loan_hist <- plot_histogram_with_specs(
  loan_data, "processing_hours",
  USL = loan_USL, target = loan_target,
  title = "Loan Processing Time Distribution"
)
ggsave("loan_processing_time_histogram.png", loan_hist, width = 8, height = 5, dpi = 150)
cat("  Saved: loan_processing_time_histogram.png\n")

# Six-pack for loan data
if (requireNamespace("gridExtra", quietly = TRUE)) {
  plot_capability_sixpack(
    loan_data, "processing_hours",
    USL = loan_USL, target = loan_target,
    title = "Loan Processing Time - Capability Six-Pack"
  )
  cat("  Capability six-pack displayed.\n")
}

cat("\n")

# =============================================================================
# SUMMARY & EXERCISES
# =============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("SUMMARY: Process Capability Assessment\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

cat("+----------------------+----------------+----------------+\n")
cat("| Index                | Battery Cap.   | Loan Proc.     |\n")
cat("+----------------------+----------------+----------------+\n")
cat(sprintf("| Cp                   | %14.3f | %14.3f |\n", Cp_val, loan_Cp))
cat(sprintf("| Cpk                  | %14.3f | %14.3f |\n", Cpk_val, loan_Cpk))
cat(sprintf("| Pp                   | %14.3f | %14.3f |\n", Pp_val, loan_Pp))
cat(sprintf("| Ppk                  | %14.3f | %14.3f |\n", Ppk_val, loan_Ppk))
cat("+----------------------+----------------+----------------+\n\n")

cat("EXERCISES FOR STUDENTS:\n\n")
cat("1. Battery Capacity (Bilateral Spec):\n")
cat("   a) The process is NOT capable (Cpk < 1.0). What SPECIFIC changes\n")
cat("      would improve Cpk to 1.33? Consider both centering and variation.\n")
cat("   b) How much would you need to reduce sigma to achieve Cpk = 1.33\n")
cat("      without shifting the mean?\n")
cat("   c) If you could shift the mean to 3500, what would Cpk become?\n")
cat("   d) Which supplier has the best capability? Analyze by cell_supplier.\n\n")

cat("2. Loan Processing Time (One-Sided Spec):\n")
cat("   a) The process is barely capable. What actions would reduce the mean\n")
cat("      processing time toward the target of 24 hours?\n")
cat("   b) Calculate Cpk for each loan type separately. Which type is most\n")
cat("      at risk of exceeding the USL?\n")
cat("   c) Does officer experience correlate with processing time?\n")
cat("   d) How does document completeness affect the distribution?\n\n")

cat("3. Advanced (DfLSS Black Belt Level):\n")
cat("   a) Compare Cp-Cpk gap for both processes. What does this tell you\n")
cat("      about centering vs. spread as the primary issue for each?\n")
cat("   b) Calculate the expected DPMO (defects per million opportunities)\n")
cat("      for each process. Convert to sigma level.\n")
cat("   c) Design an experiment (DOE) to identify the key factors affecting\n")
cat("      battery capacity. What factors would you include?\n")
cat("   d) Create a control plan for the loan process. What monitoring\n")
cat("      frequency and response protocol would you recommend?\n")

cat("\n")
cat("=== Analysis Complete ===\n")
