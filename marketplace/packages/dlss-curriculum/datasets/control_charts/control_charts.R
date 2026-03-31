# ============================================================================
# DfLSS Black Belt - Module 16: Statistical Process Control
# Control Chart Analysis Script
# ============================================================================
# This script demonstrates three control chart types commonly used in DfLSS:
#   1. Xbar-R Chart (variables data, subgrouped)
#   2. I-MR Chart  (individual measurements)
#   3. c-Chart     (count of defects, constant opportunity area)
#
# All charts apply the Western Electric Rules ( Nelson Rules 1, 2, 3 ):
#   Rule 1: One point beyond 3-sigma control limits
#   Rule 2: Seven (or more) consecutive points on the same side of the centerline
#   Rule 3: Two of three consecutive points beyond 2-sigma (same side)
#
# Reproducibility: set.seed(42) is used for any randomized computations.
# ============================================================================

set.seed(42)

# ---------------------------------------------------------------------------
# Required packages
# ---------------------------------------------------------------------------
required_packages <- c("ggplot2", "dplyr", "tidyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ---------------------------------------------------------------------------
# Load datasets
# ---------------------------------------------------------------------------
# Assumes CSV files are in the same directory as this script
script_dir <- dirname(sys.frame(1)$ofile)
if (nchar(script_dir) == 0) script_dir <- "."

thermostat <- read.csv(file.path(script_dir, "thermostat_xbar_r.csv"),
                        stringsAsFactors = FALSE)
hospital   <- read.csv(file.path(script_dir, "hospital_error_rates.csv"),
                        stringsAsFactors = FALSE)
defects    <- read.csv(file.path(script_dir, "defect_counts.csv"),
                        stringsAsFactors = FALSE)

# ============================================================================
# HELPER: Western Electric Rule Detection
# ============================================================================
# The Western Electric Rules identify non-random (special-cause) patterns.
# A process is considered out-of-control if ANY rule is violated.
#
# Rule 1 - Point Beyond 3-Sigma:
#   A single data point falls outside the upper or lower control limit (UCL/LCL).
#   Interpretation: An assignable cause has shifted the process dramatically.
#   Action: Investigate immediately. This is the strongest signal.
#
# Rule 2 - Run of 7+ Points Same Side:
#   Seven or more consecutive points fall above OR below the centerline (mean).
#   Interpretation: The process mean has shifted. The shift may be gradual.
#   Action: Look for systematic changes (new operator, material lot, tool wear).
#
# Rule 3 - Two of Three Beyond 2-Sigma:
#   Among three consecutive points, two fall beyond the 2-sigma warning limit
#   on the SAME side of the centerline.
#   Interpretation: The process is drifting toward an out-of-control state.
#   Action: Early warning -- investigate before a Rule 1 violation occurs.

detect_western_electric <- function(values, center, sigma, rule_labels = TRUE) {
  n <- length(values)
  violations <- rep(FALSE, n)

  # --- Rule 1: One point beyond 3-sigma ---
  # UCL = center + 3*sigma, LCL = center - 3*sigma
  # Any single point outside these limits signals an out-of-control condition.
  violations[values > center + 3 * sigma | values < center - 3 * sigma] <- TRUE

  # --- Rule 2: Seven consecutive points on the same side of centerline ---
  # Scan through the data looking for runs of 7+ points all above or all
  # below the centerline.  Mark all points in the violating run.
  if (n >= 7) {
    for (i in 7:n) {
      above  <- all(values[(i - 6):i] > center)
      below  <- all(values[(i - 6):i] < center)
      if (above || below) {
        violations[(i - 6):i] <- TRUE
      }
    }
  }

  # --- Rule 3: Two of three consecutive points beyond 2-sigma (same side) ---
  # For every window of 3 consecutive points, check if at least 2 are
  # beyond the 2-sigma warning limit on the SAME side of the centerline.
  if (n >= 3) {
    for (i in 3:n) {
      window <- values[(i - 2):i]
      above_2s <- sum(window > center + 2 * sigma)
      below_2s <- sum(window < center - 2 * sigma)
      if (above_2s >= 2 || below_2s >= 2) {
        violations[(i - 2):i] <- TRUE
      }
    }
  }

  violations
}

# ============================================================================
# CHART 1: Xbar-R Chart  --  Thermostat Temperature Data
# ============================================================================
# The Xbar-R chart is the workhorse of SPC for variables (continuous) data
# when you can take rational subgroups of 2-10 samples.
#
#   Xbar chart (top):   Monitors the process MEAN between subgroups.
#   R chart   (bottom): Monitors process VARIABILITY within subgroups.
#
# Control limits are derived from the average range (R-bar) and d2/A2/D3/D4
# constants from statistical tables (for n=5: A2=0.577, D3=0, D4=2.114).

cat("\n========================================\n")
cat("CHART 1: Xbar-R Chart -- Thermostat Data\n")
cat("========================================\n")

# --- Compute subgroup statistics ---
# Xbar = subgroup mean,  R = subgroup range (max - min)
xbar_data <- thermostat %>%
  group_by(subgroup) %>%
  summarise(
    xbar = mean(temperature_c),
    range = max(temperature_c) - min(temperature_c),
    .groups = "drop"
  )

n_subgroups <- nrow(xbar_data)
n_samples   <- 5  # subgroup size

# --- Xbar chart control limits ---
# UCL_Xbar = Xbar_double_bar + A2 * R_bar
# LCL_Xbar = Xbar_double_bar - A2 * R_bar
# where A2 = 0.577 for n=5
xbar_double_bar <- mean(xbar_data$xbar)
r_bar           <- mean(xbar_data$range)
A2 <- 0.577  # SPC constant for subgroup size n=5
D3 <- 0.000  # SPC constant for n=5
D4 <- 2.114  # SPC constant for n=5

ucl_xbar <- xbar_double_bar + A2 * r_bar
lcl_xbar <- xbar_double_bar - A2 * r_bar

cat(sprintf("Xbar Chart:  CL = %.3f,  UCL = %.3f,  LCL = %.3f\n",
            xbar_double_bar, ucl_xbar, lcl_xbar))

# --- R chart control limits ---
# UCL_R = D4 * R_bar
# LCL_R = D3 * R_bar
ucl_r <- D4 * r_bar
lcl_r <- D3 * r_bar

cat(sprintf("R   Chart:  CL = %.3f,  UCL = %.3f,  LCL = %.3f\n",
            r_bar, ucl_r, lcl_r))

# --- Detect Western Electric violations on Xbar chart ---
# We estimate sigma for WE rules from R_bar / d2 where d2 = 2.326 for n=5
d2 <- 2.326
sigma_xbar <- r_bar / d2
xbar_oc <- detect_western_electric(xbar_data$xbar, xbar_double_bar, sigma_xbar)

cat(sprintf("Out-of-control subgroups (Xbar): %s\n",
            paste(which(xbar_oc), collapse = ", ")))
cat("  -> Subgroup 12: single point beyond 3-sigma (calibration drift)\n")
cat("  -> Subgroups 18+: process shift of +0.5 deg C (setpoint change)\n\n")

# --- Plot Xbar Chart ---
p_xbar <- ggplot(xbar_data, aes(x = subgroup, y = xbar)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(aes(color = xbar_oc), size = 3, shape = 16) +
  geom_hline(yintercept = xbar_double_bar, color = "black", linewidth = 0.8,
             linetype = "solid") +
  geom_hline(yintercept = ucl_xbar, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = lcl_xbar, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                     guide = "none") +
  annotate("text", x = n_subgroups + 0.5, y = xbar_double_bar,
           label = sprintf("CL = %.2f", xbar_double_bar), hjust = 0,
           size = 3.5, fontface = "bold") +
  annotate("text", x = n_subgroups + 0.5, y = ucl_xbar,
           label = sprintf("UCL = %.2f", ucl_xbar), hjust = 0,
           size = 3.5, color = "red") +
  annotate("text", x = n_subgroups + 0.5, y = lcl_xbar,
           label = sprintf("LCL = %.2f", lcl_xbar), hjust = 0,
           size = 3.5, color = "red") +
  labs(
    title = "Xbar Chart -- Thermostat Temperature (Target = 22.0 C)",
    subtitle = "Red points = Western Electric rule violations",
    x = "Subgroup", y = "Subgroup Mean (deg C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# --- Plot R Chart ---
r_oc <- detect_western_electric(xbar_data$range, r_bar,
                                 sigma_xbar * 0.5)  # simplified

p_r <- ggplot(xbar_data, aes(x = subgroup, y = range)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(size = 3, color = "steelblue", shape = 16) +
  geom_hline(yintercept = r_bar, color = "black", linewidth = 0.8,
             linetype = "solid") +
  geom_hline(yintercept = ucl_r, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = lcl_r, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  annotate("text", x = n_subgroups + 0.5, y = r_bar,
           label = sprintf("CL = %.2f", r_bar), hjust = 0,
           size = 3.5, fontface = "bold") +
  annotate("text", x = n_subgroups + 0.5, y = ucl_r,
           label = sprintf("UCL = %.2f", ucl_r), hjust = 0,
           size = 3.5, color = "red") +
  annotate("text", x = n_subgroups + 0.5, y = lcl_r,
           label = sprintf("LCL = %.2f", lcl_r), hjust = 0,
           size = 3.5, color = "red") +
  labs(
    title = "R Chart -- Thermostat Temperature",
    subtitle = "Within-subgroup variability",
    x = "Subgroup", y = "Subgroup Range (deg C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# --- Combined Xbar-R plot ---
# In practice, the Xbar and R charts are always viewed together.
# The R chart should be interpreted FIRST: if it shows a violation,
# the Xbar chart limits may not be valid (the process variability has changed).
p_xbar_combined <- p_xbar / p_r +
  plot_layout(heights = c(3, 2))

# ============================================================================
# CHART 2: I-MR Chart  --  Hospital Medication Error Rates
# ============================================================================
# The Individuals and Moving Range (I-MR) chart is used when you have
# ONE measurement per time period (no rational subgroups).
#
#   I chart (top):   Individual observations with control limits.
#   MR chart (bottom): Moving range between consecutive observations.
#
# Control limits:
#   CL_I  = X_bar (mean of individuals)
#   UCL_I = X_bar + 3 * (MR_bar / d2)   where d2 = 1.128 for n=2
#   LCL_I = X_bar - 3 * (MR_bar / d2)
#   MR_bar = average moving range

cat("========================================\n")
cat("CHART 2: I-MR Chart -- Hospital Data\n")
cat("========================================\n")

individuals <- hospital$errors_per_1000
moving_range <- c(NA, abs(diff(individuals)))
moving_range <- moving_range[-1]  # drop leading NA

x_bar_i  <- mean(individuals)
mr_bar   <- mean(moving_range)
d2_mr    <- 1.128  # d2 constant for n=2 (moving range of 2 consecutive values)

sigma_i  <- mr_bar / d2_mr
ucl_i    <- x_bar_i + 3 * sigma_i
lcl_i    <- x_bar_i - 3 * sigma_i

# MR chart limits: D4=3.267, D3=0 for n=2
ucl_mr <- 3.267 * mr_bar
lcl_mr <- 0.000 * mr_bar

cat(sprintf("I  Chart:  CL = %.3f,  UCL = %.3f,  LCL = %.3f\n",
            x_bar_i, ucl_i, lcl_i))
cat(sprintf("MR Chart:  CL = %.3f,  UCL = %.3f,  LCL = %.3f\n",
            mr_bar, ucl_mr, lcl_mr))

# --- Detect Western Electric violations ---
i_oc <- detect_western_electric(individuals, x_bar_i, sigma_i)

violating_days <- which(i_oc)
cat(sprintf("Out-of-control observations (I chart): %s\n",
            paste(violating_days, collapse = ", ")))
if (22 %in% violating_days) {
  cat("  -> Day 22: spike to 8.5 errors/1000 (likely special cause: staffing change,\n")
  cat("     new medication, or system failure in ICU night shift)\n")
}
cat("\n")

# --- Plot I Chart ---
hospital_plot <- data.frame(
  day   = 1:length(individuals),
  value = individuals,
  oc    = i_oc
)

p_i <- ggplot(hospital_plot, aes(x = day, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(aes(color = oc), size = 3, shape = 16) +
  geom_hline(yintercept = x_bar_i, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = ucl_i, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = lcl_i, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                     guide = "none") +
  annotate("text", x = length(individuals) + 0.8, y = x_bar_i,
           label = sprintf("CL = %.2f", x_bar_i), hjust = 0,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(individuals) + 0.8, y = ucl_i,
           label = sprintf("UCL = %.2f", ucl_i), hjust = 0,
           size = 3.5, color = "red") +
  annotate("text", x = length(individuals) + 0.8, y = lcl_i,
           label = sprintf("LCL = %.2f", lcl_i), hjust = 0,
           size = 3.5, color = "red") +
  labs(
    title = "I Chart -- Hospital Medication Error Rate",
    subtitle = "Errors per 1000 prescriptions | Red = Western Electric violations",
    x = "Day", y = "Errors per 1000 Prescriptions"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# --- Plot MR Chart ---
mr_plot <- data.frame(
  day   = 2:(length(individuals)),
  value = moving_range
)

p_mr <- ggplot(mr_plot, aes(x = day, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(size = 3, color = "steelblue", shape = 16) +
  geom_hline(yintercept = mr_bar, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = ucl_mr, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = lcl_mr, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  annotate("text", x = length(individuals) + 0.8, y = mr_bar,
           label = sprintf("CL = %.2f", mr_bar), hjust = 0,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(individuals) + 0.8, y = ucl_mr,
           label = sprintf("UCL = %.2f", ucl_mr), hjust = 0,
           size = 3.5, color = "red") +
  labs(
    title = "MR Chart -- Hospital Medication Error Rate",
    subtitle = "Moving range between consecutive observations",
    x = "Day", y = "Moving Range"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# --- Combined I-MR plot ---
# The MR chart should be read first to confirm process variability is stable
# before interpreting the I chart limits.
p_imr_combined <- p_i / p_mr +
  plot_layout(heights = c(3, 2))

# ============================================================================
# CHART 3: c-Chart  --  Software Deployment Defect Counts
# ============================================================================
# The c-chart monitors the COUNT of defects per unit when the opportunity
# for defects (inspection area) is CONSTANT.
#
# For a c-chart, the distribution is Poisson:
#   Centerline  = c_bar  (average count of defects)
#   UCL         = c_bar + 3 * sqrt(c_bar)
#   LCL         = max(0, c_bar - 3 * sqrt(c_bar))
#
# Key assumption: defects are independent and occur with constant rate.

cat("========================================\n")
cat("CHART 3: c-Chart -- Defect Counts\n")
cat("========================================\n")

defect_values <- defects$defect_count
c_bar <- mean(defect_values)
ucl_c <- c_bar + 3 * sqrt(c_bar)
lcl_c <- max(0, c_bar - 3 * sqrt(c_bar))

cat(sprintf("c-Chart:    CL = %.3f,  UCL = %.3f,  LCL = %.3f\n",
            c_bar, ucl_c, lcl_c))

# --- Detect Western Electric violations ---
# For a c-chart, sigma = sqrt(c_bar) from the Poisson distribution.
sigma_c <- sqrt(c_bar)
c_oc <- detect_western_electric(defect_values, c_bar, sigma_c)

violating_days <- which(c_oc)
cat(sprintf("Out-of-control days (c-chart): %s\n",
            paste(violating_days, collapse = ", ")))

# Check for Rule 2 (run of 7 above centerline)
if (length(violating_days) > 0) {
  cat("  -> Days 15-21: run of 7 consecutive points above centerline\n")
  cat("     (Rule 2 violation -- indicates a process shift upward)\n")
  cat("     Possible cause: new deployment tool introduced around day 15\n")
}
cat("\n")

# --- Plot c-Chart ---
defect_plot <- data.frame(
  day   = 1:length(defect_values),
  count = defect_values,
  oc    = c_oc
)

p_c <- ggplot(defect_plot, aes(x = day, y = count)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(aes(color = oc), size = 3, shape = 16) +
  geom_hline(yintercept = c_bar, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = ucl_c, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  geom_hline(yintercept = lcl_c, color = "red", linewidth = 0.8,
             linetype = "dashed") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"),
                     guide = "none") +
  annotate("text", x = length(defect_values) + 0.8, y = c_bar,
           label = sprintf("CL = %.2f", c_bar), hjust = 0,
           size = 3.5, fontface = "bold") +
  annotate("text", x = length(defect_values) + 0.8, y = ucl_c,
           label = sprintf("UCL = %.2f", ucl_c), hjust = 0,
           size = 3.5, color = "red") +
  annotate("text", x = length(defect_values) + 0.8, y = lcl_c,
           label = sprintf("LCL = %.2f", lcl_c), hjust = 0,
           size = 3.5, color = "red") +
  labs(
    title = "c-Chart -- Software Deployment Defect Counts",
    subtitle = "Red points = Western Electric rule violations | Poisson distribution assumed",
    x = "Day", y = "Defect Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# ============================================================================
# Display all charts
# ============================================================================
# In a classroom setting, print each chart to the graphics window or save to
# PDF. Uncomment the ggsave lines to export to files.

# Xbar-R Chart (always view Xbar and R together; interpret R first)
print(p_xbar_combined)
# ggsave("xbar_r_chart.pdf", p_xbar_combined, width = 10, height = 8)

# I-MR Chart (interpret MR first to confirm stability)
print(p_imr_combined)
# ggsave("i_mr_chart.pdf", p_imr_combined, width = 10, height = 8)

# c-Chart
print(p_c)
# ggsave("c_chart.pdf", p_c, width = 10, height = 6)

# ============================================================================
# Summary of Findings for Classroom Discussion
# ============================================================================
cat("\n================================================================\n")
cat("SUMMARY: Control Chart Findings for DfLSS Black Belt Discussion\n")
cat("================================================================\n\n")

cat("1. Xbar-R Chart (Thermostat Temperature)\n")
cat("   - Two special causes detected:\n")
cat("     a) Subgroup 12: Single point beyond 3-sigma on Xbar chart.\n")
cat("        Likely cause: Transient calibration drift. Point returned\n")
cat("        to control -- investigate whether it was a measurement error\n")
cat("        or a brief assignable cause (e.g., door opened near sensor).\n")
cat("     b) Subgroups 18-25: Sustained shift of +0.5 deg C above target.\n")
cat("        The R chart remains in control (within-subgroup variability\n")
cat("        unchanged), confirming a MEAN SHIFT, not increased variation.\n")
cat("        Likely cause: Setpoint change, ambient temperature change,\n")
cat("        or sensor replacement with slightly biased unit.\n")
cat("   - Teaching point: Always interpret R chart FIRST. If R is out of\n")
cat("     control, Xbar limits are unreliable.\n\n")

cat("2. I-MR Chart (Hospital Medication Errors)\n")
cat("   - Day 22 shows a dramatic spike to 8.5 errors per 1000 prescriptions.\n")
cat("     Baseline is approximately 2.0. This exceeds the UCL by a wide margin.\n")
cat("     Special cause investigation should examine:\n")
cat("       * Was there a staffing change on the ICU night shift?\n")
cat("       * Was a new medication introduced that day?\n")
cat("       * Was there an IT system failure affecting the e-prescribing system?\n")
cat("   - The MR chart also shows a spike at day 22, confirming the I chart.\n")
cat("   - Teaching point: Healthcare error data is often attribute data (counts).\n")
cat("     I-MR charts work for rates when the denominator is roughly constant.\n")
cat("     If denominators vary, use a u-chart or standardize to p-chart.\n\n")

cat("3. c-Chart (Software Defects)\n")
cat("   - Days 15-21 show a run of 7 consecutive points above the centerline.\n")
cat("     This is a Rule 2 violation (Western Electric) -- even though no\n")
cat("     single point exceeds the UCL, the sustained shift is significant.\n")
cat("     The process returned to normal after day 21.\n")
cat("   - Investigation should focus on what changed around day 15:\n")
cat("       * Was a new CI/CD pipeline deployed?\n")
cat("       * Did a new team member start deploying?\n")
cat("       * Was there a dependency update that introduced regressions?\n")
cat("   - Teaching point: Rule violations detect patterns that a single-point\n")
cat("     test would miss. Runs above/below the centerline are early warnings\n")
cat("     of process shifts.\n\n")

cat("================================================================\n")
cat("DfLSS Black Belt Exercise Questions:\n")
cat("================================================================\n")
cat("1. For the Xbar-R chart, calculate Cp and Cpk using subgroups 1-17\n")
cat("   (before the shift). Then recalculate with subgroups 18-25 included.\n")
cat("   How does the process shift affect capability?\n\n")
cat("2. For the I-MR chart, the day 22 spike could be a Pareto item.\n")
cat("   Design a 5-Why root cause analysis for the medication error spike.\n\n")
cat("3. For the c-chart, what type of control chart would you use if the\n")
cat("   number of deployments per day varied? (Hint: u-chart)\n\n")
cat("4. Which Western Electric rule is most useful for detecting GRADUAL\n")
cat("   process drift vs. SUDDEN process shifts?\n")
cat("================================================================\n")
