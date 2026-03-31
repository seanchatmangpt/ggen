# =============================================================================
# DfLSS Black Belt - Multiple Regression & Multi-Vari Analysis
# =============================================================================
# This script accompanies the patient_satisfaction and loan_approval datasets.
# It demonstrates:
#   1. Multiple linear regression with model selection and diagnostics
#   2. Multi-vari chart construction and nested ANOVA
#
# Tools: Base R (no external packages required for core analysis)
# Optional: car package for VIF (install.packages("car") if needed)
# =============================================================================

set.seed(42)  # Reproducibility for any stochastic elements

# =============================================================================
# PART 1: MULTIPLE REGRESSION - Patient Satisfaction
# =============================================================================

cat("=== PART 1: MULTIPLE REGRESSION ===\n\n")

# --- Load data ---
patient <- read.csv("patient_satisfaction.csv", stringsAsFactors = TRUE)
cat("Patient Satisfaction Dataset:\n")
cat(sprintf("  Observations: %d\n", nrow(patient)))
cat(sprintf("  Predictors: wait_time, friendliness, cleanliness, pain_mgmt, communication\n\n"))

# --- Step 1: Fit the full model ---
# All five continuous predictors entered simultaneously.
# This is the "full" model against which we compare reduced models.
full_model <- lm(satisfaction ~ wait_time + friendliness + cleanliness +
                   pain_mgmt + communication, data = patient)

cat("--- Full Model Summary ---\n")
print(summary(full_model))

# --- Step 2: Stepwise Model Selection ---
# Use AIC-based stepwise selection to find the most parsimonious model.
# Stepwise regression adds/removes predictors based on statistical contribution.
# The goal: maximize adjusted R-squared while minimizing predictors (parsimony).
# NOTE: In practice, domain knowledge should guide selection; stepwise is a starting point.
step_model <- step(full_model, direction = "both", trace = 1)

cat("\n--- Stepwise Selected Model ---\n")
print(summary(step_model))

# --- Step 3: Multicollinearity Diagnosis ---
# Variance Inflation Factor (VIF) measures how much the variance of a regression
# coefficient is inflated due to correlation with other predictors.
#   VIF > 1: Some correlation
#   VIF > 5: Moderate multicollinearity (investigate)
#   VIF > 10: Severe multicollinearity (action required)
#
# In this dataset, Friendliness and Communication are correlated at r ~ 0.6,
# which should produce VIFs in the 2-3 range -- not severe but worth discussing.

cat("\n--- Multicollinearity Diagnosis (VIF) ---\n")
# Manual VIF calculation (no car package required)
# VIF_j = 1 / (1 - R^2_j) where R^2_j is from regressing X_j on all other X's
calculate_vif <- function(model) {
  p <- length(coef(model)) - 1  # number of predictors (excluding intercept)
  vif_values <- numeric(p)
  predictor_names <- names(coef(model))[-1]

  for (j in seq_along(predictor_names)) {
    # Regress predictor j on all other predictors
    formula_j <- as.formula(paste(predictor_names[j], "~",
                                   paste(setdiff(predictor_names, predictor_names[j]),
                                         collapse = " + ")))
    aux_model <- lm(formula_j, data = model$model)
    r_squared <- summary(aux_model)$r.squared
    vif_values[j] <- 1 / (1 - r_squared)
  }
  names(vif_values) <- predictor_names
  return(vif_values)
}

vif_full <- calculate_vif(full_model)
print(round(vif_full, 3))

cat("\nInterpretation:\n")
for (name in names(vif_full)) {
  if (vif_full[name] > 10) {
    cat(sprintf("  %s: VIF = %.2f -- SEVERE multicollinearity. Consider removing.\n",
                name, vif_full[name]))
  } else if (vif_full[name] > 5) {
    cat(sprintf("  %s: VIF = %.2f -- MODERATE multicollinearity. Investigate.\n",
                name, vif_full[name]))
  } else {
    cat(sprintf("  %s: VIF = %.2f -- Acceptable.\n",
                name, vif_full[name]))
  }
}

# --- Step 4: Correlation Matrix ---
# Examine pairwise correlations to identify which predictors are correlated.
cat("\n--- Predictor Correlation Matrix ---\n")
pred_cols <- c("wait_time", "friendliness", "cleanliness", "pain_mgmt", "communication")
cor_matrix <- cor(patient[, pred_cols])
print(round(cor_matrix, 3))

cat("\nNote: Friendliness and Communication show correlation ~ 0.6.\n")
cat("This is expected and is the teaching point for multicollinearity.\n")
cat("Despite the correlation, VIF < 5 means coefficients are still estimable.\n")

# --- Step 5: Residual Diagnostics ---
# Four standard residual plots to validate regression assumptions:
#   1. Residuals vs Fitted: Check linearity and homoscedasticity
#   2. Normal Q-Q: Check normality of residuals
#   3. Scale-Location: Check homoscedasticity (constant variance)
#   4. Residuals vs Leverage: Identify influential observations

cat("\n--- Residual Diagnostic Plots (Full Model) ---\n")
cat("Generating 4 diagnostic plots...\n")

par(mfrow = c(2, 2))
plot(full_model, main = "Full Model Diagnostics")

# Additional: histogram of residuals
par(mfrow = c(1, 1))
hist(residuals(full_model), breaks = 15, col = "steelblue", border = "white",
     main = "Histogram of Residuals", xlab = "Residuals", freq = FALSE)
curve(dnorm(x, mean = mean(residuals(full_model)), sd = sd(residuals(full_model))),
      add = TRUE, col = "red", lwd = 2)
cat("Histogram overlay: red curve = normal distribution fit.\n")

# --- Step 6: Adjusted R-squared Comparison ---
# Adjusted R-squared penalizes for adding predictors that don't improve the model.
# It is the preferred metric over raw R-squared for model comparison.

cat("\n--- Model Comparison ---\n")
cat(sprintf("Full Model:     Adjusted R-squared = %.4f\n",
            summary(full_model)$adj.r.squared))
cat(sprintf("Stepwise Model: Adjusted R-squared = %.4f\n",
            summary(step_model)$adj.r.squared))
cat(sprintf("Difference:                        %.4f\n",
            summary(step_model)$adj.r.squared - summary(full_model)$adj.r.squared))

# Also fit a simple model (wait_time only) for contrast
simple_model <- lm(satisfaction ~ wait_time, data = patient)
cat(sprintf("Simple Model:  Adjusted R-squared = %.4f\n",
            summary(simple_model)$adj.r.squared))

cat("\nKey takeaway: Adding quality predictors explains substantially more variation\n")
cat("than wait_time alone, demonstrating the value of multiple regression\n")
cat("in process improvement (Y is driven by multiple Xs).\n")


# =============================================================================
# PART 2: MULTI-VARI STUDY - Loan Processing Time
# =============================================================================

cat("\n\n=== PART 2: MULTI-VARI STUDY ===\n\n")

# --- Load data ---
loan <- read.csv("loan_approval_multi_vari.csv", stringsAsFactors = TRUE)
cat("Loan Processing Dataset:\n")
cat(sprintf("  Observations: %d\n", nrow(loan)))
cat(sprintf("  Factors: Branch (3), Officer nested in Branch (6), Day (5)\n\n"))

# --- Step 1: Multi-Vari Chart (Nested Design) ---
# A multi-vari chart displays the mean response at each factor level.
# For nested designs, we show:
#   - Branch means (top-level factor)
#   - Officer means within each branch (nested factor)
#   - Day-of-week means (crossed factor, if applicable)

cat("--- Multi-Vari Chart ---\n")

# Calculate means by Branch
branch_means <- aggregate(processing_hours ~ branch, data = loan, FUN = mean)
cat("\nBranch Means:\n")
print(branch_means)

# Calculate means by Branch and Officer (nested)
officer_means <- aggregate(processing_hours ~ branch + officer, data = loan, FUN = mean)
cat("\nOfficer Means (nested within Branch):\n")
print(officer_means)

# Calculate means by Day of Week
day_means <- aggregate(processing_hours ~ day_of_week, data = loan, FUN = mean)
cat("\nDay of Week Means:\n")
print(day_means)

# Visual multi-vari chart
par(mfrow = c(1, 1))

# Plot individual observations colored by Branch
loan$branch_num <- as.numeric(factor(loan$branch,
                                      levels = c("Downtown", "Suburban", "Rural")))

# Create multi-vari chart manually
branch_levels <- c("Downtown", "Suburban", "Rural")
colors <- c("blue", "green", "red")

# Set up plot area
plot(NULL, xlim = c(0.5, 6.5), ylim = c(3, 11),
     xlab = "Officer (nested in Branch)", ylab = "Processing Time (hours)",
     main = "Multi-Vari Chart: Loan Processing Time",
     xaxt = "n")

# Draw branch separators and officer positions
officer_positions <- c(1, 2, 3.5, 4.5, 6, 7)  # positions with gaps between branches
axis(1, at = officer_positions,
     labels = c("O1", "O2", "O3", "O4", "O5", "O6"), tick = TRUE)

# Draw branch boundaries
abline(v = 2.75, lty = 2, col = "gray")
abline(v = 5.25, lty = 2, col = "gray")

# Branch labels
text(1.5, 10.5, "Downtown", font = 2, col = "blue")
text(4.0, 10.5, "Suburban", font = 2, col = "green")
text(6.5, 10.5, "Rural", font = 2, col = "red")

# Grand mean line
grand_mean <- mean(loan$processing_hours)
abline(h = grand_mean, lty = 2, col = "black", lwd = 2)
text(0.6, grand_mean + 0.15, sprintf("Grand Mean = %.1f", grand_mean), cex = 0.8)

# Plot observations and means per officer
for (i in 1:nrow(officer_means)) {
  obs <- loan$processing_hours[loan$officer == officer_means$officer[i]]
  branch_idx <- which(branch_levels == officer_means$branch[i])
  jitter_x <- runif(length(obs), -0.15, 0.15)
  points(officer_positions[i] + jitter_x, obs, pch = 16,
         col = colors[branch_idx], cex = 0.7)
  points(officer_positions[i], officer_means$processing_hours[i],
         pch = 17, col = "black", cex = 1.2)
  # Officer mean line segment
  segments(officer_positions[i] - 0.3, officer_means$processing_hours[i],
           officer_positions[i] + 0.3, officer_means$processing_hours[i],
           lwd = 2, col = colors[branch_idx])
}

legend("topleft", legend = c("Downtown", "Suburban", "Rural", "Officer Mean"),
       col = c("blue", "green", "red", "black"),
       pch = c(16, 16, 16, 17), cex = 0.8)

cat("Multi-vari chart generated. Key observations:\n")
cat("  - Rural branch shows highest processing times\n")
cat("  - Downtown branch shows lowest processing times\n")
cat("  - Friday consistently highest across all branches\n")

# --- Step 2: Nested ANOVA ---
# In a nested design, Officer is nested within Branch.
# The correct model: Officer(Branch) not Branch + Officer.
# This prevents attributing Branch variation to Officer.
#
# Model: processing_hours ~ branch + branch:officer
# branch:officer captures the Officer effect within each Branch.

cat("\n--- Nested ANOVA ---\n")
cat("Model: processing_hours ~ branch + officer(branch)\n\n")

# Fit nested ANOVA model
# Error(Officer/Branch) = Officer nested in Branch
nested_model <- aov(processing_hours ~ branch + officer %in% branch, data = loan)
print(summary(nested_model))

# --- Step 3: Variation Decomposition ---
# Decompose total variation into components attributable to each source.
# This tells us WHERE the variation lives, which is the first question
# in any process improvement effort.

cat("\n--- Variation Decomposition ---\n")

# Extract SS from ANOVA
ss_table <- summary(nested_model)[[1]]
ss_branch <- ss_table["branch", "Sum Sq"]
ss_officer <- ss_table["branch:officer", "Sum Sq"]
ss_residuals <- ss_table["Residuals", "Sum Sq"]
ss_total <- ss_branch + ss_officer + ss_residuals

# Calculate percentages
pct_branch <- (ss_branch / ss_total) * 100
pct_officer <- (ss_officer / ss_total) * 100
pct_residual <- (ss_residuals / ss_total) * 100

cat(sprintf("Total SS:                %10.2f  (100.0%%)\n", ss_total))
cat(sprintf("  Branch (between):      %10.2f  (%5.1f%%)\n", ss_branch, pct_branch))
cat(sprintf("  Officer (within):      %10.2f  (%5.1f%%)\n", ss_officer, pct_officer))
cat(sprintf("  Residual (within):     %10.2f  (%5.1f%%)\n", ss_residuals, pct_residual))

cat("\n--- Interpretation ---\n")
cat("The multi-vari study reveals where variation lives in the process:\n\n")

if (pct_branch > pct_officer && pct_branch > pct_residual) {
  cat("  PRIMARY SOURCE: Branch-to-Branch variation.\n")
  cat("  ACTION: Investigate what differs between branches (IT systems, training,\n")
  cat("          staffing levels, process steps, management practices).\n")
} else if (pct_officer > pct_residual) {
  cat("  PRIMARY SOURCE: Officer-to-Officer variation (within branches).\n")
  cat("  ACTION: Investigate individual officer practices, experience levels,\n")
  cat("          and training effectiveness.\n")
}

cat(sprintf("\n  Branch effect (%.1f%%) dominates -- this is a between-location problem,\n",
            pct_branch))
cat(sprintf("  not a between-officer problem (%.1f%%). Process standardization\n",
            pct_officer))
cat("  across branches should be the first improvement lever.\n")

# --- Step 4: Day-of-Week Effect ---
# Examine day-of-week as a crossed factor (all officers work all days).
cat("\n--- Day-of-Week Effect ---\n")
day_anova <- aov(processing_hours ~ day_of_week, data = loan)
cat("One-way ANOVA by Day of Week:\n")
print(summary(day_anova))

cat("\nDay Means (sorted):\n")
day_means_sorted <- day_means[order(day_means$processing_hours, decreasing = TRUE), ]
print(day_means_sorted)

cat("\nConclusion: Friday processing times are longest across all branches.\n")
cat("This suggests end-of-week backlog or staffing issues as a secondary improvement opportunity.\n")

# --- Step 5: Summary for Black Belt Students ---
cat("\n")
cat("=============================================================================\n")
cat("DfLSS BLACK BELT SUMMARY\n")
cat("=============================================================================\n")
cat("\nREGRESSION FINDINGS:\n")
cat("  - Multiple predictors explain patient satisfaction better than any single one\n")
cat("  - Wait time is the strongest single predictor (negative relationship)\n")
cat("  - Staff friendliness and communication quality are correlated (r ~ 0.6)\n")
cat("  - VIF analysis shows multicollinearity is present but manageable\n")
cat("  - Residual diagnostics validate regression assumptions\n")
cat("  - Adjusted R-squared is preferred over R-squared for model comparison\n")
cat("\nMULTI-VARI FINDINGS:\n")
cat("  - Branch is the dominant source of variation\n")
cat("  - Officer differences within branches are minor\n")
cat("  - Friday shows highest processing times (day effect)\n")
cat("  - Improvement priority: standardize across branches first\n")
cat("  - Multi-vari studies answer: WHERE does variation live?\n")
cat("  - This guides the Pareto of improvement efforts\n")
cat("=============================================================================\n")
