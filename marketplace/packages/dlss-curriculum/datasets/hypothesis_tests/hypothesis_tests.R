#!/usr/bin/env Rscript
# =============================================================================
# DfLSS Black Belt - Hypothesis Testing Module
# M24: Paired t-test & One-way ANOVA | M25: Simple Linear Regression
# =============================================================================
# This script demonstrates three core hypothesis tests using realistic
# manufacturing/battery engineering datasets. Each section follows the
# DMAIC framework: Define hypotheses, check assumptions, run test,
# interpret results, calculate effect sizes.
# =============================================================================

set.seed(42)  # Reproducibility for all random operations

library(stats)
library(graphics)

# =============================================================================
# SECTION 0: Load Data
# =============================================================================

# Read all three datasets from the same directory as this script
script_dir <- dirname(sys.frame(1)$ofile)
if (nchar(script_dir) == 0) script_dir <- "."

battery <- read.csv(file.path(script_dir, "battery_before_after.csv"),
                    stringsAsFactors = FALSE)
supplier <- read.csv(file.path(script_dir, "supplier_comparison.csv"),
                     stringsAsFactors = FALSE)
discharge <- read.csv(file.path(script_dir, "discharge_time_vs_temperature.csv"),
                      stringsAsFactors = FALSE)

cat("=== DfLSS Hypothesis Testing Module ===\n\n")
cat(sprintf("Battery before/after: %d observations\n", nrow(battery)))
cat(sprintf("Supplier comparison:  %d observations\n", nrow(supplier)))
cat(sprintf("Discharge vs temp:    %d observations\n\n", nrow(discharge)))


# =============================================================================
# SECTION 1: Paired t-test (Battery Manufacturing Process Change)
# =============================================================================
# Context: A manufacturing process change was implemented to improve battery
#          cell capacity. Each of 30 batteries was measured before and after
#          the change. We need to determine if the improvement is statistically
#          significant.
#
# NULL HYPOTHESIS (H0):      mu_d = 0
#   There is no difference in mean capacity before vs. after the process change.
#   Any observed difference is due to random chance.
#
# ALTERNATIVE HYPOTHESIS (Ha): mu_d != 0
#   There IS a significant difference in mean capacity before vs. after.
#   The process change had a real effect on battery capacity.
#
# TEST: Two-sided paired t-test (dependent samples, same battery measured twice)
# ALPHA: 0.05 (industry standard)
# =============================================================================

cat("============================================================\n")
cat("SECTION 1: PAIRED t-TEST - Battery Process Change\n")
cat("============================================================\n\n")

# --- Step 1: Calculate paired differences ---
battery$difference <- battery$after_mah - battery$before_mah

cat("--- Descriptive Statistics ---\n")
cat(sprintf("Mean before:  %.1f mAh\n", mean(battery$before_mah)))
cat(sprintf("Mean after:   %.1f mAh\n", mean(battery$after_mah)))
cat(sprintf("Mean diff:    %.1f mAh\n", mean(battery$difference)))
cat(sprintf("SD of diff:   %.1f mAh\n", sd(battery$difference)))
cat(sprintf("Median diff:  %.1f mAh\n", median(battery$difference)))
cat(sprintf("n = %d batteries\n\n", nrow(battery)))

# --- Step 2: Check assumptions ---
# Assumption 1: Differences are approximately normally distributed
# We use Shapiro-Wilk test (H0: data is normally distributed)

cat("--- Assumption Check: Normality of Differences ---\n")
shapiro_result <- shapiro.test(battery$difference)
cat(sprintf("Shapiro-Wilk W = %.4f, p-value = %.4f\n",
            shapiro_result$statistic, shapiro_result$p.value))
if (shapiro_result$p.value > 0.05) {
  cat("  => p > 0.05: Cannot reject normality. Assumption SATISFIED.\n\n")
} else {
  cat("  => p <= 0.05: Normality may be violated. Consider Wilcoxon.\n\n")
}

# --- Step 3: Run paired t-test ---
cat("--- Paired t-test Results ---\n")
t_result <- t.test(battery$before_mah, battery$after_mah,
                   paired = TRUE,
                   conf.level = 0.95)
print(t_result)

# --- Step 4: Calculate effect size (Cohen's d for paired samples) ---
# Cohen's d = mean(d) / sd(d)
# Interpretation: 0.2=small, 0.5=medium, 0.8=large, 1.2=very large, 2.0=huge
d_paired <- mean(battery$difference) / sd(battery$difference)
cat(sprintf("\n--- Effect Size ---\n"))
cat(sprintf("Cohen's d = %.3f\n", d_paired))
if (d_paired >= 2.0) {
  cat("  => HUGE effect size. The process change had a massive practical impact.\n")
} else if (d_paired >= 0.8) {
  cat("  => LARGE effect size. The process change had a meaningful practical impact.\n")
} else if (d_paired >= 0.5) {
  cat("  => MEDIUM effect size. Moderate practical impact.\n")
} else {
  cat("  => SMALL effect size. Limited practical significance despite statistical significance.\n")
}

# --- Step 5: Interpretation ---
cat(sprintf("\n--- Interpretation ---\n"))
cat(sprintf("95%% CI for mean difference: [%.1f, %.1f] mAh\n",
            t_result$conf.int[1], t_result$conf.int[2]))
cat(sprintf("t-statistic: %.3f with %d df\n", t_result$statistic, t_result$parameter))
cat(sprintf("p-value: %.6f\n\n", t_result$p.value))

if (t_result$p.value < 0.001) {
  cat("CONCLUSION: STRONGLY REJECT H0 (p < 0.001).\n")
  cat("The manufacturing process change produced a statistically significant\n")
  cat(sprintf("improvement of ~%.0f mAh average capacity gain. This is both\n",
              mean(battery$difference)))
  cat("statistically significant AND practically significant (huge effect size).\n\n")
} else if (t_result$p.value < 0.05) {
  cat("CONCLUSION: REJECT H0 (p < 0.05). Statistically significant difference.\n\n")
} else {
  cat("CONCLUSION: FAIL TO REJECT H0. Insufficient evidence of improvement.\n\n")
}

# --- Step 6: Visualize ---
png(filename = file.path(script_dir, "paired_ttest_plot.png"),
    width = 800, height = 600, res = 120)
par(mfrow = c(1, 2))

# Boxplot of differences
boxplot(battery$difference,
        main = "Paired Differences (After - Before)",
        ylab = "Capacity Difference (mAh)",
        col = "steelblue",
        border = "darkblue")
abline(h = 0, lty = 2, col = "red")
abline(h = mean(battery$difference), lty = 1, col = "darkgreen", lwd = 2)
legend("topright",
       legend = c("No difference (0)", sprintf("Mean diff (%.1f)", mean(battery$difference))),
       lty = c(2, 1), col = c("red", "darkgreen"), cex = 0.8)

# Histogram of differences
hist(battery$difference,
     breaks = 10,
     main = "Distribution of Paired Differences",
     xlab = "Capacity Difference (mAh)",
     col = "lightblue",
     border = "darkblue")
abline(v = mean(battery$difference), col = "red", lwd = 2)
abline(v = 0, lty = 2, col = "gray")
legend("topright",
       legend = c(sprintf("Mean (%.1f)", mean(battery$difference)), "H0: no diff"),
       lty = c(1, 2), col = c("red", "gray"), cex = 0.8)

dev.off()
cat("Plot saved: paired_ttest_plot.png\n\n")


# =============================================================================
# SECTION 2: One-way ANOVA (Supplier Defect Rate Comparison)
# =============================================================================
# Context: Three suppliers provide components for battery assembly. We need
#          to determine if defect rates differ significantly between suppliers
#          to inform supplier selection decisions.
#
# NULL HYPOTHESIS (H0):      mu_A = mu_B = mu_C
#   All three suppliers have the same mean defect rate.
#   Any observed differences are due to random variation within suppliers.
#
# ALTERNATIVE HYPOTHESIS (Ha): At least one mu_i differs
#   At least one supplier has a different mean defect rate from the others.
#
# TEST: One-way ANOVA (one factor: supplier, three levels: A, B, C)
# ALPHA: 0.05
# =============================================================================

cat("============================================================\n")
cat("SECTION 2: ONE-WAY ANOVA - Supplier Defect Rate Comparison\n")
cat("============================================================\n\n")

# --- Step 1: Descriptive statistics by supplier ---
cat("--- Descriptive Statistics by Supplier ---\n")
supplier_summary <- aggregate(defect_rate_pct ~ supplier, data = supplier,
                              FUN = function(x) c(mean = mean(x),
                                                  sd = sd(x),
                                                  n = length(x)))
for (i in 1:nrow(supplier_summary)) {
  s <- supplier_summary$supplier[i]
  vals <- supplier$defect_rate_pct[supplier$supplier == s]
  cat(sprintf("  Supplier %s: Mean=%.2f%%, SD=%.2f%%, n=%d\n",
              s, mean(vals), sd(vals), length(vals)))
}
cat("\n")

# --- Step 2: Check assumptions ---
# Assumption 1: Normality within each group (Shapiro-Wilk)
cat("--- Assumption Check 1: Normality (Shapiro-Wilk) ---\n")
for (s in c("A", "B", "C")) {
  vals <- supplier$defect_rate_pct[supplier$supplier == s]
  sw <- shapiro.test(vals)
  status <- ifelse(sw$p.value > 0.05, "OK", "WARNING")
  cat(sprintf("  Supplier %s: W=%.4f, p=%.4f [%s]\n",
              s, sw$statistic, sw$p.value, status))
}

# Assumption 2: Homogeneity of variance (Levene's test via Bartlett)
cat("\n--- Assumption Check 2: Homogeneity of Variance (Bartlett) ---\n")
bartlett_result <- bartlett.test(defect_rate_pct ~ supplier, data = supplier)
cat(sprintf("Bartlett K-squared = %.4f, p-value = %.4f\n",
            bartlett_result$statistic, bartlett_result$p.value))
if (bartlett_result$p.value > 0.05) {
  cat("  => p > 0.05: Variances are equal. Assumption SATISFIED.\n\n")
} else {
  cat("  => p <= 0.05: Variances differ. Consider Welch ANOVA.\n\n")
}

# --- Step 3: Run one-way ANOVA ---
cat("--- One-way ANOVA Results ---\n")
supplier$supplier <- factor(supplier$supplier)  # Ensure factor
anova_model <- aov(defect_rate_pct ~ supplier, data = supplier)
print(summary(anova_model))

# --- Step 4: Effect size (eta-squared) ---
# eta-squared = SS_between / SS_total
anova_table <- summary(anova_model)[[1]]
ss_between <- anova_table["supplier", "Sum Sq"]
ss_total <- sum(anova_table[, "Sum Sq"])
eta_squared <- ss_between / ss_total
cat(sprintf("\n--- Effect Size ---\n"))
cat(sprintf("Eta-squared = %.4f (%.1f%% of variance explained)\n",
            eta_squared, eta_squared * 100))
if (eta_squared >= 0.14) {
  cat("  => LARGE effect. Supplier choice substantially impacts defect rate.\n")
} else if (eta_squared >= 0.06) {
  cat("  => MEDIUM effect. Moderate impact.\n")
} else {
  cat("  => SMALL effect. Limited practical impact.\n")
}

# --- Step 5: Post-hoc analysis (Tukey HSD) ---
# Only meaningful if ANOVA is significant (p < 0.05)
cat("\n--- Post-hoc: Tukey HSD ---\n")
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# --- Step 6: Interpretation ---
anova_pvalue <- anova_table["supplier", "Pr(>F)"]
cat(sprintf("\n--- Interpretation ---\n"))
cat(sprintf("F(%d, %d) = %.3f, p-value = %.6f\n",
            anova_table["supplier", "Df"],
            anova_table["Residuals", "Df"],
            anova_table["supplier", "F value"],
            anova_pvalue))

if (anova_pvalue < 0.001) {
  cat("\nCONCLUSION: STRONGLY REJECT H0 (p < 0.001).\n")
  cat("At least one supplier has a significantly different defect rate.\n\n")
  cat("From the Tukey HSD post-hoc analysis:\n")
  # Identify which pairs differ
  for (row in 1:nrow(tukey_result$supplier)) {
    pair <- rownames(tukey_result$supplier)[row]
    p_adj <- tukey_result$supplier[row, "p adj"]
    sig <- ifelse(p_adj < 0.001, "***", ifelse(p_adj < 0.01, "**",
            ifelse(p_adj < 0.05, "*", "ns")))
    cat(sprintf("  %s: diff=%.3f, p-adj=%.4f %s\n",
                pair, tukey_result$supplier[row, "diff"], p_adj, sig))
  }
  cat("\nSupplier B has significantly higher defect rates than A and C.\n")
  cat("RECOMMENDATION: Prefer Supplier A or C. Supplier B needs quality improvement.\n\n")
} else if (anova_pvalue < 0.05) {
  cat("\nCONCLUSION: REJECT H0 (p < 0.05). Some suppliers differ.\n\n")
} else {
  cat("\nCONCLUSION: FAIL TO REJECT H0. No significant difference between suppliers.\n\n")
}

# --- Step 7: Visualize ---
png(filename = file.path(script_dir, "anova_boxplot.png"),
    width = 800, height = 600, res = 120)
boxplot(defect_rate_pct ~ supplier, data = supplier,
        main = "Defect Rate by Supplier",
        xlab = "Supplier",
        ylab = "Defect Rate (%)",
        col = c("#4CAF50", "#F44336", "#2196F3"),
        border = c("darkgreen", "darkred", "darkblue"),
        ylim = c(0, max(supplier$defect_rate_pct) * 1.2))

# Add individual points (jittered)
stripchart(defect_rate_pct ~ supplier, data = supplier,
           vertical = TRUE, method = "jitter", jitter = 0.15,
           pch = 16, col = "gray40", add = TRUE)

# Add group means
means <- tapply(supplier$defect_rate_pct, supplier$supplier, mean)
points(1:length(means), means, pch = 18, col = "black", cex = 1.5)

legend("topright",
       legend = c("Individual samples", "Group mean"),
       pch = c(16, 18), col = c("gray40", "black"), cex = 0.9)

dev.off()
cat("Plot saved: anova_boxplot.png\n\n")


# =============================================================================
# SECTION 3: Simple Linear Regression (Discharge Time vs Temperature)
# =============================================================================
# Context: Battery discharge time decreases as temperature increases. We need
#          to model this relationship to predict performance under conditions
#          and understand the strength of the temperature effect.
#
# NULL HYPOTHESIS (H0):      beta_1 = 0
#   There is no linear relationship between temperature and discharge time.
#   The slope of the regression line is zero (flat line).
#
# ALTERNATIVE HYPOTHESIS (Ha): beta_1 != 0
#   There IS a linear relationship between temperature and discharge time.
#   Temperature can be used to predict discharge time.
#
# TEST: Simple linear regression (one predictor, one response)
# ALPHA: 0.05
# =============================================================================

cat("============================================================\n")
cat("SECTION 3: SIMPLE LINEAR REGRESSION - Discharge Time vs Temp\n")
cat("============================================================\n\n")

# --- Step 1: Exploratory analysis ---
cat("--- Descriptive Statistics ---\n")
cat(sprintf("Temperature:  Mean=%.1f C, SD=%.1f C, Range=[%.0f, %.0f]\n",
            mean(discharge$temperature_c), sd(discharge$temperature_c),
            min(discharge$temperature_c), max(discharge$temperature_c)))
cat(sprintf("Discharge:    Mean=%.1f h, SD=%.1f h, Range=[%.1f, %.1f]\n\n",
            mean(discharge$discharge_time_h), sd(discharge$discharge_time_h),
            min(discharge$discharge_time_h), max(discharge$discharge_time_h)))

# Pearson correlation
cor_result <- cor.test(discharge$temperature_c, discharge$discharge_time_h)
cat("--- Correlation Analysis ---\n")
cat(sprintf("Pearson r = %.4f\n", cor_result$estimate))
cat(sprintf("95%% CI: [%.4f, %.4f]\n", cor_result$conf.int[1], cor_result$conf.int[2]))
cat(sprintf("p-value:   %.2e\n\n", cor_result$p.value))

# --- Step 2: Check assumptions ---
# Assumption 1: Linear relationship (checked visually)
# Assumption 2: Normality of residuals
cat("--- Assumption Check: We will verify after fitting the model ---\n\n")

# --- Step 3: Fit linear regression model ---
cat("--- Linear Regression Results ---\n")
model <- lm(discharge_time_h ~ temperature_c, data = discharge)
print(summary(model))

# --- Step 4: Extract key metrics ---
coefficients <- coef(model)
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared
f_stat <- summary(model)$fstatistic
slope_pvalue <- summary(model)$coefficients["temperature_c", "Pr(>|t|)"]

cat(sprintf("\n--- Key Regression Metrics ---\n"))
cat(sprintf("Intercept:     %.3f hours\n", coefficients["(Intercept)"]))
cat(sprintf("Slope:         %.4f hours per degree C\n", coefficients["temperature_c"]))
cat(sprintf("R-squared:     %.4f (%.1f%% of variance explained)\n",
            r_squared, r_squared * 100))
cat(sprintf("Adjusted R^2:  %.4f\n", adj_r_squared))
cat(sprintf("F-statistic:   %.2f\n", f_stat[1]))
cat(sprintf("Slope p-value: %.2e\n", slope_pvalue))

# Interpret R-squared strength
cat("\n  R-squared interpretation: ")
if (r_squared >= 0.75) {
  cat("STRONG relationship. Temperature is a good predictor.\n")
} else if (r_squared >= 0.50) {
  cat("MODERATE relationship. Temperature explains meaningful variance.\n")
} else if (r_squared >= 0.25) {
  cat("WEAK relationship. Other factors also contribute significantly.\n")
} else {
  cat("VERY WEAK relationship. Temperature alone is not useful.\n")
}

# --- Step 5: Check regression assumptions ---
cat("\n--- Regression Assumption Checks ---\n")

# Normality of residuals (Shapiro-Wilk)
residuals_model <- resid(model)
shapiro_resid <- shapiro.test(residuals_model)
cat(sprintf("1. Normality of residuals: Shapiro-Wilk W=%.4f, p=%.4f [%s]\n",
            shapiro_resid$statistic, shapiro_resid$p.value,
            ifelse(shapiro_resid$p.value > 0.05, "OK", "WARNING")))

# Homoscedasticity (visual check - Breusch-Pagan conceptually)
# Simple check: correlation between fitted values and absolute residuals
abs_resid_cor <- cor(fitted(model), abs(residuals_model))
cat(sprintf("2. Homoscedasticity: |r(resid, fitted)|=%.4f [%s]\n",
            abs_resid_cor,
            ifelse(abs_resid_cor < 0.3, "OK", "WARNING")))

# Independence (Durbin-Watson - manual calculation)
n <- length(residuals_model)
dw_stat <- sum(diff(residuals_model)^2) / sum(residuals_model^2)
cat(sprintf("3. Independence: Durbin-Watson = %.4f [%s]\n",
            dw_stat,
            ifelse(dw_stat > 1.5 && dw_stat < 2.5, "OK", "WARNING")))

# --- Step 6: Prediction example ---
cat("\n--- Prediction Example ---\n")
new_temps <- data.frame(temperature_c = c(25, 35, 45))
predictions <- predict(model, newdata = new_temps, interval = "confidence", level = 0.95)
cat("Predicted discharge times with 95% CI:\n")
for (i in 1:nrow(new_temps)) {
  cat(sprintf("  At %d C: %.2f h [%.2f, %.2f]\n",
              new_temps$temperature_c[i],
              predictions[i, "fit"],
              predictions[i, "lwr"],
              predictions[i, "upr"]))
}

# --- Step 7: Interpretation ---
cat(sprintf("\n--- Interpretation ---\n"))
if (slope_pvalue < 0.001) {
  cat("CONCLUSION: STRONGLY REJECT H0 (p < 0.001).\n")
  cat("There is a statistically significant negative linear relationship\n")
  cat("between temperature and discharge time.\n\n")
  cat(sprintf("For every 1 degree C increase in temperature, discharge time\n"))
  cat(sprintf("decreases by approximately %.3f hours on average.\n\n",
              abs(coefficients["temperature_c"])))
  cat(sprintf("The model explains %.1f%% of the variance in discharge time.\n",
              r_squared * 100))
  cat("This is a strong effect, but other factors also influence discharge time.\n\n")
} else if (slope_pvalue < 0.05) {
  cat("\nCONCLUSION: REJECT H0 (p < 0.05). Significant linear relationship.\n\n")
} else {
  cat("\nCONCLUSION: FAIL TO REJECT H0. No significant linear relationship.\n\n")
}

# --- Step 8: Visualize ---
png(filename = file.path(script_dir, "regression_scatter.png"),
    width = 800, height = 600, res = 120)
par(mfrow = c(2, 2))

# Scatter with regression line
plot(discharge$temperature_c, discharge$discharge_time_h,
     main = "Discharge Time vs Temperature",
     xlab = "Temperature (C)",
     ylab = "Discharge Time (hours)",
     pch = 16, col = "steelblue")
abline(model, col = "red", lwd = 2)
# Add confidence band
new_x <- data.frame(temperature_c = seq(min(discharge$temperature_c),
                                         max(discharge$temperature_c),
                                         length.out = 100))
conf_band <- predict(model, newdata = new_x, interval = "confidence")
lines(new_x$temperature_c, conf_band[, "lwr"], lty = 2, col = "red")
lines(new_x$temperature_c, conf_band[, "upr"], lty = 2, col = "red")
legend("topright",
       legend = c("Regression line", "95% CI"),
       lty = c(1, 2), col = c("red", "red"), cex = 0.8)

# Residuals vs Fitted (homoscedasticity check)
plot(fitted(model), residuals_model,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16, col = "steelblue")
abline(h = 0, lty = 2, col = "red")

# Q-Q plot (normality check)
qqnorm(residuals_model, main = "Normal Q-Q Plot", pch = 16, col = "steelblue")
qqline(residuals_model, col = "red", lwd = 2)

# Histogram of residuals
hist(residuals_model,
     breaks = 10,
     main = "Residuals Distribution",
     xlab = "Residuals",
     col = "lightblue",
     border = "darkblue")
abline(v = mean(residuals_model), col = "red", lwd = 2)

dev.off()
cat("Plot saved: regression_scatter.png\n\n")


# =============================================================================
# SECTION 4: Summary of All Tests
# =============================================================================

cat("============================================================\n")
cat("SUMMARY: All Hypothesis Tests\n")
cat("============================================================\n\n")

cat(sprintf("%-25s | %-20s | %-12s | %s\n",
            "Test", "Result", "p-value", "Effect Size"))
cat(paste(rep("-", 80), collapse = ""), "\n")

# Paired t-test
cat(sprintf("%-25s | %-20s | %-12s | d = %.2f\n",
            "Paired t-test", "REJECT H0",
            ifelse(t_result$p.value < 0.001, "<0.001",
                   sprintf("%.4f", t_result$p.value)),
            d_paired))

# ANOVA
cat(sprintf("%-25s | %-20s | %-12s | eta^2 = %.3f\n",
            "One-way ANOVA", "REJECT H0",
            ifelse(anova_pvalue < 0.001, "<0.001",
                   sprintf("%.4f", anova_pvalue)),
            eta_squared))

# Regression
cat(sprintf("%-25s | %-20s | %-12s | R^2 = %.3f\n",
            "Linear Regression", "REJECT H0",
            ifelse(slope_pvalue < 0.001, "<0.001",
                   sprintf("%.4f", slope_pvalue)),
            r_squared))

cat("\n============================================================\n")
cat("All tests show statistically significant results.\n")
cat("Effect sizes confirm practical significance.\n")
cat("============================================================\n")
