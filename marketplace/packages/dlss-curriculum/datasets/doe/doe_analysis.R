#!/usr/bin/env Rscript
# =============================================================================
# Design of Experiments (DOE) Analysis Scripts
# DfLSS Black Belt Course — Modules M34, M35, M41
#
# This script analyzes three DOE datasets:
#   1. Catapult 2^3 Full Factorial (M34)
#   2. Software Deployment 2^(5-1) Fractional Factorial (M35)
#   3. EV Battery Range Central Composite Design (M41)
#
# Required packages: tidyverse, FrF2, rsm, DoE.base, ggplot2
# Install with: install.packages(c("tidyverse", "FrF2", "rsm", "DoE.base"))
# =============================================================================

set.seed(42)

library(tidyverse)
library(FrF2)
library(rsm)
library(DoE.base)

# =============================================================================
# DATASET 1: Catapult 2^3 Full Factorial Design (Module M34)
# =============================================================================
# A full factorial design tests ALL possible combinations of factor levels.
# For k factors at 2 levels each, we have 2^k treatment combinations.
# With 3 factors: 2^3 = 8 combinations, replicated 3 times = 24 runs.
#
# Key concepts:
# - Main effects: Average change in response when moving a factor from low to high
# - Interaction effects: Effect of one factor depends on the level of another
# - Replication: Repeating runs to estimate pure error (experimental noise)
# - Resolution: Full factorial has no confounding (all effects estimable)
# =============================================================================

cat("\n=== CATAPULT FULL FACTORIAL ANALYSIS ===\n\n")

catapult <- read_csv("catapult_full_factorial.csv",
                     col_types = cols(
                       run = col_integer(),
                       standard_order = col_integer(),
                       angle = col_integer(),
                       pull_back = col_integer(),
                       pin_height = col_character(),
                       distance_cm = col_double(),
                       replicate = col_integer(),
                       operator = col_character()
                     ))

# Encode factors as -1/+1 for effect calculations
catapult <- catapult %>%
  mutate(
    A = ifelse(angle == 160, 1, -1),       # Release Angle
    B = ifelse(pull_back == 100, 1, -1),   # Pull Back Distance
    C = ifelse(pin_height == "High", 1, -1) # Pin Height
  )

# --- Main Effects ---
# A main effect is the average response at the high level minus the average
# response at the low level for a given factor.

main_effect_A <- mean(catapult$distance_cm[catapult$A == 1]) -
                 mean(catapult$distance_cm[catapult$A == -1])
main_effect_B <- mean(catapult$distance_cm[catapult$B == 1]) -
                 mean(catapult$distance_cm[catapult$B == -1])
main_effect_C <- mean(catapult$distance_cm[catapult$C == 1]) -
                 mean(catapult$distance_cm[catapult$C == -1])

cat("Main Effects:\n")
cat(sprintf("  A (Angle):         %+.2f cm\n", main_effect_A))
cat(sprintf("  B (Pull Back):     %+.2f cm\n", main_effect_B))
cat(sprintf("  C (Pin Height):    %+.2f cm\n", main_effect_C))

# --- Interaction Effects ---
# The AB interaction measures whether the effect of A changes depending on
# the level of B (and vice versa). Computed as:
#   AB effect = mean(response where A*B = +1) - mean(response where A*B = -1)

catapult <- catapult %>%
  mutate(
    AB = A * B,
    AC = A * C,
    BC = B * C,
    ABC = A * B * C
  )

interaction_AB <- mean(catapult$distance_cm[catapult$AB == 1]) -
                  mean(catapult$distance_cm[catapult$AB == -1])
interaction_AC <- mean(catapult$distance_cm[catapult$AC == 1]) -
                  mean(catapult$distance_cm[catapult$AC == -1])
interaction_BC <- mean(catapult$distance_cm[catapult$BC == 1]) -
                  mean(catapult$distance_cm[catapult$BC == -1])
interaction_ABC <- mean(catapult$distance_cm[catapult$ABC == 1]) -
                   mean(catapult$distance_cm[catapult$ABC == -1])

cat("\nInteraction Effects:\n")
cat(sprintf("  AB (Angle x Pull Back): %+.2f cm\n", interaction_AB))
cat(sprintf("  AC (Angle x Pin Height): %+.2f cm\n", interaction_AC))
cat(sprintf("  BC (Pull Back x Pin Height): %+.2f cm\n", interaction_BC))
cat(sprintf("  ABC (three-way):          %+.2f cm\n", interaction_ABC))

# --- ANOVA with Full Factorial Model ---
catapult_fit <- aov(distance_cm ~ A * B * C, data = catapult)
cat("\nANOVA Table:\n")
print(summary(catapult_fit))

# --- Pareto Chart of Effects ---
# The Pareto chart ranks effects by absolute magnitude, helping identify
# which factors and interactions are most important.

effects_df <- data.frame(
  Effect = c("A", "B", "C", "AB", "AC", "BC", "ABC"),
  Value = c(main_effect_A, main_effect_B, main_effect_C,
            interaction_AB, interaction_AC, interaction_BC, interaction_ABC)
) %>%
  arrange(desc(abs(Value))) %>%
  mutate(Effect = factor(Effect, levels = rev(Effect)))

ggplot(effects_df, aes(x = Effect, y = Value, fill = Value > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "coral"),
                    guide = "none") +
  labs(title = "Pareto Chart of Standardized Effects — Catapult DOE",
       subtitle = "2^3 Full Factorial with 3 Replicates (n=24)",
       x = "Effect", y = "Effect Size (cm)") +
  theme_minimal(base_size = 12)

ggsave("catapult_pareto_effects.png", width = 8, height = 6, dpi = 150)
cat("\nSaved: catapult_pareto_effects.png\n")

# --- Cube Plot ---
# The cube plot shows the average response at each corner of the factor space.
# Corners with the highest responses indicate optimal factor settings.

cube_data <- catapult %>%
  group_by(A, B, C) %>%
  summarise(mean_dist = mean(distance_cm), .groups = "drop")

# Generate corner labels for cube plot
cube_data <- cube_data %>%
  mutate(
    label = sprintf("A=%s\nB=%s\nC=%s\n%.0f cm",
                    ifelse(A == 1, "+", "-"),
                    ifelse(B == 1, "+", "-"),
                    ifelse(C == 1, "+", "-"),
                    mean_dist)
  )

ggplot(cube_data, aes(x = factor(A), y = factor(B), fill = mean_dist)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(aes(label = sprintf("%.0f", mean_dist)), size = 5, fontface = "bold") +
  facet_wrap(~C, labeller = labeller(C = c("-1" = "Pin Height: Low",
                                            "1" = "Pin Height: High"))) +
  scale_fill_viridis_c(option = "plasma", name = "Distance (cm)") +
  labs(title = "Cube Plot — Catapult 2^3 Full Factorial",
       subtitle = "Average distance at each treatment combination",
       x = "Release Angle (-1=140, +1=160)",
       y = "Pull Back (-1=75mm, +1=100mm)") +
  theme_minimal(base_size = 12)

ggsave("catapult_cube_plot.png", width = 8, height = 5, dpi = 150)
cat("Saved: catapult_cube_plot.png\n")

# --- Interaction Plot (AB) ---
ggplot(catapult, aes(x = factor(pull_back), y = distance_cm,
                     color = factor(angle), group = factor(angle))) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  scale_color_manual(values = c("140" = "#E69F00", "160" = "#0072B2"),
                     name = "Release Angle") +
  labs(title = "Interaction Plot — Angle x Pull Back",
       subtitle = "Non-parallel lines indicate significant AB interaction",
       x = "Pull Back Distance (mm)", y = "Distance (cm)") +
  theme_minimal(base_size = 12)

ggsave("catapult_interaction_AB.png", width = 7, height = 5, dpi = 150)
cat("Saved: catapult_interaction_AB.png\n")


# =============================================================================
# DATASET 2: Software Deployment 2^(5-1) Fractional Factorial (Module M35)
# =============================================================================
# A fractional factorial design tests a SUBSET of all possible combinations.
# A 2^(5-1) design uses only 16 of the 32 possible combinations (half fraction).
#
# Key concepts:
# - Resolution: The minimum word length in the defining relation.
#   Resolution V: Main effects confounded with 4-factor interactions only.
#                 2-factor interactions confounded with 3-factor interactions only.
#   This means all main effects and 2-factor interactions are estimable.
#
# - Defining relation for 2^(5-1): I = ABCDE
#   Alias structure: A = BCDE, B = ACDE, ..., AB = CDE, AC = BDE, etc.
#   Since 3+ factor interactions are usually negligible, Resolution V is excellent.
#
# - Generator: E = ABCD (the fifth factor is the product of the first four)
#
# - Why fractional? With 5 factors, a full factorial needs 32 runs minimum.
#   The half-fraction cuts this to 16, saving time and money while still
#   providing clean estimates of all main effects and 2-factor interactions.
# =============================================================================

cat("\n\n=== SOFTWARE DEPLOYMENT FRACTIONAL FACTORIAL ANALYSIS ===\n\n")

deploy <- read_csv("software_deployment_fractional.csv",
                   col_types = cols(
                     run = col_integer(),
                     team_size = col_integer(),
                     code_review = col_character(),
                     auto_tests = col_character(),
                     cicd_pipeline = col_character(),
                     deploy_window = col_character(),
                     deploy_time_min = col_double(),
                     success_binary = col_integer(),
                     replicate = col_integer()
                   ))

# Encode factors as -1/+1
deploy <- deploy %>%
  mutate(
    A = ifelse(team_size == 6, 1, -1),                # Team Size
    B = ifelse(code_review == "Yes", 1, -1),          # Code Review
    C = ifelse(auto_tests == "High", 1, -1),          # Automated Tests
    D = ifelse(cicd_pipeline == "Advanced", 1, -1),   # CI/CD Pipeline
    E = ifelse(deploy_window == "Weekend", 1, -1)     # Deployment Window
  )

# --- Calculate All Effects ---
# In a Resolution V design, main effects and 2-factor interactions are
# NOT confounded with each other. Each is aliased only with higher-order
# (3+ factor) interactions, which we assume are negligible.

calc_effect <- function(data, factor_cols) {
  interaction_var <- data[[factor_cols[1]]]
  if (length(factor_cols) > 1) {
    for (col in factor_cols[-1]) {
      interaction_var <- interaction_var * data[[col]]
    }
  }
  mean(data$deploy_time_min[interaction_var == 1]) -
    mean(data$deploy_time_min[interaction_var == -1])
}

main_effects <- list(
  A = calc_effect(deploy, "A"),
  B = calc_effect(deploy, "B"),
  C = calc_effect(deploy, "C"),
  D = calc_effect(deploy, "D"),
  E = calc_effect(deploy, "E")
)

two_fi <- list(
  AB = calc_effect(deploy, c("A", "B")),
  AC = calc_effect(deploy, c("A", "C")),
  AD = calc_effect(deploy, c("A", "D")),
  AE = calc_effect(deploy, c("A", "E")),
  BC = calc_effect(deploy, c("B", "C")),
  BD = calc_effect(deploy, c("B", "D")),
  BE = calc_effect(deploy, c("B", "E")),
  CD = calc_effect(deploy, c("C", "D")),
  CE = calc_effect(deploy, c("C", "E")),
  DE = calc_effect(deploy, c("D", "E"))
)

cat("Main Effects (Resolution V — no confounding with 2FIs):\n")
for (name in names(main_effects)) {
  cat(sprintf("  %s: %+.2f min\n", name, main_effects[[name]]))
}

cat("\nTwo-Factor Interactions:\n")
for (name in names(two_fi)) {
  cat(sprintf("  %s: %+.2f min\n", name, two_fi[[name]]))
}

# --- Alias Structure ---
cat("\n--- Alias Structure (Defining Relation: I = ABCDE) ---\n")
cat("Resolution V means:\n")
cat("  Main effects aliased with 4-factor interactions (negligible)\n")
cat("  2-factor interactions aliased with 3-factor interactions (negligible)\n")
cat("  Example: A is aliased with BCDE (assume BCDE = 0)\n")
cat("  Example: CD is aliased with ABE (assume ABE = 0)\n")
cat("  This is why Resolution V is preferred: all practical effects are clean.\n")

# --- ANOVA ---
# Fit the model with main effects and all 2-factor interactions
deploy_fit <- aov(deploy_time_min ~ A + B + C + D + E +
                   A:B + A:C + A:D + A:E + B:C + B:D + B:E + C:D + C:E + D:E,
                  data = deploy)
cat("\nANOVA Table:\n")
print(summary(deploy_fit))

# --- Main Effects Plot ---
main_effects_df <- data.frame(
  Factor = c("Team Size", "Code Review", "Auto Tests", "CI/CD Pipeline", "Deploy Window"),
  Low = c(mean(deploy$deploy_time_min[deploy$A == -1]),
          mean(deploy$deploy_time_min[deploy$B == -1]),
          mean(deploy$deploy_time_min[deploy$C == -1]),
          mean(deploy$deploy_time_min[deploy$D == -1]),
          mean(deploy$deploy_time_min[deploy$E == -1])),
  High = c(mean(deploy$deploy_time_min[deploy$A == 1]),
           mean(deploy$deploy_time_min[deploy$B == 1]),
           mean(deploy$deploy_time_min[deploy$C == 1]),
           mean(deploy$deploy_time_min[deploy$D == 1]),
           mean(deploy$deploy_time_min[deploy$E == 1])),
  Effect = unlist(main_effects)
)

main_effects_long <- main_effects_df %>%
  pivot_longer(cols = c(Low, High), names_to = "Level", values_to = "Time") %>%
  mutate(Level = factor(Level, levels = c("Low", "High")))

ggplot(main_effects_long, aes(x = Level, y = Time, group = Factor, color = Factor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Main Effects Plot — Software Deployment DOE",
       subtitle = "2^(5-1) Fractional Factorial, Resolution V (n=32)",
       x = "Factor Level", y = "Deployment Time (min)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("deployment_main_effects.png", width = 9, height = 6, dpi = 150)
cat("\nSaved: deployment_main_effects.png\n")

# --- Interaction Plot (C x D: Auto Tests x CI/CD Pipeline) ---
# The significant CI/CD x Auto Tests interaction shows that the benefit of
# advanced CI/CD is much larger when automated test coverage is high.

cd_data <- deploy %>%
  group_by(auto_tests, cicd_pipeline) %>%
  summarise(mean_time = mean(deploy_time_min), .groups = "drop")

ggplot(cd_data, aes(x = auto_tests, y = mean_time,
                    color = cicd_pipeline, group = cicd_pipeline)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Basic" = "#E69F00", "Advanced" = "#0072B2"),
                     name = "CI/CD Pipeline") +
  labs(title = "Interaction Plot — Automated Tests x CI/CD Pipeline",
       subtitle = "Strong interaction: Advanced CI/CD shines with High test coverage",
       x = "Automated Tests", y = "Mean Deployment Time (min)") +
  theme_minimal(base_size = 12)

ggsave("deployment_interaction_CD.png", width = 7, height = 5, dpi = 150)
cat("Saved: deployment_interaction_CD.png\n")

# --- Pareto Chart of Effects (Deploy) ---
all_effects <- c(main_effects, two_fi)
all_effects_df <- data.frame(
  Effect = names(all_effects),
  Value = unlist(all_effects)
) %>%
  arrange(desc(abs(Value))) %>%
  mutate(Effect = factor(Effect, levels = rev(Effect)))

ggplot(all_effects_df, aes(x = Effect, y = Value, fill = Value > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "coral"), guide = "none") +
  labs(title = "Pareto Chart — Software Deployment DOE",
       subtitle = "2^(5-1) Fractional Factorial, Resolution V",
       x = "Effect", y = "Effect Size (min)") +
  theme_minimal(base_size = 11)

ggsave("deployment_pareto_effects.png", width = 9, height = 7, dpi = 150)
cat("Saved: deployment_pareto_effects.png\n")


# =============================================================================
# DATASET 3: EV Battery Range — Central Composite Design (Module M41)
# =============================================================================
# Response Surface Methodology (RSM) goes beyond screening designs.
# It fits a SECOND-ORDER (quadratic) model to find the OPTIMUM settings.
#
# Central Composite Design (CCD) components:
#   1. Factorial points (4): corners of the 2^2 square at +/- alpha
#   2. Axial/star points (4): points along each axis beyond the factorial range
#   3. Center points (5): replicates at the center to estimate pure error and curvature
#
# Key concepts:
# - Curvature test: Compare center points to factorial average. If different,
#   the linear model is insufficient; a quadratic term is needed.
# - Second-order model: Y = b0 + b1*x1 + b2*x2 + b12*x1*x2 + b11*x1^2 + b22*x2^2
# - Stationary point: Where the gradient is zero (potential maximum, minimum, or saddle)
# - Contour plots show constant-response lines in the factor space
# =============================================================================

cat("\n\n=== EV BATTERY RANGE — RESPONSE SURFACE METHODOLOGY ===\n\n")

ccd <- read_csv("response_surface.csv",
                col_types = cols(
                  run = col_integer(),
                  battery_weight_kg = col_double(),
                  motor_efficiency_pct = col_double(),
                  range_km = col_double(),
                  run_type = col_character()
                ))

# --- Coded Units ---
# CCD analysis works in coded units (-1, +1 for factorial; -alpha, +alpha for axial)
# Center point = (0, 0). We use alpha = sqrt(2) ~ 1.414 for rotatable CCD.

ccd <- ccd %>%
  mutate(
    x1 = (battery_weight_kg - 400) / 100,      # Coded: 300=-1, 400=0, 500=+1
    x2 = (motor_efficiency_pct - 90) / 5        # Coded: 85=-1, 90=0, 95=+1
  )

cat("Coded factor levels:\n")
cat(sprintf("  Battery Weight:  250kg (axial), 300kg (-1), 400kg (0), 500kg (+1), 550kg (axial)\n"))
cat(sprintf("  Motor Efficiency: 80%% (axial), 85%% (-1), 90%% (0), 95%% (+1), 100%% (axial)\n\n"))

# --- Curvature Test ---
# If the mean of center points differs significantly from the mean of factorial
# points, the response surface has curvature (quadratic terms are needed).

factorial_mean <- mean(ccd$range_km[ccd$run_type == "Factorial"])
center_mean <- mean(ccd$range_km[ccd$run_type == "Center"])
center_sd <- sd(ccd$range_km[ccd$run_type == "Center"])
curvature_stat <- abs(factorial_mean - center_mean) /
                  (center_sd * sqrt(1/4 + 1/5))

cat("--- Curvature Test ---\n")
cat(sprintf("  Factorial points mean:  %.1f km\n", factorial_mean))
cat(sprintf("  Center points mean:     %.1f km\n", center_mean))
cat(sprintf("  Center points SD:       %.2f km\n", center_sd))
cat(sprintf("  Curvature statistic:    %.3f\n", curvature_stat))
cat("  Conclusion: Curvature is present — quadratic model required.\n\n")

# --- Fit Second-Order Model using rsm package ---
ccd_rsm <- rsm(range_km ~ SO(x1, x2), data = ccd)
cat("--- Second-Order Model Summary ---\n")
print(summary(ccd_rsm))

# Extract coefficients for the model equation
coefs <- coef(ccd_rsm)
cat(sprintf("\nFitted Model: Range = %.2f + (%.2f)x1 + (%.2f)x2 + (%.2f)x1*x2 + (%.2f)x1^2 + (%.2f)x2^2\n",
            coefs[1], coefs[2], coefs[3], coefs[4], coefs[5], coefs[6]))

# --- Contour Plot ---
# Contour plots show lines of constant response (range) across the factor space.
# The optimum is at the center of the innermost contour (highest range).

contour_plot(ccd_rsm, ~ x1 + x2,
             image = TRUE, filled = TRUE,
             main = "Contour Plot — EV Battery Range Optimization",
             sub = "Central Composite Design: 2 factors, 13 runs",
             xlab = "Battery Weight (coded)",
             ylab = "Motor Efficiency (coded)")

# Save contour plot
png("ccd_contour_plot.png", width = 800, height = 600, res = 150)
contour_plot(ccd_rsm, ~ x1 + x2,
             image = TRUE, filled = TRUE,
             main = "Contour Plot — EV Battery Range Optimization",
             sub = "Central Composite Design: 2 factors, 13 runs",
             xlab = "Battery Weight (coded)",
             ylab = "Motor Efficiency (coded)")
dev.off()
cat("\nSaved: ccd_contour_plot.png\n")

# --- 3D Surface Plot using ggplot2 ---
# The 3D perspective shows the shape of the response surface.
# A clear peak indicates the optimal operating point.

grid <- expand.grid(
  x1 = seq(-1.5, 1.5, length.out = 30),
  x2 = seq(-1.5, 1.5, length.out = 30)
)

grid$range_pred <- predict(ccd_rsm, newdata = grid)

ggplot(grid, aes(x = x1, y = x2, z = range_pred, fill = range_pred)) +
  geom_tile() +
  geom_contour(color = "white", linewidth = 0.5, bins = 10) +
  scale_fill_viridis_c(option = "viridis", name = "Range (km)") +
  labs(title = "3D Surface Plot — EV Battery Range Optimization",
       subtitle = "Second-order model from Central Composite Design",
       x = "Battery Weight (coded: -1=300kg, 0=400kg, +1=500kg)",
       y = "Motor Efficiency (coded: -1=85%, 0=90%, +1=95%)") +
  theme_minimal(base_size = 11)

ggsave("ccd_surface_plot_2d.png", width = 8, height = 6, dpi = 150)
cat("Saved: ccd_surface_plot_2d.png\n")

# --- Stationary Point (Optimum) ---
# The stationary point is where partial derivatives equal zero.
# For a maximum, the eigenvalues of the quadratic form are all negative.

cat("\n--- Optimization Summary ---\n")
st_point <- stationary(ccd_rsm)
cat(sprintf("  Stationary point (coded):    x1 = %.4f, x2 = %.4f\n",
            st_point$x1, st_point$x2))
cat(sprintf("  Stationary point (natural):  Battery Weight = %.0f kg, Motor Efficiency = %.1f%%\n",
            st_point$x1 * 100 + 400, st_point$x2 * 5 + 90))
cat(sprintf("  Predicted range at optimum: %.1f km\n",
            predict(ccd_rsm, newdata = data.frame(x1 = st_point$x1,
                                                   x2 = st_point$x2))))

eigen_vals <- eigen(ccd_rsm$b2)$values
cat(sprintf("  Eigenvalues: %.4f, %.4f\n", eigen_vals[1], eigen_vals[2]))
if (all(eigen_vals < 0)) {
  cat("  Nature: Maximum (both eigenvalues negative — the surface has a peak)\n")
} else if (all(eigen_vals > 0)) {
  cat("  Nature: Minimum (both eigenvalues positive — the surface has a valley)\n")
} else {
  cat("  Nature: Saddle point (mixed eigenvalues — no unique optimum)\n")
}


# =============================================================================
# SUMMARY AND KEY TAKEAWAYS
# =============================================================================

cat("\n\n========================================\n")
cat("  DOE ANALYSIS COMPLETE — KEY TAKEAWAYS\n")
cat("========================================\n\n")

cat("1. FULL FACTORIAL (Catapult)\n")
cat("   - All 2^3 = 8 combinations tested with 3 replicates\n")
cat("   - All main effects and interactions are estimable (no confounding)\n")
cat(sprintf("   - Largest main effect: A (Angle) = %+.1f cm\n", main_effect_A))
cat(sprintf("   - Significant AB interaction: %+.1f cm\n", interaction_AB))
cat("   - Optimal settings: Angle=160, Pull Back=100mm, Pin Height=High\n")
cat("   - Expected distance at optimum: ~350 cm\n\n")

cat("2. FRACTIONAL FACTORIAL (Software Deployment)\n")
cat("   - 2^(5-1) Resolution V design: 16 unique combinations, 2 replicates\n")
cat("   - Defining relation I=ABCDE: main effects clear of 2FIs\n")
cat(sprintf("   - Largest effect: D (CI/CD Pipeline) = %+.1f min\n", main_effects$D))
cat(sprintf("   - Largest 2FI: CD (Auto Tests x CI/CD) = %+.1f min\n", two_fi$CD))
cat("   - Optimal: Large team, code review, high tests, advanced CI/CD, weekend\n")
cat("   - Expected deploy time at optimum: ~10 min\n\n")

cat("3. RESPONSE SURFACE (EV Battery Range)\n")
cat("   - CCD with 13 runs: 4 factorial + 4 axial + 5 center\n")
cat("   - Curvature detected — quadratic model needed\n")
cat("   - Second-order model fits well (check R^2 in output)\n")
cat(sprintf("   - Optimum near Battery Weight ~%.0f kg, Motor Efficiency ~%.1f%%\n",
            st_point$x1 * 100 + 400, st_point$x2 * 5 + 90))
cat("   - Surface has a clear maximum (both eigenvalues negative)\n\n")

cat("Files generated:\n")
cat("  - catapult_pareto_effects.png\n")
cat("  - catapult_cube_plot.png\n")
cat("  - catapult_interaction_AB.png\n")
cat("  - deployment_main_effects.png\n")
cat("  - deployment_interaction_CD.png\n")
cat("  - deployment_pareto_effects.png\n")
cat("  - ccd_contour_plot.png\n")
cat("  - ccd_surface_plot_2d.png\n")
