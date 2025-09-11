# === Combined EFA Wellbeing Factor Score and Analysis Script ===

# Load environment and data
source("scripts/01_setup_environment.R")
source("scripts/02_load_clean_data.R")

# Load additional packages for EFA
library(psych)      # For EFA and factor analysis
library(GPArotation) # For factor rotation methods

# Use df_model_base as the analysis dataset
df_analysis <- df_model_base
cat("Using df_model_base as analysis dataset:", nrow(df_analysis), "rows x", ncol(df_analysis), "cols\n")

# ===================================================================
# PART 1: EFA ANALYSIS
# ===================================================================

cat("\n", rep("=", 60), "\n")
cat("PART 1: EXPLORATORY FACTOR ANALYSIS\n")
cat(rep("=", 60), "\n")

# Select the wellbeing variables for EFA
wellbeing_vars <- c("Happy", "Satis", "GnrlHealth")

# Check that all variables exist in the dataset
if (!all(wellbeing_vars %in% names(df_analysis))) {
  missing_vars <- wellbeing_vars[!wellbeing_vars %in% names(df_analysis)]
  available_vars <- names(df_analysis)[grepl("Happy|Satis|Health", names(df_analysis), ignore.case = TRUE)]
  cat("Available similar variables:", paste(available_vars, collapse = ", "), "\n")
  stop(paste("Variables not found:", paste(missing_vars, collapse = ", ")))
}

cat("Found all wellbeing variables:", paste(wellbeing_vars, collapse = ", "), "\n")

# Create subset with complete cases for EFA variables
efa_data <- df_analysis %>%
  select(all_of(wellbeing_vars)) %>%
  na.omit()

cat("EFA dataset dimensions:", nrow(efa_data), "observations x", ncol(efa_data), "variables\n")

# Examine correlations
cat("\nCorrelation matrix for wellbeing variables:\n")
cor_matrix <- cor(efa_data)
print(round(cor_matrix, 3))

# Check if correlations are suitable for factor analysis
# Kaiser-Meyer-Olkin (KMO) test
kmo_result <- KMO(efa_data)
cat("\nKMO Overall MSA:", round(kmo_result$MSA, 3), "\n")
cat("KMO Individual MSA values:\n")
print(round(kmo_result$MSAi, 3))

# Bartlett's test of sphericity
bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(efa_data))
cat("\nBartlett's test of sphericity:\n")
cat("Chi-square:", round(bartlett_result$chisq, 2), "\n")
cat("p-value:", format.pval(bartlett_result$p.value), "\n")

# Determine number of factors using parallel analysis
cat("\n=== FACTOR NUMBER DETERMINATION ===\n")
parallel_result <- fa.parallel(efa_data, fm = "ml", fa = "fa", n.iter = 100)
cat("Parallel analysis suggests", parallel_result$nfact, "factor(s)\n")

# Perform EFA with 1 factor (expected for wellbeing)
efa_result <- fa(efa_data, nfactors = 1, rotate = "none", fm = "ml")

cat("\n=== EFA RESULTS (1 Factor) ===\n")
print(efa_result, cut = 0.3, sort = TRUE)

# Factor loadings
cat("\nFactor loadings:\n")
loadings_matrix <- as.matrix(efa_result$loadings)
print(round(loadings_matrix, 3))

# Communalities
cat("\nCommunalities (h2):\n")
print(round(efa_result$communality, 3))

# Extract factor scores for the complete dataset
# Create a version with the same variables but allowing missing values
efa_data_full <- df_analysis %>%
  select(all_of(wellbeing_vars))

# Calculate factor scores using regression method
factor_scores <- fa(efa_data_full, nfactors = 1, rotate = "none", fm = "ml", 
                    scores = "regression", missing = TRUE)

# Add factor scores to main dataset
df_analysis$Wellbeing_Factor <- as.numeric(factor_scores$scores[, 1])

# Standardize factor scores (mean = 0, SD = 1)
df_analysis$Wellbeing_Factor_Std <- scale(df_analysis$Wellbeing_Factor)[, 1]

# Create 0-1 scaled version for consistency with other outcomes
df_analysis$Wellbeing_Factor_01 <- scales::rescale(df_analysis$Wellbeing_Factor, to = c(0, 1))

cat("\n=== FACTOR SCORE DESCRIPTIVES ===\n")
cat("Raw factor scores:\n")
print(summary(df_analysis$Wellbeing_Factor))
cat("\nStandardized factor scores:\n")  
print(summary(df_analysis$Wellbeing_Factor_Std))
cat("\n0-1 scaled factor scores:\n")
print(summary(df_analysis$Wellbeing_Factor_01))

# Check correlations with original variables
cat("\nCorrelations between factor score and original variables:\n")
factor_cors <- df_analysis %>%
  select(Wellbeing_Factor_01, all_of(wellbeing_vars)) %>%
  cor(use = "complete.obs")
print(round(factor_cors[1, -1], 3))

# Create summary table
wellbeing_efa_summary <- data.frame(
  Variable = wellbeing_vars,
  Loading = as.numeric(loadings_matrix[, 1]),
  Communality = efa_result$communality,
  Mean = sapply(efa_data, mean),
  SD = sapply(efa_data, sd)
)

cat("\n=== EFA SUMMARY TABLE ===\n")
print(wellbeing_efa_summary)

cat("\n✓ EFA analysis complete!\n")
cat("✓ Wellbeing factor score created: 'Wellbeing_Factor_01' (0-1 scale)\n")

# ===================================================================
# PART 2: REGRESSION ANALYSIS
# ===================================================================

cat("\n", rep("=", 60), "\n")
cat("PART 2: REGRESSION ANALYSIS\n")
cat(rep("=", 60), "\n")

# Prepare data for regression analysis (matching your working script structure)
df_model <- df_analysis %>%
  filter(
    !is.na(Wellbeing_Factor_01),
    !is.na(T_mode),
    !is.na(Cur_Sex)
  ) %>%
  mutate(
    # Create properly coded variables for model (matching your working script)
    CAWI = ifelse(T_mode == "CAWI", 1, 0),  # Dummy for CAWI vs CATI
    Male = ifelse(Cur_Sex == "2", 1, 0)     # Dummy for Male vs Female
  )

cat("Wellbeing Factor analysis sample:", nrow(df_model), "observations\n")

# Create survey design object for proper standard errors (if weights available)
if ("May25_Weight_calib" %in% names(df_model)) {
  svy_design <- svydesign(
    ids = ~1,
    weights = ~May25_Weight_calib,
    data = df_model
  )
  use_survey <- TRUE
} else {
  cat("No weights found - using unweighted analysis\n")
  use_survey <- FALSE
}

# Fit models
if (use_survey) {
  # Using survey package for weighted analysis
  model_interaction <- svyglm(
    Wellbeing_Factor_01 ~ CAWI * Male,
    design = svy_design,
    family = gaussian()
  )
  
  model_main <- svyglm(
    Wellbeing_Factor_01 ~ CAWI + Male,
    design = svy_design,
    family = gaussian()
  )
} else {
  # Using regular lm for unweighted analysis
  model_interaction <- lm(Wellbeing_Factor_01 ~ CAWI * Male, data = df_model)
  model_main <- lm(Wellbeing_Factor_01 ~ CAWI + Male, data = df_model)
}

# Print model sample sizes
cat("Model sample sizes:\n")
cat("- Interaction model:", nobs(model_interaction), "observations\n")
cat("- Main effects model:", nobs(model_main), "observations\n")

# Calculate ATE estimates from coefficients (CAWI→CATI interpretation)
beta1 <- coef(model_interaction)["CAWI"]
beta3 <- coef(model_interaction)["CAWI:Male"] 

ATE_women <- -beta1  # Effect of switching from CAWI to CATI for women
ATE_men <- -(beta1 + beta3)  # Effect of switching from CAWI to CATI for men
ATE_main <- -coef(model_main)["CAWI"]  # Overall effect of switching from CAWI to CATI

# Standard errors
se_beta1 <- sqrt(vcov(model_interaction)["CAWI", "CAWI"])
se_beta3 <- sqrt(vcov(model_interaction)["CAWI:Male", "CAWI:Male"])
se_main <- sqrt(vcov(model_main)["CAWI", "CAWI"])

# For ATE_men, need variance of β1 + β3
cov_beta1_beta3 <- vcov(model_interaction)["CAWI", "CAWI:Male"]
se_men <- sqrt(vcov(model_interaction)["CAWI", "CAWI"] + 
                 vcov(model_interaction)["CAWI:Male", "CAWI:Male"] + 
                 2 * cov_beta1_beta3)

# Print ATE results
cat("\n=== ATE ESTIMATES (CAWI → CATI) ===\n")
cat("- Overall (main effect):", sprintf("%.4f (SE: %.4f)", ATE_main, se_main), "\n")
cat("- Women:", sprintf("%.4f (SE: %.4f)", ATE_women, se_beta1), "\n")
cat("- Men:", sprintf("%.4f (SE: %.4f)", ATE_men, se_men), "\n")
cat("- Sex difference:", sprintf("%.4f (SE: %.4f)", -beta3, se_beta3), "\n")

# P-values for significance tests
p_main <- 2 * (1 - pt(abs(ATE_main / se_main), df = df.residual(model_main)))
p_interaction <- 2 * (1 - pt(abs(-beta3 / se_beta3), df = df.residual(model_interaction)))

cat("\n=== SIGNIFICANCE TESTS ===\n")
cat("- Main effect p-value:", format.pval(p_main), "\n")
cat("- Interaction p-value:", format.pval(p_interaction), "\n")

if (p_interaction < 0.1) {
  cat("*** Significant interaction at p < 0.1 ***\n")
} else {
  cat("Non-significant interaction (p >= 0.1)\n")
}

# === DESCRIPTIVE STATISTICS ===
cat("\n=== WELLBEING FACTOR DESCRIPTIVES BY TREATMENT ===\n")

descriptives <- df_model %>%
  group_by(T_mode) %>%
  summarise(
    n = n(),
    mean = mean(Wellbeing_Factor_01, na.rm = TRUE),
    sd = sd(Wellbeing_Factor_01, na.rm = TRUE),
    median = median(Wellbeing_Factor_01, na.rm = TRUE),
    min = min(Wellbeing_Factor_01, na.rm = TRUE),
    max = max(Wellbeing_Factor_01, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(descriptives)

# By treatment and sex
cat("\n=== WELLBEING FACTOR DESCRIPTIVES BY TREATMENT AND SEX ===\n")

descriptives_by_sex <- df_model %>%
  group_by(T_mode, Cur_Sex) %>%
  summarise(
    n = n(),
    mean = mean(Wellbeing_Factor_01, na.rm = TRUE),
    sd = sd(Wellbeing_Factor_01, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(descriptives_by_sex)

# Create results table for display
results_table <- data.frame(
  Group = c("Overall", "Women", "Men"),
  ATE = c(ATE_main, ATE_women, ATE_men),
  SE = c(se_main, se_beta1, se_men),
  CI_lower = c(ATE_main - 1.96*se_main, ATE_women - 1.96*se_beta1, ATE_men - 1.96*se_men),
  CI_upper = c(ATE_main + 1.96*se_main, ATE_women + 1.96*se_beta1, ATE_men + 1.96*se_men),
  stringsAsFactors = FALSE
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

cat("\n=== FINAL RESULTS TABLE ===\n")
print(results_table)

# ===================================================================
# PART 3: VISUALIZATION
# ===================================================================

cat("\n", rep("=", 60), "\n")
cat("PART 3: VISUALIZATION\n")
cat(rep("=", 60), "\n")

# Create plotting data from ATE coefficients
plot_data <- data.frame(
  Sex = factor(c("Female", "Male"), levels = c("Female", "Male")),
  ATE = c(ATE_women, ATE_men),
  SE = c(se_beta1, se_men)
)

# Main effect ATE
main_effect_data <- data.frame(
  ATE = ATE_main,
  SE = se_main
)

# P-value labels for annotations
main_p_label <- paste0("Main effect p = ", ifelse(p_main < 0.001, "<0.001", sprintf("%.3f", p_main)))
int_p_label <- paste0("Interaction p = ", ifelse(p_interaction < 0.001, "<0.001", sprintf("%.3f", p_interaction)))

# Create plot showing ATE coefficients (treatment effects)
plot_wellbeing <- ggplot() +
  # ATE by sex
  geom_col(data = plot_data, 
           aes(x = Sex, y = ATE, fill = Sex), 
           width = 0.6) +
  
  # Main ATE (overall effect)
  geom_col(data = main_effect_data,
           aes(x = 1.5, y = ATE), 
           width = 0.2, 
           fill = "gray85",
           alpha = 0.9) +
  
  # Reference line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Labels showing ATE values
  geom_text(data = plot_data, 
            aes(x = Sex, y = ATE, label = sprintf("%.3f", ATE)), 
            vjust = ifelse(plot_data$ATE >= 0, -0.5, 1.5), size = 3.5) +
  
  geom_text(data = main_effect_data,
            aes(x = 1.5, y = ATE, label = sprintf("%.3f", ATE)),
            vjust = ifelse(main_effect_data$ATE >= 0, -0.5, 1.5), 
            size = 3, color = "gray20", fontface = "bold") +
  
  annotate("text", x = 1.5, y = min(c(plot_data$ATE, main_effect_data$ATE)) - 0.02, 
           label = "Main effect", size = 2.5, color = "gray20", fontface = "italic") +
  
  # P-value annotations
  annotate("text", x = 1, y = max(c(plot_data$ATE, main_effect_data$ATE)) + 0.02, 
           label = main_p_label, size = 3, fontface = "italic") +
  annotate("text", x = 2, y = max(c(plot_data$ATE, main_effect_data$ATE)) + 0.02, 
           label = int_p_label, size = 3, fontface = "italic") +
  
  scale_fill_manual(values = c("Female" = "gray60", "Male" = "gray30")) +
  labs(
    #title = "Wellbeing Factor Score: Treatment Effect (CAWI → CATI)",
    subtitle = "Factor derived from EFA of Happiness, Life Satisfaction, and General Health",
    x = "Sex", 
    y = "Average Treatment Effect (CAWI → CATI)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60")
  )

# Display plot
if (interactive()) print(plot_wellbeing)

cat("✓ Plot created and displayed\n")

# ===================================================================
# FINAL SUMMARY
# ===================================================================

cat("\n", rep("=", 60), "\n")
cat("COMBINED ANALYSIS COMPLETE\n")
cat(rep("=", 60), "\n")
cat("✓ EFA performed on Happy, Satis, GnrlHealth\n")
cat("✓ Single wellbeing factor extracted\n") 
cat("✓ Factor scores created and scaled to 0-1\n")
cat("✓ Treatment effects analyzed (full sample + by sex)\n")
cat("✓ Interaction effects tested\n")
cat("✓ All results displayed above\n")