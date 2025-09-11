# === Drink Alcohol outcome analysis ===

# Load environment and data
source("scripts/01_setup_environment.R")
source("scripts/02_load_clean_data.R")

# Outcome-specific data preparation with 0-1 scaling
df_model <- df_model_base %>%
  mutate(DrinkFreq = as.numeric(as.character(DrinkFreq))) %>%
  filter(
    !is.na(DrinkFreq),
    !is.na(T_mode),
    !is.na(Cur_Sex),
    !is.na(Cur_AgeCat),
    !is.na(Cur_HEdQual),
    !is.na(Cur_Ethnic6),
    !is.na(Cur_IntUse3),
    !is.na(Cur_PartyID5),
    !is.na(Cur_RelStat5),
    !is.na(Cur_Tenure5),
    !is.na(Cur_HHType),
    !is.na(Cur_HHChild)
  ) %>%
  mutate(
    # Scale DrinkFreq to 0-1 range
    DrinkFreq_scaled = (DrinkFreq - min(DrinkFreq, na.rm = TRUE)) / 
      (max(DrinkFreq, na.rm = TRUE) - min(DrinkFreq, na.rm = TRUE)),
    # Create properly coded variables for model
    CAWI = ifelse(T_mode == "CAWI", 1, 0),  # Dummy for CAWI vs CATI
    Male = ifelse(Cur_Sex == "2", 1, 0)     # Dummy for Male vs Female
  )

cat("Drink Alcohol analysis sample: ", nrow(df_model), " observations\n")
cat("Original DrinkFreq range: ", min(df_model$DrinkFreq, na.rm = TRUE), " to ", max(df_model$DrinkFreq, na.rm = TRUE), "\n")
cat("Scaled DrinkFreq range: ", min(df_model$DrinkFreq_scaled, na.rm = TRUE), " to ", max(df_model$DrinkFreq_scaled, na.rm = TRUE), "\n")

# Create survey design object for proper standard errors
svy_design <- svydesign(
  ids = ~1,
  weights = ~May25_Weight_calib,
  data = df_model
)

# Fit models using survey package for proper weighted standard errors
model_interaction <- svyglm(
  DrinkFreq_scaled ~ CAWI * Male + Cur_AgeCat + Cur_HEdQual + Cur_Ethnic6 + 
    Cur_IntUse3 + Cur_PartyID5 + Cur_RelStat5 + Cur_Tenure5 + Cur_HHType + Cur_HHChild,
  design = svy_design,
  family = gaussian()
)

model_main <- svyglm(
  DrinkFreq_scaled ~ CAWI + Male + Cur_AgeCat + Cur_HEdQual + Cur_Ethnic6 + 
    Cur_IntUse3 + Cur_PartyID5 + Cur_RelStat5 + Cur_Tenure5 + Cur_HHType + Cur_HHChild,
  design = svy_design,
  family = gaussian()
)

# Print model sample sizes
cat("Model sample sizes:\n")
cat("- Interaction model: ", nobs(model_interaction), " observations\n")
cat("- Main effects model: ", nobs(model_main), " observations\n")

# Calculate ATE estimates from coefficients and flip signs for CAWI→CATI interpretation
beta1 <- coef(model_interaction)["CAWI"]
beta3 <- coef(model_interaction)["CAWI:Male"] 

ATE_women <- -beta1  # Effect of switching from CAWI to CATI for women
ATE_men <- -(beta1 + beta3)  # Effect of switching from CAWI to CATI for men
ATE_main <- -coef(model_main)["CAWI"]  # Overall effect of switching from CAWI to CATI

# Standard errors (same as before since we're just flipping signs)
se_beta1 <- sqrt(vcov(model_interaction)["CAWI", "CAWI"])
se_beta3 <- sqrt(vcov(model_interaction)["CAWI:Male", "CAWI:Male"])
se_main <- sqrt(vcov(model_main)["CAWI", "CAWI"])

# For ATE_men, need variance of β1 + β3
cov_beta1_beta3 <- vcov(model_interaction)["CAWI", "CAWI:Male"]
se_men <- sqrt(vcov(model_interaction)["CAWI", "CAWI"] + 
                 vcov(model_interaction)["CAWI:Male", "CAWI:Male"] + 
                 2 * cov_beta1_beta3)

# Print ATE results
cat("ATE Estimates (CAWI → CATI):\n")
cat("- Overall (main effect): ", sprintf("%.4f (SE: %.4f)", ATE_main, se_main), "\n")
cat("- Women: ", sprintf("%.4f (SE: %.4f)", ATE_women, se_beta1), "\n")
cat("- Men: ", sprintf("%.4f (SE: %.4f)", ATE_men, se_men), "\n")
cat("- Sex difference: ", sprintf("%.4f (SE: %.4f)", -beta3, se_beta3), "\n")

# === SAVE FOREST PLOT DATA ===
forest_data <- data.frame(
  outcome = "Drink_Alcohol",
  group = c("Full Sample", "Women", "Men"),
  estimate = c(ATE_main, ATE_women, ATE_men),
  std.error = c(se_main, se_beta1, se_men),
  conf.low = c(ATE_main - 1.96*se_main, ATE_women - 1.96*se_beta1, ATE_men - 1.96*se_men),
  conf.high = c(ATE_main + 1.96*se_main, ATE_women + 1.96*se_beta1, ATE_men + 1.96*se_men)
)

# Save to forest plot data file
forest_file <- "outputs_LPM/forest_plot_data.csv"
if (file.exists(forest_file)) {
  existing_forest <- read.csv(forest_file)
  updated_forest <- rbind(existing_forest[existing_forest$outcome != "Drink_Alcohol",], forest_data)
} else {
  updated_forest <- forest_data
}
write.csv(updated_forest, forest_file, row.names = FALSE)

# === SAVE MODEL RESULTS TO EXCEL FILES ===

# Prepare results 
main_effects_results <- broom::tidy(model_main)
interaction_results <- broom::tidy(model_interaction)

# File paths (save to outputs_LPM folder)
dir.create("outputs_LPM", showWarnings = FALSE)
main_effects_file <- "outputs_LPM/main_effects.xlsx"
interaction_file <- "outputs_LPM/interaction_models.xlsx"

# Handle main effects Excel file
existing_main_data <- list()
if (file.exists(main_effects_file)) {
  tryCatch({
    existing_sheets <- readxl::excel_sheets(main_effects_file)
    for (sheet in existing_sheets) {
      if (sheet != "Drink_Alcohol") {
        existing_main_data[[sheet]] <- readxl::read_xlsx(main_effects_file, sheet = sheet)
      }
    }
  }, error = function(e) {
    cat("Warning: Could not read existing main effects file, creating new one\n")
    existing_main_data <- list()
  })
}

existing_main_data[["Drink_Alcohol"]] <- main_effects_results
writexl::write_xlsx(existing_main_data, path = main_effects_file)

# Handle interaction models Excel file  
existing_int_data <- list()
if (file.exists(interaction_file)) {
  tryCatch({
    existing_sheets <- readxl::excel_sheets(interaction_file)
    for (sheet in existing_sheets) {
      if (sheet != "Drink_Alcohol") {
        existing_int_data[[sheet]] <- readxl::read_xlsx(interaction_file, sheet = sheet)
      }
    }
  }, error = function(e) {
    cat("Warning: Could not read existing interaction file, creating new one\n")
    existing_int_data <- list()
  })
}

existing_int_data[["Drink_Alcohol"]] <- interaction_results
writexl::write_xlsx(existing_int_data, path = interaction_file)

# === CREATE PLOT BASED ON ATE COEFFICIENTS ===

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

# P-values for annotations
p_main <- 2 * (1 - pt(abs(ATE_main / se_main), df = df.residual(model_main)))
p_interaction <- 2 * (1 - pt(abs(-beta3 / se_beta3), df = df.residual(model_interaction)))

main_p_label <- paste0("Main effect p = ", ifelse(p_main < 0.001, "<0.001", sprintf("%.3f", p_main)))
int_p_label <- paste0("Interaction p = ", ifelse(p_interaction < 0.001, "<0.001", sprintf("%.3f", p_interaction)))

# Create plot showing ATE coefficients (treatment effects)
plot_drinkalcohol <- ggplot() +
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
  
  annotate("text", x = 1.5, y = -0.065, 
           label = "Main effect", size = 2.5, color = "gray20", fontface = "italic") +
  
  # P-value annotations
  annotate("text", x = 1, y = 0.065, 
           label = main_p_label, size = 3, fontface = "italic") +
  annotate("text", x = 2, y = 0.065, 
           label = int_p_label, size = 3, fontface = "italic") +
  
  scale_y_continuous(limits = c(-0.08, 0.08), labels = scales::percent_format()) +
  scale_fill_manual(values = c("Female" = "gray60", "Male" = "gray30")) +
  labs(
    x = "Sex", 
    y = "Average Treatment Effect (CAWI → CATI)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

# Save plot to outputs_LPM folder
ggsave("outputs_LPM/drinkalcohol_interaction_plot.pdf", plot_drinkalcohol, width = 6.5, height = 4.8, bg = "white")

# Show if interactive
if (interactive()) print(plot_drinkalcohol)

cat("Results saved:\n")
cat("- Main effects: outputs_LPM/main_effects.xlsx (Drink_Alcohol tab)\n")
cat("- Interactions: outputs_LPM/interaction_models.xlsx (Drink_Alcohol tab)\n") 
cat("- Plot: outputs_LPM/drinkalcohol_interaction_plot.pdf\n")