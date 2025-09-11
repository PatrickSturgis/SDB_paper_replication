# === Forest Plot Script (Updated for 3 rows per outcome) ===

# Load environment and data
source("scripts/01_setup_environment.R")
source("scripts/02_load_clean_data.R")

# Read the forest plot data CSV file
forest_data_file <- "outputs_LPM/forest_plot_data.csv"

if (!file.exists(forest_data_file)) {
  stop("Forest plot data file not found. Please run your outcome analysis scripts first.")
}

# Read forest plot data
combined_results <- read.csv(forest_data_file)

# Create readable outcome names and group ordering
combined_results <- combined_results %>%
  mutate(
    # Create readable outcome names
    outcome_clean = case_when(
      outcome == "Happy" ~ "Happiness",
      outcome == "Life_Satisfaction" ~ "Life Satisfaction", 
      outcome == "General_Health" ~ "General Health",
      outcome == "Drink_Alcohol" ~ "Drinks Alcohol",
      outcome == "Sports_Exercise" ~ "Sports or Exercise",
      outcome == "Gives_Charity" ~ "Gives to Charity",
      outcome == "Volunteering" ~ "Volunteering",
      outcome == "Help_Community" ~ "Helped in Community",
      outcome == "At_Risk_Gambling" ~ "Gambling Impacts",
      outcome == "Currently_Smokes" ~ "Currently Smokes",
      outcome == "Currently_Vapes" ~ "Currently Vapes", 
      outcome == "Cannabis" ~ "Ever Smoked Cannabis",
      outcome == "Ever_Gambled" ~ "Gambled Past Year",
      TRUE ~ outcome
    ),
    # Create combined outcome-group labels for y-axis
    outcome_group = paste0(outcome_clean, " - ", group),
    # Order groups within each outcome: Full Sample, Men, Women
    group_factor = factor(group, levels = c("Full Sample", "Men", "Women"))
  )

# Define positive and negative outcomes
positive_outcomes <- c(
  "General Health", "Helped in Community", "Happiness", "Life Satisfaction", 
  "Volunteering", "Gives to Charity", "Sports or Exercise"
)

negative_outcomes <- c(
  "Gambling Impacts", "Currently Vapes", "Currently Smokes", 
  "Gambled Past Year", "Ever Smoked Cannabis", "Drinks Alcohol"
)

# Add interaction significance calculation and outcome type
combined_results <- combined_results %>%
  group_by(outcome_clean) %>%
  mutate(
    # Determine interaction significance based on Men vs Women estimates
    men_estimate = estimate[group == "Men"],
    women_estimate = estimate[group == "Women"],
    men_se = std.error[group == "Men"],
    women_se = std.error[group == "Women"],
    # Test if difference between men and women is significant (p < 0.1)
    diff_estimate = men_estimate - women_estimate,
    diff_se = sqrt(men_se^2 + women_se^2),  # Approximate SE for difference
    diff_t = abs(diff_estimate / diff_se),
    interaction_significant = diff_t > 1.645,  # p < 0.1
    outcome_type = case_when(
      outcome_clean %in% positive_outcomes ~ "Positive",
      outcome_clean %in% negative_outcomes ~ "Negative",
      TRUE ~ "Other"
    )
  ) %>%
  ungroup()

# Order outcomes by main effect estimate for cleaner layout
main_effects <- combined_results %>% filter(group == "Full Sample")
outcome_order <- main_effects$outcome_clean[order(main_effects$estimate)]

# Create factor with proper ordering (3 rows per outcome)
combined_results <- combined_results %>%
  mutate(
    outcome_clean = factor(outcome_clean, levels = rev(outcome_order)),
    # Create plotting order: for each outcome, show Full Sample, Men, Women
    plot_order = paste0(outcome_clean, "_", group_factor)
  ) %>%
  arrange(outcome_clean, group_factor)

# Create y-axis factor with proper spacing and gaps between outcomes
combined_results <- combined_results %>%
  arrange(outcome_clean, desc(group_factor)) %>%
  group_by(outcome_clean) %>%
  mutate(
    outcome_group_id = cur_group_id(),
    within_outcome_position = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    # Add larger gaps between outcomes: 3 positions per outcome + 2 gap = 5 positions per outcome
    y_position = (outcome_group_id - 1) * 5 + within_outcome_position
  )

# Set plotting range
x_limits <- range(c(combined_results$conf.low, combined_results$conf.high), na.rm = TRUE)
x_range <- diff(x_limits)
x_limits <- c(x_limits[1] - 0.1 * x_range, x_limits[2] + 0.1 * x_range)

# Create background data for shading
background_data <- combined_results %>% 
  group_by(outcome_clean, outcome_type) %>% 
  summarise(ymin = min(y_position) - 0.4, 
            ymax = max(y_position) + 0.4, .groups = "drop")

# Create asterisk data for significant interactions
asterisk_data <- combined_results %>% 
  filter(group == "Full Sample" & interaction_significant)

# Forest plot
forest_plot <- ggplot(combined_results, aes(x = estimate, y = y_position)) +
  # Background shading
  geom_rect(data = background_data,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = outcome_type),
            alpha = 0.1, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Positive" = "#90EE90", "Negative" = "#FFB6C1", "Other" = "white"),
                    guide = "none") +
  # Main plot elements
  geom_point(aes(shape = group_factor, color = group_factor), size = 1.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = group_factor), 
                 height = 0.2, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Group labels
  geom_text(aes(x = x_limits[1] - 0.02 * x_range, y = y_position, label = group), 
            hjust = 1, size = 3, color = "gray40") +
  # Asterisks for significant interactions
  geom_text(data = asterisk_data,
            aes(x = conf.high, y = y_position, alpha = "Significant Sex Interaction (p<0.1)"),
            label = "*", size = 6, color = "red", fontface = "bold", hjust = -0.5) +
  # Scales and formatting
  scale_shape_manual(values = c("Full Sample" = 17, "Women" = 15, "Men" = 16), name = "Group") +
  scale_color_manual(values = c("Full Sample" = "#2E86AB", "Women" = "#A23B72", "Men" = "#F18F01"), name = "Group") +
  scale_alpha_manual(values = c("Significant Sex Interaction (p<0.1)" = 1), name = "", guide = guide_legend(override.aes = list(label = "*", color = "red", size = 6))) +
  scale_x_continuous(limits = x_limits, labels = function(x) sprintf("%.3f", x)) +
  scale_y_continuous(breaks = seq(2, max(combined_results$y_position), by = 5),
                     labels = rev(outcome_order), expand = c(0.02, 0)) +
  labs(# title = "Effect of Survey Mode (CAWI -> CATI) by Outcome and Sex",
       subtitle = "Background: Light green = positive, Light pink = negative outcomes; * = significant sex interaction (p<0.1)",
       x = "Average Treatment Effect (CAWI -> CATI)", y = NULL) +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray60"),
        legend.position = "bottom",
        plot.margin = margin(t = 5, r = 25, b = 5, l = 5, unit = "pt"),
        axis.text.y = element_text(colour = "black"))

# Save the plot
ggsave("outputs_LPM/forest_plot_by_sex.pdf", forest_plot, width = 12, height = 10, bg = "white", dpi = 300)

# Show if interactive
if (interactive()) print(forest_plot)

cat("Forest plot saved to: outputs_LPM/forest_plot_by_sex.pdf\n")
cat("Combined results include", nrow(combined_results), "estimates across", length(unique(combined_results$outcome)), "outcomes\n")
cat("All coefficients represent ATE on unified 0-1 scale\n")