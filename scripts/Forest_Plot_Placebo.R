# === Leisure Activities Analysis ===

source("scripts/01_setup_environment.R")
source("scripts/02_load_clean_data.R")

# Full set of leisure activity outcomes
activity_vars <- c(
  "LeisureAct_FriendsFamily_q", "LeisureAct_ListenMusic_q", "LeisureAct_WatchTV_q",
  "LeisureAct_LiveSport_q", "LeisureAct_VideoGames_q", "LeisureAct_BoardGames_q",
  "LeisureAct_Restaurants_q", "LeisureAct_CoffeeShop_q", "LeisureAct_PubBarClub_q",
  "LeisureAct_GoShopping_q", 
  "LeisureAct_MuseumGallery_q", "LeisureAct_TheatreLiveMusic_q"
)

results_list <- list()

for (var in activity_vars) {
  cat("\n==== Fitting model for:", var, "====\n")
  
  df_temp <- df_respondents %>%
    filter(
      .data[[var]] %in% c(1, 2),
      !is.na(T_mode),
      !is.na(Cur_Sex),
      !is.na(Cur_AgeCat),
      !is.na(Cur_HEdQual),
      !is.na(Cur_Ethnic6),
      !is.na(Cur_IntUse3)
    )
  
  outcome_binary <- ifelse(df_temp[[var]] == 1, 1L, 0L)
  
  model_df <- tibble(
    outcome_binary = outcome_binary,
    T_mode         = factor(df_temp$T_mode, levels = c(1, 2), labels = c("CATI", "CAWI")),
    Cur_Sex        = fct_na_value_to_level(as_factor(df_temp$Cur_Sex), "Missing"),
    Cur_AgeCat     = fct_na_value_to_level(as_factor(df_temp$Cur_AgeCat), "Missing"),
    Cur_HEdQual    = fct_na_value_to_level(as_factor(df_temp$Cur_HEdQual), "Missing"),
    Cur_Ethnic6    = fct_na_value_to_level(as_factor(df_temp$Cur_Ethnic6), "Missing"),
    Cur_IntUse3    = fct_na_value_to_level(as_factor(df_temp$Cur_IntUse3), "Missing"),
    weight         = df_temp$May25_Weight_calib
  )
  
  model <- glm(
    outcome_binary ~ T_mode + Cur_Sex + Cur_AgeCat + Cur_HEdQual + Cur_Ethnic6 + Cur_IntUse3,
    data = model_df,
    family = binomial(),
    weights = weight
  )
  
  coefs <- summary(model)$coefficients
  if ("T_modeCAWI" %in% rownames(coefs)) {
    est <- coefs["T_modeCAWI", "Estimate"]
    se  <- coefs["T_modeCAWI", "Std. Error"]
    
    results_list[[var]] <- tibble(
      term      = "T_modeCAWI",
      estimate  = est,
      std.error = se,
      conf.low  = est - 1.96 * se,
      conf.high = est + 1.96 * se,
      p.value   = coefs["T_modeCAWI", "Pr(>|z|)"],
      outcome   = var
    )
  } else {
    cat("⚠️ Skipping", var, "- no T_modeCAWI term found.\n")
  }
}

# Combine results
if (length(results_list) == 0) stop("No models succeeded.")
results <- bind_rows(results_list)

# === SAVE TO SEPARATE EXCEL FILE ===

# Prepare results for Excel
leisure_results <- results %>%
  mutate(
    # Clean outcome names
    outcome = recode(outcome,
                     LeisureAct_FriendsFamily_q = "Friends/Family",
                     LeisureAct_ListenMusic_q = "Listening to music",
                     LeisureAct_WatchTV_q = "Watching TV",
                     LeisureAct_LiveSport_q = "Live sport",
                     LeisureAct_VideoGames_q = "Video games",
                     LeisureAct_BoardGames_q = "Board games",
                     LeisureAct_Restaurants_q = "Restaurants",
                     LeisureAct_CoffeeShop_q = "Coffee shop",
                     LeisureAct_PubBarClub_q = "Pub/Bar/Club",
                     LeisureAct_GoShopping_q = "Shopping",
                     LeisureAct_SportsExer_q = "Sport/Exercise",
                     LeisureAct_MuseumGallery_q = "Museum/Gallery",
                     LeisureAct_TheatreLiveMusic_q = "Theatre/Live Music"
    )
  ) %>%
  # Add broom::tidy style columns for consistency with other results
  select(term, estimate, std.error, p.value, conf.low, conf.high, outcome)

# Save to separate Excel file for leisure activities
leisure_file <- "outputs/leisure_activities_results.xlsx"
writexl::write_xlsx(list("Leisure_Activities" = leisure_results), path = leisure_file)

cat("Results saved to:\n")
cat("- Leisure activities: outputs/leisure_activities_results.xlsx\n")

# === FLIP SIGNS TO SHOW CAWI → CATI DIRECTION ===

# Flip signs to show CAWI → CATI direction (consistent with other analyses)
results <- results %>%
  mutate(
    estimate = -estimate,
    temp_low = conf.low,
    conf.low = -conf.high,
    conf.high = -temp_low
  ) %>%
  select(-temp_low)

# === CREATE FOREST PLOT ===

# Reorder outcomes by estimate for plotting
results <- results %>%
  mutate(
    # Clean outcome names for plot
    outcome_clean = recode(outcome,
                           LeisureAct_FriendsFamily_q = "Friends/Family",
                           LeisureAct_ListenMusic_q = "Listening to music",
                           LeisureAct_WatchTV_q = "Watching TV",
                           LeisureAct_LiveSport_q = "Live sport",
                           LeisureAct_VideoGames_q = "Video games",
                           LeisureAct_BoardGames_q = "Board games",
                           LeisureAct_Restaurants_q = "Restaurants",
                           LeisureAct_CoffeeShop_q = "Coffee shop",
                           LeisureAct_PubBarClub_q = "Pub/Bar/Club",
                           LeisureAct_GoShopping_q = "Shopping",
                           LeisureAct_SportsExer_q = "Sport/Exercise",
                           LeisureAct_MuseumGallery_q = "Museum/Gallery",
                           LeisureAct_TheatreLiveMusic_q = "Theatre/Live Music"
    ),
    outcome_clean = factor(outcome_clean, levels = rev(unique(outcome_clean[order(estimate)])))
  )

# Forest plot
plot_leisure <- ggplot(results, aes(x = estimate, y = outcome_clean)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(
    x = "Treatment Effect: CAWI → CATI (log-odds with 95% CI)",
    y = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(axis.text.y = element_text(size = 10))

# Save plot
ggsave("outputs/leisure_activities_forest_plot.pdf", plot_leisure, 
       width = 8, height = 6, bg = "white")

# Show plot
print(plot_leisure)

cat("- Forest plot: outputs/leisure_activities_forest_plot.pdf\n")