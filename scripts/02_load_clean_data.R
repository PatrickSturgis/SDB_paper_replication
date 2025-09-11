# === 02_load_clean_data.R (Streamlined) ===

# Load raw data
df_combined <- haven::read_dta("data/GC_experiment.dta")

# Create respondents dataset (productive only)
df_respondents <- df_combined %>%
  filter(May25OC == 110)  # Keep only productive respondents

# Create analysis-ready dataset with standard filtering and factors
df_model_base <- df_respondents %>%
  filter(
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
    T_mode = factor(T_mode, levels = c(1, 2), labels = c("CATI", "CAWI")),
    Cur_Sex = factor(Cur_Sex),
    Cur_AgeCat = factor(Cur_AgeCat),
    Cur_HEdQual = factor(Cur_HEdQual),
    Cur_Ethnic6 = factor(Cur_Ethnic6),
    Cur_IntUse3 = factor(Cur_IntUse3),
    Cur_PartyID5 = factor(Cur_PartyID5),
    Cur_RelStat5 = factor(Cur_RelStat5),
    Cur_Tenure5 = factor(Cur_Tenure5),
    Cur_HHType = factor(Cur_HHType),
    Cur_HHChild = factor(Cur_HHChild)
  )

cat("Data loaded successfully!\n")
cat("- df_respondents: ", nrow(df_respondents), " productive respondents\n")
cat("- df_model_base: ", nrow(df_model_base), " respondents ready for analysis\n")