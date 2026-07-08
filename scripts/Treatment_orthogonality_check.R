# === Robustness: do the other factorial treatments modify the mode effect? ===
#
# For each of the 13 outcomes, re-estimates the main-effects model adding
# the other two experimental treatments (invitation topic, activities list)
# and their interactions with mode. Reports (a) the change in the estimated
# mode ATE and (b) p values for the mode-by-treatment interactions.
# For the R2 response letter (Reviewer 3, minor point 5).

.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
suppressMessages({
  library(dplyr)
  library(haven)
  library(survey)
})

source("scripts/02_load_clean_data.R")

covars <- c("Cur_AgeCat", "Cur_HEdQual", "Cur_Ethnic6", "Cur_IntUse3",
            "Cur_PartyID5", "Cur_RelStat5", "Cur_Tenure5", "Cur_HHType",
            "Cur_HHChild")
cov_rhs <- paste(covars, collapse = " + ")

minmax <- function(x) (x - min(x, na.rm = TRUE)) /
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

outcome_defs <- list(
  Happy             = function(d) minmax(as.numeric(as.character(d$Happy))),
  Life_Satisfaction = function(d) minmax(as.numeric(as.character(d$Satis))),
  General_Health    = function(d) {
    g <- as.numeric(as.character(d$GnrlHealth))
    minmax((max(g, na.rm = TRUE) + min(g, na.rm = TRUE)) - g)
  },
  Sports_Exercise   = function(d) ifelse(as.numeric(as.character(d$LeisureAct_SportsExer_q)) == 1, 1, 0),
  Volunteering      = function(d) ifelse(as.numeric(as.character(d$VolUnPd07)) == 0, 1, 0),
  Help_Community    = function(d) ifelse(as.numeric(as.character(d$LeisureAct_HelpCommunity_q)) == 1, 1, 0),
  Gives_Charity     = function(d) ifelse(as.numeric(as.character(d$GiveTy07)) == 0, 1, 0),
  Currently_Smokes  = function(d) ifelse(as.numeric(as.character(d$SmokeNow)) == 1, 1, 0),
  Currently_Vapes   = function(d) ifelse(as.numeric(as.character(d$VapeNow)) == 1, 1, 0),
  Cannabis          = function(d) ifelse(as.numeric(as.character(d$CanEver)) == 1, 1, 0),
  Drink_Alcohol     = function(d) minmax(as.numeric(as.character(d$DrinkFreq))),
  Ever_Gambled      = function(d) ifelse(as.numeric(as.character(d$EverGambled)) == 1, 1, 0),
  At_Risk_Gambling  = function(d) ifelse(!is.na(d$PGSI_total) & d$PGSI_total > 0, 1, 0)
)

df_base <- df_model_base %>%
  mutate(
    CAWI     = ifelse(T_mode == "CAWI", 1, 0),
    Male     = ifelse(Cur_Sex == "2", 1, 0),
    T_invite = as.numeric(T_invitation),
    T_actlist = as.numeric(T_list)
  ) %>%
  mutate(
    T_invite = T_invite - min(T_invite, na.rm = TRUE),   # 0/1
    T_actlist = T_actlist - min(T_actlist, na.rm = TRUE) # 0/1
  )

res <- bind_rows(lapply(names(outcome_defs), function(nm) {
  d <- df_base
  if (nm == "At_Risk_Gambling") d <- d[!is.na(d$PGSI_total), ]
  d$y <- outcome_defs[[nm]](d)
  d <- d[!is.na(d$y) & !is.na(d$T_invite) & !is.na(d$T_actlist), ]

  dsn <- svydesign(ids = ~1, weights = ~May25_Weight_calib, data = d)
  m0 <- svyglm(as.formula(paste("y ~ CAWI + Male +", cov_rhs)),
               design = dsn, family = gaussian())
  m1 <- svyglm(as.formula(paste(
    "y ~ CAWI * T_invite + CAWI * T_actlist + Male +", cov_rhs)),
    design = dsn, family = gaussian())

  # ATE (CAWI -> CATI) in the base model, and averaged over treatment arms in m1
  ate0 <- -coef(m0)["CAWI"]
  b <- coef(m1)
  ate1 <- -(b["CAWI"] + b["CAWI:T_invite"] * mean(d$T_invite) +
              b["CAWI:T_actlist"] * mean(d$T_actlist))

  s <- summary(m1)$coefficients
  data.frame(outcome = nm,
             ate_base = ate0, ate_adjusted = ate1,
             delta = ate1 - ate0,
             p_mode_x_invite = s["CAWI:T_invite", 4],
             p_mode_x_list = s["CAWI:T_actlist", 4])
}))

res$p_x_invite_holm <- p.adjust(res$p_mode_x_invite, "holm")
res$p_x_list_holm <- p.adjust(res$p_mode_x_list, "holm")

write.csv(res, "outputs_LPM/treatment_orthogonality_check.csv", row.names = FALSE)
cat("=== Mode ATE with vs without other-treatment interactions ===\n")
print(res %>% mutate(across(where(is.numeric), ~signif(.x, 3))), row.names = FALSE)
cat("\nMax |change| in ATE:", signif(max(abs(res$delta)), 3), "\n")
cat("Mode-by-invitation interactions p<.05:", sum(res$p_mode_x_invite < .05),
    "(Holm:", sum(res$p_x_invite_holm < .05), ")\n")
cat("Mode-by-list interactions p<.05:", sum(res$p_mode_x_list < .05),
    "(Holm:", sum(res$p_x_list_holm < .05), ")\n")
