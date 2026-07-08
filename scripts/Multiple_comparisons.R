# === Multiple comparisons: Holm adjustment and joint Wald tests ===
#
# Re-fits the 13 outcome models with the same specification as the
# outcome-specific scripts (weighted LPM via svyglm, CAWI and Male dummies,
# full covariate block), then:
#   1. Holm-adjusts the 13 main-effect and 13 interaction p values.
#   2. Fits stacked models across all 13 outcomes with respondent-clustered
#      standard errors and computes joint Wald tests of (a) the 13 mode
#      main effects and (b) the 13 mode-by-sex interactions.
# Outputs: outputs_LPM/multiple_comparisons.csv and printed joint tests.

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

# Outcome definitions replicating each outcome script exactly.
# 'valence' is used for reporting only.
outcome_defs <- list(
  Happy             = list(f = function(d) minmax(as.numeric(as.character(d$Happy))),                 valence = "positive"),
  Life_Satisfaction = list(f = function(d) minmax(as.numeric(as.character(d$Satis))),                 valence = "positive"),
  General_Health    = list(f = function(d) {
                            g <- as.numeric(as.character(d$GnrlHealth))
                            minmax((max(g, na.rm = TRUE) + min(g, na.rm = TRUE)) - g)
                          },                                                                          valence = "positive"),
  Sports_Exercise   = list(f = function(d) ifelse(as.numeric(as.character(d$LeisureAct_SportsExer_q)) == 1, 1, 0),   valence = "positive"),
  Volunteering      = list(f = function(d) ifelse(as.numeric(as.character(d$VolUnPd07)) == 0, 1, 0),  valence = "positive"),
  Help_Community    = list(f = function(d) ifelse(as.numeric(as.character(d$LeisureAct_HelpCommunity_q)) == 1, 1, 0), valence = "positive"),
  Gives_Charity     = list(f = function(d) ifelse(as.numeric(as.character(d$GiveTy07)) == 0, 1, 0),   valence = "positive"),
  Currently_Smokes  = list(f = function(d) ifelse(as.numeric(as.character(d$SmokeNow)) == 1, 1, 0),   valence = "negative"),
  Currently_Vapes   = list(f = function(d) ifelse(as.numeric(as.character(d$VapeNow)) == 1, 1, 0),    valence = "negative"),
  Cannabis          = list(f = function(d) ifelse(as.numeric(as.character(d$CanEver)) == 1, 1, 0),    valence = "negative"),
  Drink_Alcohol     = list(f = function(d) minmax(as.numeric(as.character(d$DrinkFreq))),             valence = "negative"),
  Ever_Gambled      = list(f = function(d) ifelse(as.numeric(as.character(d$EverGambled)) == 1, 1, 0), valence = "negative"),
  At_Risk_Gambling  = list(f = function(d) ifelse(!is.na(d$PGSI_total) & d$PGSI_total > 0, 1, 0),     valence = "negative")
)
# At_Risk_Gambling is only defined where PGSI_total is observed (as in PGSI>0.R)
pgsi_filter <- function(d) !is.na(d$PGSI_total)

df_base <- df_model_base %>%
  mutate(
    pid  = row_number(),
    CAWI = ifelse(T_mode == "CAWI", 1, 0),
    Male = ifelse(Cur_Sex == "2", 1, 0)
  )

# --- 1. Per-outcome models (replicating the outcome scripts) ---
per_outcome <- lapply(names(outcome_defs), function(nm) {
  def <- outcome_defs[[nm]]
  d <- df_base
  if (nm == "At_Risk_Gambling") d <- d[pgsi_filter(d), ]
  d$y <- def$f(d)
  d <- d[!is.na(d$y), ]

  dsn <- svydesign(ids = ~1, weights = ~May25_Weight_calib, data = d)
  m_int  <- svyglm(as.formula(paste("y ~ CAWI * Male +", cov_rhs)),
                   design = dsn, family = gaussian())
  m_main <- svyglm(as.formula(paste("y ~ CAWI + Male +", cov_rhs)),
                   design = dsn, family = gaussian())

  b_main <- -coef(m_main)["CAWI"]           # ATE CAWI -> CATI
  se_main <- sqrt(vcov(m_main)["CAWI", "CAWI"])
  p_main <- 2 * (1 - pt(abs(b_main / se_main), df = df.residual(m_main)))

  # ATE_women - ATE_men = -beta1 - (-(beta1 + beta3)) = +beta3
  b_int <- coef(m_int)["CAWI:Male"]         # women minus men difference in ATE
  se_int <- sqrt(vcov(m_int)["CAWI:Male", "CAWI:Male"])
  p_int <- 2 * (1 - pt(abs(b_int / se_int), df = df.residual(m_int)))

  data.frame(outcome = nm, valence = def$valence, n = nrow(d),
             ate = b_main, se_ate = se_main, p_ate = p_main,
             diff_wf = b_int, se_diff = se_int, p_diff = p_int)
})
res <- bind_rows(per_outcome)
res$p_ate_holm  <- p.adjust(res$p_ate, method = "holm")
res$p_diff_holm <- p.adjust(res$p_diff, method = "holm")

dir.create("outputs_LPM", showWarnings = FALSE)
write.csv(res, "outputs_LPM/multiple_comparisons.csv", row.names = FALSE)

cat("\n=== Per-outcome results with Holm adjustment ===\n")
print(res %>% mutate(across(where(is.numeric), ~signif(.x, 3))), row.names = FALSE)
cat("\nMain effects significant at .05: unadjusted =", sum(res$p_ate < .05),
    "| Holm =", sum(res$p_ate_holm < .05), "\n")
cat("Interactions significant at .05: unadjusted =", sum(res$p_diff < .05),
    "| Holm =", sum(res$p_diff_holm < .05), "\n")
cat("Interactions significant at .10: unadjusted =", sum(res$p_diff < .10),
    "| Holm =", sum(res$p_diff_holm < .10), "\n")
# SDB-consistent direction: women's mode effect larger means diff_wf > 0 for
# positively valenced outcomes and diff_wf < 0 for negatively valenced ones.
res$women_larger_sdb <- ifelse(res$valence == "positive", res$diff_wf > 0, res$diff_wf < 0)
cat("Women larger mode effect in SDB-consistent direction:",
    sum(res$women_larger_sdb), "of", nrow(res), "\n")
st <- binom.test(sum(res$women_larger_sdb), nrow(res))
cat("Sign test p =", signif(st$p.value, 3), "\n")

# --- 2. Stacked models with respondent-clustered SEs and joint Wald tests ---
stack <- bind_rows(lapply(names(outcome_defs), function(nm) {
  def <- outcome_defs[[nm]]
  d <- df_base
  if (nm == "At_Risk_Gambling") d <- d[pgsi_filter(d), ]
  d$y <- def$f(d)
  d <- d[!is.na(d$y), ]
  d$out <- nm
  d[, c("pid", "out", "y", "CAWI", "Male", "May25_Weight_calib", covars)]
}))
stack$out <- factor(stack$out, levels = names(outcome_defs))

dsn_stack <- svydesign(ids = ~pid, weights = ~May25_Weight_calib, data = stack)

# Fully interacted stacked models reproduce the separate per-outcome fits,
# while clustering on pid captures the covariance between outcomes.
f_int  <- as.formula(paste("y ~ 0 + out + out:(CAWI * Male) + out:(", cov_rhs, ")"))
f_main <- as.formula(paste("y ~ 0 + out + out:(CAWI + Male) + out:(", cov_rhs, ")"))

m_stack_int  <- svyglm(f_int,  design = dsn_stack, family = gaussian())
m_stack_main <- svyglm(f_main, design = dsn_stack, family = gaussian())

joint_wald <- function(model, pattern) {
  keep <- grep(pattern, names(coef(model)))
  b <- coef(model)[keep]
  V <- vcov(model)[keep, keep]
  W <- as.numeric(t(b) %*% solve(V) %*% b)
  k <- length(b)
  data.frame(k = k, wald_chisq = W, p_chisq = 1 - pchisq(W, k))
}

cat("\n=== Joint Wald tests (stacked estimation, respondent-clustered) ===\n")
jt_main <- joint_wald(m_stack_main, ":CAWI$")
cat("H0: all 13 mode main effects = 0:  chi2(", jt_main$k, ") =",
    round(jt_main$wald_chisq, 1), ", p =", format.pval(jt_main$p_chisq, digits = 3), "\n")
jt_int <- joint_wald(m_stack_int, ":CAWI:Male$")
cat("H0: all 13 mode-by-sex interactions = 0:  chi2(", jt_int$k, ") =",
    round(jt_int$wald_chisq, 1), ", p =", format.pval(jt_int$p_chisq, digits = 3), "\n")

# Cross-check: stacked point estimates should match the per-outcome fits
chk <- -coef(m_stack_main)[grep(":CAWI$", names(coef(m_stack_main)))]
cat("\nMax abs difference stacked vs per-outcome ATEs:",
    signif(max(abs(sort(chk) - sort(res$ate))), 3), "\n")

saveRDS(list(per_outcome = res, joint_main = jt_main, joint_int = jt_int),
        "outputs_LPM/multiple_comparisons.rds")
cat("\nSaved: outputs_LPM/multiple_comparisons.csv and .rds\n")
