# ============================================================
# SECTION 9: FIT DISCOMB() — ADDITIVE CNMA (HR OS)
# ============================================================
# discomb() is specifically built for disconnected networks.
# It bridges isolated subnetworks through shared components.
# Additivity assumption cannot be statistically tested in a
# disconnected network — must be justified clinically in paper.
#
# pw_OS was built manually in contrast-based format (Script 1).
# Renouf 2022 seTE uses z = 1.645 (90% CI).
# All other studies use z = 1.96 (95% CI).
# discomb() identifies multi-arm studies via identical studlab
# values and applies internal multi-arm reweighting.

discomb_OS <- discomb(
  TE        = TE,
  seTE      = seTE,
  treat1    = treat1,
  treat2    = treat2,
  studlab   = studlab,
  data      = pw_OS,
  sm        = "HR",
  common    = TRUE,    # ← add this
  random    = TRUE,
  sep.comps = "+"
)

summary(discomb_OS)
print(discomb_OS)

# ============================================================
# SECTION 10: KEY RESULTS TO EXTRACT
# ============================================================

# Component-level effect estimates (log scale, random effects)
discomb_OS$Comp.random        # log(iHR) for each component
discomb_OS$lower.Comp.random  # lower CI (log scale)
discomb_OS$upper.Comp.random  # upper CI (log scale)
discomb_OS$pval.Comp.random   # p-values

# Heterogeneity
discomb_OS$tau    # between-study SD
discomb_OS$tau^2  # tau squared
discomb_OS$I2     # I-squared

# ============================================================
# SECTION 11: BUILD CLEAN RESULTS TABLE — HR OS
# ============================================================
# exp() transforms log(HR) back to HR scale.
# iHR < 1 = reduced hazard = survival benefit.
# iHR > 1 = increased hazard = harm.
# NA = component not identifiable (backbone only).

results_OS <- data.frame(
  Component  = discomb_OS$comps,
  iHR        = exp(discomb_OS$Comp.random),
  lower_95CI = exp(discomb_OS$lower.Comp.random),
  upper_95CI = exp(discomb_OS$upper.Comp.random),
  pvalue     = discomb_OS$pval.Comp.random
)

cat("\n======= COMPONENT ESTIMATES — HR OS (Random Effects) =======\n")
print(results_OS)

cat("\nIdentifiable components only:\n")
print(results_OS[!is.na(results_OS$iHR), ])

# ============================================================
# SECTION 11.5: FULL TABLE — COMMON AND RANDOM EFFECTS
# ============================================================
# iHR < 1 = reduced hazard = survival benefit
results_OS_full <- data.frame(
  Component    = discomb_OS$comps,
  iHR_random   = round(exp(discomb_OS$Comp.random), 3),
  lower_random = round(exp(discomb_OS$lower.Comp.random), 3),
  upper_random = round(exp(discomb_OS$upper.Comp.random), 3),
  pval_random  = round(discomb_OS$pval.Comp.random, 4),
  iHR_common   = round(exp(discomb_OS$Comp.common), 3),
  lower_common = round(exp(discomb_OS$lower.Comp.common), 3),
  upper_common = round(exp(discomb_OS$upper.Comp.common), 3),
  pval_common  = round(discomb_OS$pval.Comp.common, 4)
)
cat("\n======= FULL COMPONENT TABLE — COMMON AND RANDOM =======\n")
print(results_OS_full)

