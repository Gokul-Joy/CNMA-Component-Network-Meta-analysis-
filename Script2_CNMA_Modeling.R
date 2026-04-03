# ============================================================
# SECTION 9: FIT DISCOMB() — ADDITIVE CNMA (ORR)
# ============================================================
# discomb() is specifically built for disconnected networks.
# It bridges isolated trials through shared components (e.g. ICI).
# Additivity assumption cannot be statistically tested here —
# must be justified clinically in your paper.

discomb_ORR <- discomb(
  TE        = TE,
  seTE      = seTE,
  treat1    = treat1,
  treat2    = treat2,
  studlab   = studlab,
  data      = pw_ORR,
  sm        = "OR",
  random    = TRUE,
  sep.comps = "+"
)

summary(discomb_ORR)
print(discomb_ORR)

# ============================================================
# SECTION 10: FIT DISCOMB() — ADDITIVE CNMA (DCR)
# ============================================================

discomb_DCR <- discomb(
  TE        = TE,
  seTE      = seTE,
  treat1    = treat1,
  treat2    = treat2,
  studlab   = studlab,
  data      = pw_DCR,
  sm        = "OR",
  random    = TRUE,
  sep.comps = "+"
)

summary(discomb_DCR)
print(discomb_DCR)

# ============================================================
# SECTION 10B: FIT DISCOMB() — ADDITIVE CNMA (HR OS)
# ============================================================
# pw_OS was created by pairwise() using pre-computed log(HR)
# and back-calculated seTE values.
# Renouf 2022 already has its seTE correctly computed using
# z = 1.645 (90% CI) — no further adjustment needed here.
# pairwise() has already handled the three-arm covariance
# correction for any multi-arm study (e.g. Enzler 2024).

discomb_OS <- discomb(
  TE        = TE,
  seTE      = seTE,
  treat1    = treat1,
  treat2    = treat2,
  studlab   = studlab,
  data      = pw_OS,
  sm        = "HR",
  random    = TRUE,
  sep.comps = "+"
)

summary(discomb_OS)
print(discomb_OS)

# ============================================================
# SECTION 11: KEY RESULTS TO EXTRACT
# ============================================================

# Component-level effect estimates (log scale, random effects)
discomb_ORR$Comp.random   # ORR
discomb_DCR$Comp.random   # DCR
discomb_OS$Comp.random    # HR OS

# Heterogeneity
discomb_ORR$tau            # between-study SD — ORR
discomb_DCR$tau            # between-study SD — DCR
discomb_OS$tau             # between-study SD — HR OS

# ============================================================
# SECTION 11.5: EXTRACT CLEAN COMPONENT RESULTS TABLES
# ============================================================

# --- Slot inspection ---
discomb_ORR$Comp.random;       discomb_ORR$lower.Comp.random
discomb_ORR$upper.Comp.random; discomb_ORR$pval.Comp.random

discomb_DCR$Comp.random;       discomb_DCR$lower.Comp.random
discomb_DCR$upper.Comp.random; discomb_DCR$pval.Comp.random

discomb_OS$Comp.random;        discomb_OS$lower.Comp.random
discomb_OS$upper.Comp.random;  discomb_OS$pval.Comp.random

# ============================================================
# BUILD CLEAN RESULTS TABLE — ORR
# ============================================================

results_ORR <- data.frame(
  Component  = discomb_ORR$comps,
  iOR        = exp(discomb_ORR$Comp.random),
  lower_95CI = exp(discomb_ORR$lower.Comp.random),
  upper_95CI = exp(discomb_ORR$upper.Comp.random),
  pvalue     = discomb_ORR$pval.Comp.random
)
print(results_ORR)
results_ORR[!is.na(results_ORR$iOR), ]   # identifiable only

# ============================================================
# BUILD CLEAN RESULTS TABLE — DCR
# ============================================================

results_DCR <- data.frame(
  Component  = discomb_DCR$comps,
  iOR        = exp(discomb_DCR$Comp.random),
  lower_95CI = exp(discomb_DCR$lower.Comp.random),
  upper_95CI = exp(discomb_DCR$upper.Comp.random),
  pvalue     = discomb_DCR$pval.Comp.random
)
print(results_DCR)
results_DCR[!is.na(results_DCR$iOR), ]

# ============================================================
# BUILD CLEAN RESULTS TABLE — HR OS
# ============================================================
# Note: exp() of log(HR) gives back the Hazard Ratio.
# HR < 1 = benefit (reduced hazard of death).
# The component-level estimates here are incremental HRs (iHR).

results_OS <- data.frame(
  Component  = discomb_OS$comps,
  iHR        = exp(discomb_OS$Comp.random),
  lower_95CI = exp(discomb_OS$lower.Comp.random),
  upper_95CI = exp(discomb_OS$upper.Comp.random),
  pvalue     = discomb_OS$pval.Comp.random
)
print(results_OS)
results_OS[!is.na(results_OS$iHR), ]     # identifiable only
