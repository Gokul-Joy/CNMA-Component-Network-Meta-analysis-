

# ============================================================
# SECTION 9: FIT DISCOMB() — ADDITIVE CNMA (ORR)
# ============================================================
# discomb() is specifically built for disconnected networks
# It bridges the isolated trials through the common ICI component
# IMPORTANT: Additivity assumption cannot be statistically tested
# here — must be justified clinically in your paper

discomb_ORR <- discomb(
  TE      = TE,
  seTE    = seTE,
  treat1  = treat1,
  treat2  = treat2,
  studlab = studlab,
  data    = pw_ORR,
  sm      = "OR",
  random  = TRUE,
  sep.comps = "+"      # tells discomb your separator symbol
)

# Inspect the results
summary(discomb_ORR)
print(discomb_ORR)

# ============================================================
# SECTION 10: FIT DISCOMB() — ADDITIVE CNMA (DCR)
# ============================================================

pw_DCR <- pairwise(
  treat   = list(treat1, treat2, treat3),
  event   = list(event1_DCR, event2_DCR, event3_DCR),
  n       = list(n1, n2, n3),
  studlab = studlab,
  data    = df,
  sm      = "OR"
)

# Verify Enzler DCR rows
pw_DCR[pw_DCR$studlab == "Enzler 2024", c("studlab","treat1","treat2","TE","seTE")]

discomb_DCR <- discomb(
  TE      = TE,
  seTE    = seTE,
  treat1  = treat1,
  treat2  = treat2,
  studlab = studlab,
  data    = pw_DCR,
  sm      = "OR",
  random  = TRUE,
  sep.comps = "+"
)

summary(discomb_DCR)
print(discomb_DCR)

# ============================================================
# SECTION 11: KEY RESULTS TO EXTRACT
# ============================================================

# Component-level effect estimates (what you actually want)
# This shows the individual effect of each component (ICI etc.)
discomb_ORR$Comb.random   # random effects component estimates — ORR
discomb_DCR$Comb.random   # random effects component estimates — DCR

# Heterogeneity
discomb_ORR$tau            # between-study SD — ORR
discomb_DCR$tau            # between-study SD — DCR
# ============================================================
# SECTION 11.5: EXTRACT CLEAN COMPONENT RESULTS TABLES
# ============================================================

# ============================================================
# CORRECT SLOTS FOR COMPONENT-LEVEL ESTIMATES
# ============================================================

# ORR — component level log ORs (random effects)
discomb_ORR$Comp.random        # point estimates (log scale)
discomb_ORR$lower.Comp.random  # lower CI
discomb_ORR$upper.Comp.random  # upper CI
discomb_ORR$pval.Comp.random   # p-values

# DCR — component level log ORs (random effects)
discomb_DCR$Comp.random
discomb_DCR$lower.Comp.random
discomb_DCR$upper.Comp.random
discomb_DCR$pval.Comp.random

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




#Without NA
results_ORR[!is.na(results_ORR$iOR), ]
results_DCR[!is.na(results_DCR$iOR), ]
