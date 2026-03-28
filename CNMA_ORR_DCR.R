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
# SECTION 12: VISCOMP VISUALIZATIONS (NOW UNLOCKED)
# ============================================================
# discomb objects work with viscomp since they inherit
# netmeta class — confirm first

class(discomb_ORR)

# If output includes "netmeta" run these:
compGraph(
  model = discomb_ORR,
  sep   = "+",
  mostF = 5,
  title = "Most Frequent Component Combinations — ORR"
)

heatcomp(
  model  = discomb_ORR,
  sep    = "+",
  random = TRUE,
  freq   = TRUE
)

compdesc(
  model      = discomb_ORR,
  sep        = "+",
  heatmap    = TRUE,
  percentage = TRUE
)