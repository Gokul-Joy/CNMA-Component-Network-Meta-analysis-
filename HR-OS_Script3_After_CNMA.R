# ============================================================
# SECTION 13: FOREST PLOT — BUILT-IN (HR OS)
# ============================================================
# Uses comb.random/comb.common — correct for current netmeta
# version based on confirmed behaviour in this environment.

forest(
  discomb_OS,
  comb.random  = TRUE,
  comb.common  = FALSE,
  sortvar      = discomb_OS$Comp.random,
  smlab        = "Incremental HR (OS)",
  digits       = 2,
  digits.se    = 2,
  col.square   = "forestgreen",
  col.diamond  = "darkgreen",
  col.inside   = "white",
  print.tau2   = TRUE,
  print.I2     = TRUE,
  main         = "Incremental Effect of Each Component — HR OS"
)

# ============================================================
# SECTION 13C: CUSTOM GGPLOT FOREST PLOT — HR OS
# ============================================================
# Use if built-in forest() output looks messy.
# Drops NA (non-identifiable) components automatically.
# HR < 1 = benefit, plotted left of the dashed reference line.

results_OS_clean <- results_OS[!is.na(results_OS$iHR), ]
results_OS_clean$sig <- ifelse(results_OS_clean$pvalue < 0.05,
                               "Significant", "Not Significant")

ggplot(results_OS_clean,
       aes(x = iHR, y = reorder(Component, iHR))) +
  geom_point(aes(color = sig), size = 4) +
  geom_errorbarh(aes(xmin = lower_95CI, xmax = upper_95CI,
                     color = sig), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Significant"     = "darkgreen",
                                "Not Significant" = "forestgreen")) +
  scale_x_log10() +
  labs(
    title   = "Incremental Effect of Each Component — HR OS",
    x       = "Incremental Hazard Ratio (log scale)",
    y       = "Component",
    color   = "",
    caption = "Dashed line = no effect (HR = 1). HR < 1 favours treatment.\nRenouf 2022 SE derived from 90% CI (z = 1.645). Random effects model."
  ) +
  theme_classic() +
  theme(plot.title      = element_text(face = "bold", size = 13),
        axis.text       = element_text(size = 11),
        legend.position = "bottom")

# ============================================================
# SECTION 14: DIAGNOSTICS & INFERENCE EXTRACTION
# ============================================================

# ============================================================
# SECTION 14A: NETWORK STRUCTURE SUMMARY
# ============================================================

cat("======= NETWORK STRUCTURE — HR OS =======\n")
cat("Number of studies (k):", discomb_OS$k, "\n")
cat("Number of pairwise comparisons (m):", discomb_OS$m, "\n")
cat("Number of treatments (n):", discomb_OS$n, "\n")
cat("Number of components (c):", discomb_OS$c, "\n")
cat("Number of subnetworks (s):", discomb_OS$s, "\n")
cat("Number of unidentifiable components:", discomb_OS$na.unident, "\n")
cat("Unidentifiable components:\n")
print(discomb_OS$comps.unident)

# ============================================================
# SECTION 14B: HETEROGENEITY SUMMARY
# ============================================================

cat("\n======= HETEROGENEITY — HR OS =======\n")
cat("tau:", discomb_OS$tau, "\n")
cat("tau^2:", discomb_OS$tau^2, "\n")
cat("I^2 (%):", discomb_OS$I2 * 100, "\n")
cat("Q statistic:", discomb_OS$Q.additive, "\n")
cat("Q df:", discomb_OS$df.Q.additive, "\n")
cat("Q p-value:", discomb_OS$pval.Q.additive, "\n")

# ============================================================
# SECTION 14C: ADDITIVITY TEST
# ============================================================
# Compares additive CNMA vs standard NMA.
# In disconnected networks this will return NA — expected.
# Additivity must be justified clinically in your paper.

cat("\n======= ADDITIVITY TEST — HR OS =======\n")
cat("Q additive:", discomb_OS$Q.additive, "\n")
cat("Q standard:", discomb_OS$Q.standard, "\n")
cat("Q difference:", discomb_OS$Q.diff, "\n")
cat("Q diff df:", discomb_OS$df.Q.diff, "\n")
cat("Q diff p-value:", discomb_OS$pval.Q.diff, "\n")
cat("(If p > 0.05: additivity assumption holds)\n")
cat("(NA is expected in fully disconnected networks)\n")

# ============================================================
# SECTION 14D: FULL COMPONENT ESTIMATES
# ============================================================

cat("\n======= COMPONENT ESTIMATES — HR OS (Random Effects) =======\n")
print(results_OS_full)

cat("\nIdentifiable only:\n")
print(results_OS_full[!is.na(results_OS_full$iHR_random), ])

# ============================================================
# SECTION 14E: TREATMENT LEVEL ESTIMATES
# ============================================================
cat("\n======= TREATMENT ESTIMATES — HR OS =======\n")

ref_col <- which(discomb_OS$trts == discomb_OS$reference.group)

treat_OS <- data.frame(
  Treatment    = discomb_OS$trts,
  HR_random    = round(exp(-discomb_OS$TE.random[, ref_col]), 3),
  lower_random = round(exp(-discomb_OS$upper.random[, ref_col]), 3),  # ⚠️ swapped
  upper_random = round(exp(-discomb_OS$lower.random[, ref_col]), 3),  # ⚠️ swapped
  pval_random  = round(discomb_OS$pval.random[, ref_col], 4)          # unchanged
)
print(treat_OS)

# ============================================================
# SECTION 14F: IDENTIFIABILITY REPORT
# ============================================================

cat("\n======= IDENTIFIABILITY REPORT — HR OS =======\n")
cat("Total components:", length(discomb_OS$comps), "\n")
identifiable_OS     <- discomb_OS$comps[!is.na(discomb_OS$Comp.random)]
not_identifiable_OS <- discomb_OS$comps[is.na(discomb_OS$Comp.random)]
cat("Identifiable components:\n");     print(identifiable_OS)
cat("Non-identifiable components:\n"); print(not_identifiable_OS)
cat("\nReason: Non-identifiable components appear only as control arm\n")
cat("backbones in isolated subnetworks with no shared component bridge.\n")
cat("This is structurally expected in a disconnected CNMA and must\n")
cat("be acknowledged as a limitation in your paper.\n")

# ============================================================
# SECTION 14G: SAVE RESULTS TO CSV
# ============================================================

#write.csv(results_OS_full, "CNMA_results_OS.csv", row.names = FALSE)
#cat("\nResults saved to CNMA_results_OS.csv\n")

# ============================================================
# SECTION 15: SENSITIVITY — COMMON VS RANDOM COMPARISON
# ============================================================
# Large differences = tau is meaningfully inflating uncertainty.
# Small differences = common effects model is adequate approximation.

comparison_OS <- data.frame(
  Component     = results_OS_full$Component,
  iHR_common    = results_OS_full$iHR_common,
  iHR_random    = results_OS_full$iHR_random,
  pval_common   = results_OS_full$pval_common,
  pval_random   = results_OS_full$pval_random,
  Estimate_diff = round(abs(results_OS_full$iHR_random -
                              results_OS_full$iHR_common), 3)
)

cat("\n======= SENSITIVITY — COMMON VS RANDOM — HR OS =======\n")
print(comparison_OS[!is.na(comparison_OS$iHR_random), ])

