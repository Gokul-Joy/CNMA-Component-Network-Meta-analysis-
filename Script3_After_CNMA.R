# ============================================================
# SECTION 13: FOREST PLOTS — COMPONENT LEVEL ESTIMATES
# ============================================================
# Note: In netmeta >= 2.0, comb.random/comb.common were renamed
# to random/common. Both versions shown — use whichever matches
# your installed netmeta version. Check with: packageVersion("netmeta")

# --- ORR ---
forest(
  discomb_ORR,
  comb.random  = TRUE,
  comb.common  = FALSE,
  sortvar      = discomb_ORR$Comp.random,
  smlab        = "Incremental OR (ORR)",
  digits       = 2,
  digits.se    = 2,
  col.square   = "steelblue",
  col.diamond  = "darkblue",
  col.inside   = "white",
  print.tau2   = TRUE,
  print.I2     = TRUE,
  main         = "Incremental Effect of Each Component — ORR"
)

# --- DCR ---
forest(
  discomb_DCR,
  comb.random  = TRUE,
  comb.common  = FALSE,
  sortvar      = discomb_DCR$Comp.random,
  smlab        = "Incremental OR (DCR)",
  digits       = 2,
  digits.se    = 2,
  col.square   = "tomato",
  col.diamond  = "darkred",
  col.inside   = "white",
  print.tau2   = TRUE,
  print.I2     = TRUE,
  main         = "Incremental Effect of Each Component — DCR"
)

# --- HR OS ---
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
# SECTION 13C: CUSTOM GGPLOT FOREST PLOTS
# ============================================================
# Use if built-in forest() output looks messy.
# Drops NA (non-identifiable) components automatically.

library(ggplot2)

# --- ORR ---
results_ORR_clean <- results_ORR[!is.na(results_ORR$iOR), ]
results_ORR_clean$sig <- ifelse(results_ORR_clean$pvalue < 0.05,
                                "Significant", "Not Significant")

ggplot(results_ORR_clean,
       aes(x = iOR, y = reorder(Component, iOR))) +
  geom_point(aes(color = sig), size = 4) +
  geom_errorbarh(aes(xmin = lower_95CI, xmax = upper_95CI,
                     color = sig), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Significant"     = "darkblue",
                                "Not Significant" = "steelblue")) +
  scale_x_log10() +
  labs(
    title   = "Incremental Effect of Each ICI Component — ORR",
    x       = "Incremental Odds Ratio (log scale)",
    y       = "Component",
    color   = "",
    caption = "Dashed line = no effect (OR = 1). Random effects model."
  ) +
  theme_classic() +
  theme(plot.title      = element_text(face = "bold", size = 13),
        axis.text       = element_text(size = 11),
        legend.position = "bottom")

# --- DCR ---
results_DCR_clean <- results_DCR[!is.na(results_DCR$iOR), ]
results_DCR_clean$sig <- ifelse(results_DCR_clean$pvalue < 0.05,
                                "Significant", "Not Significant")

ggplot(results_DCR_clean,
       aes(x = iOR, y = reorder(Component, iOR))) +
  geom_point(aes(color = sig), size = 4) +
  geom_errorbarh(aes(xmin = lower_95CI, xmax = upper_95CI,
                     color = sig), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Significant"     = "darkred",
                                "Not Significant" = "tomato")) +
  scale_x_log10() +
  labs(
    title   = "Incremental Effect of Each ICI Component — DCR",
    x       = "Incremental Odds Ratio (log scale)",
    y       = "Component",
    color   = "",
    caption = "Dashed line = no effect (OR = 1). Random effects model."
  ) +
  theme_classic() +
  theme(plot.title      = element_text(face = "bold", size = 13),
        axis.text       = element_text(size = 11),
        legend.position = "bottom")

# --- HR OS ---
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
    title   = "Incremental Effect of Each ICI Component — HR OS",
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

cat("======= NETWORK STRUCTURE — ORR =======\n")
cat("Number of studies (k):", discomb_ORR$k, "\n")
cat("Number of pairwise comparisons (m):", discomb_ORR$m, "\n")
cat("Number of treatments (n):", discomb_ORR$n, "\n")
cat("Number of components (c):", discomb_ORR$c, "\n")
cat("Number of subnetworks (s):", discomb_ORR$s, "\n")
cat("Number of unidentifiable components:", discomb_ORR$na.unident, "\n")
cat("Unidentifiable components:\n"); print(discomb_ORR$comps.unident)

cat("\n======= NETWORK STRUCTURE — DCR =======\n")
cat("Number of studies (k):", discomb_DCR$k, "\n")
cat("Number of pairwise comparisons (m):", discomb_DCR$m, "\n")
cat("Number of treatments (n):", discomb_DCR$n, "\n")
cat("Number of components (c):", discomb_DCR$c, "\n")
cat("Number of subnetworks (s):", discomb_DCR$s, "\n")
cat("Number of unidentifiable components:", discomb_DCR$na.unident, "\n")
cat("Unidentifiable components:\n"); print(discomb_DCR$comps.unident)

cat("\n======= NETWORK STRUCTURE — HR OS =======\n")
cat("Number of studies (k):", discomb_OS$k, "\n")
cat("Number of pairwise comparisons (m):", discomb_OS$m, "\n")
cat("Number of treatments (n):", discomb_OS$n, "\n")
cat("Number of components (c):", discomb_OS$c, "\n")
cat("Number of subnetworks (s):", discomb_OS$s, "\n")
cat("Number of unidentifiable components:", discomb_OS$na.unident, "\n")
cat("Unidentifiable components:\n"); print(discomb_OS$comps.unident)

# ============================================================
# SECTION 14B: HETEROGENEITY SUMMARY
# ============================================================

cat("\n======= HETEROGENEITY — ORR =======\n")
cat("tau:", discomb_ORR$tau, "\n")
cat("tau^2:", discomb_ORR$tau^2, "\n")
cat("I^2 (%):", discomb_ORR$I2 * 100, "\n")
cat("Q statistic:", discomb_ORR$Q.additive, "\n")
cat("Q df:", discomb_ORR$df.Q.additive, "\n")
cat("Q p-value:", discomb_ORR$pval.Q.additive, "\n")

cat("\n======= HETEROGENEITY — DCR =======\n")
cat("tau:", discomb_DCR$tau, "\n")
cat("tau^2:", discomb_DCR$tau^2, "\n")
cat("I^2 (%):", discomb_DCR$I2 * 100, "\n")
cat("Q statistic:", discomb_DCR$Q.additive, "\n")
cat("Q df:", discomb_DCR$df.Q.additive, "\n")
cat("Q p-value:", discomb_DCR$pval.Q.additive, "\n")

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
# In disconnected networks this may return NA — expected.
# Additivity must be justified clinically in your paper.

cat("\n======= ADDITIVITY TEST — ORR =======\n")
cat("Q additive:", discomb_ORR$Q.additive, "\n")
cat("Q standard:", discomb_ORR$Q.standard, "\n")
cat("Q difference:", discomb_ORR$Q.diff, "\n")
cat("Q diff df:", discomb_ORR$df.Q.diff, "\n")
cat("Q diff p-value:", discomb_ORR$pval.Q.diff, "\n")
cat("(If p > 0.05: additivity assumption holds)\n")

cat("\n======= ADDITIVITY TEST — DCR =======\n")
cat("Q additive:", discomb_DCR$Q.additive, "\n")
cat("Q standard:", discomb_DCR$Q.standard, "\n")
cat("Q difference:", discomb_DCR$Q.diff, "\n")
cat("Q diff df:", discomb_DCR$df.Q.diff, "\n")
cat("Q diff p-value:", discomb_DCR$pval.Q.diff, "\n")
cat("(If p > 0.05: additivity assumption holds)\n")

cat("\n======= ADDITIVITY TEST — HR OS =======\n")
cat("Q additive:", discomb_OS$Q.additive, "\n")
cat("Q standard:", discomb_OS$Q.standard, "\n")
cat("Q difference:", discomb_OS$Q.diff, "\n")
cat("Q diff df:", discomb_OS$df.Q.diff, "\n")
cat("Q diff p-value:", discomb_OS$pval.Q.diff, "\n")
cat("(If p > 0.05: additivity assumption holds)\n")

# ============================================================
# SECTION 14D: FULL COMPONENT ESTIMATES — ALL THREE MODELS
# ============================================================

cat("\n======= COMPONENT ESTIMATES — ORR (Random Effects) =======\n")
results_ORR_full <- data.frame(
  Component    = discomb_ORR$comps,
  iOR_random   = round(exp(discomb_ORR$Comp.random), 3),
  lower_random = round(exp(discomb_ORR$lower.Comp.random), 3),
  upper_random = round(exp(discomb_ORR$upper.Comp.random), 3),
  pval_random  = round(discomb_ORR$pval.Comp.random, 4),
  iOR_common   = round(exp(discomb_ORR$Comp.common), 3),
  lower_common = round(exp(discomb_ORR$lower.Comp.common), 3),
  upper_common = round(exp(discomb_ORR$upper.Comp.common), 3),
  pval_common  = round(discomb_ORR$pval.Comp.common, 4)
)
print(results_ORR_full)

cat("\n======= COMPONENT ESTIMATES — DCR (Random Effects) =======\n")
results_DCR_full <- data.frame(
  Component    = discomb_DCR$comps,
  iOR_random   = round(exp(discomb_DCR$Comp.random), 3),
  lower_random = round(exp(discomb_DCR$lower.Comp.random), 3),
  upper_random = round(exp(discomb_DCR$upper.Comp.random), 3),
  pval_random  = round(discomb_DCR$pval.Comp.random, 4),
  iOR_common   = round(exp(discomb_DCR$Comp.common), 3),
  lower_common = round(exp(discomb_DCR$lower.Comp.common), 3),
  upper_common = round(exp(discomb_DCR$upper.Comp.common), 3),
  pval_common  = round(discomb_DCR$pval.Comp.common, 4)
)
print(results_DCR_full)

cat("\n======= COMPONENT ESTIMATES — HR OS (Random Effects) =======\n")
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
print(results_OS_full)

# ============================================================
# SECTION 14E: TREATMENT LEVEL ESTIMATES
# ============================================================

cat("\n======= TREATMENT ESTIMATES — ORR =======\n")
treat_ORR <- data.frame(
  Treatment    = discomb_ORR$trts,
  OR_random    = round(exp(discomb_ORR$TE.random[,
                                                 which(discomb_ORR$trts == discomb_ORR$reference.group)]), 3),
  lower_random = round(exp(discomb_ORR$lower.random[,
                                                    which(discomb_ORR$trts == discomb_ORR$reference.group)]), 3),
  upper_random = round(exp(discomb_ORR$upper.random[,
                                                    which(discomb_ORR$trts == discomb_ORR$reference.group)]), 3),
  pval_random  = round(discomb_ORR$pval.random[,
                                               which(discomb_ORR$trts == discomb_ORR$reference.group)], 4)
)
print(treat_ORR)

cat("\n======= TREATMENT ESTIMATES — DCR =======\n")
treat_DCR <- data.frame(
  Treatment    = discomb_DCR$trts,
  OR_random    = round(exp(discomb_DCR$TE.random[,
                                                 which(discomb_DCR$trts == discomb_DCR$reference.group)]), 3),
  lower_random = round(exp(discomb_DCR$lower.random[,
                                                    which(discomb_DCR$trts == discomb_DCR$reference.group)]), 3),
  upper_random = round(exp(discomb_DCR$upper.random[,
                                                    which(discomb_DCR$trts == discomb_DCR$reference.group)]), 3),
  pval_random  = round(discomb_DCR$pval.random[,
                                               which(discomb_DCR$trts == discomb_DCR$reference.group)], 4)
)
print(treat_DCR)

cat("\n======= TREATMENT ESTIMATES — HR OS =======\n")
treat_OS <- data.frame(
  Treatment    = discomb_OS$trts,
  HR_random    = round(exp(discomb_OS$TE.random[,
                                                which(discomb_OS$trts == discomb_OS$reference.group)]), 3),
  lower_random = round(exp(discomb_OS$lower.random[,
                                                   which(discomb_OS$trts == discomb_OS$reference.group)]), 3),
  upper_random = round(exp(discomb_OS$upper.random[,
                                                   which(discomb_OS$trts == discomb_OS$reference.group)]), 3),
  pval_random  = round(discomb_OS$pval.random[,
                                              which(discomb_OS$trts == discomb_OS$reference.group)], 4)
)
print(treat_OS)

# ============================================================
# SECTION 14F: IDENTIFIABILITY REPORT
# ============================================================

cat("\n======= IDENTIFIABILITY REPORT — ORR =======\n")
cat("Total components:", length(discomb_ORR$comps), "\n")
identifiable_ORR     <- discomb_ORR$comps[!is.na(discomb_ORR$Comp.random)]
not_identifiable_ORR <- discomb_ORR$comps[is.na(discomb_ORR$Comp.random)]
cat("Identifiable:\n");     print(identifiable_ORR)
cat("Non-identifiable:\n"); print(not_identifiable_ORR)

cat("\n======= IDENTIFIABILITY REPORT — DCR =======\n")
cat("Total components:", length(discomb_DCR$comps), "\n")
identifiable_DCR     <- discomb_DCR$comps[!is.na(discomb_DCR$Comp.random)]
not_identifiable_DCR <- discomb_DCR$comps[is.na(discomb_DCR$Comp.random)]
cat("Identifiable:\n");     print(identifiable_DCR)
cat("Non-identifiable:\n"); print(not_identifiable_DCR)

cat("\n======= IDENTIFIABILITY REPORT — HR OS =======\n")
cat("Total components:", length(discomb_OS$comps), "\n")
identifiable_OS     <- discomb_OS$comps[!is.na(discomb_OS$Comp.random)]
not_identifiable_OS <- discomb_OS$comps[is.na(discomb_OS$Comp.random)]
cat("Identifiable:\n");     print(identifiable_OS)
cat("Non-identifiable:\n"); print(not_identifiable_OS)
cat("\nReason: Components only appearing as control arm backbones\n")
cat("in disconnected subnetworks with no shared component bridge.\n")
cat("This is expected and must be acknowledged as a limitation.\n")

# ============================================================
# SECTION 14G: SAVE ALL RESULTS TO CSV
# ============================================================

#write.csv(results_ORR_full, "CNMA_results_ORR.csv", row.names = FALSE)
#write.csv(results_DCR_full, "CNMA_results_DCR.csv", row.names = FALSE)
#write.csv(results_OS_full,  "CNMA_results_OS.csv",  row.names = FALSE)
#cat("\nResults saved.\n")

# ============================================================
# SECTION 15: SENSITIVITY — COMMON VS RANDOM COMPARISON
# ============================================================
# Large differences between common and random estimates
# indicate tau is meaningfully inflating uncertainty.

# --- ORR ---
comparison_ORR <- data.frame(
  Component     = results_ORR_full$Component,
  iOR_common    = results_ORR_full$iOR_common,
  iOR_random    = results_ORR_full$iOR_random,
  pval_common   = results_ORR_full$pval_common,
  pval_random   = results_ORR_full$pval_random,
  Estimate_diff = round(abs(results_ORR_full$iOR_random -
                              results_ORR_full$iOR_common), 3)
)
print(comparison_ORR[!is.na(comparison_ORR$iOR_random), ])

# --- DCR ---
comparison_DCR <- data.frame(
  Component     = results_DCR_full$Component,
  iOR_common    = results_DCR_full$iOR_common,
  iOR_random    = results_DCR_full$iOR_random,
  pval_common   = results_DCR_full$pval_common,
  pval_random   = results_DCR_full$pval_random,
  Estimate_diff = round(abs(results_DCR_full$iOR_random -
                              results_DCR_full$iOR_common), 3)
)
print(comparison_DCR[!is.na(comparison_DCR$iOR_random), ])

# --- HR OS ---
comparison_OS <- data.frame(
  Component     = results_OS_full$Component,
  iHR_common    = results_OS_full$iHR_common,
  iHR_random    = results_OS_full$iHR_random,
  pval_common   = results_OS_full$pval_common,
  pval_random   = results_OS_full$pval_random,
  Estimate_diff = round(abs(results_OS_full$iHR_random -
                              results_OS_full$iHR_common), 3)
)
print(comparison_OS[!is.na(comparison_OS$iHR_random), ])

