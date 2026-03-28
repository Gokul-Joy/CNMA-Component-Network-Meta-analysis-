# ============================================================
# SECTION 13: FOREST PLOTS — COMPONENT LEVEL ESTIMATES
# ============================================================

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
# ============================================================
# SECTION 13C: CLEAN CUSTOM FOREST PLOT (if built-in looks bad)
# ============================================================
# Use this if forest() output is messy — builds from results tables
# Uses only the identifiable components (drops NA rows)

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
  geom_vline(xintercept = 1, linetype = "dashed", 
             color = "grey40") +
  scale_color_manual(values = c("Significant"     = "darkblue",
                                "Not Significant" = "steelblue")) +
  scale_x_log10() +           # log scale for OR
  labs(
    title    = "Incremental Effect of Each ICI Component — ORR",
    x        = "Incremental Odds Ratio (log scale)",
    y        = "Component",
    color    = "",
    caption  = "Dashed line = no effect (OR=1). Random effects model."
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(face = "bold", size = 13),
    axis.text    = element_text(size = 11),
    legend.position = "bottom"
  )

# --- DCR ---
results_DCR_clean <- results_DCR[!is.na(results_DCR$iOR), ]
results_DCR_clean$sig <- ifelse(results_DCR_clean$pvalue < 0.05,
                                "Significant", "Not Significant")

ggplot(results_DCR_clean,
       aes(x = iOR, y = reorder(Component, iOR))) +
  geom_point(aes(color = sig), size = 4) +
  geom_errorbarh(aes(xmin = lower_95CI, xmax = upper_95CI,
                     color = sig), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey40") +
  scale_color_manual(values = c("Significant"     = "darkred",
                                "Not Significant" = "tomato")) +
  scale_x_log10() +
  labs(
    title    = "Incremental Effect of Each ICI Component — DCR",
    x        = "Incremental Odds Ratio (log scale)",
    y        = "Component",
    color    = "",
    caption  = "Dashed line = no effect (OR=1). Random effects model."
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(face = "bold", size = 13),
    axis.text    = element_text(size = 11),
    legend.position = "bottom"
  )

