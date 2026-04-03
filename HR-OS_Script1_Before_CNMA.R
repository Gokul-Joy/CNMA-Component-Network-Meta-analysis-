# ============================================================
# SECTION 1: INSTALL & LOAD PACKAGES
# ============================================================

#install.packages(c("netmeta", "readxl", "dplyr", "UpSetR", "viscomp",
#                  "corrplot", "ComplexHeatmap", "circlize", "ggplot2"))
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")

library(netmeta)
library(readxl)
library(dplyr)
library(UpSetR)
library(viscomp)
library(corrplot)
library(ComplexHeatmap)
library(circlize)
library(ggplot2)

# ============================================================
# SECTION 2: LOAD DATA
# ============================================================

df <- read_excel("13paper_combined_v2.xlsx")   # <-- update filename here

# ============================================================
# SECTION 3: RENAME COLUMNS & SET DATA TYPES
# ============================================================

df <- df %>%
  rename(
    studlab           = `studlab`,
    treat1            = `treat1`,
    treat2            = `treat2`,
    treat3            = `treat3`,
    hr_os_t2vt1       = `Hazard Ratio (OS)`,
    hr_os_lower_t2vt1 = `95% CI Lower Bound(OS)`,
    hr_os_upper_t2vt1 = `95% CI Upper Bound(OS)`,
    hr_os_t3vt1       = `HR OS t3 vs t1`,
    hr_os_lower_t3vt1 = `Lower CI t3 vs t1`,
    hr_os_upper_t3vt1 = `Upper CI t3 vs t1`
  )

df$studlab            <- as.character(df$studlab)
df$treat1             <- as.character(df$treat1)
df$treat2             <- as.character(df$treat2)
df$treat3             <- as.character(df$treat3)
df$hr_os_t2vt1        <- as.numeric(df$hr_os_t2vt1)
df$hr_os_lower_t2vt1  <- as.numeric(df$hr_os_lower_t2vt1)
df$hr_os_upper_t2vt1  <- as.numeric(df$hr_os_upper_t2vt1)
df$hr_os_t3vt1        <- as.numeric(df$hr_os_t3vt1)
df$hr_os_lower_t3vt1  <- as.numeric(df$hr_os_lower_t3vt1)
df$hr_os_upper_t3vt1  <- as.numeric(df$hr_os_upper_t3vt1)

# ============================================================
# NORMALIZE + SPACING CONSISTENTLY
# ============================================================
# Critical — discomb() splits on " + " exactly.
# All treatment names must have exactly one space either side of +

df$treat1 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat1))
df$treat2 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat2))
df$treat3 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat3))

# Verify no NA in core columns
colSums(is.na(df[, c("studlab", "treat1", "treat2",
                     "hr_os_t2vt1", "hr_os_lower_t2vt1", "hr_os_upper_t2vt1")]))

# ============================================================
# SECTION 3B: PRE-COMPUTE log(HR) AND seTE FOR HR OS
# ============================================================
# Renouf 2022 uses 90% CI → z = 1.645
# All other studies use 95% CI → z = 1.96
#
# Formula:
#   TE   = log(HR)
#   seTE = (log(Upper CI) - log(Lower CI)) / (2 * z)
#
# Computed for t2vt1 AND t3vt1.
# t3vt2 is derived manually in Section 4C for three-arm studies.

df <- df %>%
  mutate(
    z_score = ifelse(studlab == "Renouf 2022", 1.645, 1.96),

    # t2 vs t1
    TE_t2vt1   = log(hr_os_t2vt1),
    seTE_t2vt1 = (log(hr_os_upper_t2vt1) - log(hr_os_lower_t2vt1)) / (2 * z_score),

    # t3 vs t1 (only populated for three-arm studies)
    TE_t3vt1   = log(hr_os_t3vt1),
    seTE_t3vt1 = (log(hr_os_upper_t3vt1) - log(hr_os_lower_t3vt1)) / (2 * z_score)
  )

# Sanity check — verify z_score assignment and computed values
df[, c("studlab", "z_score", "TE_t2vt1", "seTE_t2vt1", "TE_t3vt1", "seTE_t3vt1")]

# Verify three-arm studies are correctly identified
cat("\nThree-arm studies detected:\n")
print(df[!is.na(df$hr_os_t3vt1), c("studlab", "treat1", "treat2", "treat3",
                                     "TE_t2vt1", "seTE_t2vt1",
                                     "TE_t3vt1", "seTE_t3vt1")])

# ============================================================
# SECTION 4C: BUILD pw_OS MANUALLY (contrast-based format)
# ============================================================
# pairwise() cannot handle pre-computed TE/seTE in wide format
# reliably with NA structure — all attempts produced errors or
# sign reversal. Manual build is used instead.
#
# Two-arm studies:  1 row each  (t2 vs t1)
# Three-arm studies: 3 rows each
#   Row 1 — t2 vs t1: directly from data
#   Row 2 — t3 vs t1: directly from data
#   Row 3 — t3 vs t2: TE   = TE_t3vt1 - TE_t2vt1
#                     seTE = sqrt(seTE_t3vt1^2 + seTE_t2vt1^2)
#
# NOTE ON COVARIANCE: The sqrt() formula assumes independence
# between arms. In a three-arm trial this slightly inflates seTE
# for the t3vt2 row because t2vt1 and t3vt1 share the control arm.
# However, discomb() identifies multi-arm studies via identical
# studlab values and applies its own internal reweighting.
# Since each three-arm study occupies its own isolated subnetwork
# in a disconnected CNMA, this inflation has no effect on
# identifiable component estimates. Acknowledge as minor limitation.

two_arm <- df %>%
  filter(is.na(hr_os_t3vt1)) %>%
  transmute(
    studlab = studlab,
    treat1  = treat1,
    treat2  = treat2,
    TE      = TE_t2vt1,
    seTE    = seTE_t2vt1
  )

e <- df %>% filter(!is.na(hr_os_t3vt1))

three_arm <- bind_rows(
  # Row 1: t2 vs t1
  transmute(e, studlab, treat1, treat2,
            TE   = TE_t2vt1,
            seTE = seTE_t2vt1),
  # Row 2: t3 vs t1
  transmute(e, studlab, treat1,
            treat2 = treat3,
            TE     = TE_t3vt1,
            seTE   = seTE_t3vt1),
  # Row 3: t3 vs t2 — derived
  transmute(e, studlab,
            treat1 = treat2,
            treat2 = treat3,
            TE     = TE_t3vt1 - TE_t2vt1,
            seTE   = sqrt(seTE_t3vt1^2 + seTE_t2vt1^2))
)

pw_OS <- as.data.frame(bind_rows(two_arm, three_arm))
pw_OS$studlab <- as.character(pw_OS$studlab)
pw_OS$treat1  <- as.character(pw_OS$treat1)
pw_OS$treat2  <- as.character(pw_OS$treat2)

# Verify full pw_OS — expect: (n two-arm studies × 1) + (n three-arm studies × 3) rows
print(pw_OS)

# Verify each three-arm study shows 3 rows
cat("\nThree-arm study rows in pw_OS:\n")
three_arm_studies <- df$studlab[!is.na(df$hr_os_t3vt1)]
for (s in three_arm_studies) {
  cat("\n---", s, "---\n")
  print(pw_OS[pw_OS$studlab == s, ])
}

# ============================================================
# SECTION 5: INSPECT DISCONNECTED NETWORK STRUCTURE
# ============================================================

nc_OS <- netconnection(
  treat1  = treat1,
  treat2  = treat2,
  studlab = studlab,
  data    = pw_OS
)

print(nc_OS)

# ============================================================
# SECTION 6: NETWORK DIAGRAM — HR OS
# ============================================================

netgraph(
  nc_OS,
  plastic      = FALSE,
  thickness    = "number.of.studies",
  points       = TRUE,
  col          = "black",
  col.points   = "forestgreen",
  cex.points   = 3,
  cex          = 0.8,
  main         = "Network Graph — HR OS (Disconnected Network)"
)

# ============================================================
# SECTION 7: UPSET PLOT — HR OS
# ============================================================

all_treats_OS     <- unique(c(pw_OS$treat1, pw_OS$treat2))
all_treats_OS     <- all_treats_OS[!is.na(all_treats_OS)]
all_components_OS <- unique(trimws(unlist(strsplit(all_treats_OS, "\\+"))))
all_components_OS

arm_data_OS <- data.frame(arm = all_treats_OS)
for (comp in all_components_OS) {
  arm_data_OS[[comp]] <- as.integer(grepl(comp, arm_data_OS$arm, fixed = TRUE))
}
upset_input_OS <- arm_data_OS[, -1]

upset(
  upset_input_OS,
  sets             = all_components_OS,
  order.by         = "freq",
  decreasing       = TRUE,
  main.bar.color   = "forestgreen",
  sets.bar.color   = "darkgreen",
  text.scale       = 1.3,
  mb.ratio         = c(0.6, 0.4),
  mainbar.y.label  = "Number of Arms with Combination",
  sets.x.label     = "Total Arms per Component"
)

# ============================================================
# SECTION 8: CO-OCCURRENCE HEATMAP — HR OS
# ============================================================

n_comp_OS <- length(all_components_OS)
comat_OS  <- matrix(0, nrow = n_comp_OS, ncol = n_comp_OS,
                    dimnames = list(all_components_OS, all_components_OS))

for (arm in all_treats_OS) {
  present <- trimws(unlist(strsplit(arm, "\\+")))
  for (i in present) for (j in present) comat_OS[i, j] <- comat_OS[i, j] + 1
}

# --- Option A: ComplexHeatmap ---
col_fun_OS <- colorRamp2(
  c(0, max(comat_OS) / 2, max(comat_OS)),
  c("white", "forestgreen", "darkgreen")
)

Heatmap(
  comat_OS,
  name            = "# Arms",
  col             = col_fun_OS,
  cell_fun        = function(j, i, x, y, width, height, fill) {
    grid.text(comat_OS[i, j], x, y, gp = gpar(fontsize = 10, col = "black"))
  },
  cluster_rows    = FALSE,
  cluster_columns = FALSE,
  row_title       = "Component",
  column_title    = "Component Co-occurrence — HR OS Network",
  rect_gp         = gpar(col = "grey80", lwd = 1)
)

# --- Option B: corrplot ---
comat_OS_prop <- comat_OS / max(comat_OS)
corrplot(
  comat_OS_prop,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.cex      = 0.9,
  addCoef.col = "black",
  col         = colorRampPalette(c("white", "forestgreen", "darkgreen"))(200),
  title       = "Component Co-occurrence — HR OS Identifiability Check",
  mar         = c(0, 0, 2, 0)
)
