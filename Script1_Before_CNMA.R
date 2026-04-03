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
# SECTION 2: LOAD & INSPECT DATA
# ============================================================

df <- read_excel("7paper_complete_v5.xlsx")

# ============================================================
# SECTION 3: RENAME COLUMNS & SET DATA TYPES
# ============================================================

df <- df %>%
  rename(
    studlab      = `studlab`,
    treat1       = `treat1`,
    treat2       = `treat2`,
    n1           = `n1`,
    n2           = `n2`,
    event1_ORR   = `event1_ORR`,
    event2_ORR   = `event2_ORR`,
    event1_DCR   = `event1_DCR`,
    event2_DCR   = `event2_DCR`,
    hr_os_t2vt1      = `Hazard Ratio (OS)`,
    hr_os_upper_t2vt1 = `95% CI Upper Bound(OS)`,
    hr_os_lower_t2vt1 = `95% CI Lower Bound(OS)`,
    hr_os_t3vt1      = `HR OS t3 vs t1`,
    hr_os_upper_t3vt1 = `Upper CI t3 vs t1`,
    hr_os_lower_t3vt1 = `Lower CI t3 vs t1`
  )

df$studlab    <- as.character(df$studlab)
df$treat1     <- as.character(df$treat1)
df$treat2     <- as.character(df$treat2)
df$treat3     <- as.character(df$treat3)
df$n1         <- as.numeric(df$n1)
df$n2         <- as.numeric(df$n2)
df$n3         <- as.numeric(df$n3)
df$event1_ORR <- as.numeric(df$event1_ORR)
df$event2_ORR <- as.numeric(df$event2_ORR)
df$event1_DCR <- as.numeric(df$event1_DCR)
df$event2_DCR <- as.numeric(df$event2_DCR)
df$event3_ORR <- as.numeric(df$event3_ORR)
df$event3_DCR <- as.numeric(df$event3_DCR)

df$hr_os_t2vt1       <- as.numeric(df$hr_os_t2vt1)
df$hr_os_upper_t2vt1 <- as.numeric(df$hr_os_upper_t2vt1)
df$hr_os_lower_t2vt1 <- as.numeric(df$hr_os_lower_t2vt1)
df$hr_os_t3vt1       <- as.numeric(df$hr_os_t3vt1)
df$hr_os_upper_t3vt1 <- as.numeric(df$hr_os_upper_t3vt1)
df$hr_os_lower_t3vt1 <- as.numeric(df$hr_os_lower_t3vt1)

# ============================================================
# NORMALIZE ALL + SPACING CONSISTENTLY
# ============================================================

df$treat1 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat1))
df$treat2 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat2))
df$treat3 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat3))

# Verify no NA in core columns
colSums(is.na(df[, c("studlab","treat1","treat2",
                     "n1","n2","event1_ORR","event2_ORR")]))

# ============================================================
# SECTION 3B: PRE-COMPUTE log(HR) AND seTE FOR HR OS
# ============================================================
# Renouf 2022 uses 90% CI  → z = 1.645
# All other studies use 95% CI → z = 1.96
#
# Formula:
#   TE   = log(HR)
#   seTE = (log(Upper CI) - log(Lower CI)) / (2 * z)
#
# We compute these for BOTH t2 vs t1 AND t3 vs t1.
# pairwise() will then handle deriving t3 vs t2 with correct
# covariance adjustment for multi-arm trials.

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

# Quick sanity check
df[, c("studlab", "z_score", "TE_t2vt1", "seTE_t2vt1", "TE_t3vt1", "seTE_t3vt1")]

# ============================================================
# SECTION 4: PAIRWISE TRANSFORMATION (ORR)
# ============================================================

pw_ORR <- pairwise(
  treat   = list(treat1, treat2, treat3),
  event   = list(event1_ORR, event2_ORR, event3_ORR),
  n       = list(n1, n2, n3),
  studlab = studlab,
  data    = df,
  sm      = "OR",
  allstudies = TRUE
)

# Verify three-arm study shows 3 rows
pw_ORR[pw_ORR$studlab == "Enzler 2024", c("studlab","treat1","treat2","TE","seTE")]

# ============================================================
# SECTION 4B: PAIRWISE TRANSFORMATION (DCR)
# ============================================================

pw_DCR <- pairwise(
  treat   = list(treat1, treat2, treat3),
  event   = list(event1_DCR, event2_DCR, event3_DCR),
  n       = list(n1, n2, n3),
  studlab = studlab,
  data    = df,
  sm      = "OR",
  allstudies = TRUE
)

pw_DCR[pw_DCR$studlab == "Enzler 2024", c("studlab","treat1","treat2","TE","seTE")]

# ============================================================
# SECTION 4C: PAIRWISE TRANSFORMATION (HR OS)
# ============================================================
# pairwise() is used here with pre-computed TE and seTE.
# It handles:
#   - The t2 vs t1 comparison (from hr_os columns)
#   - The t3 vs t1 comparison (from hr_os_t3 columns)
#   - The t3 vs t2 comparison (derived internally with correct
#     multi-arm covariance — do NOT compute this manually)
#
# NOTE: For two-arm studies, treat3/TE_t3vt1/seTE_t3vt1 will be NA
# and pairwise() will simply produce one row for that study.

pw_OS <- pairwise(
  treat   = list(treat1, treat2, treat3),
  TE      = list(TE_t2vt1, TE_t3vt1, NA),   # t2vt1, t3vt1; t3vt2 derived
  seTE    = list(seTE_t2vt1, seTE_t3vt1, NA),
  studlab = studlab,
  data    = df,
  sm      = "HR"
)

# Verify: three-arm study should show 3 rows; two-arm studies show 1 row
pw_OS[pw_OS$studlab == "Enzler 2024", c("studlab","treat1","treat2","TE","seTE")]

# ============================================================
# SECTION 5: INSPECT DISCONNECTED NETWORK STRUCTURE (ORR)
# ============================================================

nc_ORR <- netconnection(
  treat1  = treat1,
  treat2  = treat2,
  studlab = studlab,
  data    = pw_ORR
)

print(nc_ORR)

# ============================================================
# SECTION 5B: INSPECT DISCONNECTED NETWORK STRUCTURE (HR OS)
# ============================================================

nc_OS <- netconnection(
  treat1  = treat1,
  treat2  = treat2,
  studlab = studlab,
  data    = pw_OS
)

print(nc_OS)

# ============================================================
# SECTION 6: NETWORK DIAGRAM — ORR
# ============================================================

netgraph(
  nc_ORR,
  plastic      = FALSE,
  thickness    = "number.of.studies",
  points       = TRUE,
  col          = "black",
  col.points   = "steelblue",
  cex.points   = 3,
  cex          = 0.8,
  main         = "Standard Network Graph — ORR (Disconnected Network)"
)

# ============================================================
# SECTION 6B: NETWORK DIAGRAM — HR OS
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
  main         = "Standard Network Graph — HR OS (Disconnected Network)"
)

# ============================================================
# SECTION 7: UPSET PLOT — COMPONENT CO-OCCURRENCE
# ============================================================
# Uses ORR treatment arms (same arms apply across all outcomes)

ls("package:viscomp")

all_treats     <- unique(c(df$treat1, df$treat2, df$treat3))
all_treats     <- all_treats[!is.na(all_treats)]
all_components <- unique(trimws(unlist(strsplit(all_treats, "\\+"))))
all_components

arm_data <- data.frame(arm = all_treats)

for (comp in all_components) {
  arm_data[[comp]] <- as.integer(
    grepl(comp, arm_data$arm, fixed = TRUE)
  )
}

upset_input <- arm_data[, -1]

upset(
  upset_input,
  sets             = all_components,
  order.by         = "freq",
  decreasing       = TRUE,
  main.bar.color   = "steelblue",
  sets.bar.color   = "tomato",
  text.scale       = 1.3,
  mb.ratio         = c(0.6, 0.4),
  mainbar.y.label  = "Number of Arms with Combination",
  sets.x.label     = "Total Arms per Component"
)

# ============================================================
# SECTION 8: COMPONENT CO-OCCURRENCE HEATMAP
# ============================================================

n_comp <- length(all_components)
comat  <- matrix(0, nrow = n_comp, ncol = n_comp,
                 dimnames = list(all_components, all_components))

for (arm in all_treats) {
  present <- trimws(unlist(strsplit(arm, "\\+")))
  for (i in present) {
    for (j in present) {
      comat[i, j] <- comat[i, j] + 1
    }
  }
}

# --- Option A: corrplot ---
comat_prop <- comat / max(comat)

corrplot(
  comat_prop,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.cex      = 0.9,
  addCoef.col = "black",
  col         = colorRampPalette(c("white", "steelblue", "darkblue"))(200),
  title       = "Component Co-occurrence — Check ICI Identifiability",
  mar         = c(0, 0, 2, 0)
)

# --- Option B: ComplexHeatmap ---
col_fun <- colorRamp2(
  c(0, max(comat) / 2, max(comat)),
  c("white", "steelblue", "darkblue")
)

Heatmap(
  comat,
  name            = "# Arms",
  col             = col_fun,
  cell_fun        = function(j, i, x, y, width, height, fill) {
    grid.text(comat[i, j], x, y,
              gp = gpar(fontsize = 10, col = "black"))
  },
  cluster_rows    = FALSE,
  cluster_columns = FALSE,
  row_title       = "Component",
  column_title    = "Component Co-occurrence — CNMA Identifiability Check",
  rect_gp         = gpar(col = "grey80", lwd = 1)
)
