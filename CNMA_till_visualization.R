# ============================================================
# SECTION 1: INSTALL & LOAD PACKAGES
# ============================================================

#install.packages(c("netmeta", "readxl", "dplyr", "UpSetR", "viscomp", 
#                  "corrplot", "ComplexHeatmap", "circlize"))
#if (!require("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")


library(netmeta)
library(readxl)
library(dplyr)
library(UpSetR)
library(viscomp)
library(corrplot)
library(ComplexHeatmap)
library(circlize)

# ============================================================
# SECTION 2: LOAD & INSPECT DATA
# ============================================================

df <- read_excel("7paper_complete_v4.1.xlsx")

str(df)
head(df)
colnames(df)

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
    event2_DCR   = `event2_DCR`
  )

df$studlab    <- as.character(df$studlab)
df$treat1     <- as.character(df$treat1)
df$treat2     <- as.character(df$treat2)
df$n1         <- as.numeric(df$n1)
df$n2         <- as.numeric(df$n2)
df$event1_ORR <- as.numeric(df$event1_ORR)
df$event2_ORR <- as.numeric(df$event2_ORR)
df$event1_DCR <- as.numeric(df$event1_DCR)
df$event2_DCR <- as.numeric(df$event2_DCR)
df$treat3     <- as.character(df$treat3)
df$n3         <- as.numeric(df$n3)
df$event3_ORR <- as.numeric(df$event3_ORR)
df$event3_DCR <- as.numeric(df$event3_DCR)
# Check for any inconsistent spacing around + signs
# This normalizes ALL treatment names in one go
df$treat1 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat1))
df$treat2 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat2))
df$treat3 <- gsub("\\s*\\+\\s*", " + ", trimws(df$treat3))



colSums(is.na(df[, c("studlab","treat1","treat2",
                     "n1","n2","event1_ORR","event2_ORR")]))


#==========================================================================
df <- df %>%
  mutate(
    event2_ORR = ifelse(studlab == "Enzler 2024" & 
                          event2_ORR == 0 & event3_ORR == 0,
                        event2_ORR + 0.5, event2_ORR),
    event3_ORR = ifelse(studlab == "Enzler 2024" & 
                          event3_ORR == 0 & event2_ORR %in% c(0, 0.5),
                        event3_ORR + 0.5, event3_ORR)
  )

# Verify the correction
df[df$studlab == "Enzler 2024", 
   c("studlab", "event1_ORR", "event2_ORR", "event3_ORR",
     "n1", "n2", "n3")]
#=========================================================================


# ============================================================
# SECTION 4: PAIRWISE TRANSFORMATION (ORR)
# ============================================================
pw_ORR <- pairwise(
  treat   = list(treat1, treat2, treat3),
  event   = list(event1_ORR, event2_ORR, event3_ORR),
  n       = list(n1, n2, n3),
  studlab = studlab,
  data    = df,
  sm      = "OR"    # adds 0.5 to ALL zero cells automatically
)

# Verify Enzler shows exactly 3 rows in pw_ORR
pw_ORR[pw_ORR$studlab == "Enzler 2024", c("studlab","treat1","treat2","TE","seTE")]
# ============================================================
# SECTION 5: INSPECT DISCONNECTED NETWORK STRUCTURE
# ============================================================
# netconnection() is specifically designed to map disconnected
# networks — use this INSTEAD of netmeta() for your data

nc <- netconnection(
  treat1  = treat1,
  treat2  = treat2,
  studlab = studlab,
  data    = pw_ORR
)

print(nc)
# This will show you the 6 separate sub-networks —
# confirm which trials fall into which island

# ============================================================
# SECTION 6: VISUALIZATION 1 — NETWORK DIAGRAM
# ============================================================
# netgraph() works directly on the netconnection object
# It will show the disconnected islands — which is exactly
# the visual proof you need for your paper

netgraph(
  nc,
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
# SECTION 7: VISUALIZATION 2 — CNMA UPSET PLOT
# ============================================================
# PURPOSE: Unlike the network graph above, this plot will
# reveal the ICI component as the COMMON THREAD running
# through all disconnected arms — visually justifying discomb()
#==================================================================
# Check what functions viscomp actually exports
ls("package:viscomp")
#==================================================================
# Step 1: Extract all unique treatment arms
all_treats <- unique(c(df$treat1, df$treat2))

# Step 2: Split on "+" to get individual components
all_components <- unique(trimws(unlist(strsplit(all_treats, "\\+"))))
all_components   # inspect — ICI component should appear frequently

# Step 3: Build binary presence/absence matrix
arm_data <- data.frame(arm = all_treats)

for (comp in all_components) {
  arm_data[[comp]] <- as.integer(
    grepl(comp, arm_data$arm, fixed = TRUE)
  )
}

upset_input <- arm_data[, -1]

# Step 4: Plot
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

# viscomp placeholder — activated after discomb() in next session
# viscomp(discomb_object)        # <-- note: discomb, NOT netcomb

# ============================================================
# SECTION 8: VISUALIZATION 3 — CNMA HEAT MAP
# ============================================================
# KEY THING TO LOOK FOR: The ICI component's row/column should
# show co-occurrence with EVERY chemotherapy backbone.
# If any two components always appear together (off-diagonal
# value = diagonal value), flag them — identifiability problem.

# Step 1: Build co-occurrence matrix
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

# --- Option B: ComplexHeatmap (recommended — shows raw counts) ---
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

