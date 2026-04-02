# Install/load required packages
if (!require("survival")) install.packages("survival")
if (!require("dplyr")) install.packages("dplyr")

library(survival)
library(dplyr)

# Load your data
data <- read.csv("enzler_Final_pseudoIPD.csv")

# Preview the data
head(data)
str(data)

# Ensure Treatment is a factor with treat1 as reference/baseline
data$Treatment <- factor(data$Treatment, levels = c("treat1", "treat2", "treat3"))

# Fit Cox Proportional Hazards Model
cox_model <- coxph(Surv(Time, Status) ~ Treatment, data = data)

# View full model summary
summary(cox_model)

# ── Extract HR and 95% CI ──────────────────────────────────────────
cox_results <- summary(cox_model)

hr_table <- data.frame(
  Comparison      = rownames(cox_results$conf.int),
  HR              = round(cox_results$conf.int[, "exp(coef)"],    4),
  CI_Lower_95     = round(cox_results$conf.int[, "lower .95"],    4),
  CI_Upper_95     = round(cox_results$conf.int[, "upper .95"],    4),
  P_Value         = round(cox_results$coefficients[, "Pr(>|z|)"], 4)
)

# Clean up comparison labels
hr_table$Comparison <- gsub("Treatment", "", hr_table$Comparison)
hr_table$Comparison <- paste0(hr_table$Comparison, " vs treat1")

# Print final table
print(hr_table)

# ── Optional: Save results to CSV ─────────────────────────────────
write.csv(hr_table, "HR_OS_Results.csv", row.names = FALSE)