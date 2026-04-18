# ==================== 3.4 Primary Analysis: Association Between Serum Ferritin and Total IgE ====================

library(dplyr)
library(gt)
library(stats)

# Create output folder
output_dir <- "Results_Output/4_Primary_Association"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("✅ Folder created:", output_dir, "\n")
}

# Load and clean data
df <- read.csv("Correlation total IGE  Vs ferritin.csv", 
               stringsAsFactors = FALSE, 
               check.names = FALSE, 
               na.strings = c("", "NA", "NaN"))

colnames(df)[1] <- "Patient_ID"
colnames(df) <- trimws(colnames(df))
colnames(df) <- make.names(colnames(df), unique = TRUE)
df <- df[, !grepl("^Unnamed", colnames(df))]

# Filter for primary analysis (N=150)
df_primary <- df %>% 
  filter(!is.na(Serum_Ferritin) & !is.na(Total_IgE))

cat("Number of patients in primary analysis:", nrow(df_primary), "\n\n")

# Create readable labels
df_primary <- df_primary %>%
  mutate(
    Ferritin_Label = case_when(Serum_Ferritin == 1 ~ "Low", 
                               Serum_Ferritin == 2 ~ "Normal"),
    IgE_Label = case_when(Total_IgE == 1 ~ "Low",
                          Total_IgE == 2 ~ "Normal",
                          Total_IgE == 3 ~ "High")
  )

# Create contingency table
cont_table <- table(df_primary$Ferritin_Label, df_primary$IgE_Label)
print("Contingency Table (Ferritin vs IgE):")
print(cont_table)

# Chi-square test
chi_result <- chisq.test(cont_table, correct = FALSE)

cat("\n=== Chi-square Test Results ===\n")
cat("Chi-square statistic :", round(chi_result$statistic, 3), "\n")
cat("Degrees of freedom   :", chi_result$parameter, "\n")
cat("p-value              :", format.pval(chi_result$p.value, digits = 4), "\n")

# Spearman rank correlation (treating categories as ordinal)
spear_result <- cor.test(as.numeric(df_primary$Serum_Ferritin), 
                         as.numeric(df_primary$Total_IgE), 
                         method = "spearman")

cat("\n=== Spearman Rank Correlation ===\n")
cat("rho                  :", round(spear_result$estimate, 3), "\n")
cat("p-value              :", format.pval(spear_result$p.value, digits = 4), "\n")

# Save nice HTML Table 4
cont_df <- as.data.frame.matrix(cont_table)
cont_df <- cbind("Serum Ferritin" = rownames(cont_df), cont_df)
rownames(cont_df) <- NULL

cont_df %>%
  gt() %>%
  tab_header(title = "Table 4. Association Between Serum Ferritin Categories and Total IgE Levels (N = 150)") %>%
  tab_spanner(label = "Total IgE Category", columns = c(Low, Normal, High)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  fmt_number(columns = c(Low, Normal, High), decimals = 0) %>%
  tab_footnote(footnote = "Data presented as n. Chi-square test and Spearman correlation were performed.") %>%
  gtsave("4_Primary_Association_Table.html", path = output_dir)

cat("\n✅ Subsection 3.4 completed successfully!\n")
cat("Table saved as: 4_Primary_Association_Table.html\n")