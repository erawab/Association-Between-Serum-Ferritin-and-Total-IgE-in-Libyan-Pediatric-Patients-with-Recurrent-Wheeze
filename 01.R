# ==================== 3.1 Study Flow and Analytical Sample ====================

library(dplyr)
library(gt)

# Automatically create output folder
output_dir <- "Results_Output/1_Study_Flow"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("✅ Folder created:", output_dir, "\n")
}

# Load the data
df <- read.csv("Correlation total IGE  Vs ferritin.csv", 
               stringsAsFactors = FALSE, 
               check.names = FALSE)   # Important: keeps original column names

# Clean column names (remove extra spaces and fix first column)
colnames(df)[1] <- "Patient_ID"
colnames(df) <- trimws(colnames(df))   # Remove leading/trailing spaces

# Remove completely empty columns (Unnamed)
df <- df[, !grepl("^Unnamed", colnames(df))]

cat("Cleaned columns:", paste(colnames(df), collapse = ", "), "\n\n")

total_screened <- nrow(df)

# Primary analysis: complete Serum_Ferritin AND Total_IgE
complete_primary <- sum(!is.na(df$Serum_Ferritin) & !is.na(df$Total_IgE))

# All main variables complete
vars_all <- c("Age", "Gender", "Patient_atopy", "Family_atopy", 
              "Total_IgE", "Serum_Ferritin", "RBCs", "Hb", "platelet")

complete_all <- sum(complete.cases(df[vars_all]))

missing_primary <- total_screened - complete_primary

# Create flow table
flow_table <- data.frame(
  Stage = c("Records initially screened",
            "Excluded due to missing serum ferritin or total IgE",
            "Included in primary analysis (ferritin and IgE)",
            "Complete data for all study variables"),
  N = c(total_screened, missing_primary, complete_primary, complete_all),
  Percentage = c(100, 
                 round(100 * missing_primary / total_screened, 1),
                 round(100 * complete_primary / total_screened, 1),
                 round(100 * complete_all / total_screened, 1))
)

# Print to console
print(flow_table)

# Save beautiful HTML table
flow_table %>%
  gt() %>%
  tab_header(title = "Study Flow and Analytical Sample") %>%
  cols_label(
    Stage = "Stage",
    N = "n",
    Percentage = "%"
  ) %>%
  fmt_number(columns = Percentage, decimals = 1) %>%
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_column_labels()) %>%
  gtsave("1_Study_Flow_Table.html", path = output_dir)

cat("\n✅ Success! Study Flow Table saved in:\n")
cat(output_dir, "/1_Study_Flow_Table.html\n")