# ==================== 3.3 Distribution of Serum Ferritin and Total IgE Levels ====================

library(dplyr)
library(gt)
library(ggplot2)
library(tidyr)

# Create output folder
output_dir <- "Results_Output/3_Ferritin_IgE_Distribution"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("✅ Folder created:", output_dir, "\n")
}

# Load and clean data
df <- read.csv("Correlation total IGE  Vs ferritin.csv", 
               stringsAsFactors = FALSE, 
               check.names = FALSE, 
               na.strings = c("", "NA", "NaN"))

# Clean column names
colnames(df)[1] <- "Patient_ID"
colnames(df) <- trimws(colnames(df))
colnames(df) <- make.names(colnames(df), unique = TRUE)
df <- df[, !grepl("^Unnamed", colnames(df))]

# Use the correct 150 patients with complete ferritin and IgE
df_primary <- df %>% 
  filter(!is.na(Serum_Ferritin) & !is.na(Total_IgE))

cat("Number of patients in primary analysis (N):", nrow(df_primary), "\n\n")

# Create readable labels
df_primary <- df_primary %>%
  mutate(
    Ferritin_Label = case_when(
      Serum_Ferritin == 1 ~ "Low",
      Serum_Ferritin == 2 ~ "Normal",
      TRUE ~ "Other"
    ),
    IgE_Label = case_when(
      Total_IgE == 1 ~ "Low",
      Total_IgE == 2 ~ "Normal",
      Total_IgE == 3 ~ "High",
      TRUE ~ "Missing"
    )
  )

# Cross-tabulation
cross_tab <- df_primary %>%
  count(Ferritin_Label, IgE_Label) %>%
  pivot_wider(names_from = IgE_Label, values_from = n, values_fill = 0)

print("Cross-tabulation of Serum Ferritin vs Total IgE:")
print(cross_tab)

# Save as HTML Table 3
cross_tab %>%
  gt() %>%
  tab_header(title = "Table 3. Distribution of Total IgE Levels According to Serum Ferritin Categories (N = 150)") %>%
  cols_label(Ferritin_Label = "Serum Ferritin Category") %>%
  tab_spanner(label = "Total IgE Category", columns = c(Low, Normal, High)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_footnote(footnote = "Data presented as n. Categories defined according to hospital pediatric reference ranges.") %>%
  gtsave("3_Ferritin_IgE_CrossTab.html", path = output_dir)

# Colored Stacked Bar Chart (Figure 3)
ggplot(df_primary, aes(x = Ferritin_Label, fill = IgE_Label)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_fill_manual(values = c("Low" = "#E74C3C", 
                               "Normal" = "#3498DB", 
                               "High" = "#2ECC71")) +
  labs(title = "Distribution of Total IgE Levels by Serum Ferritin Category",
       x = "Serum Ferritin Category",
       y = "Proportion of Patients",
       fill = "Total IgE Level") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")
ggsave("3_Ferritin_IgE_Stacked.png", path = output_dir, width = 8, height = 6, dpi = 600)

cat("\n✅ Subsection 3.3 completed successfully!\n")
cat("Files saved in:", output_dir, "\n")
cat("   • 3_Ferritin_IgE_CrossTab.html   (Table 3)\n")
cat("   • 3_Ferritin_IgE_Stacked.png    (Figure 3)\n")