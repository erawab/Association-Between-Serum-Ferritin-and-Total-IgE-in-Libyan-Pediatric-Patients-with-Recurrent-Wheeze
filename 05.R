# ==================== 3.5 Exploratory and Subgroup Analyses ====================

library(dplyr)
library(gt)
library(ggplot2)

# Create output folder
output_dir <- "Results_Output/5_Subgroup_Analyses"
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

# Primary analysis - N=150
df_primary <- df %>% 
  filter(!is.na(Serum_Ferritin) & !is.na(Total_IgE))

cat("Number of patients in primary analysis:", nrow(df_primary), "\n\n")

# Create readable labels
df_primary <- df_primary %>%
  mutate(
    Age_Group = case_when(Age == 1 ~ "2–6 years", 
                          Age == 2 ~ "6–10 years", 
                          Age == 3 ~ "10–14 years"),
    Gender_Label = case_when(Gender == 1 ~ "Male", Gender == 2 ~ "Female"),
    Patient_Atopy_Label = case_when(Patient_atopy == 1 ~ "None",
                                    Patient_atopy == 2 ~ "Allergic rhinitis",
                                    Patient_atopy == 3 ~ "Eczema"),
    Family_Atopy_Label = case_when(Family_atopy == 1 ~ "None",
                                   Family_atopy == 2 ~ "Bronchial asthma",
                                   Family_atopy == 3 ~ "Allergic rhinitis",
                                   Family_atopy == 4 ~ "Eczema"),
    Ferritin_Label = case_when(Serum_Ferritin == 1 ~ "Low", 
                               Serum_Ferritin == 2 ~ "Normal"),
    IgE_Label = case_when(Total_IgE == 1 ~ "Low",
                          Total_IgE == 2 ~ "Normal",
                          Total_IgE == 3 ~ "High")
  )

# Function to run subgroup analysis
run_subgroup <- function(data, group_var, group_name) {
  tab <- table(data[[group_var]], data$IgE_Label)
  chi_p <- chisq.test(tab, simulate.p.value = TRUE)$p.value
  
  summary_df <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      `High IgE n (%)` = paste0(sum(IgE_Label == "High"), " (", 
                                round(100 * mean(IgE_Label == "High"), 1), "%)"),
      `p-value` = round(chi_p, 3)
    )
  
  cat("\n=== Subgroup:", group_name, "===\n")
  print(summary_df)
  
  # Save as HTML
  summary_df %>%
    gt() %>%
    tab_header(title = paste("High IgE Distribution by", group_name, "(N = 150)")) %>%
    tab_footnote(footnote = "P-values from Chi-square test (simulated).") %>%
    gtsave(paste0("5_Subgroup_", gsub(" ", "_", group_name), ".html"), path = output_dir)
}

# Run subgroups
run_subgroup(df_primary, "Age_Group", "Age Group")
run_subgroup(df_primary, "Gender_Label", "Gender")
run_subgroup(df_primary, "Patient_Atopy_Label", "Patient Atopy")
run_subgroup(df_primary, "Family_Atopy_Label", "Family Atopy")

# Figure 4: IgE distribution by Age Group
ggplot(df_primary, aes(x = Age_Group, fill = IgE_Label)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Low" = "#E74C3C", "Normal" = "#3498DB", "High" = "#2ECC71")) +
  labs(title = "Total IgE Distribution by Age Group",
       x = "Age Group", 
       y = "Proportion of Patients",
       fill = "Total IgE Level") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("5_IgE_by_Age.png", path = output_dir, width = 8, height = 6, dpi = 600)

cat("\n✅ Subsection 3.5 completed successfully!\n")
cat("All subgroup tables and Figure 4 saved in:", output_dir, "\n")