# ==================== 3.2 Demographic and Clinical Characteristics ====================

library(dplyr)
library(gt)
library(ggplot2)
library(tidyr)

output_dir <- "Results_Output/2_Demographics"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

df <- read.csv("Correlation total IGE  Vs ferritin.csv", 
               stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))

colnames(df)[1] <- "Patient_ID"
colnames(df) <- trimws(colnames(df))
colnames(df) <- make.names(colnames(df), unique = TRUE)
df <- df[, !grepl("^Unnamed", colnames(df))]

df_primary <- df %>% filter(!is.na(Serum_Ferritin) & !is.na(Total_IgE))

df_primary <- df_primary %>%
  mutate(
    Age_Group = case_when(Age == 1 ~ "2–6 years", Age == 2 ~ "6–10 years", Age == 3 ~ "10–14 years"),
    Gender_Label = case_when(Gender == 1 ~ "Male", Gender == 2 ~ "Female"),
    Patient_Atopy_Label = case_when(Patient_atopy == 1 ~ "None", 
                                    Patient_atopy == 2 ~ "Allergic rhinitis", 
                                    Patient_atopy == 3 ~ "Eczema"),
    Family_Atopy_Label = case_when(Family_atopy == 1 ~ "None",
                                   Family_atopy == 2 ~ "Bronchial asthma",
                                   Family_atopy == 3 ~ "Allergic rhinitis",
                                   Family_atopy == 4 ~ "Eczema")
  )

# Summary table
demo_summary <- df_primary %>%
  summarise(
    `Age group: 2–6 years` = sum(Age_Group == "2–6 years", na.rm = TRUE),
    `Age group: 6–10 years` = sum(Age_Group == "6–10 years", na.rm = TRUE),
    `Age group: 10–14 years` = sum(Age_Group == "10–14 years", na.rm = TRUE),
    `Gender: Male` = sum(Gender_Label == "Male", na.rm = TRUE),
    `Gender: Female` = sum(Gender_Label == "Female", na.rm = TRUE),
    `Patient atopy: None` = sum(Patient_Atopy_Label == "None", na.rm = TRUE),
    `Patient atopy: Allergic rhinitis` = sum(Patient_Atopy_Label == "Allergic rhinitis", na.rm = TRUE),
    `Patient atopy: Eczema` = sum(Patient_Atopy_Label == "Eczema", na.rm = TRUE),
    `Family atopy: None` = sum(Family_Atopy_Label == "None", na.rm = TRUE),
    `Family atopy: Bronchial asthma` = sum(Family_Atopy_Label == "Bronchial asthma", na.rm = TRUE),
    `Family atopy: Allergic rhinitis` = sum(Family_Atopy_Label == "Allergic rhinitis", na.rm = TRUE),
    `Family atopy: Eczema` = sum(Family_Atopy_Label == "Eczema", na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "n") %>%
  mutate(
    Category = sub(".*: ", "", Variable),
    Variable = sub(":.*", "", Variable),
    Percent = round(100 * n / nrow(df_primary), 1)
  ) %>%
  select(Variable, Category, n, Percent)

print(demo_summary)

demo_summary %>%
  gt() %>%
  tab_header(title = "Table 2. Baseline Characteristics of Study Population (N = 151)") %>%
  cols_label(Variable = "Variable", Category = "Category", n = "n", Percent = "Percent (%)") %>%
  fmt_number(columns = Percent, decimals = 1) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
  tab_footnote(footnote = "Data presented as n (%).") %>%
  gtsave("2_Demographics_Table.html", path = output_dir)

# Gender distribution figure
ggplot(df_primary, aes(x = Gender_Label, fill = Gender_Label)) +
  geom_bar(width = 0.7) +
  scale_fill_manual(values = c("Male" = "#2E86C1", "Female" = "#E67E22")) +
  labs(title = "Gender Distribution", x = "", y = "Number of Patients") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("2_Gender_Distribution.png", path = output_dir, width = 6.5, height = 5, dpi = 600)

cat("\n✅ Subsection 3.2 files saved in Results_Output/2_Demographics\n")
