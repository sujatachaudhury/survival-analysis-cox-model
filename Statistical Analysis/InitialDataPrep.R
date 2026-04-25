library(xlsx)
library(survival)
library(survminer)
library(glmnet)
library(dplyr)
library(writexl)
library(mice)

rename_id_column <- function(df) {
  df$ID <- sapply(df$ID, function(x) {
    x <- as.character(x)
    while (nchar(x) > 4 && !grepl("^[1-9]", x)) {
      x <- substring(x, 2)
    }
    return (x)
  })
  return(df)
}

df_patient <- read.xlsx("dataset_students_copy.xlsx", sheetName = "Foglio1")
head(df_patient)
df_patient <- rename_id_column(df_patient)

data_dvh_heart <- read.xlsx("dvh_students.xlsx", sheetName = "Heart")
data_dvh_heart <- rename_id_column(data_dvh_heart)
data_dvh_lungs <- read.xlsx("dvh_students.xlsx", sheetName = "Lungs")
data_dvh_lungs <- rename_id_column(data_dvh_lungs)

data_muscle_fat <- read.xlsx("fat_and_muscles_volume_results.xlsx", sheetIndex = 1)
head(data_muscle_fat)

calcium_scores_data <- read.xlsx("results.xlsx", sheetIndex = 1)
calcium_scores_data <- rename_id_column(calcium_scores_data)
head(calcium_scores_data)

colnames(data_muscle_fat)[colnames(data_muscle_fat) == "Patient.Name"] <- "ID"
data_muscle_fat <- rename_id_column(data_muscle_fat)

df_new_chemo <- read.xlsx("stud_new.xlsx", sheetName = "Foglio1")
df_new_chemo <- rename_id_column(df_new_chemo)

df_laa <- read.xlsx("output_laa_results_copy.xlsx", sheetName = "Sheet1")
df_laa <- rename_id_column(df_laa)

nrow(df_patient)

missing_in_fat <- setdiff(df_patient$ID, data_muscle_fat$ID)
cat("IDs in df_patient not in data_dvh_heart:\n")
print(missing_in_fat)


# Merge df_patient_info and df_patient_dosage_heart on "ID" (inner join)
df_patient <- merge(df_patient, data_dvh_heart, by = "ID", all.x = TRUE)
#
# # Merge with df_patient_dosage_lungs, adding suffixes
df_patient <- merge(df_patient, data_dvh_lungs, by = "ID", all.x = TRUE,
                    suffixes = c("_heart", "_lungs"))

# Merge with df_patient_fat_and_muscle_volume
df_patient <- merge(df_patient, data_muscle_fat, by = "ID", all.x = TRUE)

df_patient <- merge(df_patient, calcium_scores_data, by = "ID", all.x = TRUE)

df_patient <- merge(df_patient, df_new_chemo, by = "ID", all.x = TRUE)

df_patient <- merge(df_patient, df_laa, by = "ID", all.x = TRUE)


# 1. Select specific columns
df_patient <- df_patient[, c(
  "ID", "Age", "Sex", "Weight", "Height", "BMI", "Diabetes.drugs", "Hypertension",
  "Smoking.status", "Prescription.Dose", "KPS", "Stage",
  "Stage_at_RT", "Site.of.Primary", "Mediastino", "CHEMO", "Immunotherapy",
  "Volume..cm3._heart", "Volume..cm3._lungs", "Max_dose_str._heart", "Mean_dose_str._heart", "Max_dose_str._lungs", "Mean_dose_str._lungs",
  "Subcutaneous.Fat", "Torso.Fat", "Skeletal.Muscle", "Intermuscular.Fat",
  "Overall.Survival.at.2.years", "OS_months", "agatston_score", "total_volume_mm3", "Chemo_Schedule_new", "Body_Volume..cm3.", "LAA."
)]

# 2. Rename the columns
colnames(df_patient) <- c(
  "ID", "Age", "Sex", "Weight", "Height", "BMI", "Diabetes_Drugs", "Hypertension",
  "Smoking_Status", "Prescription_Dose", "KPS", "Cancer_Stage",
  "Stage_at_RT", "Site_Of_Primary", "Mediastino", "Chemotherapy", "Immunotherapy",
  "Volume_cm3_heart", "Volume_cm3_lungs", "Max_dose_str_heart", "Max_dose_str_lungs", "Mean_dose_str_heart", "Mean_dose_str_lungs",
  "Subcutaneous_Fat", "Torso_Fat", "Skeletal_Muscle", "Intermuscular_Fat",
  "Overall_Survival_at_2_years", "OS_months", "Agatston_Score", "Calcification_Volume", "Chemo_Schedule_New", "Body_Volume_cm3.", "LAA"
)

# 3. Set ID as rownames
rownames(df_patient) <- df_patient$ID
#df_patient$ID <- NULL

write_xlsx(df_patient, path = "data_merged_4.xlsx", col_names = TRUE, format_headers = FALSE)

# Count rows with at least one NA
num_na_rows <- sum(!complete.cases(df_patient))
cat("Number of rows with NA:", num_na_rows, "\n")

na_per_column <- colSums(is.na(df_patient))
print(na_per_column)

# Count number of rows with at least one NA
num_na_rows <- sum(apply(df, 1, function(x) any(is.na(x))))
cat("Number of rows with NA:", num_na_rows, "\n")

# Remove rows with any NA
df_clean <- df[complete.cases(df), ]

# Remove rows with any NA
df_clean <- na.omit(df)

#multiple imputation using Chained Equations

# 4. Print columns with NA values
na_columns <- colnames(df_patient)[colSums(is.na(df_patient)) > 0]
cat("These columns have null values:", paste(na_columns, collapse = ", "), "\n")

na_columns <- setdiff(na_columns, "OS_months")
na_columns <- setdiff(na_columns, "BMI")
#=====================================
#------------
#only impute for Weight and Height
#------------


to_be_imputed <- c("Height", "Weight")

impute_data <- df_patient[, to_be_imputed]

imp <- mice(impute_data, m = 5, seed = 123)

imp$method

completed_data <- complete(imp, 1)

df_patient[, to_be_imputed] <- completed_data
#update BMI


missing_bmi_values <- is.na(df_patient$BMI) & !is.na(df_patient$Weight) & !is.na(df_patient$Height)

df_patient$BMI[missing_bmi_values] <- df_patient$Weight[missing_bmi_values] / ((df_patient$Height[missing_bmi_values]/100)^2)

df_patient[missing_bmi_values, c("Weight", "Height", "BMI")]

write_xlsx(df_patient, path = "data_imputed_4.xlsx", col_names = TRUE, format_headers = FALSE)
#================================

#visualize
md.pattern(df_patient)

library(VIM)
aggr(df_patient, numbers = TRUE, sortVars = TRUE, cex.axis = 0.7, gap = 0)

impute_data <- df_patient[, na_columns]

imp <- mice(impute_data, m = 5, seed = 123)

meth <- imp$method
meth["Antilipidic_Drugs"] <- "logreg"
meth["Antithrombotic"] <- "logreg"
meth["Beta_Blockers"] <- "logreg"
meth["Smoking_Status"] <- "polyreg"
meth["Target_Therapy"] <- "polyreg"
meth["Site_Of_Primary"] <- "polyreg"
meth["Chemo_Schedule_New"] <- "polyreg"
meth["Mediastino"] <- "logreg"
meth["Weight"] <- "pmm"
meth["Height"] <- "pmm"
meth["KPS"] <- "pmm"

cat_vars_logreg <- c("Antilipidic_Drugs", "Antithrombotic", "Beta_Blockers", "Mediastino")

cat_vars_polyreg <- c("Smoking_Status", "Site_Of_Primary", "Target_Therapy", "Chemo_Schedule_New")

# Convert to factor
impute_data[cat_vars_logreg] <- lapply(impute_data[cat_vars_logreg], factor)
impute_data[cat_vars_polyreg] <- lapply(impute_data[cat_vars_polyreg], factor)

imp <- mice(impute_data, method = meth, m = 5, seed = 123)

imp$method

completed_data <- complete(imp, 1)

df_patient[, na_columns] <- completed_data

#update BMI


missing_bmi_values <- is.na(df_patient$BMI) & !is.na(df_patient$Weight) & !is.na(df_patient$Height)

df_patient$BMI[missing_bmi_values] <- df_patient$Weight[missing_bmi_values] / ((df_patient$Height[missing_bmi_values]/100)^2)

df_patient[missing_bmi_values, c("Weight", "Height", "BMI")]

write_xlsx(df_patient, path = "data_imputed_3.xlsx", col_names = TRUE, format_headers = FALSE)

# 6. Print shape of cleaned data
cat("Patients with complete data:", nrow(df_patient), "rows and", ncol(df_patient), "columns\n")
