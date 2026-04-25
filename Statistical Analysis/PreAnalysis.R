library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(GGally)
library(caret)

# Load survival analysis packages
library(survival)
library(survminer)

# Read the data
df <- read_excel("data_imputed_3.xlsx", sheet = "Sheet1")



ALL_COLUMNS <- c(
  'Age', 'Sex', 'Weight', 'Height', 'BMI', 'Diabetes_Drugs',
  'Antilipidic_Drugs', 'Hypertension', 'Antithrombotic', 'Beta_Blockers',
  'Smoking_Status', 'Prescription_Dose', 'KPS',
  'Stage_at_RT', 'Target_Therapy', 'Site_Of_Primary', 'Mediastino',
  'Chemotherapy', 'Immunotherapy', 'Volume_cm3_heart', 'Volume_cm3_lungs',
  'Max_dose_str_heart', 'Max_dose_str_lungs', 'Mean_dose_str_heart',
  'Mean_dose_str_lungs', 'Subcutaneous_Fat', 'Torso_Fat',
  'Skeletal_Muscle', 'Intermuscular_Fat',
  'Agatston_Score', 'Calcification_Volume', 'Overall_Survival_at_2_years', 'OS_months',
  "Chemo_Schedule_New", "Body_Volume_cm3", "LAA"
)


TARGET_COLUMN <- "OS_months"

df <- df[, ALL_COLUMNS]

df <- df[!is.na(df$OS_months), ]

#rownames(df) <- df$ID
#df$ID <- NULL

df$Subcutaneous_Fat <- df$Subcutaneous_Fat/df$Body_Volume_cm3
df$Torso_Fat <- df$Torso_Fat/df$Body_Volume_cm3
df$Skeletal_Muscle <- df$Skeletal_Muscle/df$Body_Volume_cm3
df$Intermuscular_Fat <- df$Intermuscular_Fat/df$Body_Volume_cm3
df$Body_Volume_cm3 <- NULL
ALL_COLUMNS <- setdiff(ALL_COLUMNS, "Body_Volume_cm3")

df$KPS_cat <- factor(ifelse(df$KPS %in% c(90, 100), 0, 1))

ALL_COLUMNS <- setdiff(ALL_COLUMNS, "KPS")
df$KPS <- NULL
ALL_COLUMNS <- c(ALL_COLUMNS, "KPS_cat")

# Identify numerical columns
numerical_columns <- sapply(df, is.numeric)
numerical_cols <- names(df)[numerical_columns]

# One-hot encode categorical variables
categorical_cols <- setdiff(ALL_COLUMNS, numerical_cols)

# Create dummy variables (one-hot encoding)
df_encoded <- df
for (col in categorical_cols) {
  if (col %in% names(df)) {
    # Create dummy variables
    factor_col <- as.factor(df[[col]])
    dummies <- model.matrix(~ factor_col - 1)
    # Set column names correctly
    colnames(dummies) <- paste0(col, "_", levels(factor_col))
    # Add all except first column to dataframe
    if (ncol(dummies) > 1) {
      df_encoded <- cbind(df_encoded, dummies[, -1, drop = FALSE])
    }
    # Remove original categorical column
    df_encoded <- df_encoded[, !(names(df_encoded) == col)]
  }
}

# Compute correlation matrix
correlation_matrix <- cor(df_encoded, use = "complete.obs")
write.csv(correlation_matrix, "corr.csv")

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = NULL,
         tl.col = "black", tl.srt = 45, tl.cex = 0.5,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Matrix")

#Calculate VIF (Variance Inflation Factor)
calculate_vif <- function(df) {
  # Remove target variable
  df_predictors <- df[, !(names(df) == TARGET_COLUMN)]

  # Calculate VIF for each predictor
  vif_values <- vif(lm(as.formula(paste(TARGET_COLUMN, "~ .")), data = df))

  vif_data <- data.frame(
    feature = names(vif_values),
    VIF = vif_values
  )

  return(vif_data)
}

# Calculate VIF
vif_results <- tryCatch({
  calculate_vif(df_encoded)
}, error = function(e) {
  message("Error calculating VIF: ", e$message)
  message("This might be due to perfect multicollinearity or other issues.")
  return(data.frame(feature = character(0), VIF = numeric(0)))
})

print("Variance Inflation Factors:")
print(vif_results)

# Calculate condition number
condition_number <- function(X) {
  # Remove target variable
  X_predictors <- X[, !(names(X) == TARGET_COLUMN)]

  # Calculate eigenvalues of the correlation matrix
  eigen_values <- eigen(cor(X_predictors))$values

  # Condition number is the square root of the ratio of the largest to smallest eigenvalue
  return(sqrt(max(eigen_values) / min(eigen_values)))
}

cond_num <- condition_number(df_encoded)
print(paste("Condition Number:", cond_num))

pairs(df[, numerical_cols], main = "Pairwise Scatterplots")

# Standardize the features (excluding target)
X_std <- scale(df_encoded[, !(names(df_encoded) == TARGET_COLUMN)])

# Compute the correlation matrix
corr_matrix <- cor(X_std)

# Compute eigenvalues
eigenvalues <- eigen(corr_matrix)$values

print("Eigenvalues:")
print(eigenvalues)

# Compute condition index
condition_index <- sqrt(max(eigenvalues) / eigenvalues)
print("Condition Index:")
print(condition_index)

# Identify the most problematic features to drop due to multicollinearity

# High VIF (above 100, critical multicollinearity)
high_vif_features <- c(
  "Height", "Weight", "BMI"  # BMI chosen to keep, drop Weight & Height
)

# Highly correlated features (|r| > 0.95)
high_corr_pairs <- list(
  c("Agatston_Score", "Calcification_Volume")  # keep only one
)

# Decide which feature to keep from each pair
drop_due_to_corr <- c("Calcification_Volume")  # keep Agatston_Score

# Dummy variable trap: drop one dummy per group
drop_onehot_trap <- c(
  "Stage_at_RT_IIIA"  # drop one of stage variables
)

# Final list of features to drop
features_to_drop <- unique(c(high_vif_features, drop_due_to_corr, drop_onehot_trap))

# Drop features
df_encoded <- df_encoded[, !(names(df_encoded) %in% features_to_drop)]
write.csv(df_encoded, "data_imputed_cleaned.csv", row.names = FALSE)

df <- read.csv("data_imputed_cleaned.csv")

# Separate independent variables (X) and the dependent variable (y)
y <- df[[TARGET_COLUMN]]
X <- df[, !(names(df) == TARGET_COLUMN)]

# Compute Pearson correlation
pearson_corr <- sapply(X, function(x) cor(x, y, method = "pearson"))
pearson_corr <- pearson_corr[order(abs(pearson_corr), decreasing = TRUE)]

# Compute Spearman correlation
spearman_corr <- sapply(X, function(x) cor(x, y, method = "spearman"))
spearman_corr <- spearman_corr[order(abs(spearman_corr), decreasing = TRUE)]

# Combine into one data frame
correlation_df <- data.frame(
  Pearson = pearson_corr,
  Spearman = spearman_corr
)

# Display top correlated features
head(correlation_df[order(abs(correlation_df$Pearson), decreasing = TRUE), ], 10)
correlation_df$Feature <- rownames(correlation_df)

# Get top 10 most correlated features
top10 <- correlation_df[order(abs(correlation_df$Pearson), decreasing = TRUE), ]
top10 <- head(top10, 10)

# Reorder factor levels for plotting (most correlated at top)
top10$Feature <- factor(top10$Feature, levels = top10$Feature[order(abs(top10$Pearson))])

# Plot using ggplot2
library(ggplot2)
ggplot(top10, aes(x = Feature, y = Pearson, fill = Pearson > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  labs(title = "Top 10 Most Correlated Features",
       x = "Feature",
       y = "Pearson Correlation",
       fill = "Positive Correlation") +
  theme_minimal()

# Visualize the correlation matrix
corrplot(cor(df), method = "color", type = "upper",
         addCoef.col = NULL, tl.col = "black", tl.srt = 45, tl.cex = 0.5,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Matrix")

# Get top 10 features by absolute Pearson correlation
top_features <- names(pearson_corr)[1:10]
top_values <- abs(pearson_corr[1:10])

# Create barplot
barplot_data <- data.frame(
  Feature = top_features,
  Correlation = top_values
)

ggplot(barplot_data, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Features Correlated with OS_months (Pearson)",
       x = "Features",
       y = "Absolute Correlation")

vif_results1 <- tryCatch({
  calculate_vif(df_encoded)
}, error = function(e) {
  message("Error calculating VIF: ", e$message)
  message("This might be due to perfect multicollinearity or other issues.")
  return(data.frame(feature = character(0), VIF = numeric(0)))
})

print("Variance Inflation Factors:")
print(vif_results1)

cond_num <- condition_number(df_encoded)
print(paste("Condition Number:", cond_num))

source("./R/improved_survival_analysis.R")
improved_results <- comprehensive_model_improvement('data_imputed_cleaned.csv')

df_reg <- read.csv('./data_imputed_cleaned.csv', row.names = 1)
lr_results <- logistic_regression_alternative(df_reg)


