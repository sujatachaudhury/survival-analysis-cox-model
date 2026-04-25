    library(xlsx)
    library(survival)
    library(survminer)
    library(glmnet)
    library(dplyr)
    library(writexl)
    library(ggplot2)
    library(gridExtra)
    library(splines)
    library(rms)
    library(car)
    library(patchwork)

    df <- read.xlsx("data_merged_4.xlsx", sheetName = "Sheet1")

    ALL_COLUMNS <- c(
      'Age', 'Sex', 'BMI', 'Diabetes_Drugs',
      'Hypertension','Smoking_Status', 'Prescription_Dose', 'KPS',
      'Stage_at_RT', 'Site_Of_Primary', 'Mediastino',
      'Chemotherapy', 'Immunotherapy', 'Volume_cm3_heart', 'Volume_cm3_lungs',
      'Max_dose_str_heart', 'Max_dose_str_lungs', 'Mean_dose_str_heart',
      'Mean_dose_str_lungs', 'Subcutaneous_Fat', 'Torso_Fat',
      'Skeletal_Muscle', 'Intermuscular_Fat',
      'Agatston_Score', 'Overall_Survival_at_2_years', 'OS_months',
      "Chemo_Schedule_New", "Body_Volume_cm3", "LAA"
    )

    rownames(df) <- df$ID
    df$ID <- NULL

    df$Subcutaneous_Fat <- df$Subcutaneous_Fat/df$Body_Volume_cm3
    df$Torso_Fat <- df$Torso_Fat/df$Body_Volume_cm3
    df$Skeletal_Muscle <- df$Skeletal_Muscle/df$Body_Volume_cm3
    df$Intermuscular_Fat <- df$Intermuscular_Fat/df$Body_Volume_cm3
    df$Body_Volume_cm3 <- NULL
    ALL_COLUMNS <- setdiff(ALL_COLUMNS, "Body_Volume_cm3")

    #dropping Weight, Height, Calcification_Volume for colinearity
    df$Weight <- NULL
    df$Height <- NULL
    df$Calcification_Volume <- NULL

ALL_COLUMNS <- setdiff(ALL_COLUMNS, c("Weight", "Height", "Calcification_Volume"))

df <- na.omit(df)

cat("Patients with complete data:", nrow(df), "rows and", ncol(df), "columns\n")

df$Dose_Group <- cut(
  df$Prescription_Dose,
  breaks = c(-Inf, 54.9, 59.9, 60.1, Inf),  # custom breaks to isolate 60
  labels = c("<55", "55-59.9", "60", ">60")
)
df$Dose_Group <- as.factor(df$Dose_Group)

df$KPS_cat <- factor(ifelse(df$KPS %in% c(90, 100), 0, 1))

ALL_COLUMNS <- setdiff(ALL_COLUMNS, c("KPS", "Prescription_Dose"))
df$KPS <- NULL
ALL_COLUMNS <- c(ALL_COLUMNS, c("KPS_cat", "Dose_Group"))

df$Prescription_Dose <- NULL


# Identify numerical columns
numerical_columns <- sapply(df, is.numeric)
numerical_cols <- names(df)[numerical_columns]

# One-hot encode categorical variables
categorical_cols <- setdiff(ALL_COLUMNS, numerical_cols)

df$OS_months <- ifelse(df$OS_months >= 24, 24, df$OS_months)


df$event <- ifelse(df$Overall_Survival_at_2_years == "NO", 1, 0)
df$OS_months <- as.numeric(df$OS_months)
surv_obj <- Surv(df$OS_months, df$event)

vars <- setdiff(names(df), c("event", "OS_months", "Overall_Survival_at_2_years"))

df[categorical_cols] <- lapply(df[categorical_cols], factor)

low_level_vars <- sapply(df, function(col) {
  if (is.factor(col)) {
    nlevels(col) < 2
  } else {
    length(unique(na.omit(col))) < 2
  }
})

names(low_level_vars[low_level_vars])

library(moments)
numeric_vars <- sapply(df, is.numeric)

continuous_vars_to_transform <- c("Age", "Mean_dose_str_lungs", "Max_dose_str_lungs",
                                  "Torso_Fat", "Intermuscular_Fat", "Volume_cm3_heart",
                                  "Skeletal_Muscle", "Subcutaneous_Fat",
                                  "LAA", "Agatston_Score")

create_transformed_vars <- function(df, var_name) {
  var_data <- df[[var_name]]

  # Basic quality checks
  if (is.null(var_data) || all(is.na(var_data))) {
    cat("Skipping", var_name, ": all NA values\n")
    return(NULL)
  }

  var_clean <- var_data[!is.na(var_data)]

  if (length(var_clean) < 10) {
    cat("Skipping", var_name, ": too few non-NA values\n")
    return(NULL)
  }

  if (var(var_clean) == 0) {
    cat("Skipping", var_name, ": no variation\n")
    return(NULL)
  }

  transformations <- list()
  unique_vals <- length(unique(var_clean))

  # Original variable
  transformations[[var_name]] <- var_data

  # Polynomial terms
  if (unique_vals > 5) {
    tryCatch({
      poly_result <- poly(var_data, 2, raw = TRUE)
      for (i in 1:ncol(poly_result)) {
        transformations[[paste0(var_name, "_poly", i)]] <- poly_result[, i]
      }
    }, error = function(e) {
      cat("Could not create polynomial for", var_name, ":", e$message, "\n")
    })
  }

  # Natural spline terms
  # if (unique_vals > 10) {
  #   tryCatch({
  #     df_to_use <- min(3, max(2, floor(unique_vals / 5)))
  #     spline_basis <- suppressWarnings(ns(var_data, df = df_to_use))
  #     for (i in 1:ncol(spline_basis)) {
  #       transformations[[paste0(var_name, "_spline", i)]] <- spline_basis[, i]
  #     }
  #   }, error = function(e) {
  #     cat("Could not create splines for", var_name, ":", e$message, "\n")
  #   })
  # }

  # Log transformation
  if (all(var_clean >= 0)) {
    tryCatch({
      min_val <- min(var_clean[var_clean > 0], na.rm = TRUE)
      constant <- ifelse(any(var_clean == 0), min_val / 10, 0)
      log_var <- log(var_data + constant)
      if (any(is.nan(log_var) | is.infinite(log_var))) stop("Invalid log result")
      transformations[[paste0(var_name, "_log")]] <- log_var
    }, error = function(e) {
      cat("Could not create log transformation for", var_name, ":", e$message, "\n")
    })
  }

  # Square root transformation
  if (all(var_clean >= 0)) {
    tryCatch({
      sqrt_var <- sqrt(var_data)
      transformations[[paste0(var_name, "_sqrt")]] <- sqrt_var
    }, error = function(e) {
      cat("Could not create sqrt transformation for", var_name, ":", e$message, "\n")
    })
  }

  # Reciprocal transformation
  if (all(var_clean != 0)) {
    tryCatch({
      recip_var <- 1 / var_data
      transformations[[paste0(var_name, "_recip")]] <- recip_var
    }, error = function(e) {
      cat("Could not create reciprocal transformation for", var_name, ":", e$message, "\n")
    })
  }

  # Box-Cox transformation
  if (all(var_clean > 0)) {
    tryCatch({
      pt <- powerTransform(var_clean)
      lambda <- pt$lambda
      # Apply the transformation to the original vector (with NA preserved)
      boxcox_var <- ifelse(
        var_data > 0,
        (var_data^lambda - 1) / lambda,
        NA
      )
      # If lambda is close to zero, use log transform (per Box-Cox definition)
      if (abs(lambda) < 1e-5) {
        boxcox_var <- log(var_data)
      }
      transformations[[paste0(var_name, "_boxcox")]] <- boxcox_var
    }, error = function(e) {
      cat("Could not create Box-Cox transformation for", var_name, ":", e$message, "\n")
    })
  }

  cat("Created", length(transformations), "transformations for", var_name, "\n")
  return(transformations)
}

# Create transformed dataset with progress tracking
df_transformed <- df
cat("Creating transformed variables...\n")
for(var in continuous_vars_to_transform) {
  if(var %in% names(df)) {
    cat("Processing", var, "...\n")
    transformed_vars <- create_transformed_vars(df, var)
    if(!is.null(transformed_vars)) {
      for(trans_name in names(transformed_vars)) {
        df_transformed[[trans_name]] <- transformed_vars[[trans_name]]
      }
    }
  } else {
    cat("Variable", var, "not found in dataset\n")
  }
}

# Update vars list to include transformations
original_vars <- vars
transformation_vars <- names(df_transformed)[!names(df_transformed) %in% names(df)]
vars_with_transforms <- c(original_vars, transformation_vars)

# Univariate analysis with transformations
univ_results <- lapply(vars_with_transforms, function(v) {
  if(!v %in% names(df_transformed)) return(NULL)

  variable <- df_transformed[[v]]

  # Skip variables with no variation
  if(all(is.na(variable))) return(NULL)
  if(is.factor(variable) && nlevels(variable) < 2) return(NULL)
  if(is.numeric(variable) && var(variable, na.rm = TRUE) == 0) return(NULL)

  formula <- as.formula(paste("surv_obj ~", v))
  model <- try(coxph(formula, data = df_transformed), silent = TRUE)
  if(inherits(model, "try-error")) return(NULL)

  summary_model <- summary(model)
  data.frame(
    variable = v,
    HR = round(summary_model$coefficients[1, "exp(coef)"], 2),
    HR_lower_CI = round(summary_model$conf.int[,"lower .95"], 2),
    HR_upper_CI = round(summary_model$conf.int[,"upper .95"], 2),
    p_value = round(summary_model$coefficients[1, "Pr(>|z|)"], 4),
    stringsAsFactors = FALSE
  )
})

univ_results_df <- do.call(rbind, Filter(Negate(is.null), univ_results))

# Adjust p-values
univ_results_df$BH_p_value <- p.adjust(univ_results_df$p_value, method = "BH")
univ_results_df$Bonferroni_p_value <- p.adjust(univ_results_df$p_value, method = "bonferroni")

# Sort by p-value
univ_results_sorted <- univ_results_df[order(univ_results_df$p_value),]
print(univ_results_sorted)

# Variable selection function
select_best_transformation <- function(results_df, base_vars) {
  selected <- character()

  for(base_var in base_vars) {
    # Find all transformations of this variable
    var_results <- results_df[grepl(paste0("^", base_var), results_df$variable),]

    if(nrow(var_results) > 0) {
      # Select the transformation with lowest p-value
      best_var <- var_results[which.min(var_results$p_value), "variable"]
      selected <- c(selected, best_var)
    }
  }

  # Add non-transformed variables that don't need transformation
  non_transform_vars <- results_df$variable[!grepl(paste(continuous_vars_to_transform, collapse = "|"),
                                                   results_df$variable)]
  significant_non_transform <- non_transform_vars[non_transform_vars %in%
                                                    results_df$variable[results_df$BH_p_value < 0.05]]

  selected <- c(selected, significant_non_transform)
  return(unique(selected))
}

# Select variables
significant_vars <- univ_results_sorted$variable[univ_results_sorted$BH_p_value < 0.05]

selected_vars <- select_best_transformation(univ_results_df, continuous_vars_to_transform)
selected_vars <- c(selected_vars, significant_vars[!grepl(paste(continuous_vars_to_transform, collapse = "|"),
                                                          significant_vars)])


# # Removed Prescription_Dose_recip from significant_vars
# selected_vars <- selected_vars[selected_vars <- unique(selected_vars) != "Prescription_Dose_recip"]
#
# # Added Prescription_Dose_log manually
# if (!"Prescription_Dose_log" %in% selected_vars) {
#   selected_vars <- c(selected_vars, "Prescription_Dose_log")
# }

library(GGally)
ggpairs(df_transformed[, c("OS_months", selected_vars)], title = "Pairwise plots with OS")

selected_vars <- unique(selected_vars)

print(selected_vars)

print(paste("Selected variables:", paste(selected_vars, collapse = ", ")))

# Fit multivariate model
formula_str <- paste("surv_obj ~", paste(selected_vars, collapse = " + "))
cox_multiv <- coxph(as.formula(formula_str), data = df_transformed)

summary(cox_multiv)

library(car)
vif(cox_multiv)

vars_to_drop <- c(
  "Max_dose_str_lungs_poly2",
  "Volume_cm3_heart_log"
)

# vars_to_drop <- c(
#   "Volume_cm3_heart_poly2",
#   "Torso_Fat_sqrt",
#   "Max_dose_str_lungs_spline3",
#   "Skeletal_Muscle_spline1",
#   "LAA_spline1",
#   "Agatston_Score_spline1",
#   "Intermuscular_Fat",
#   "Age_spline3"
# )
#
# #drop non significant variables
# vars_to_drop <- c(
#   "Volume_cm3_heart_poly2",
#   "Torso_Fat",
#   "Max_dose_str_lungs_poly2",
#   "Skeletal_Muscle_boxcox",
#   "LAA_sqrt",
#   "Agatston_Score_log",
#   "Intermuscular_Fat"
# )

selected_vars <- setdiff(selected_vars, vars_to_drop)
print(selected_vars)

print(paste("Selected variables:", paste(selected_vars, collapse = ", ")))

#selected_vars <- c(selected_vars, "Intermuscular_Fat")

formula_str <- paste("surv_obj ~", paste(selected_vars, collapse = " + "))
cox_multiv <- coxph(as.formula(formula_str), data = df_transformed)

summary(cox_multiv)
vif(cox_multiv)

#Mean_dose_str_lungs_poly2

formula_str1 <- "surv_obj ~ Mean_dose_str_lungs_poly2 + LAA_sqrt + Agatston_Score_poly2 + ns(Subcutaneous_Fat, df = 2) +
ns(Torso_Fat, df = 2) + ns(Intermuscular_Fat, df = 2) + Skeletal_Muscle_sqrt + Mean_dose_str_lungs_poly2 + Hypertension + Diabetes_Drugs"

formula_str12 <- "surv_obj ~ Mean_dose_str_lungs_poly2 + LAA_sqrt + Agatston_Score_poly2 + ns(Subcutaneous_Fat, df = 2) +
Torso_Fat + Intermuscular_Fat + Skeletal_Muscle_sqrt + Mean_dose_str_lungs_poly2 + Hypertension + Diabetes_Drugs"

formula_str13 <- "surv_obj ~ Mean_dose_str_lungs_poly2 + Torso_Fat + Hypertension + Diabetes_Drugs"

cox_ns <- coxph(as.formula(formula_str1), data = df_transformed)
summary(cox_ns)

cox_ns12 <- coxph(as.formula(formula_str12), data = df_transformed)
summary(cox_ns12)

cox_ns13 <- coxph(as.formula(formula_str13), data = df_transformed)
summary(cox_ns13)

anova(cox_ns12, cox_ns13)

summary_model <- summary(cox_ns13)

multivariate_results <- data.frame(
  Variable = rownames(summary_model$coefficients),
  HR = summary_model$coefficients[, "exp(coef)"],
  HR_lower = summary_model$conf.int[, "lower .95"],
  HR_upper = summary_model$conf.int[, "upper .95"],
  P_value = summary_model$coefficients[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

print(multivariate_results)

vif(cox_ns)

#visualize spline

library(survival)
library(splines)
library(ggplot2)

# Create a new data frame with a sequence of Torso Fat values covering its range
newdata <- data.frame(
  Torso_Fat = seq(min(df_transformed$Torso_Fat),
                  max(df_transformed$Torso_Fat), length.out = 100),

  Mean_dose_str_lungs_poly2 = mean(df_transformed$Mean_dose_str_lungs_poly2),
  Hypertension = factor("YES", levels = c("NO", "YES")),
  Diabetes_Drugs = factor("NO", levels = c("NO", "YES"))
)


newdata$lp <- predict(cox_ns13, newdata = newdata, type = "lp")

# Convert linear predictor to hazard ratio relative to baseline (e.g., median value)
baseline_lp <- predict(cox_ns13, newdata = newdata[50, , drop = FALSE], type = "lp")

newdata$HR <- exp(newdata$lp - baseline_lp)

# Plot
ggplot(newdata, aes(x = Torso_Fat, y = HR)) +
  geom_line() +
  ylab("Hazard Ratio (relative)") +
  xlab("Torso_Fat") +
  theme_minimal()

# Create a new data frame with a sequence of Subcutaneous Fat values covering its range
newdata <- data.frame(
  Subcutaneous_Fat = seq(min(df_transformed$Subcutaneous_Fat),
                         max(df_transformed$Subcutaneous_Fat), length.out = 100),
  Mean_dose_str_lungs_poly2 = mean(df_transformed$Mean_dose_str_lungs_poly2),
  Hypertension = factor("NO", levels = c("NO", "YES")),
  Diabetes_Drugs = factor("NO", levels = c("NO", "YES"))
)


newdata$lp <- predict(cox_ns13, newdata = newdata, type = "lp")

# Convert linear predictor to hazard ratio relative to baseline (e.g., median value)
baseline_lp <- predict(cox_ns13, newdata = newdata[50, , drop = FALSE], type = "lp")

newdata$HR <- exp(newdata$lp - baseline_lp)

# Plot
ggplot(newdata, aes(x = Subcutaneous_Fat, y = HR)) +
  geom_line() +
  ylab("Hazard Ratio (relative)") +
  xlab("Subcutaneous Fat") +
  theme_minimal()


# Forest Plot for Multivariate Analysis
if (exists("multivariate_results")) {
  multivariate_results$Variable_clean <- gsub("_", " ", multivariate_results$Variable)
  multivariate_results$HR_CI <- paste0(sprintf("%.2f", multivariate_results$HR),
                                       " (", sprintf("%.2f", multivariate_results$HR_lower),
                                       "-", sprintf("%.2f", multivariate_results$HR_upper), ")")

  p3 <- ggplot(multivariate_results, aes(x = HR, y = reorder(Variable_clean, -P_value))) +
    geom_point(size = 3, color = "darkred") +
    geom_errorbarh(aes(xmin = HR_lower, xmax = HR_upper), height = 0.3, color = "darkred") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_x_log10() +
    labs(title = "Multivariate Cox Regression - Forest Plot",
         x = "Hazard Ratio (log scale)",
         y = "Variables") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10))

  print(p3)
}

library(forestmodel)
forest_model(cox_multiv)



# Kaplan-Meier Survival Curve
km_fit <- survfit(surv_obj ~ 1, data = df)
p1 <- ggsurvplot(km_fit,
                 data = df,
                 title = "Overall Survival Curve",
                 xlab = "Time (months)",
                 ylab = "Survival Probability",
                 risk.table = TRUE,
                 conf.int = TRUE,
                 palette = "blue")

print(p1)


# Check proportional hazards assumption
if (exists("multivariate_results")) {
  ph_test <- cox.zph(cox_multiv)
  print("Proportional Hazards Test:")
  print(ph_test)

  # Plot Schoenfeld residuals
  p4 <- ggcoxzph(ph_test, caption = "Schoenfeld Residuals Test for Proportional Hazards")
  print(p4)
}

# Model diagnostics plots
if (exists("multivariate_results")) {
  # Martingale residuals
  df$martingale_resid <- residuals(cox_multiv, type = "martingale")

  p5 <- ggplot(df, aes(x = predict(cox_multiv), y = martingale_resid)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Martingale Residuals vs Linear Predictor",
         x = "Linear Predictor",
         y = "Martingale Residuals") +
    theme_minimal()

  print(p5)
}

# Survival curves by significant categorical variables
if ("Hypertension" %in% selected_vars) {
  km_sex <- survfit(surv_obj ~ Hypertension, data = df)
  p6 <- ggsurvplot(km_sex,
                   data = df,
                   title = "Survival by Hypertension",
                   legend.title = "Hypertension",
                   legend.labs = c("Yes", "No"),
                   pval = TRUE,
                   risk.table = TRUE)
  print(p6)
}

# Martingale residuals for each selected variable

plot_list <- list()

for (var in selected_vars) {
  if(var %in% names(df_transformed)) {
    print(var)
    p <- ggplot(df_transformed, aes_string(x = var, y = "martingale_residuals")) +
      geom_point(alpha = 0.6) +
      geom_smooth(se = FALSE, color = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste("MR vs", var),
           x = var, y = "martingale_residuals") +
      theme_minimal()

    plot_list[[var]] <- p
  }
}

valid_plots <- Filter(Negate(is.null), plot_list)
grid.arrange(grobs = valid_plots, ncol = 4)

variables_to_plot <- c("Age_spline3", "Prescription_Dose_log", "Mean_dose_str_lungs_spline2", "Skeletal_Muscle_spline1", "Subcutaneous_Fat_spline1")

#variables_to_plot <- c("Age", "Prescription_Dose", "Mean_dose_str_lungs", "Skeletal_Muscle", "Subcutaneous_Fat")

plot_list <- lapply(variables_to_plot, function(var) {
  if (var %in% names(df_transformed)) {
    ggplot(df_transformed, aes_string(x = var, y = "OS_months")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      labs(title = paste(var, "vs OS months"), x = var, y = "OS months") +
      theme_minimal()
  }
})

# Combine all plots into one grid
wrap_plots(plot_list, ncol = 2)
