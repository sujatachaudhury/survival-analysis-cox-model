# Cardiac Calcification and Overall Survival in Lung Cancer Patients Undergoing Radiotherapy

**Applied Statistics Project — Politecnico di Milano, Semester II**

## Overview

This project investigates whether **cardiac calcification** (measured via the Agatston Score) and other clinical and dosimetric variables are associated with **overall survival** in lung cancer patients treated with radiation therapy (RT).

The analysis combines:
- Automated cardiac calcification scoring from CT DICOM images (Python)
- Statistical survival analysis using Cox Proportional Hazards models (R)

![Poster Presentation](Poster%20Presentation%20Applied%20Statistics.png)

---

## Repository Structure

```
├── Calcification/
│   └── Original_code_calcification_in_heart.ipynb   # DICOM processing & Agatston Score computation
│
├── Statistical Analysis/
│   ├── InitialDataPrep.R     # Data merging, cleaning, and multiple imputation
│   ├── PreAnalysis.R         # Exploratory analysis, correlation, VIF, feature selection
│   └── Coxfinal.R            # Cox PH model fitting, variable selection, diagnostics
│
└── Poster Presentation Applied Statistics.png
```

---

## Methods

### 1. Cardiac Calcification Scoring (`Calcification/`)

The Jupyter notebook processes per-patient DICOM CT images to:

- Load CT volumes and heart segmentation masks using `DicomRTTool`
- Detect calcified lesions within the heart (HU threshold: 130–700)
- Filter regions by area and eccentricity to reduce noise
- Compute the **Agatston Score** (density-weighted area of calcifications per slice)
- Compute the **total calcification volume** (mm³)
- Extract **Dose-Volume Histogram (DVH)** statistics for heart and lungs

**Key libraries:** `DicomRTTool`, `pydicom`, `SimpleITK`, `scikit-image`, `NumPy`, `Matplotlib`

### 2. Data Preparation (`InitialDataPrep.R`)

- Merges multiple data sources: patient demographics, DVH statistics (heart & lungs), body composition (fat/muscle volumes), calcium scores, chemotherapy schedule, and LAA (Low Attenuation Area)
- Normalizes body composition features (fat/muscle) by total body volume
- Handles missing data using **Multiple Imputation by Chained Equations (MICE)** with method-specific strategies:
  - Continuous variables: predictive mean matching (`pmm`)
  - Binary variables: logistic regression (`logreg`)
  - Multinomial variables: polytomous regression (`polyreg`)

**Key libraries:** `mice`, `survival`, `glmnet`, `writexl`

### 3. Pre-Analysis (`PreAnalysis.R`)

- Computes and visualizes the **correlation matrix** across all features
- Calculates **Variance Inflation Factors (VIF)** and the **condition number** to identify multicollinearity
- Drops collinear features: `Weight` and `Height` (kept `BMI`), `Calcification_Volume` (kept `Agatston_Score`)
- Applies **one-hot encoding** for categorical variables
- Computes Pearson and Spearman correlations with the survival outcome (`OS_months`)

**Key libraries:** `corrplot`, `car`, `GGally`, `caret`

### 4. Cox Proportional Hazards Modelling (`Coxfinal.R`)

- Defines the survival object: `Surv(OS_months, event)` (event = death within 2 years)
- Applies variable transformations (polynomial, log, sqrt, Box-Cox) to continuous predictors and selects the best-fitting form via univariate screening
- Adjusts p-values using Benjamini-Hochberg (BH) and Bonferroni corrections
- Fits univariate and multivariate Cox PH models
- Performs model diagnostics:
  - Proportional hazards assumption (Schoenfeld residuals via `cox.zph`)
  - Martingale residuals plot
  - Forest plot of hazard ratios
  - Kaplan-Meier survival curves stratified by significant variables

**Key libraries:** `survival`, `survminer`, `rms`, `splines`, `ggplot2`, `forestmodel`

---

## Key Variables

| Category | Variables |
|---|---|
| Demographics | Age, Sex, BMI |
| Comorbidities | Hypertension, Diabetes Drugs, Antilipidic Drugs, Beta Blockers, Antithrombotic |
| Lifestyle | Smoking Status |
| Treatment | Prescription Dose, Stage at RT, Chemotherapy, Immunotherapy, Target Therapy, Chemo Schedule |
| Dosimetry | Max/Mean dose to heart & lungs, heart & lung volumes |
| Body composition | Subcutaneous Fat, Torso Fat, Skeletal Muscle, Intermuscular Fat (normalized by body volume) |
| Calcification | Agatston Score, LAA (Low Attenuation Area) |
| Outcome | OS_months, Overall Survival at 2 years |

---

## Requirements

### Python (Calcification notebook)
```
DicomRTTool
pydicom
SimpleITK
scikit-image
numpy
matplotlib
pandas
openpyxl
```

### R (Statistical Analysis)
```r
install.packages(c(
  "readxl", "writexl", "xlsx", "dplyr", "ggplot2",
  "corrplot", "car", "GGally", "caret",
  "survival", "survminer", "glmnet",
  "mice", "VIM", "rms", "splines",
  "moments", "gridExtra", "patchwork", "forestmodel"
))
```

---

## How to Run

1. **Calcification scoring**: Run `Calcification/Original_code_calcification_in_heart.ipynb` on your DICOM dataset. Update the `dicom_folder` path to point to your patient directories. The notebook outputs Agatston scores and DVH statistics as Excel files.

2. **Data preparation**: Place all input Excel files in the same directory as the R scripts and run `InitialDataPrep.R`. This produces `data_imputed_3.xlsx`.

3. **Pre-analysis**: Run `PreAnalysis.R` to perform multicollinearity checks and generate `data_imputed_cleaned.csv`.

4. **Survival modelling**: Run `Coxfinal.R` to fit Cox PH models and generate diagnostic plots.

> **Note:** The raw patient DICOM and clinical data are not included in this repository due to patient privacy constraints.

---

## Results

The final multivariate Cox model identified the following variables as significant predictors of overall survival:

- Mean lung dose (polynomial term)
- Torso fat fraction
- Hypertension
- Diabetes medication use

Detailed results and interpretation are presented in the poster (`Poster Presentation Applied Statistics.png`).
