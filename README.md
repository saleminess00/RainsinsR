# Raisin Data Analysis and Modeling in R

## Overview

This repository contains a robust statistical analysis project that begins with data preparation, explores the data with descriptive statistics, tests hypotheses using various statistical tests, applies linear regression models, and implements Generalized Linear Models (GLMs). The data set is analyzed using different modeling techniques, and the project is divided into several tasks as detailed below.

## Tasks

1. **Data Preparation:** Preprocessing the dataset to clean and transform the variables.

2. **Descriptive Statistics & Data Visualization:** Conducting exploratory data analysis using summary statistics and various data visualization techniques. This step allows a thorough understanding of the data distributions and inter-variable relationships.

3. **Statistical Hypothesis Testing:** Performing statistical tests to ascertain relationships between different variables. Depending on the type of variables, different tests are used such as t-tests, chi-square tests, or ANOVA.

4. **Linear Regression Modeling:** Following the EDA and hypothesis testing, linear regression models are built. Models are iteratively refined by examining p-values and eliminating less significant variables. The final model is chosen based on the Akaike information criterion (AIC).

5. **Principal Component Analysis (PCA):** The dimensionality of the data is reduced using PCA. The residuals of the final linear model are examined and visualized.

6. **Generalized Linear Models (GLMs):** Different GLM models are created, including Gaussian, Binomial, Gamma, and Poisson models. Model performance is evaluated using R-squared values, deviances, and AIC.  

Throughout these tasks, various R packages such as `stats`, `caret`, `FactoMineR`, `factoextra`, and `mgcv` are used.

Please feel free to explore this repository. Feedback and contributions are always welcomed!
