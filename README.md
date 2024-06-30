# Tree Based Models Presentation

This repository contains a presentation on tree-based models, created by Marco Gaudio as part of the Master di II livello in Artificial Intelligence & Data Science. The presentation is built using R and the Quarto framework.

## Table of Contents

- [Introduction](#introduction)
- [Libraries and Packages Used](#libraries-and-packages-used)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Modeling](#modeling)
  - [CART Model](#cart-model)
  - [Random Forest Model](#random-forest-model)
  - [GBM Model](#gbm-model)
- [Model Comparison](#model-comparison)
- [How to Run](#how-to-run)
- [License](#license)

## Introduction

This presentation provides an overview of tree-based models, including:

- Presentation of the dataset
- Exploratory data analysis
- CART Model
- Random Forest model
- GBM model
- Model comparison

## Libraries and Packages Used

The following libraries and packages are utilized in the presentation:

- `MLDataR`
- `dplyr`
- `tidyr`
- `tidymodels`
- `data.table`
- `ConfusionTableR`
- `OddsPlotty`
- `randomForest`
- `caret`
- `rpart`
- `rpart.plot`
- `pROC`
- `gbm3`

## Exploratory Data Analysis

The dataset `heartdisease` from the `MLDataR` package is used, which contains 918 observations of patients and 10 variables:

- `Age`
- `Sex`
- `RestingBP`: Resting blood pressure
- `Cholesterol`
- `FastingBS`: Blood sugar level
- `RestingECG`: Myocardial indicator
- `MaxHR`: Maximum heart rate
- `Angina`
- `HeartPeakReading`
- `HeartDisease`

The analysis includes the percentage and number of patients with heart disease in the dataset.

## Modeling

### CART Model

A CART model is built and evaluated to understand the relationships in the dataset.

### Random Forest Model

A Random Forest model is used to improve the prediction accuracy and handle the complexity of the dataset.

### GBM Model

A Gradient Boosting Machine (GBM) model is implemented to compare its performance with other tree-based models.

## Model Comparison

The results of the different models are compared to determine the best performing model for this dataset.

## How to Run

To run the presentation:

1. Ensure you have R and the necessary libraries installed.
2. Clone this repository.
3. Open the `.qmd` file in RStudio or any other compatible IDE.
4. Render the presentation using Quarto.
