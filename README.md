<img src = "https://github.com/daya6489/DriveML/blob/master/man/figures/driveml_image.PNG">

[![CRAN status](https://www.r-pkg.org/badges/version/DriveML)](https://cran.r-project.org/package=DriveML)

[![Downloads](http://cranlogs.r-pkg.org/badges/DriveML)](https://cran.r-project.org/package=DriveML)

[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/DriveML)](https://cran.r-project.org/package=DriveML)

---

# DriveML

DriveML for automated machine learning. DriveML helps in implementing some of the pillars of an automated machine learning pipeline such as automated data preparation, feature engineering, model building, and model explanation by executing simple functions for each purpose instead of writing lengthy R codes.

**DriveML Framework:**

![DriveML](https://github.com/daya6489/DriveML/blob/master/man/figures/driveml_framework.PNG)

  **DriveML** is a series of functions such as `AutoDataPrep`, `AutoMAR`, `autoMLmodel`.  **DriveML** automates some of the most difficult machine learning functions such as data cleaning, data transformations, feature engineering, model training, model validation, model tuning and model selection. 

## Functionalities of DriveML

Three key features of DriveML : Pre-processing, ML Techniques and Model interpretations

1. Automatic data preparation
    + `AutoDataPrep` function to generate a novel features based on the functional understanding of the dataset
    
2. ML Model Optimization
    + `autoMLmodel` function to develop baseline machine learning models using regression and tree based classification techniques

3. Model interpretations
    + `autoMLReport` function to print the machine learning model outcome in HTML format

# Installation

Install from CRAN within R using:

```R
install.packages("DriveML")
```

Install the latest development version of the DriveML from github with:

```R
devtools::install_github("daya6489/DriveML")
```

# Example

## Data
In this vignette, we will be using Heart Disease - Classifications data set

Data source [UCI](https://archive.ics.uci.edu/ml/datasets/heart+Disease)

```R
	library("DriveML")
	library("SmartEDA")
	## Load heart disease dataset 
	data(heart)
	
```

## Overview of the data
Understanding the dimensions of the dataset, variable names, overall missing summary and data types of each variables

```R
## overview of the data; 
	ExpData(data = heart, type = 1)
## structure of the data	
	ExpData(data = heart, type = 2)
```

## Summary of numerical variables
To summarise the numeric variables, you can use following r codes from this pacakge

```R
## Summary statistics by – overall
	ExpNumStat(heart, by = "GA", gp = "target_var", Qnt = seq(0, 1, 0.1), MesofShape = 2, Outlier = TRUE, round = 2)
```
## Graphical representation of all numeric features

```R
## Generate Boxplot by category
ExpNumViz(heart, gp = "target_var", type = 2, nlim = 25, Page = c(2, 2))

## Generate Density plot
ExpNumViz(heart, gp = NULL, type = 3, nlim = 10, Page = c(2, 2))

## Generate Scatter plot
ExpNumViz(heart, target="target_var", nlim = 4, scatter = TRUE, Page=c(2, 1))
```

# Machine learning model using 3 simple steps

## Data preparation using autoDataprep function

One function to prepare a input data for machine learning model

```R
# Data Preparation
small_data <- autoDataprep(heart, target = "target_var", missimpute = "default",
                       auto_mar = TRUE, mar_object = NULL, dummyvar = TRUE,
                       char_var_limit = 12, aucv = 0.02, corr = 0.99,
                       outlier_flag = TRUE, interaction_var = TRUE,
                       frequent_var = TRUE, uid = NULL, onlykeep = NULL, drop = NULL)

# Print output on R console
printautoDataprep(small_data)

# Final prepared master data

small_data_t <- small_data$master_data
```

## Machine learning classification model using autoMLmodel function

One function to develop machine learning binary classification model 

```R
# DriveML Model development
small_ml_random <- autoMLmodel(small_data_t,  target = "target_var",  testSplit = 0.2,
                      tuneIters = 5,  tuneType = "random",
                      models = "all", varImp = 10,  liftGroup = 10,  maxObs = 10000,  uid = NULL,
                      pdp = T, positive = 1, htmlreport = FALSE, seed = 1991)

# Model summary results
small_ml_random$results

```

## Present model report using autoMLReport function

Generate a report in html format for the output of autoDataprep and autoMLmodel functions. 

```R
autoMLReport(mlobject = small_ml_random, mldata = small_data, op_file = "driveML_ouput_heart_data.html")

```

## Articles

The pre-print version of the paper on DriveML is available at ArXiv at- https://arxiv.org/pdf/2005.00478.pdf.


## References

Boulange, A. (2020) automl: Deep Learning with Metaheuristic. URL:https://CRAN.R-project.org/package=automlr package version 1.3.2

Chen et al. (2020). xgboost: Extreme Gradient Boosting. URL:https://CRAN.R-project.org/package=xgboostr package version 1.0.0.2. 

He, X., Zhao, K., & Chu, X. (2020).  Automl: A survey of the state-of-the-art. arXiv:1908.00709v4. URL: https://arxiv.org/pdf/1908.00709.pdf

Therneau, T., & Atkinson, B. (2019). rpart: Recursive Partitioning and Regression Trees. URL:
https://CRAN.R-project.org/package=rpart r package version 4.1-15.

Wright, M. N., & Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C ++ and R. Journal of Statistical Software, 77, 1–17. doi:10.18637/jss.v077.i01

Maher, M., & Sakr, S. (2019). Smartml: A meta learning-based framework for au-tomated selection and hyperparameter tuning for machine learning algorithms. In Advances in Database Technology-EDBT 2019: 22nd International Conference on Extending Database Technology . Lisbon, Portugal.

