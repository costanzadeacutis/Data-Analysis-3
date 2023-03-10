---
title: "A3-analysis"
author: "costanzadeacutis"
date: "2023-02-24"
output: 
  html_document: 
    keep_md: yes
---


```r
rm(list=ls())

# Import libraries
library(haven)
library(glmnet)
```

```
## Loading required package: Matrix
```

```
## Loaded glmnet 4.1-6
```

```r
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)
```

```
## 
## Attaching package: 'modelsummary'
```

```
## The following object is masked from 'package:Hmisc':
## 
##     Mean
```

```r
library(rattle)
```

```
## Loading required package: tibble
```

```
## Loading required package: bitops
```

```
## 
## Attaching package: 'bitops'
```

```
## The following object is masked from 'package:Matrix':
## 
##     %&%
```

```
## Rattle: A free graphical interface for data science with R.
## Version 5.5.1 Copyright (c) 2006-2021 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data.
```

```r
library(caret)
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:survival':
## 
##     cluster
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
```

```
## 
## Attaching package: 'pROC'
```

```
## The following object is masked from 'package:gmodels':
## 
##     ci
```

```
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```

```r
library(ranger)
```

```
## 
## Attaching package: 'ranger'
```

```
## The following object is masked from 'package:rattle':
## 
##     importance
```

```r
library(rpart)
library(partykit)
```

```
## Loading required package: grid
```

```
## Loading required package: libcoin
```

```
## Loading required package: mvtnorm
```

```r
library(rpart.plot)
library(viridis)
```

```
## Loading required package: viridisLite
```

```r
# load theme and functions
source("~/subdirectory/Assignment3/A3/A3/theme_bg.R") 
```

```
## ?????? Attaching packages
## ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
## tidyverse 1.3.2 ??????
```

```
## ??? tidyr   1.2.1      ??? stringr 1.5.0 
## ??? readr   2.1.3      ??? forcats 0.5.2 
## ??? dplyr   1.0.10     
## ?????? Conflicts ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????? tidyverse_conflicts() ??????
## ??? tidyr::expand()     masks Matrix::expand()
## ??? dplyr::filter()     masks stats::filter()
## ??? dplyr::group_rows() masks kableExtra::group_rows()
## ??? dplyr::lag()        masks stats::lag()
## ??? caret::lift()       masks purrr::lift()
## ??? tidyr::pack()       masks Matrix::pack()
## ??? dplyr::src()        masks Hmisc::src()
## ??? dplyr::summarize()  masks Hmisc::summarize()
## ??? tidyr::unpack()     masks Matrix::unpack()
## 
## Attaching package: 'scales'
## 
## 
## The following object is masked from 'package:readr':
## 
##     col_factor
## 
## 
## The following object is masked from 'package:viridis':
## 
##     viridis_pal
## 
## 
## The following object is masked from 'package:purrr':
## 
##     discard
```

![](bisnode_analysis_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
source("~/subdirectory/Assignment3/A3/A3/functions.R")
```

```
## 
## Please cite as: 
## 
##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer
```

```r
options(digits = 3) 

data_in <- "~/subdirectory/Assignment3/A3/A3"
data_out <- "~/subdirectory/Assignment3/A3/A3/"
output <- "~/subdirectory/Assignment3/A3/A3/output/"
#create_output_if_doesnt_exist(output)

knitr::opts_chunk$set(echo = TRUE)
```

# A3 ANALYSIS
This analysis aims at predicting the probability of a firm being fast-growing, and at finding the classification that delivers the lowest expected loss, given the loss function.

## Load data
Load cleaned data: 13758 observations, 10391 non-fast-growing and 3367 fast-growing firms.


```r
# Use R format so it keeps factor definitions
data <- read_rds(paste(data_out,"bisnode_firms_clean.rds", sep = "/"))

table(data$fast_growth_f)
```

```
## 
## no_fast_growth    fast_growth 
##          10391           3367
```

```r
#summary
#datasummary_skim(data, type='numeric', histogram = TRUE)
#datasummary_skim(data, type="categorical")
```

## Models
### Set of variables
I group variables in sets 


```r
rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")
#financial variables 
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
#growth sales
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")
#human resources 
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "foreign_management")
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")
```

### Interactions, set of regressors for logit and LASSO, random forest variables
In the following section, I construct 6 different models where the target variable is regressed on
- Logit X1 (11 predictors): log sales, log sales squared, growth sales, profit/loss, industry category
- Logit X2 (18): Logit X1 + fixed assets, share eq, current liabilities (and flags), age,            foreign_management, industry category
- Logit X3 (35): Logit X2 + financial var 1 + flag growth sales
- Logit X4 (75): Logit X3 + financial var 2 + fin var 3 + human resource + quality var
- Logit X5 (142): Logit X4 + interactions
- Logit LASSO: same specification as Logit X5
Moreover, I will perform also Random Forest based on the benchmark model identified among those listed above, based on RMSE, AUC, and lowest expected loss (see PART 1.b)


```r
# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")

# set of regressors for logit (model 1 to 5)
X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs","curr_liab_bs ",   "curr_liab_bs_flag_high ", "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar,                   d1)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars, interactions1, interactions2)

# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, d1, hr, firm, qualityvars, interactions1, interactions2)
```

## Set up 
I randomly create:
- Train set (data_work, 80% of the total data): 11007 observations, 8339 non-fast-growing and 2668 fast-growing firms
- Holdout set (data_holdout, 20% of the total data): 2751 observations, 2052 non-fast and 699 fast-growing firms.
Then, I look at the target variable in each dataset.


```r
set.seed(13505)

train_indices <- as.integer(createDataPartition(data$fast_growth, p = 0.8, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
```

```
## [1] 11007   118
```

```r
dim(data_holdout)
```

```
## [1] 2751  118
```

```r
Hmisc::describe(data$fast_growth_f)
```

```
## data$fast_growth_f 
##        n  missing distinct 
##    13758        0        2 
##                                         
## Value      no_fast_growth    fast_growth
## Frequency           10391           3367
## Proportion          0.755          0.245
```

```r
Hmisc::describe(data_train$fast_growth_f)
```

```
## data_train$fast_growth_f 
##        n  missing distinct 
##    11007        0        2 
##                                         
## Value      no_fast_growth    fast_growth
## Frequency            8339           2668
## Proportion          0.758          0.242
```

```r
Hmisc::describe(data_holdout
                $fast_growth_f)
```

```
## data_holdout$fast_growth_f 
##        n  missing distinct 
##     2751        0        2 
##                                         
## Value      no_fast_growth    fast_growth
## Frequency            2052            699
## Proportion          0.746          0.254
```


# PART 1.a - Predict probabilities with Logit and LASSO
## Logit and Logit Lasso
I set up a 5-fold cross-validation for model selection on the 5 logit models. Then, I look into LASSO, which is an automatic method to select variables: it shrinks coefficients towards zero of those variables that variables whose inclusion does not improve the fit of the regression. This is equivalent to removing them from the regression. I ran the LASSO algorithm with 5-fold cross-validation for selecting the optimal for the tuning parameter lambda (which gives the weight to the penalty term). The optimal lambda is  0.00464 and the suggested features to be kept are 43: LASSO removed 100 features that were included in Logit X5.
With the summary table, I can compare the performance of the different models in predicting probabilities. To choose the best model among those evaluated, I look at the lowest RMSE in the test set. Among the logit models, this corresponds to Logit X3. While, more complex models as X4 and X5 are overfitting. Logit X3 has the same average RMSE as Logit LASSO, but the latter has more predictors; thus, I would keep Logit X3 as the best model.


```r
# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)


# Train Logit Models 

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]

  set.seed(13505)
  glm_model <- train(
    formula(paste0("fast_growth_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]

}

# Logit lasso 

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("fast_growth_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})
```

```
##    user  system elapsed 
##   23.50    1.34   25.89
```

```r
tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
print(best_lambda)
```

```
## [1] 0.00464
```

```r
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
sum(lasso_coeffs != 0)
```

```
## [1] 43
```

```r
write.csv(lasso_coeffs, paste0(output, "lasso_logit_coeffs.csv"))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]


# For each model look at average RMSE

CV_RMSE_1 <- list()

for (model_name in names(logit_models)) {
  CV_RMSE_1[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
}

# 6 models, (5 logit and the logit lasso). For each I have a 5-CV RMSE and AUC.
# Find preferred model based on that

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE_1))

kable(x = logit_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","CV RMSE")) %>%
  cat(.,file= paste0(output, "logit_summary.tex"))
```

### ROC and AUC
First, I create the ROC curves and calculate the AUC for each fold of both Logit and Logit LASSO models. The ROC curve displays 1-specificity and sensitivity for all possible thresholds for classification (between 0 and 1) and it illustrates the non-linear trade-off between making false positive and false negative errors. The AUC is the area under the ROC curve and is used as a statistic to compare models (the larger the better). Once the AUC is calculated for each fold, I take the average and compare the models. From the table, it can be seen that the AUC points at the Logit X3 as the best model (0.642 compared to 0.627 of Logit LASSO).


```r
# Draw ROC Curve and calculate AUC for each folds
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {

  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)

    roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }

  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                              "AUC" = unlist(auc))
}
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```r
# For each model: average RMSE and average AUC for models

CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

# 6 models, (5 logit and the logit lasso). For each I have a 5-CV RMSE and AUC.
# Find preferred model based on that

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

kable(x = logit_summary1, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","CV RMSE","CV AUC")) %>%
  cat(.,file= paste0(output, "logit_summary1.tex"))
```

### Bring best model to holdout
After picking Logit X3 as the best model, I bring this model to the holdout set and I calculate RMSE on this subset of the data. The model performs worse on the holdout than in the training set (RMSE = 0.426).


```r
# Take best model and estimate RMSE on holdout 

best_logit_no_loss <- logit_models[["X3"]]

logit_predicted_probabilities_holdout <- predict(best_logit_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"fast_growth"]
RMSE(data_holdout[, "best_logit_no_loss_pred", drop=TRUE], data_holdout$fast_growth)
```

```
## [1] 0.426
```

### Calibration curve 
How well do estimated vs actual event probabilities relate to each other?


```r
create_calibration_plot(data_holdout, 
  file_name = "logit-x3-calibration", 
  prob_var = "best_logit_no_loss_pred", 
  actual_var = "fast_growth",
  n_bins = 10)
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ??? Please use `linewidth` instead.
```

```
## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
## ??? Please use the `linewidth` argument instead.
```

```
## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
## ??? Please use the `linewidth` argument instead.
```

```
## Warning: Removed 4 rows containing missing values (`geom_line()`).
## Removed 4 rows containing missing values (`geom_line()`).
## Removed 4 rows containing missing values (`geom_line()`).
```

![](bisnode_analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->






# PART 2.a - Classification with loss function
## Classification Logit and Logit LASSO
The aim of this analysis is to find fast-growing firms for investment decisions. The type of investment here considered have a fixed cost of 4000 euros per firm. If the bank invests in a fast-growing firm, the return is 5000 euros, otherwise the bank looses the money invested. Thus, if the firm is predicted to be fast-growing but in reality it is not, investing in it would mean loosing 4000. If instead the firm is predicted non-fast-growing and the investment is not carried out, the profit opportunity of 1000 is lost.
FN = predict no fast growth and instead fast growing -> 5000-4000=1000
FP = predict fast growth but instead non-fast-growing -> 4000
With the loss function, I can find the optimal threshold for classification, i.e. the threshold that delivers the lowest expected loss from the classification.
Under ideal circumstances, the optimal threshold can be calculated by the formula
loss(FN)/(loss(FP) + loss(FN)), which in this case would be equal to 5/9 = 0.556. This would be true if the prediction model I chose, i.e. Logit X3, were the best possible prediction model. Therefore, I resort to the algorithm to find the optimal threshold via cross-validation. The resulting selected threshold for Logit X3 is 0.548, with CV expected loss 0.964; for Logit LASSO and other logit models, except for Logit X2, the resulting optimal threshold is "Inf", meaning that there is no optimal threshold and the best way to proceed would be just not to invest. The lowest average expected loss is given by Logit X2, but Logit X3 produces a very similar loss.


```r
# Introduce loss function
# relative cost of a false negative classification (as compared with a false positive classification)
FP=5
FN=4
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$fast_growth)/length(data_train$fast_growth)

# Draw ROC Curve and find optimal threshold with loss function 

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

for (model_name in names(logit_models)) {

  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")

  best_tresholds_cv <- list()
  expected_loss_cv <- list()

  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)

    roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$fast_growth)
  }

  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))

  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]

  }
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```r
logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))

kable(x = logit_summary2, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) %>%
  cat(.,file= paste0(output, "logit_summary2.tex"))
```

### Plots
I report the plots on the 5th fold of the CV search algorithm of 
- the expected loss and optimal threshold 
- the ROC curves
for each model


```r
# Create plots based on Fold5 in CV 

for (model_name in names(logit_cv_rocs)) {

  r <- logit_cv_rocs[[model_name]]
  best_coords <- logit_cv_threshold[[model_name]]
  createLossPlot(r, best_coords,
                 paste0(model_name, "_loss_plot"))
  createRocPlotWithOptimal(r, best_coords,
                           paste0(model_name, "_roc_plot"))
}
```


### Best model
I pick best based on average expected loss (Logit X3), and bring it to holdout


```r
# Pick best model based on average expected loss 

best_logit_with_loss <- logit_models[["X3"]]
best_logit_optimal_treshold <- best_tresholds[["X3"]]

logit_predicted_probabilities_holdout <- predict(best_logit_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"fast_growth"]
```

#### ROC and expected loss
The expected loss on the holdout set when Logit X3 is applied is equal to 1.02.


```r
# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "best_logit_with_loss_pred", drop=TRUE])
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```r
# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout
```

```
## [1] 1.02
```


#### Confusion table
This confusion table shows true and and miss-classifications on the holdout set. It appears that the model does well in classifying correctly those firms that are non-fast-growing (99.81%), while it classifies correctly as fast-growing only 0.3% of the total. Given that the highest loss is registered for false positive, it is reassuring that the miss-classification of non-fast growing as fast-growing is so small. However, it is very difficult to find fast-growing firms because they are almost always classified as non-fast-growing.


```r
# Confusion table on holdout with optimal threshold
holdout_prediction <-
  ifelse(data_holdout$best_logit_with_loss_pred < best_logit_optimal_treshold, "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object3 <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm3 <- cm_object3$table
cm3
```

```
##                 Reference
## Prediction       no_fast_growth fast_growth
##   no_fast_growth           2048         697
##   fast_growth                 4           2
```

```r
kable(x = cm3, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "") %>%
  cat(.,file= paste0(output, "confusion.tex"))
```




# PART 1.b - Predict probabilities with Random Forest
## CV Random forest
I build a probability random forest model to predict probabilities based on the benchmark model Logit X3, but no feature engineering (random forest should approximate principal non-linearities and interactions). 
The parameters for the RF are 
- number of bootstrap samples (500)
- number of variables considered in each split (decorrelation - square root of total number of variables - try 4 5 6)
- minimum number of observations in terminal nodes of each tree (stopping rule - 5 10)
and I use a 5-fold CV with Gini impurity index, which is the same as using MSE to find the best fit.
The model is outperformed by Logit X3: the cross validated RMSE of 0.42 and AUC of 0.635.


```r
# same as X3 but no interactions, no modified features
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, firm)

# Probability forest
# Split by gini, ratio of 1's in each tree, average over trees

# 5 fold cross-validation

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c(4, 5, 6),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

# getModelInfo("ranger")
set.seed(13505)
rf_model_p <- train(
  formula(paste0("fast_growth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)
```

```
## + Fold1: mtry=4, splitrule=gini, min.node.size=10 
## - Fold1: mtry=4, splitrule=gini, min.node.size=10 
## + Fold1: mtry=5, splitrule=gini, min.node.size=10 
## - Fold1: mtry=5, splitrule=gini, min.node.size=10 
## + Fold1: mtry=6, splitrule=gini, min.node.size=10 
## - Fold1: mtry=6, splitrule=gini, min.node.size=10 
## + Fold1: mtry=4, splitrule=gini, min.node.size=15 
## - Fold1: mtry=4, splitrule=gini, min.node.size=15 
## + Fold1: mtry=5, splitrule=gini, min.node.size=15 
## - Fold1: mtry=5, splitrule=gini, min.node.size=15 
## + Fold1: mtry=6, splitrule=gini, min.node.size=15 
## - Fold1: mtry=6, splitrule=gini, min.node.size=15 
## + Fold2: mtry=4, splitrule=gini, min.node.size=10 
## - Fold2: mtry=4, splitrule=gini, min.node.size=10 
## + Fold2: mtry=5, splitrule=gini, min.node.size=10 
## - Fold2: mtry=5, splitrule=gini, min.node.size=10 
## + Fold2: mtry=6, splitrule=gini, min.node.size=10 
## - Fold2: mtry=6, splitrule=gini, min.node.size=10 
## + Fold2: mtry=4, splitrule=gini, min.node.size=15 
## - Fold2: mtry=4, splitrule=gini, min.node.size=15 
## + Fold2: mtry=5, splitrule=gini, min.node.size=15 
## - Fold2: mtry=5, splitrule=gini, min.node.size=15 
## + Fold2: mtry=6, splitrule=gini, min.node.size=15 
## - Fold2: mtry=6, splitrule=gini, min.node.size=15 
## + Fold3: mtry=4, splitrule=gini, min.node.size=10 
## - Fold3: mtry=4, splitrule=gini, min.node.size=10 
## + Fold3: mtry=5, splitrule=gini, min.node.size=10 
## - Fold3: mtry=5, splitrule=gini, min.node.size=10 
## + Fold3: mtry=6, splitrule=gini, min.node.size=10 
## - Fold3: mtry=6, splitrule=gini, min.node.size=10 
## + Fold3: mtry=4, splitrule=gini, min.node.size=15 
## - Fold3: mtry=4, splitrule=gini, min.node.size=15 
## + Fold3: mtry=5, splitrule=gini, min.node.size=15 
## - Fold3: mtry=5, splitrule=gini, min.node.size=15 
## + Fold3: mtry=6, splitrule=gini, min.node.size=15 
## - Fold3: mtry=6, splitrule=gini, min.node.size=15 
## + Fold4: mtry=4, splitrule=gini, min.node.size=10 
## - Fold4: mtry=4, splitrule=gini, min.node.size=10 
## + Fold4: mtry=5, splitrule=gini, min.node.size=10 
## - Fold4: mtry=5, splitrule=gini, min.node.size=10 
## + Fold4: mtry=6, splitrule=gini, min.node.size=10 
## - Fold4: mtry=6, splitrule=gini, min.node.size=10 
## + Fold4: mtry=4, splitrule=gini, min.node.size=15 
## - Fold4: mtry=4, splitrule=gini, min.node.size=15 
## + Fold4: mtry=5, splitrule=gini, min.node.size=15 
## - Fold4: mtry=5, splitrule=gini, min.node.size=15 
## + Fold4: mtry=6, splitrule=gini, min.node.size=15 
## - Fold4: mtry=6, splitrule=gini, min.node.size=15 
## + Fold5: mtry=4, splitrule=gini, min.node.size=10 
## - Fold5: mtry=4, splitrule=gini, min.node.size=10 
## + Fold5: mtry=5, splitrule=gini, min.node.size=10 
## - Fold5: mtry=5, splitrule=gini, min.node.size=10 
## + Fold5: mtry=6, splitrule=gini, min.node.size=10 
## - Fold5: mtry=6, splitrule=gini, min.node.size=10 
## + Fold5: mtry=4, splitrule=gini, min.node.size=15 
## - Fold5: mtry=4, splitrule=gini, min.node.size=15 
## + Fold5: mtry=5, splitrule=gini, min.node.size=15 
## - Fold5: mtry=5, splitrule=gini, min.node.size=15 
## + Fold5: mtry=6, splitrule=gini, min.node.size=15 
## - Fold5: mtry=6, splitrule=gini, min.node.size=15 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 5, splitrule = gini, min.node.size = 15 on full training set
```

```r
rf_model_p$results
```

```
##   mtry splitrule min.node.size Accuracy  Kappa  RMSE AccuracySD KappaSD  RMSESD
## 1    4      gini            10    0.759 0.0392 0.420    0.00211 0.00648 0.00110
## 2    4      gini            15    0.759 0.0347 0.420    0.00242 0.00534 0.00115
## 3    5      gini            10    0.758 0.0415 0.420    0.00350 0.01099 0.00136
## 4    5      gini            15    0.759 0.0424 0.420    0.00263 0.00373 0.00111
## 5    6      gini            10    0.758 0.0447 0.421    0.00304 0.00668 0.00160
## 6    6      gini            15    0.758 0.0427 0.420    0.00195 0.00411 0.00142
```

```r
best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

# Get average (ie over the folds) RMSE and AUC 
CV_RMSE_folds[["rf_p"]] <- rf_model_p$resample[,c("Resample", "RMSE")]

auc <- list()
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(Resample == fold)

  roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
  auc[[fold]] <- as.numeric(roc_obj$auc)
}
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```r
CV_AUC_folds[["rf_p"]] <- data.frame("Resample" = names(auc),
                                         "AUC" = unlist(auc))

CV_RMSE[["rf_p"]] <- mean(CV_RMSE_folds[["rf_p"]]$RMSE)
CV_AUC[["rf_p"]] <- mean(CV_AUC_folds[["rf_p"]]$AUC)
```
# PART 2.b - Use probability forest for classification
I use the predicted probabilities to find the optimal threshold by cross-validation and make the classification. 
The average CV expected loss is 0.958, lower than all the other models.


```r
# Now use loss function and search for best thresholds and expected loss over folds
best_tresholds_cv <- list()
expected_loss_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)

  roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
  best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
  expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$fast_growth)
}
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = no_fast_growth, case = fast_growth
```

```
## Setting direction: controls < cases
```

```r
# average
best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))
expected_loss[["rf_p"]] <- mean(unlist(expected_loss_cv))


rf_summary <- data.frame("CV RMSE" = CV_RMSE[["rf_p"]],
                         "CV AUC" = CV_AUC[["rf_p"]],
                         "Avg of optimal thresholds" = best_tresholds[["rf_p"]],
                         "Threshold for Fold5" = best_treshold$threshold,
                         "Avg expected loss" = expected_loss[["rf_p"]],
                         "Expected loss for Fold5" = expected_loss_cv[[fold]])

kable(x = rf_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("CV RMSE", "CV AUC",
                                  "Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) %>%
  cat(.,file= paste0(output, "rf_summary.tex"))

# Create plots - this is for Fold5

createLossPlot(roc_obj, best_treshold, "rf_p_loss_plot") 
```

![](bisnode_analysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
createRocPlotWithOptimal(roc_obj, best_treshold, "rf_p_roc_plot") 
```

![](bisnode_analysis_files/figure-html/unnamed-chunk-16-2.png)<!-- -->


### Results on holdout
I re-estimate the model to do predictions on the holdout set. The resulting RMSE is 0.425 and AUC is 0.643. The expected loss is 1.02, as the Logit X3 on the holdout.
The confusion table shows that the model does slightly better in identifying correctly fast-growing, compared to Logit X3.


```r
# Take model to holdout and estimate RMSE, AUC and expected loss 

rf_predicted_probabilities_holdout <- predict(rf_model_p, newdata = data_holdout, type = "prob")
data_holdout$rf_p_prediction <- rf_predicted_probabilities_holdout[,"fast_growth"]
RMSE(data_holdout$rf_p_prediction, data_holdout$fast_growth)
```

```
## [1] 0.425
```

```r
# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "rf_p_prediction", drop=TRUE])
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```r
# AUC
as.numeric(roc_obj_holdout$auc)
```

```
## [1] 0.643
```

```r
# Get expected loss on holdout with optimal threshold
holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf_p"]] , input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout
```

```
## [1] 1.02
```

```r
# Confusion table on holdout with optimal threshold
holdout_prediction_rf <-
  ifelse(data_holdout$rf_p_prediction < holdout_treshold$threshold, "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object4 <- confusionMatrix(holdout_prediction_rf,data_holdout$fast_growth_f)
cm4 <- cm_object4$table
cm4
```

```
##                 Reference
## Prediction       no_fast_growth fast_growth
##   no_fast_growth           2046         694
##   fast_growth                 6           5
```

```r
kable(x = cm4, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "") %>%
  cat(.,file= paste0(output, "confusion_rf.tex"))
```
# Summary results
I can compare the Logit models, Logit LASSO, and Random Forest in predicting probabilities and classifying observations. Logit X3 and LASSO outperform Random forest in predicting, with Logit X3 being the best model according to AUC. Instead, Random Forest performs better than the rest in classifying, with lower expected loss. 


```r
# Summary results   
nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

model_names <- c("Logit X1", "Logit X3",
                 "Logit LASSO","RF probability")
summary_results <- summary_results %>%
  filter(rownames(.) %in% c("X1", "X3", "LASSO", "rf_p"))
rownames(summary_results) <- model_names

kable(x = summary_results, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors", "CV RMSE", "CV AUC",
                                  "CV threshold", "CV expected Loss")) %>%
  cat(.,file= paste0(output, "summary_results.tex"))
```

