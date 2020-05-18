library(data.table)
library(ggplot2)
library(DriveML)
library(caret)
library(randomForest)
library(xgboost)


# heart Deseas data set ---------------------------------------------------

#Auto data prep
heart_data <- heart
setDF(heart_data)
set.seed(12345)
train.index <- createDataPartition(heart_data$target_var, p = .8, list = FALSE)
train <- heart_data[ train.index,]
valid  <- heart_data[-train.index,]
table(train$target_var) # no. of 1's - 24
table(valid$target_var) # no. of 1's= 5

#-----------------------------------------------------------------------------------
# Without DriveML data prep
#-----------------------------------------------------------------------------------
## Ranger model
library(ranger)
iv <- setdiff(names(heart_data), "target_var")
rf_formula <- formula(paste0('target_var',"~",paste0(iv,collapse = "+")))
system.time(
  myRanger1 <- ranger(
    formula   = rf_formula,seed=12345,verbose=TRUE,
    data      = train, probability = TRUE,
    num.trees = 2000)
)
pred_ranger <- predict(myRanger1, valid)
cat("Ranger model valiation AUC", auc(valid$target_var, pred_ranger$predictions[,1]))

## Random Forest model
myRF1 <-  randomForest (x=train[,iv],y = as.factor (train[,"target_var"]),
                        ntree = 1000, nodesize = 10, do.trace = 10)
class(train)
pred_rf <- predict (myRF1, valid[,iv], type = "prob")[,2]
cat("Random Forest model valiation AUC", auc(valid$target_var, pred_rf))

## XGboost model
dtrain <- xgb.DMatrix (data=as.matrix(train[,iv]), label = as.numeric(paste0(train$target_var)))
dvalid <- xgb.DMatrix (data=as.matrix(valid[,iv]), label = as.numeric(paste0(valid$target_var)))

watchlist <- list (train=dtrain,validation=dvalid)

xgb <- xgb.train(data = dtrain, nthread = 15,
                 objective = "binary:logistic", nround = 500,
                 watchlist = watchlist, eval_metric="auc", maximize=TRUE, printEveryN = 20)
pred_xgb <- predict (xgb, dvalid,xgb$best_iteration)
cat("XGBoost model valiation AUC", auc(valid$target_var, pred_xgb))

## GLM model
myGLM <- glm (as.formula(paste ("target_var", "~", paste(iv, collapse="+"))),
              as.data.frame (train), family="binomial")
pred_glm <- predict (myGLM, as.data.frame (valid), type = "response")
cat("GLM model valiation AUC", auc(valid$target_var, pred_glm))

#-----------------------------------------------------------------------------------
# With DriveML data prep
#-----------------------------------------------------------------------------------

heart_dl_data <- autoDataprep(
  train,
  target = "target_var",
  missimpute = "default",
  auto_mar = TRUE,
  mar_object = NULL,
  dummyvar = TRUE,
  char_var_limit = 12,
  aucv = 0.02,
  corr = 0.99,
  outlier_flag = TRUE,
  interaction_var = TRUE,
  frequent_var = TRUE,
  uid = NULL,
  onlykeep = NULL,
  drop = NULL,
  verbose = TRUE)

dml_heart_data <- heart_dl_data$complete_data
dml_heart_train <- heart_dl_data$master_data
names(dml_heart_train)
dim(dml_heart_data)
dim(dml_heart_train)
dml_heart_valid <- predictDataprep(heart_dl_data, valid)

## DriveML model
heart_dml <- autoMLmodel( train = dml_heart_train, test = dml_heart_valid, target = 'target_var',
                        tuneIters = 100, tuneType = "random",
                        models = c("ranger","logreg","randomforest","ranger","xgboost"),
                        varImp = 10, liftGroup = 50, maxObs = 4000, uid = NULL, seed = 1991, verbose = TRUE)


# Model Fitting time Scoring time Train AUC Test AUC Accuracy Precision
# 2       logreg  52.657 secs   0.013 secs     0.958    0.816    0.800     0.784
# 3 randomForest  45.436 secs   0.021 secs     0.980    0.885    0.833     0.795
# 4       ranger   47.39 secs   0.043 secs     0.998    0.887    0.800     0.769
# 5      xgboost  57.205 secs   0.011 secs     0.984    0.887    0.833     0.811
# Recall F1_score
# 2  0.879    0.829
# 3  0.939    0.861
# 4  0.909    0.833
# 5  0.909    0.857
save.image("C:/Users/dubrangala/OneDrive - VMware, Inc/Project_PropModel/MissingatRandom/DriveML/heart_data_ml_model_v1.RData")


# Adult data set ----------------------------------------------------------

load("C:/Users/dubrangala/OneDrive - VMware, Inc/Project_PropModel/MissingatRandom/DriveML_experitments/medium_data/adult_data.RData")

train <- heart_data[ ,]
valid  <- heart_data[-train.index,]
table(train$target_var) # no. of 1's - 24
table(valid$target_var) # no. of 1's= 5
