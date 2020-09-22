## Auc calculation function
calc_auc <- function (actual, predicted){
  r <- rank(predicted);
  n_pos <- as.numeric (sum(actual == 1))
  n_neg <- as.numeric (length(actual) - n_pos)
  denom <- as.double (as.double (n_pos) * as.double(n_neg))
  auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (denom)
  auc
}

## Auc calculation for completed adata set
calc_auc_df <- function(data, actual, variable, impvalue = 0.02){
  setDT(data)
  if (is.null(data) | is.null(variable) | is.null(actual)) stop("Input data/independent variable list is missing")
  if (length(unique(actual)) <= 1) stop(" target variable has only one outcome values")
  if (length(unique(actual)) == 2){
  aucDF <- data[, variable, with = FALSE][, lapply(.SD, function (x) calc_auc (actual, x))]
  aucDF <- as.data.frame (aucDF)
  aucDF <- t (aucDF)
  aucDF <- as.data.frame (aucDF)
  aucDF$varName <- rownames (aucDF)
  names (aucDF)[1] <- "auc"
  aucDF <- aucDF [order(aucDF$auc, decreasing = TRUE), ]
  aucDF$imp <- abs (aucDF$auc - 0.5)
} else {
  num_class <- unique(actual)
  num_class <- num_class[num_class != 0]
  k <- 0
  for (mk in num_class) {
    k <- k + 1
    actual_y <- ifelse (actual == mk, 1, 0)
    aucDF <- data[, variable, with = FALSE][, lapply(.SD, function (x) calc_auc (actual_y, x))]
    aucDF <- as.data.frame (aucDF)
    aucDF <- t (aucDF)
    aucDF <- as.data.frame (aucDF)
    aucDF$varName <- rownames (aucDF)
    names (aucDF)[1] <- paste ("target", mk, sep = "_")
    if (k == 1)
      bigaucdf <- aucDF
    if (k != 1)
      bigaucdf <- merge (bigaucdf, aucDF, by = "varName", all.x = TRUE)
  }
  setDT(bigaucdf)
  bigaucdf[, (paste0("target_", num_class)) := sapply(num_class, function(x){
    abs(bigaucdf[, paste0("target_", x), with = FALSE] - 0.5)
    })]
  bigaucdf[, imp := apply (bigaucdf[, -1, with = FALSE], 1, max)]
  aucDF <- bigaucdf
}

  aucDF <- aucDF [order(aucDF$imp, decreasing = TRUE), ]
  varSel_auc <- aucDF$varName [which (aucDF$imp >= impvalue)]
return(list(varSel_auc = varSel_auc, aucDF = aucDF))
}

globalVariables(c("imp"))


## Simple impuatation using for MAR
mimpute <- function(x, type = NULL) {
  if (is.null(type)) type <- "default"
typestat <- c("mean", "median", "min", "max", "sum", "var", "sd")
if (type == "default") xx <- 99999 else
if (tolower(type) %in% typestat) xx <- get(type)(x, na.rm = TRUE) else
if (is.numeric(type)) xx <- type else stop("impute type is not valid")
return(xx)
}


## Adding NA levels to the factor variables

addNAfactor <- function(x){
  if (anyNA(x)){
    if (is.null(levels(x))) x <- as.factor(x)
    x <- ifelse(is.na(x), 9999, paste(x))
    x <- factor(x, levels = c(unique(x)))
  } else {
    x <- x
    x <- factor(x, levels = c(levels(x)))
  }
  return(x)
}

## AutoModel
generateTask <- function(x, y = NULL, positive = 1, maxLevels = 100){
    if (is.null(y)) stop("target variable is missing")
  if (missing(x)) stop("Input data is missing")
      levels <- length(unique(x[, y]))
      if (levels == 2){
        type <- "Binary classification"
        x[, y] <- as.factor(x[, y])
        task <- mlr::makeClassifTask(data = x, target = y, positive = positive)
      } else if (levels > 2 & levels <= maxLevels){
        type <- "Multi class classification"
        x[, y] <- as.factor(x[, y])
        task <- mlr::makeClassifTask(data = x, target = y, positive = positive)
      } else stop ("target variable has more than levels")
      return(list(task = task, type = type))
    }

## Generate metrics
generateMetrics <- function(task){
  metrics <- list()
  if (task$type == "Binary classification"){
    metrics$auc <- mlr::auc
    metrics$accuracy <- mlr::acc
    metrics$balancedAccuracy <- mlr::bac
    metrics$brier <- mlr::brier
    metrics$f1 <- mlr::f1
    metrics$meanPrecRecall <- mlr::gpr
    metrics$logloss <- mlr::logloss
    } else if (task$type == "Multi class classification") {
    metrics$auc <- mlr::multiclass.au1u
    metrics$accuracy <- mlr::acc
    metrics$balancedAccuracy <- mlr::bac
    metrics$brier <- mlr::multiclass.brier
    metrics$logloss <- mlr::logloss
      }
  return(metrics)
  }

## Generate hyper params
generateHyperParams <- function(learners = NULL, task){
  hypers <- list()
    hypers$xgboost <- makeParamSet(makeIntegerParam(id = "max_depth", lower = 2, upper = 12),
                                   makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 1, upper = 15),
                                 makeNumericParam(id = "eta", lower = 0.01, upper = 0.5),
                                 makeNumericLearnerParam(id = "subsample", default = 1, lower = 0.5, upper = 1),
                                 makeNumericParam(id = "colsample_bytree", lower = 0.3, upper = 0.9),
                                 #makeDiscreteParam(id = "tree_method", default = "auto", values = c("auto", "approx", "hist")),
                                 makeIntegerParam(id = "nrounds", lower = 50, upper = 500),
                                 makeDiscreteParam(id = "early_stopping_rounds", values = 10))

  hypers$randomforest <- makeParamSet(makeIntegerParam(id = "mtry", lower = 2, upper = 10),
                                      makeIntegerParam(id = "ntree", lower = 50, upper = 600),
                                      makeLogicalLearnerParam(id = "importance", default = TRUE),
                                      makeIntegerParam(id = "nodesize", lower = 1, upper = 20))

  hypers$ranger <- makeParamSet(makeDiscreteLearnerParam(id = "importance", values = c("impurity", "permutation"), default = "permutation", tunable = FALSE),
                                makeIntegerParam(id = "min.node.size", lower = 5, upper = 10),
                                makeIntegerParam(id = "num.trees", lower = 50, upper = 600))

  hypers$glmnet <- makeParamSet(makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
                                makeIntegerLearnerParam(id = "nlambda", default = 10L, lower = 1L, upper = 40L))

  hypers$logreg <- makeParamSet(makeLogicalLearnerParam(id = "model", default = TRUE, tunable = FALSE))

  hypers$rpart <- makeParamSet(makeNumericLearnerParam(id = "cp", lower = 0.001, upper = 0.01),
                               makeIntegerLearnerParam(id = "minbucket", lower = 2, upper = 10),
                               makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 2, upper = 20),
                               makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L))
  if (is.null(learners) == FALSE) hypers <- hypers[which(names(hypers) %in% learners)]

  return(hypers)
}


generateLearners <- function(task){
  if (task$type %in% c("Binary classification", "Multi class classification")){
  learners <- makeLearners(c("glmnet", "logreg", "randomForest", "ranger", "xgboost", "rpart"),
                             type = "classif",
                             predict.type = "prob")
    names(learners) <- c("glmnet", "logreg", "randomForest", "ranger", "xgboost", "rpart")
  }  else stop("task type is not available")
  return(learners)
}

## Stratified down sampling
autoSample <- function(x, y = NULL, maxObs = 40000, seed = 121){
  if (missing(x) == TRUE) stop("Input data is missing")
  set.seed(seed)
  if (nrow(x) > maxObs){
    p <-  maxObs / nrow(x)
  } else {
    p <- 1
  }
  if (is.null(y) == FALSE){
    if(p == 1) p <- 0.99999
    index <- sample.split(x[, y], SplitRatio = p)
  } else {
    index <- sample(nrow(x), p * nrow(x), replace = FALSE)
  }
  x <- x[index, ]
  return(x)
}

## Lift table
calc_lift_cum <- function (actual, predList, predListNms, groups, rows=NULL) {
  if (length (rows) > 0)
    actual <- actual [rows]

  actual_old <- actual
  liftDF <- NULL
  for (k in seq (1, length(predList), 1)) {
    pred <- predList [[k]]
    if (length (rows) > 0)
      pred <- pred [rows]
    actual <- actual_old [order(pred, decreasing = TRUE)]

    pred <- pred [order (pred, decreasing = TRUE)]
    deciles <- rep (1:groups, each = length(actual) / groups)
    deciles <- c(deciles, rep (groups, length (actual) - length(deciles)))
    naiveAcc <- prop.table (table (actual))[2]
    cumlifts <- NULL
    for (j in 1:groups) {
      cumlifts <- c(cumlifts, length (which (actual [deciles <= j] == 1)) / (naiveAcc * length (actual [deciles <= j])))
    }

    liftDF <- rbind (liftDF, data.frame (groups = 1:groups, lift = cumlifts, model = predListNms[k]))
  }
  return (liftDF)

}

## Stratified sampling
sampleStratified <- function (datatrain, numfolds, varstratify, numseed = 121) {
  set.seed (numseed)
  setDF(datatrain)
  num_levels <- length (unique (datatrain[, varstratify]))
  for (m in 0 : num_levels){
    indices <- rep ( c(1 : numfolds), each = nrow(datatrain[datatrain[, varstratify] == m, ]) / numfolds)
    indices <- c(indices, rep ( 1, each = nrow(datatrain[datatrain[, varstratify] == m, ]) - length (indices)))
    datatrain[datatrain[, varstratify] == m, "indices"] <- sample (indices, length(indices))
  }
  setDF(datatrain)
  return (datatrain)
}

## Stack data prep function
stackdataprep <- function(test, score,  validlist, scorelist = NULL, target, uid = NULL){
  if (missing(validlist)) stop("valid list is missing")

  setDF(test)
  if (is.null(uid)) {
    test$unique_id <- 1:nrow(test)
    if (!is.null(scorelist)) score$unique_id <- 1:nrow(score)
  } else {
    test$unique_id <- test[, uid]
    if (!is.null(scorelist)) score$unique_id <- score[, uid]
  }

  mytest <- test[c("unique_id", target)]
  setDT(mytest)
  mytest[, (paste0(names(validlist))) :=  lapply(validlist, function(x) round(x, 3))]

  if (!is.null(scorelist)){
    myscore <- score[c("unique_id", target)]
    setDT(myscore)
    myscore[, (paste0(names(scorelist))) :=  lapply(scorelist, function(x) round(x, 3))]
  } else {
    myscore <- NULL
  }

  return(list("test" = mytest, "score" = myscore))
}

globalVariables(c("unique_id"))


### PDP plot inside autoML
pdplot <- function(train, trainedModels, feat, results, seed, y, sample){
  setorder(results, -`Test AUC`)
  bmod <- paste0(results[1, ]$Model)
  trainedModel <- trainedModels[[bmod]]
  feat <- feat[[bmod]]
  feats_list <- trainedModel$model$features
  set.seed(seed)
  setDF(train)
  ind <- sample.split(train[, y], SplitRatio = sample)
  temp <- train[ind == TRUE, ]
  suppressWarnings(predObj <- Predictor$new(model = trainedModel$model, data = temp[, feats_list], y = temp[, y]))
  plots <- list()
  for (i in 1 : length(feat)){
    suppressWarnings(pd <- FeatureEffect$new(predObj, feature = feat[i], method = "pdp"))
    plots[[i]] <- plot(pd) +
      theme_bw() +
      ggtitle(paste0(feat[i], " Partial Dependence")) +
      geom_line(size = 1, col = "#3A48C5")
  }
  names(plots) <- feat
  return(list(plots = plots))
}

### Beta coef for Logreg, glmnet and raprt
betaml <- function(modelobjet, mname){
  model <- modelobjet
  if (mname == "glmnet"){
    md_coef <- predict(model$learner.model, type = "coef", s = min(model$learner.model$lambda))
    md_coef <- as.matrix(md_coef)
    md_coef <- as.data.frame(md_coef); names(md_coef) <- "coefficients"
    md_coef$Variable <- rownames(md_coef)
    md_coef <- md_coef[, c("Variable", "coefficients")]
  } else
    if (mname == "logreg"){
      md_coef <- model$learner.model$coefficients
      md_coef <- data.frame(Variable = names(md_coef), coefficients = as.numeric(md_coef))
    } else
      if (mname == "rpart") {
      md_coef <- model$learner.model$variable.importance
      md_coef <- data.frame(variable = names(md_coef), importance = as.numeric(md_coef))
    } else
      if (mname %in% c("xgboost", "randomForest", "ranger")) {
        varimp <- getFeatureImportance(model)
        varimportance1 <- varimp$res
        if ("tbl_df" %in% class(varimportance1)) {
          varimportance1 <- data.frame(varimportance1)
          varimportance1 <- varimportance1[order(varimportance1$importance, decreasing = TRUE), ]
          md_coef <- varimportance1
        } else {
          varimportance1 <- t(varimportance1)
          varimportance1 <- data.frame(importance = varimportance1)
          varimportance1$variable <- rownames(varimportance1)
          varimportance1 <- varimportance1[order(varimportance1$importance, decreasing = TRUE), ]
          md_coef <- varimportance1
        }
      }
return(md_coef)
}


