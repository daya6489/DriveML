#' Automated machine learning training of models
#'
#' Automated training, tuning and validation of machine learning models. Models are  tuned and resampling validated on an experiment set and trained on the full set and validated and testing on external sets. Classification models tune the probability threshold automatically and returns the results. Each model contains information of performance, the trained model as well as some plots.
#'
#' @param train [data.frame | Required] Training set
#' @param test [data.frame | Optional] Optional testing set to validate models on. If none is provided, one will be created internally. Default of NULL
#' @param target [integer | Required] If a target is provided classification or regression models will be trained, if left as NULL unsupervised models will be trained. Default of NULL
#' @param maxLevels [integer | Optional] Number of unique values in target feature before the problem type is seen as a regression problem. Default of 100
#' @param testSplit [numeric | Optional] Percentage of data to allocate to the test set. Stratified sampling is done. Default of 0.1
#' @param tuneIters [integer | Optional] Number of tuning iterations to search for optimal hyper parameters. Default of 10
#' @param tuneType [character | Optional] Tune method applied, options are: random, frace and grid. random uses random tuning and frace uses iterated f-racing algorithm for the best solution. Default of random
#' @param gridItune [integer | Optional] Number of tuning iterations to grid search for optimal hyper parameters. Default of 15, this is only applicable if tuneType is grid
#' @param models [character | Optional] Which models to train. Default of all. Available models are logistic regression, glment, rpart, xgboost, ranger and randomForest
#' @param varImp [integer | Optional] Number of important features to plot
#' @param liftGroup [integer | Optional] NUmber of lift value to validate the test model performance
#' @param maxObs [numeric | Optional] Number of observations in the experiment training set on which models are trained, tuned and resampled on. Default of 40000. If the training set has less than 40k observations all will be used
#' @param uid [character | Optional] unique variable to keep in test output data
#' @param seed [integer | Optional] Random number seed for reproducible results
#' @details
#' all the models trained using mlr train function, all of the functionality in mlr package can be applied to the autoMLmodel outcome
#'
#' autoMLmodel provides below information of the machine learning classification models
#' \describe{
#'   \item{\code{trainedModels}}{Model level list output contains trained model object, hyper parameters, tuned data, test data, performance and Model plots}
#'   \item{\code{results}}{Summary of all trained model reuslts like AUC, Precision, Recall, F1 score}
#'   \item{\code{lift}}{Model gain chart}
#'}
#'
#' @return List output contains trained models and results
#' @examples
#' # Run binary classification model defualt method
#' mymodel <- autoMLmodel( train = heart, test = NULL, target = 'target_var', testSplit = 0.2,
#' maxLevels = 100, tuneIters = 10, tuneType = "random",
#' models = c("xgboost", "ranger"), varImp = 10, liftGroup = 50, maxObs = 4000,
#' uid = NULL, seed = 1991)
#'
#' # Run only Logistic regression model
#' mymodel <- autoMLmodel( train = heart, test = NULL, target = 'target_var', testSplit = 0.2,
#' maxLevels = 100, tuneIters = 10, tuneType = "random",
#' models = "logreg", varImp = 10, liftGroup = 50, maxObs = 4000, uid = NULL, seed = 1991)

#'
#' @import ggplot2
#' @importFrom mlr train performance makeClassifTask makeTuneControlGrid makeTuneControlRandom makeTuneControlIrace makeResampleDesc tuneParams setHyperPars generateHyperParsEffectData getFeatureImportance resample plotCalibration generateCalibrationData generateThreshVsPerfData fpr tpr acc calculateROCMeasures plotROCCurves plotThreshVsPerf makeLearners auc ppv f1 tpr acc
#' @importFrom ParamHelpers makeParamSet makeIntegerParam makeNumericParam makeDiscreteParam makeNumericLearnerParam makeLogicalLearnerParam makeDiscreteLearnerParam makeIntegerLearnerParam
#' @importFrom SmartEDA ExpCatViz
#' @importFrom caret createDataPartition
#' @seealso
#' \code{\link[mlr:train]{train}}
#' \code{\link[mlr:makeLearner]{makeLearner}}
#' @export autoMLmodel

autoMLmodel <- function(train,  test = NULL,  target = NULL,  maxLevels = 100,  testSplit = 0.2,
  tuneIters = 200,  tuneType = "random",  gridItune = 15, models = "all",
  varImp = 20,  liftGroup = 50,  maxObs = 40000,  uid = NULL,  seed = 1991)
{
  set.seed(seed, "L'Ecuyer")
  options(scipen = 999)

  if(missing(train) == TRUE) stop("No training data provided")
  if(is.null(target)) stop("Traget variable is missing")
  setDF(train)

  if(is.null(test) == TRUE){
    ind <- createDataPartition(y = train[,target], p = (1 - testSplit), list = FALSE)
    test <- train[-ind,]
    train <- train[ind,]
    table(train$target_china_partner)
    cat("autoMLmodel < Test set created >\n")}

  if(!is.null(uid)) {
    train_uid <- train[uid]
    train <- subset(train, select = setdiff(names(train), uid))
    if(!is.null(test)) {
    test_uid <- test[uid]
    test <- subset(test, select = setdiff(names(test), uid))
    }
  }

  # Generate autosample
  auo <- autoSample(x = train, y = target, seed = seed, maxObs = maxObs)
  fullTasks <- auoTasks <- list()

  auoTasks$fullTask <- generateTask(x = auo, y = target, maxLevels = maxLevels)
  fullTasks$fullTask <- generateTask(x = train, y = target, maxLevels = maxLevels)
  cat(paste0("autoMLmodel < ", auoTasks$fullTask$type," task generated > \n"))

  # Generate model learners
  learners <- suppressWarnings(generateLearners(task = auoTasks$fullTask))
  cat(paste0("autoMLmodel < Learners generated > \n"))

  # Hyperparameter
  params <- generateHyperParams(task = auoTasks$fullTask)
  cat(paste0("autoMLmodel < Hyper parameters generated > \n"))

  # Model performance metric
  metrics <- generateMetrics(task = auoTasks$fullTask)
  metric <- metrics$auc

  cat(paste0("autoMLmodel < Performance metric generated as: ",metric$id,"> \n"))

  if(tuneType == "random"){
      tune <-  makeTuneControlRandom(maxit = tuneIters, tune.threshold = TRUE)
    } else if(tuneType == "frace") {
      tune <- makeTuneControlIrace(maxExperiments = tuneIters, tune.threshold = TRUE)
    } else if(tuneType == "grid") {
      tune <- makeTuneControlGrid(resolution = gridItune, tune.threshold = TRUE)
    }
  cat("autoMLmodel < Tune control generated > \n")

  #Percentage of data to allocate to the validation set. Stratified sampling is done. Default of 0.3
  #resamples <- makeResampleDesc(method = resampleMethod, iters = resampleIters, stratify = TRUE)
  validation <- makeResampleDesc(method = "Holdout", stratify = TRUE, split = 0.3)

  cat("autoMLmodel < Validation set generated > \n")

  names(auoTasks) <- gsub("Task","",names(auoTasks))
  names(fullTasks) <- gsub("Task","",names(fullTasks))

  auoTasks[sapply(auoTasks, is.null)] <- NULL
  fullTasks[sapply(fullTasks, is.null)] <- NULL

  resultsa <- expand.grid(Model = names(learners),
                         `Fitting time` = NA,
                         `Scoring time` = NA,
                         `Train AUC` = NA,
                         `Test AUC` = NA,
                         `Accuracy` = NA,
                         `Precision` = NA,
                         `Recall` = NA,
                         `F1_score` = NA)


  models_lp <- "all"
  modelchk <- models_lp[models_lp %in% models]
  results <- resultsa

  if(length(modelchk) == 0L) results <- subset(resultsa, tolower(resultsa$Model) %in% tolower(models))


  # if(is.null(cores) == TRUE)    cores <- (detectCores() - 1)
  # parallelStartSocket(cpus = cores, show.info = FALSE)

  trainedModels <- list()
  cat(paste0("autoMLmodel < Training learners > \n"))

  predList <- list ()

  for(i in 1:nrow(results)){
    set.seed(seed, "L'Ecuyer")
    model_st <- Sys.time()
    model <- list()
    tuneTask <- auoTasks[[which(names(auoTasks) == "full")]]$task
    trainTask <- fullTasks[[which(names(fullTasks) == "full")]]$task
    modName <- as.character(results[i, "Model"])
    mod <- learners[[which(names(learners) == modName)]]
    modelPlots <- list()
    cat(modName, "Model tuning started....", '\n')

    # Classification model
    ps <- params[[which(tolower(names(params)) == tolower(as.character(modName)))]]
      tuned <- suppressMessages(tuneParams(task = tuneTask,
                                           resampling = validation,
                                           control = tune,
                                           par.set = ps,
                                           measures = metric,
                                           learner = mod,
                                           show.info = FALSE))

      mod <- setHyperPars(mod, par.vals = tuned$x)

      model$model <- train(learner = mod, task = trainTask)
      model$tuneData <- generateHyperParsEffectData(tuned, partial.dep = TRUE)
      model$HyperPara <- mod
      results[i, "Fitting time"] <- round(Sys.time() - model_st,3)

    # Train MLR model
    ## Variable imp
    if(modName %in% c("xgboost", "randomForest", 'ranger')) {
      varimp <- getFeatureImportance(model$model)
      varimportance1 <- varimp$res
      if("tbl_df" %in% class(varimportance1)) {
        varimportance1 = data.frame(varimportance1)
        varimportance1 <- varimportance1[order(varimportance1$importance, decreasing = TRUE), ]

      } else {
        varimportance1 <- t(varimportance1)
        varimportance1 <- data.frame(importance = varimportance1)
        varimportance1$variable <- rownames(varimportance1)
        varimportance1 <- varimportance1[order(varimportance1$importance, decreasing = TRUE), ]
      }

      model$varImportance <- varimportance1
      if(varImp > nrow(varimportance1)) varImp <- nrow(varimportance1)
      varplot = ExpCatViz(data=varimportance1[1 : varImp, ], target = "variable", col = "lightblue", rdata = TRUE, value = "importance", Flip = T, gtitle =paste0("Variable importance : Top ", varImp) )
    } else {
      model$varImportance <- NULL
      varplot <- NULL
    }
    cat ("autoMLmodel < All features",modName,"tuned and trained > \n")
    # cv <- resample(learner = mod, task = tuneTask, resampling = resamples,
    #                measures = metric, show.info = FALSE)
    # results[i, "Resamples"] <- round(cv$aggr, 3)
    # results[i, "ResamplesStDev"] <- round(sd(cv$measures.test[,2]), 3)

    model_st <- Sys.time()
    p.train <- predict(model$model, newdata = train[,model$model$features])
    p.train$data$truth <- train[,target]

    p.test <- predict(model$model, newdata = test[,model$model$features])
    p.test$data$truth <- test[,target]
    predList [[i]] <- p.test$data$prob.1
    results[i, "Scoring time"] <- round(Sys.time() - model_st,3)

    if(!is.null(uid)) {model$testdata <- cbind(test_uid,p.test$data)
    } else {
      model$testdata <- p.test$data
    }

    results[i, "Train AUC"] <- round(mlr::performance(pred = p.train, task = trainTask, measures = mlr::auc)[[1]], 3)
    results[i, "Test AUC"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::auc)[[1]], 3)
    results[i, "Accuracy"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::acc)[[1]], 3)
    results[i, "Precision"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::ppv)[[1]], 3)
    results[i, "Recall"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::tpr)[[1]], 3)
    results[i, "F1_score"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::f1)[[1]], 3)

    model$performance <- results[i,]

    #Model plot
    model$probCutoff <- tuned$threshold

    if(length(unique(train[,target])) == 2){
        temp <- generateThreshVsPerfData(p.train, measures = list(fpr, tpr, acc))
        temp2 <- generateThreshVsPerfData(p.test, measures = list(fpr, tpr, acc))

        plot1 <- plotROCCurves(temp) +
          ggtitle(paste0("Train ROC Curve: ", modName)) +
          annotate("text", x = .75, y = .25, label = paste("AUC =", results[i, "Train AUC"])) +
          theme_light()

        plot2 <- plotROCCurves(temp2) +
          ggtitle(paste0("Test ROC Curve: ", modName)) +
          annotate("text", x = .75, y = .25, label = paste("AUC =", results[i, "Test AUC"])) +
          theme_light()

        modelPlots$TrainROC <- plot1
        modelPlots$TestROC <- plot2

        plot <- plotThreshVsPerf(temp) +
          theme_light()

        modelPlots$Threshold <- plot
      }

    modelPlots$VarImp <- varplot

    model$modelPlots <- modelPlots
    trainedModels[[i]] <- model
  }

  ## Lift table
  actual <- as.numeric(paste0(test[,target]))
  if(length(actual) < liftGroup) liftGroup = length(actual)
  model_name <- as.character(paste0(results$Model))
  liftDF_cum <- calc_lift_cum (actual, predList, model_name, liftGroup)

 liftplot <- ggplot (data = liftDF_cum, aes (x = groups, y= lift, colour = model)) + geom_line () +
    theme (legend.position = "bottom") + scale_x_continuous(breaks = seq (0, 50, by =2)) +
    scale_y_continuous(breaks = seq (0, 100, by =2)) + ylab ("Lift") +
    ggtitle ("Cum Lift by Two Pctl - All")

  names(trainedModels) <- paste0(results$Model)

 # parallelStop()
  rm(list = setdiff(ls(), c("trainedModels","results","liftDF_cum","liftplot")))
  invisible(gc())

  return(list(trainedModels = trainedModels,
              results = results, lift = list('Lift_data' = liftDF_cum, 'Lift_plot' = liftplot)))
}

## To avoid the cran error checks
globalVariables(c(":=", ".", "Overall", "TP", "Group_by",
                  "groups", "lift", "sum1"))
