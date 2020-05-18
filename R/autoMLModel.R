#' Automated machine learning training of models
#'
#' Automated training, tuning and validation of machine learning models. Models are tuned, resampled and validated on an experimental dataset and trained on the full dataset and validated/tested on external datasets. Classification models tune the probability threshold automatically and returns the results. Each model contains information on performance, model object and evaluation plots.
#'
#' @param train [data.frame | Required] training set
#' @param test [data.frame | Optional] optional testing set to validate models on. If none is provided, one will be created internally. Default of NULL
#' @param score [data.frame | Optional] optional score the models on best trained model based on AUC. If none is provided, scorelist will be null. Default of NULL
#' @param target [integer | Required] if a target is provided classification or regression models will be trained, if left as NULL unsupervised models will be trained. Default of NULL
#' @param testSplit [numeric | Optional] percentage of data to allocate to the test set. Stratified sampling is done. Default of 0.1
#' @param tuneIters [integer | Optional] number of tuning iterations to search for optimal hyper parameters. Default of 10
#' @param tuneType [character | Optional] tune method applied, list of options are:
#' \itemize{
#' \item "random" - random search hyperparameter tuning
#' \item "frace" - frace uses iterated f-racing algorithm for the best solution from irace package
#' }
#' @param models [character | Optional] which models to train. Default option is all. Please find below the names for each of the methods
#' \itemize{
#' \item {randomForest}{ - random forests using the randomForest package}
#' \item {ranger}{ - random forests using the ranger package}
#' \item {xgboost}{ - gradient boosting using xgboost}
#' \item {rpart}{ - decision tree classification using rpart}
#' \item {glmnet}{ - regularised regression  from glmnet}
#' \item {logreg}{ - logistic regression from stats}
#' }
#' @param perMetric [character | Optional] model validation metric. Default is "auc"
#' \itemize{
#' \item {auc} - area under the curve; mlr::auc
#' \item {accuracy} - accuracy; mlr::acc
#' \item {balancedAccuracy} - balanced accuracy; mlr::bac
#' \item {brier} - brier score; mlr::brier
#' \item {f1} - F1 measure; mlr::f1
#' \item {meanPrecRecall} - geometric mean of precision and recall; mlr::gpr
#' \item {logloss} - logarithmic loss; mlr:logloss
#' }
#' @param varImp [integer | Optional] number of important features to plot
#' @param liftGroup [integer | Optional] lift value to validate the test model performance
#' @param maxObs [numeric | Optional] number of observations in the experiment training dataset on which models are trained, tuned and resampled. Default of 40,000. If the training dataset has less than 40k observations then all the observations will be used
#' @param uid [character | Optional] unique variables to keep in test output data
#' @param pdp [logical | Optional] partial dependence plot for important variables
#' @param positive [character | Optional] positive class for the target variable
#' @param htmlreport [logical | Optional] to view the model outcome in html format
#' @param seed [integer | Optional] random number seed for reproducible results
#' @param verbose [logical | Optional] display executions steps on console. Default is FALSE
#' @details
#' all the models trained using mlr train function, all of the functionality in mlr package can be applied to the autoMLmodel outcome
#'
#' autoMLmodel provides below the information of the various machine learning classification models
#' \itemize{
#' \item{trainedModels}{ - model level list output contains trained model object, hyper parameters, tuned data, test data, performance and Model plots}
#' \item{results}{ - summary of all trained model result like AUC, Precision, Recall, F1 score}
#' \item{modelexp}{ - model gain chart}
#' \item{predicted_score}{ - predicted score}
#' \item{datasummary}{ - summary of the input data}
#' }
#'
#' @return List output contains trained models and results
#' @examples
#' \donttest{
#' # Run only Logistic regression model
#' mymodel <- autoMLmodel( train = heart, test = NULL, target = 'target_var',
#' testSplit = 0.2, tuneIters = 10, tuneType = "random", models = "logreg",
#' varImp = 10, liftGroup = 50, maxObs = 4000, uid = NULL, seed = 1991)
#'}
#' @import ggplot2
#' @importFrom mlr train performance makeClassifTask makeTuneControlGrid makeTuneControlRandom makeTuneControlIrace makeResampleDesc tuneParams setHyperPars generateHyperParsEffectData getFeatureImportance resample plotCalibration generateCalibrationData generateThreshVsPerfData fpr tpr acc calculateROCMeasures plotROCCurves plotThreshVsPerf makeLearners auc ppv f1 tpr acc
#' @importFrom ParamHelpers makeParamSet makeIntegerParam makeNumericParam makeDiscreteParam makeNumericLearnerParam makeLogicalLearnerParam makeDiscreteLearnerParam makeIntegerLearnerParam makeIntegerVectorLearnerParam
#' @importFrom caTools sample.split
#' @importFrom SmartEDA ExpCatViz
#' @seealso
#' \code{\link[mlr:train]{mlr train}}
#' \code{\link[caret:train]{caret train}}
#' \code{\link[mlr:makeLearner]{makeLearner}}
#' \code{\link[mlr:tuneParams]{tuneParams}}
#' @export autoMLmodel

autoMLmodel <- function(train,  test = NULL,  score = NULL, target = NULL,
                        testSplit = 0.2, tuneIters = 10,  tuneType = "random",  models = "all",
                        perMetric = "auc", varImp = 10,  liftGroup = 50,  maxObs = 10000,  uid = NULL,
                        pdp = FALSE, positive = 1, htmlreport = FALSE, seed = 1991, verbose = FALSE){
  set.seed(seed, "L'Ecuyer")
  if (missing(train)) stop ("No training data provided")
  if (is.null(target)) stop ("Target variable is missing")
  cl <- match.call()
  setDF(train)
  if (is.null(test)){
    ind <- sample.split(train[, target], SplitRatio = (1 - testSplit))
    test <- train[ind == FALSE, ]
    train <- train[ind == TRUE, ]
  }
  if (!is.null(uid)) {
    train <- subset(train, select = setdiff(names(train), uid))
  }
  datstrain <- datstest <- datsscore <- NULL
  datstrain <- ExpData(train)
  datstest <- ExpData(test)
  if (is.null(score) == FALSE){
    if (is.null(uid)) stop ("Define unique identifier for scoring")
    names(train) %in% names(score)
    nvar <- setdiff(names(train), names(score))
    datsscore <- ExpData(score)
    if (length(nvar) > 0) stop (cat("List of variables are not there in score data set are: ", "\n", paste0(nvar, "\n")))
  }
  auo <- autoSample(x = train, y = target, seed = seed, maxObs = maxObs)
  fullTasks <- auoTasks <- list()
  auoTasks$fullTask <- generateTask(x = auo, y = target, positive = positive, maxLevels = 100)
  fullTasks$fullTask <- generateTask(x = train, y = target, positive = positive, maxLevels = 100)
  learners <- suppressWarnings(generateLearners(task = auoTasks$fullTask))
  params <- generateHyperParams(task = auoTasks$fullTask)
  metrics <- generateMetrics(task = auoTasks$fullTask)
  metric <- metrics[[which(tolower(names(metrics)) == tolower(perMetric))]]
  if (tuneType == "random"){
    tune <-  makeTuneControlRandom(maxit = tuneIters, tune.threshold = TRUE)
  } else if (tuneType == "frace") {
    tune <- makeTuneControlIrace(maxExperiments = tuneIters, tune.threshold = TRUE)
  }
  validation <- makeResampleDesc(method = "Holdout", stratify = TRUE, split = 0.3)
  names(auoTasks) <- gsub("Task", "", names(auoTasks))
  names(fullTasks) <- gsub("Task", "", names(fullTasks))
  auoTasks[sapply(auoTasks, is.null)] <- NULL
  fullTasks[sapply(fullTasks, is.null)] <- NULL
  resultsa <- expand.grid(Model = names(learners), `Fitting time` = NA,
                          `Scoring time` = NA, `Train AUC` = NA,
                          `Test AUC` = NA, `Accuracy` = NA,
                          `Precision` = NA, `Recall` = NA, `F1_score` = NA)
  models_lp <- "all"
  modelchk <- models_lp[models_lp %in% models]
  results <- resultsa
  if (length(modelchk) == 0L) results <- subset(resultsa, tolower(resultsa$Model) %in% tolower(models))
  trainedModels <- predList <- scoreList <- top_var <- pdplot <- list ()
  for (i in 1 : nrow(results)){
    set.seed(seed, "L'Ecuyer")
    invisible(gc(verbose = FALSE))
    model_st <- Sys.time()
    model <- list()
    tuneTask <- auoTasks[[which(names(auoTasks) == "full")]]$task
    trainTask <- fullTasks[[which(names(fullTasks) == "full")]]$task
    modName <- as.character(results[i, "Model"])
    mod <- learners[[which(names(learners) == modName)]]
    modelPlots <- list()
    if(verbose) cat(modName, "Model tuning started....", "\n")
    ps <- params[[which(tolower(names(params)) == tolower(as.character(modName)))]]
    tuned <- suppressMessages(tuneParams(task = tuneTask, resampling = validation,
                                         control = tune, par.set = ps,
                                         measures = metric, learner = mod,
                                         show.info = FALSE))
    mod <- setHyperPars(mod, par.vals = tuned$x)
    model$model <- train(learner = mod, task = trainTask)
    model$tuneData <- generateHyperParsEffectData(tuned, partial.dep = TRUE)
    model$HyperPara <- mod
    tt1 <- round(Sys.time() - model_st, 3)
    results[i, "Fitting time"] <- paste0(tt1, " ", units(tt1))
    ind_var <- model$model$features
    varimportance1 <- betaml(model$model, modName)
    model$var_coef_imp <- varimportance1
    if (modName %in% c("xgboost", "randomForest", "ranger", "rpart")) {
      if (varImp > nrow(varimportance1)) varImp <- nrow(varimportance1)
      top_v <- as.character(varimportance1[1 : varImp, ]$variable)
      varplot <- ExpCatViz(data = varimportance1[1 : varImp, ], target = "variable", col = "lightblue", rdata = TRUE, value = "importance", Flip = T, gtitle = paste0(modName, " - Variable importance : Top ", varImp) )
    } else {
      if (varImp > length(ind_var)) varImp <- length(ind_var)
      top_v <- ind_var[sample(1 : length(ind_var), varImp)]
      varplot <- ExpCatViz(data = varimportance1[1 : varImp, ], target = "Variable", col = "pink", rdata = TRUE, value = "coefficients", Flip = T, gtitle = paste0(modName, " - Variable coefficients") )
    }
    model_st <- Sys.time()
    p.train <- predict(model$model, newdata = train[, model$model$features])
    p.train$data$truth <- train[, target]
    p.test <- predict(model$model, newdata = test[, model$model$features])
    p.test$data$truth <- test[, target]
    predList [[i]] <- p.test$data$prob.1
    names(predList)[i] <- modName
    if (is.null(score) == FALSE) {
      p.score <- predict(model$model, newdata = score[, model$model$features])
      p.score$data$truth <- score[, target]
      scoreList [[i]] <- p.score$data$prob.1
      names(scoreList)[i] <- modName
    }
    tt <- round(Sys.time() - model_st, 3)
    results[i, "Scoring time"] <- paste0(tt, " ", units(tt))
    results[i, "Train AUC"] <- round(mlr::performance(pred = p.train, task = trainTask, measures = mlr::auc)[[1]], 3)
    results[i, "Test AUC"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::auc)[[1]], 3)
    results[i, "Accuracy"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::acc)[[1]], 3)
    results[i, "Precision"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::ppv)[[1]], 3)
    results[i, "Recall"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::tpr)[[1]], 3)
    results[i, "F1_score"] <- round(mlr::performance(pred = p.test, task = trainTask, measures = mlr::f1)[[1]], 3)
    model$performance <- results[i, ]
    model$probCutoff <- tuned$threshold
    if (length(unique(train[, target])) == 2) {
      temp <- generateThreshVsPerfData(p.train, measures = list(fpr, tpr, acc))
      temp2 <- generateThreshVsPerfData(p.test, measures = list(fpr, tpr, acc))
      plot1 <- plotROCCurves(temp) + ggtitle(paste0("Train ROC Curve: ", modName)) +
        annotate("text", x = .75, y = .25, label = paste("AUC =", results[i, "Train AUC"])) + theme_light()
      plot2 <- plotROCCurves(temp2) + ggtitle(paste0("Test ROC Curve: ", modName)) +
        annotate("text", x = .75, y = .25, label = paste("AUC =", results[i, "Test AUC"])) + theme_light()
      modelPlots$TrainROC <- plot1
      modelPlots$TestROC <- plot2
      plot <- plotThreshVsPerf(temp) + theme_light()
      modelPlots$Threshold <- plot
    }
    modelPlots$VarImp <- varplot
    model$modelPlots <- modelPlots
    trainedModels[[i]] <- model
    top_var[[i]] <- top_v
    if(verbose) cat ("autoMLmodel < All features", modName, "tuned and trained > \n")
  }
  names(trainedModels) <- paste0(results$Model)
  names(top_var) <- paste0(results$Model)
  if (pdp){
    sv <- 0.1
    if (nrow(train) < maxObs) sv <- 0.5
    pdplot <- pdplot(train, trainedModels, feat = top_var, results = results, seed = seed, y = target, sample = sv)
  }
  if (is.null(score)) scoreList <- NULL
  stackdata <- stackdataprep(test, score, validlist = predList, scorelist = scoreList, target = target, uid = uid)
  actual <- as.numeric(paste0(test[, target]))
  if (length(actual) < liftGroup) liftGroup <- length(actual)
  model_name <- as.character(paste0(results$Model))
  liftDF_cum <- calc_lift_cum (actual, predList, model_name, liftGroup)
  liftplot <- ggplot (data = liftDF_cum, aes (x = groups, y = lift, colour = model)) + geom_line () +
    theme (legend.position = "bottom") + scale_x_continuous(breaks = seq (0, 50, by = 2)) +
    scale_y_continuous(breaks = seq (0, 100, by = 2)) + ylab ("Lift") +
    ggtitle ("Cum Lift by Two Pctl - All")
  cl[[1]] <- as.name("autoMLmodel")
  fit <- list(call = cl,
              trainedModels = trainedModels,
              results = results,
              modelexp = list("Lift_data" = liftDF_cum, "Lift_plot" = liftplot, "pdp" = pdplot),
              predicted_score = stackdata,
              datasummary = list("train" = datstrain, "test" = datstest, "score" = datsscore))
  class(fit) <- "autoMLmodel"
  if (htmlreport == TRUE) {
    autoMLReport(fit, op_file = "drive_ml_model_report.html", op_dir = getwd())
  }
  return(fit)
}

## To avoid the cran error checks
globalVariables(c(":=", ".", "Overall", "TP", "Group_by", "groups",
                  "lift", "sum1", "Test AUC", "Probability", "Relative_prob"))
