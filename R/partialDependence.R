#' Generate partial dependence plots
#'
#' Partial dependence plots (PDPs) help you to visualize the relationship between a subset of the features and the response while accounting for the average effect of the other predictors in the model. They are particularly effective with black box models like random forests, gradient boosting, etc.
#'
#' @param train [data.frame | Required] training sample used to train ML model
#' @param trainedModel [model object | Required] the object holding the machine learning model and the data
#' @param feature [character | Optional] the feature name for which to compute the effects
#' @param target [character | Optional] target variable name. Specify target variable if model object is other than MLR or driveML
#' @param sample [numeric | Optional] percentage of sample to be considered for training set for faster computation. Default of 0.5
#' @param modelname [character | Optional] specify whcih model to be plotted
#' @param seed [integer | Optional] random seed number. Default is 121
#'
#' @return List object containing a plot for each feature listed.
#' @seealso
#' \code{\link[iml:FeatureEffects]{FeatureEffects}}
#' \code{\link[mlr:plotPartialDependence]{plotPartialDependence}}
#' \code{\link[pdp:partial]{partial}}
#' @examples
#'
#' \donttest{
#' #' ## Example using DriveML model object
#' mymodel = heart.model
#' pdp_chol = autoPDP(heart, mymodel, feature = "chol", sample = 0.8, seed = 1234)
#'
#' # Type one MLR package
#' mod <- mlr::train(makeLearner("classif.ranger"), iris.task)
#' cc = autoPDP(iris, mod, feature = c("Sepal.Length","Sepal.Width","Petal.Length",
#'                                    "Petal.Width"), sample = 1, seed = 121)
#' # Type 2 DrvieML object
#' hearML <- autoMLmodel(heart,  target = "target_var",  testSplit = 0.2,
#' tuneIters = 10,  tuneType = "random",
#' models = "all", varImp = 20,  liftGroup = 50, positive = 1, seed = 1991)
#' cc = autoPDP(heart, hearML, feature = "chol", sample = 0.8, seed = 1234)
#'
#' cc1 = autoPDP(heart, trainedModel,target = "target_var", feature = "chol",
#' sample = 1, modelname = "logreg", seed = 121)
#'
#' # Type 3 other ML object
#' library(randomForest)
#' library(MASS)
#' rf = randomForest(medv ~ ., data = Boston, ntree = 50)
#' cc = autoPDP(Boston, rf,target = "medv", feature = "nox", sample = 1, seed = 121)
#' }
#' @importFrom iml Predictor FeatureEffect
#' @importFrom graphics plot
#' @export autoPDP

autoPDP <- function(train, trainedModel, target, feature,
                    sample = 0.5, modelname, seed = 1991){
  if (missing(train))    stop("Provide training set")
  if (missing(trainedModel)) stop("Provide trained ML model obj")
  if (missing(feature))stop("Provide feature name list which to compute effect")
  modelclass <- paste0(class(trainedModel), collapse = "_")
  if (modelclass == "autoMLmodel") {
    message("input model object is from DriveML")
    if (missing(modelname)) {
      results <- trainedModel$results
      setorder(trainedModel$results, -`Test AUC`)
      bmod <- paste0(results[1, ]$Model)
    } else {
      bmod <- modelname
      }
    trainedModel <- trainedModel$trainedModels[[bmod]]
    feats_list <- trainedModel$model$features
    varnapp <- setdiff(feature, feats_list)
    if (length(varnapp) > 0) stop ("Feature list are not there on model object")
    feats <- feature
    y <- trainedModel$model$task.desc$target
    trainedModel <- trainedModel$model
  } else
    if (modelclass == "WrappedModel") {
      message("input model object is from MLR")
      feats_list <- trainedModel$features
      varnapp <- setdiff(feature, feats_list)
      if (length(varnapp) > 0) stop("Feature list are not there on model object")
      feats <- feature
      y <- trainedModel$task.desc$target

    } else {
      message("input model object is from other ML pacakges")
            feats_list <- names(train)
            varnapp <- setdiff(feature, feats_list)
            if (length(varnapp) > 0) stop("Feature list are not there on model object")
            feats <- feature
            if (missing(target)) stop("please specify target variable name")
            y <- target
          }

  set.seed(seed)
  setDF(train)
  if (sample == 1) sample <- 0.99999
  ind <- sample.split(train[, y], SplitRatio = sample)
  temp <- train[ind == TRUE, ]
  suppressWarnings(predObj <- Predictor$new(model = trainedModel, data = temp[, feats_list], y = temp[, y]))
  plots <- list()
  for (i in 1:length(feats)){
    suppressWarnings(pd <- FeatureEffect$new(predObj, feature = feats[i], method = "pdp"))
    plots[[i]] <- plot(pd) +
            theme_bw() +
            ggtitle(paste0(feats[i], " Partial Dependence")) +
            geom_line(size = 1, col = "#3A48C5")
  }
 names(plots) <- feats
 return(list(plots = plots))
}
