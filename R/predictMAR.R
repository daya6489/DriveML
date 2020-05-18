#' Extract predictions and MAR columns from autoMAR objects
#'
#' this function can be used for autoMAR objects to generate the variable for missing variable not completely at random
#'
#' @param x [autoMAR object  | Required] autoMAR object for which prediction is desired
#' @param data [data.frame | Required] prediction data set to prepare the autoMAR outcomes
#' @param mar_var [character list | Optional] list of predefined mar variables
#' @return flagged variables for missing not completely at random variable
#'
#' @examples
#' ## Missing at random features
#' train <- heart[1 : 199, ]
#' test <- heart[200 : 300, ]
#' marobj <- autoMAR (train, aucv = 0.9, strataname = NULL, stratasize = NULL, mar_method = "glm")
#'
#' ## print summary in console
#' testobj <- predictAutoMAR(marobj, test)
#' @importFrom data.table setDT
#' @export predictAutoMAR

predictAutoMAR <- function(x, data, mar_var = NULL){
  if (class(x) != "autoMAR") stop("Model object is not MAR output")
  if (!is.null(mar_var)) {
    mfetaure_n <- mar_var
  } else {
    mfetaure_n <- as.character(paste0(x$auc.features$Variable))
  }
  setDT(data)
  if (length(mfetaure_n) > 0){
    data[, (paste0("Y_", mfetaure_n)) := lapply(mfetaure_n, function(x)
      ifelse(is.na(get(x)), 1, 0))]
  } else {
    cat("Not found any MAR variables, please check you autoMAR object auc.features")
  }
  return(data)
}
