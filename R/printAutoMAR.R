#' Print Method for the autoMAR Class
#' Print the result of autoMAR object
#'
#' @param x [Object | Required] an object of class autoMAR
#' @export printautoMAR


printautoMAR <- function(x) {
  cat("MissingAtRandom result\n\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat("Sample size:                                ", x$num.samples, "\n")
  cat("Number of variables:                        ", x$num.variables, "\n")
  cat("Number of missing variables:                ", x$num.miss.variables, "\n","\n")
  cat("Number of not missing at random variables:  ", x$num.nmar.variables, "\n","\n")
  cat("List of features are not missing at random:  ","\n")
  cat("-------------------------------------------","\n")
  if (!is.null(x$auc.features)) {
    cat(paste0(paste(x$auc.features$Variable,round(x$auc.features$AUC,2),sep = " : "),"\n"))
  }
}
