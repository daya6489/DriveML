#' Extract predictions and generate columns from autoDataprep objects
#'
#' this function can be used for autoDataprep objects to generate the same for validation
#'
#' @param x [autoDataprep object  | Required] autoDataprep object for which prediction is desired
#' @param data [data.frame | Required] prediction data set to prepare the MAR columns
#' @return master data set same as train data set
#' @examples
#' ## Sample train data set
#' train <- heart[1:200, ]
#' test <- heart[201:350, ]
#' traindata <- autoDataprep(train, target = "target_var", missimpute = "default",
#' dummyvar = TRUE, aucv = 0.02, corr = 0.98, outlier_flag = TRUE,
#' interaction_var = TRUE, frequent_var = TRUE)
#' train <- traindata$master
#'
#' ## Predict same features for test set
#' test <- predictDataprep(traindata, test)
#'
#' @importFrom data.table setDT
#' @export predictDataprep

predictDataprep <- function(x, data){
  if (class(x) != "autoDataprep") stop("Model object is not autoDataprep output")
  setDF(data)
  num_flag <- x$var_list$first_set_var$C
  num_var <- x$var_list$first_set_var$D
  final_dum_var <- x$var_list$first_set_var$E
  mar_variable <- x$var_list$first_set_var$F
  date_var <- x$var_list$first_set_var$G
  interaction_var <- x$var_list$first_set_var$H

  target <- x$call$target
  imp <- x$call$missimpute
  final_var_list <- x$var_list$final_var_list
  unique_var <- x$var_list$Unique_col
  dumvarnam <- x$var_list$Dummy_col

  mar_list_var <- unique(c(final_dum_var, num_var, target, date_var, unique_var, mar_variable))

  XX <- data[mar_list_var]
  if (ncol(XX) < 2) stop(cat(" Data preparation for ML model is incomplete due to less number of features", "\n", "Please check the input data / function parameters..."))
  setDT(XX)
  XX[, paste0(target) := ifelse(is.na(get(target)), 0, get(target))]

  invisible(lapply(names(XX), function(x) set(XX, which(is.infinite(XX[[x]])), j = x, value = NA)))
  invisible(lapply(names(XX), function(x) set(XX, which(is.nan(XX[[x]])), j = x, value = NA)))

  if (length(mar_variable) >= 1){
    setDT(XX)
    XX[, (paste0("Y_", mar_variable)) := lapply(mar_variable, function(x) ifelse(is.na(get(x)), 1, 0))]
    setDF(XX)
  }

  setDF(XX)

  if (imp == "default"){
    impdata <- impute(XX, classes = list(factor = imputeMode(),
                                     integer = imputeMean(),
                                     numeric = imputeMedian(),
                                     character = imputeMode()))
  } else {
    impdata <- impute(XX, classes = imp)
  }
  XX <- impdata$data

  if (anyNA(XX)){
    invisible(lapply(names(XX), function(x) set(XX, which(is.na(XX[[x]])), j = x, value = 0)))
  }


  # Adding outlier flag variables
  out_summary <- NULL
  out_var <- gsub("Out_flag_", "", final_var_list[grep("Out_flag", final_var_list)])
  out_cap <- gsub("Out_caping_", "", final_var_list[grep("Out_caping", final_var_list)])
  if ( (length(out_var) + length(out_cap)) > 0){

  out_summary <- generateFeature(XX, out_var, type = "Outlier", method = "flag")
  out_summary1 <- generateFeature(XX, out_cap, type = "Outlier", method = "caping")
  ninflag <- setdiff(names(out_summary), paste0("Out_flag_", out_var))
  nincap <- setdiff(names(out_summary1), paste0("Out_caping_", out_cap))

  if (length(ninflag) > 0) {
    setDT(out_summary)
    out_summary[, (ninflag) := 0]
  }

  if (length(nincap) > 0) {
    nincap1 <- gsub("Out_caping_", "", nincap)
    newdata <- XX[nincap1]
    names(newdata) <- nincap
    out_summary1 <- cbind(out_summary1, newdata)
  }

  if (ncol(out_summary) > 0) XX <- cbind(XX, out_summary);
  if (ncol(out_summary1) > 0) XX <- cbind(XX, out_summary1);
  }
  # Adding frequent variables
    frq_var <- gsub("Freq_t_", "", final_var_list[grep("Freq_t_", final_var_list)])
    if (length(frq_var) > 0) {
      freq_var_data <- generateFeature(XX, varlist = frq_var, type = "Frequent", method = "Frequency")
      if (ncol(freq_var_data) > 0) XX <- cbind(XX, freq_var_data)
      freq_variables <- names(freq_var_data)
    }
  # Adding Interactions variables
    inter_var <- interaction_var
    if (length(inter_var) > 0) {
      inter_var_data <- generateFeature(XX, varlist = inter_var, type = "Interaction", method = "multiply")
    if (ncol(inter_var_data) > 0) XX <- cbind(XX, inter_var_data)
    }

  # Dummy variables
      if (length(dumvarnam) > 0) {
        final_dum_var <- setdiff(final_dum_var, c(target, num_flag))
        final_dum_var <- final_dum_var[sapply(XX[final_dum_var], function(x) length(unique(x)) > 1)]
        sparsemtx <- generateFeature(XX, varlist = final_dum_var, type = "Dummy", method = NULL)
        if (ncol(sparsemtx) > 0) XX <- cbind(XX, sparsemtx)
      }

 addvar <- setdiff (final_var_list, names(XX))
 setDT(XX)
 if (length(addvar) > 0) {
   XX[, (addvar) := 0]
 }

 setDF(XX)
 fvar <- c(unique_var, target, final_var_list)
 var_not_found <- setdiff (fvar, names(XX))
if ( length(var_not_found) > 0) {
  cat("List of variable's are not found in prediction data set: ", paste(var_not_found))
} else {
  master_data <- XX[fvar]
  return(master_data)
}
}
