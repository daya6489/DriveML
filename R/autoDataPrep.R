#' Automatic data preparation for ML algorithm
#'
#' Final data preparation before ML algorithm. Function provides final data set and highlights of the data preparation
#'
#' @param data [data.frame | Required] dataframe or data.table
#' @param target [integer | Required] dependent variable (binary or multiclass)
#' @param missimpute [text | Optional] missing value impuation. See more methods in details
#' @param auto_mar [character | Optional] identify any missing variable which are completely missing at random or not.(default FALSE). If TRUE this will call autoMAR()
#' @param mar_object [character | Optional] object created from autoMAR function
#' @param dummyvar [logical | Optional] categorical feature engineering i.e. one hot encoding (default is TRUE)
#' @param char_var_limit [integer | Optional] Defualt limit is 12 for a dummy variable preparation. Ex: if gender variable has two different value "M" and "F", then gender has 2 level
#' @param aucv [integer | Optional] cut off value for AUC based variable selection
#' @param corr [integer | Optional] cut off value for correaltion based variable selection
#' @param outlier_flag [logical | Optional] to add outlier features (default is False)
#' @param interaction_var [logical | Optional] bulk interactions transformer for numerical features
#' @param frequent_var [logical | Optional] Frequent tranformer for categorical features
#' @param uid [character | Optional] unique identifier column if any to keep in the final data set
#' @param onlykeep [character | Optional] only consider selected variables for data preparation
#' @param drop [character | Optional] exclude variable list from the data prepartion
#' @details
#' Missing imputation using impute function from mlr
#' \itemize{
#'   \item Defualt mean value for integer variable
#'   \item median value for numeric variable
#'   \item mode value for character or factor variable
#' }
#'
#' Feature engineering
#' \itemize{
#'   \item Missing not at random variable using autoMAR function
#'   \item Date transfomer like year, month, quarter, week
#'   \item Frequent transformer counts each categorical value in the dataset
#'   \item Intercation transformer using multiplication
#'   \item one hot dummy coding for categorical value
#'   \item outlier flag and capping variable for numerical value
#' }
#'
#' Feature reduction
#' \itemize{
#'   \item Zero variance using nearZeroVar caret function
#'   \item Pearson's Correlation value
#'   \item AUC with target variable
#' }
#' @return list output contains below objects
#'
#' \describe{
#'   \item{\code{complete_data}}{Complete data set including new novel features based on the functional understanding of the dataset}
#'   \item{\code{master_data}}{filtered data set based on the input parameter}
#'   \item{\code{final_var_list}}{list of master varaibles}
#'   \item{\code{auc_var}}{list of auc variables}
#'   \item{\code{cor_var}}{list of correlation variables}
#'   \item{\code{overall_var}}{all variables in the dataset}
#'   \item{\code{zerovariance}}{zero variance variables in the dataset}
#'}
#' @examples
#' #Auto data prep
#' traindata <- autoDataprep(heart, target = "target_var", missimpute = "default",
#' dummyvar = TRUE, aucv = 0.02, corr = 0.98, outlier_flag = TRUE,
#' interaction_var = TRUE, frequent_var = TRUE)
#'
#' # Print auto data prep object
#' printautoDataprep(traindata)
#'
#' @import data.table stats
#' @importFrom caret findCorrelation nearZeroVar dummyVars contr.ltfr
#' @importFrom sampling strata
#' @importFrom mlr impute imputeMode imputeMean imputeMedian imputeConstant
#' @seealso
#' \code{\link[mlr:impute]{impute}}
#' @export autoDataprep


autoDataprep <- function(data, target = NULL, missimpute = "default", auto_mar = FALSE, mar_object = NULL,
                         dummyvar = TRUE, char_var_limit = 12, aucv = 0.02, corr = 0.99,
                         outlier_flag = FALSE, interaction_var = FALSE, frequent_var = FALSE,
                         uid = NULL, onlykeep = NULL, drop = NULL)
{
  # Input data check
  if(!is.data.frame(data)) stop("Input data is not a data frame")
  if(is.null(target)) stop("target variable is missing")
  cl <- match.call()
  setDF(data)
  # if(!is.null(target))
  #   if(!sapply(data[target],class) %in% c("numeric", "integer")) stop("target should be numeric/integer. Ex: [0,1] or [1,2,3]")
  # Checking the number of unique values in the data set
  if(!is.null(onlykeep)) data <- subset(data, select = unique(c(onlykeep, uid)))
  # Adding MAR object
  mar_variable <- character()
  if(!is.null(mar_object) & isFALSE(auto_mar)) stop("IF mar_object is given then auto_mar option should be TRUE")
  if(isTRUE(auto_mar)){
    if(!is.null(mar_object)){
      if(class(mar_object) != "autoMAR")
        stop("mar object should be autoMAR output")
      mar_variable <- mar_object$auc.features$Variable
      cat("\n", "Started data preparation... ","\n\n")
    } else {
 ## Call MAR function with defualt input
      warning(cat(" there is no mar_object, started executing new autoMAR with default inputs", "\n", "This will take a while....", "\n"))
      xx <- subset(data, select = setdiff(names(data), c(uid, drop)))
      amrobj <- autoMAR(xx, aucv = 0.9, mar_method = "glm")
      mar_variable <- amrobj$auc.features$Variable
      cat("\n", "autoMAR is completed.", "\n\n", "Started data preparation... ","\n\n")
    }
  }

  cat("autoDataprep < feature bucket.... >","\n")
  names(data) <- gsub("[][!#$%()*,.:;<=>@^|~.{}]", "", names(data))
  all_names <- names(data)
  data <- data[,!duplicated(colnames(data))]
 # clean variables
  if(!is.null(drop)) data <- subset(data, select = setdiff(all_names, drop))
  num_var <- unique(names(data)[sapply(data, is.numeric)], names(data)[sapply(data, is.integer)])
  char_var <- names(data)[sapply(data, is.character)]
  factor_var <- names(data)[sapply(data, is.factor)]
  logical_var <- names(data)[sapply(data, is.logical)]
  date_var <- names(data)[unlist(lapply(data, function(x) class(x) == "Date" | class(x) == "POSIXct"))]
  unique_var <- character()
  if(!is.null(uid)){
    unique_var <- unique(c(unique_var, uid))
  }

 # Adding flag variable for date column
  date_flag <- character()
  if(length(date_var) > 0){
    invisible(lapply(date_var, function(x) set(data, which(is.na(data[[x]])), j = x, value = 0)))
    date_flag <- date_var[sapply(data[date_var], function(x) length(unique(x)) > 1 & length(unique(x)) <= 5)]
    if(length(date_flag) > 0){
      data[date_flag] <- lapply(data[date_flag], function(x) as.character(x))
    }
  }

  ch_fact_var <- c(factor_var, char_var, logical_var)
  factor_flag <- num_var_cnt <- num_var_drop <- num_flag <- character()
  if(length(ch_fact_var) > 0)
    factor_flag <- ch_fact_var[sapply(data[ch_fact_var], function(x) length(unique(x)) %in% c(2 : char_var_limit))]

  if(length(num_var) > 0){
    num_var_cnt <- sapply(data[num_var], function(x) {x[is.na(x)] <- 0; length(unique(x))})
    num_var_drop <- num_var[num_var_cnt == 1]
    num_flag <- num_var[num_var_cnt == 2 | num_var_cnt == 3]
    num_var <- num_var[num_var_cnt > 3]
    num_var <- setdiff(num_var, unique_var)
  }

  final_dum_var <- setdiff(unique(c(factor_flag, date_flag, num_flag)), c(unique_var, target))
  mar_list_var <- unique(c(setdiff(c(final_dum_var, num_var, target, date_var), unique_var), mar_variable))
  XX <- data[mar_list_var]
  #XX[factor_flag] <- lapply(XX[factor_flag], function(x) as.character(paste0(x)))

  if(ncol(XX) < 2) stop(cat(" Data preparation for ML model is incomplete due to less number of features", "\n", "Please check the input data / function parameters..."))

  cat("autoDataprep < missing imputation.... >","\n")
  invisible(lapply(names(XX), function(x) set(XX, which(is.infinite(XX[[x]])), j = x,value =NA)))
  invisible(lapply(names(XX), function(x) set(XX, which(is.nan(XX[[x]])), j = x,value =NA)))

  # invisible(lapply(num_flag, function(x) set(XX, which(is.na(XX[[x]])), j = x,value = 0)))
  # invisible(lapply(factor_flag, function(x) set(XX, which(XX[[x]] == ""), j = x, value = "Missing")))

  if(length(mar_variable) >= 1){
    setDT(XX)
    XX[, (paste0("Y_", mar_variable)) := lapply(mar_variable, function(x) ifelse(is.na(get(x)), 1, 0))]
    setDF(XX)
  }

  ## Missing imputation
  if(anyNA(XX)){
    if(missimpute == "default"){
      imp <- impute(XX, classes = list(factor = imputeMode(),
                                       integer = imputeMean(),
                                       numeric = imputeMedian(),
                                       character = imputeMode()))
    } else {
      imp <- impute(XX, classes = missimpute)
    }
    XX <- imp$data
    if(anyNA(XX)){
      invisible(lapply(names(XX), function(x) set(XX, which(is.na(XX[[x]])), j = x,value = 0)))
    }
  }

# Adding outlier flag variables
  out_summary <- date_summary <- out_summary1 <- dumvarnam <- NULL
  if(isTRUE(outlier_flag)) {
    cat("autoDataprep < Outlier treatment based on Tukey method....>","\n")
    out_summary <- generateFeature(XX, num_var, type = "Outlier", method = 'flag')
    out_summary1 <- generateFeature(XX, num_var, type = "Outlier", method = 'caping')
    if(ncol(out_summary) > 0) XX <- cbind(XX, out_summary);
    if(ncol(out_summary1) > 0) XX <- cbind(XX, out_summary1);
    }
  out_flag_var <-unique(c(names(out_summary), names(out_summary1)))

# Adding Date variables
  date_trans_var <- inter_variables <- freq_variables <- freq_vklist <- character()
  if(length(date_var)>0) {
    cat("autoDataprep < Date transformer....>","\n")
    date_summary <- generateFeature(XX, date_var, type = "date")
    date_trans_var <- names(date_summary)
    if(length(date_trans_var)>0) XX <- cbind(XX, date_summary)
    rm(date_summary)
  }

# Adding frequent variables
  if(isTRUE(frequent_var)) {
    if(length(factor_flag) > 0){
      cat("autoDataprep < Frequent tranformer....>","\n")
      freq_vklist <- factor_flag
      if(length(freq_vklist) > 10) freq_vklist <- factor_flag[sample(1:length(freq_vklist), 10)]
      freq_var_data <- generateFeature(XX, varlist = freq_vklist, type = "Frequent", method = "Frequency")
      if(ncol(freq_var_data) > 0) XX <- cbind(XX, freq_var_data)
      freq_variables <- names(freq_var_data)
    }
  }

# Adding Interactions variables
  if(isTRUE(interaction_var)) {
    if(length(num_var) > 0){
      cat("autoDataprep < Interactions tranformer....>","\n")
      interaction_var <- num_var
      if(length(num_var) > 10) interaction_var <- num_var[sample(1:length(num_var),10)]
      inter_var_data <- generateFeature(XX, varlist = interaction_var, type = "Interaction", method = "multiply")
      inter_variables <- names(inter_var_data)
      if(length(inter_variables) > 0 ) XX <- cbind(XX, inter_var_data)
    }
  }

  varbucket <- list("A" = factor_flag, "B" = date_flag, "C" = num_flag, "D" = num_var, "E" = final_dum_var, "F" = mar_variable, "G" = date_var, "H" = interaction_var, "I" = freq_vklist)

# Dummy variables
  if(isTRUE(dummyvar)){
    cat("autoDataprep < Categorical variable - one hot encoding....>","\n")
    final_dum_var <- setdiff(final_dum_var, c(target, num_flag))
    if(length(final_dum_var) > 0){
      sparseMtx <- generateFeature(XX, varlist = final_dum_var, type = "Dummy", method = NULL)
      XX <- cbind(XX, sparseMtx)
      dumvarnam <- colnames(sparseMtx, num_flag)
    }
  }

  all_variable <- setdiff(unique(c(num_var, dumvarnam, out_flag_var, freq_variables, inter_variables, date_trans_var)), c(unique_var, target))
  drop_var <- setdiff(all_names, c(unique_var, num_var, factor_flag, final_dum_var, date_var))
  if(length(mar_variable) >= 1)
    all_variable <- setdiff(unique(c(num_var, dumvarnam, freq_variables, inter_variables, date_trans_var, out_flag_var, paste0("Y_", mar_variable))), c(unique_var, target))

  cat("autoDataprep < variable selection - zero variance method.... >","\n")
  # Variable selections
  # Identify Zero variance variables
  master_variable <- all_variable
  zeroNms <- myNZV <- character()
  mynms <- c(out_flag_var, dumvarnam, date_trans_var)
  if(length(mynms) > 0) {
    myNZV <- nearZeroVar (XX[mynms], saveMetrics = TRUE)
    zeroNms <- c(mynms[myNZV$zeroVar], num_var_drop)
    master_variable <- setdiff(master_variable, zeroNms)
  }

  cat("autoDataprep < variable selection - pearson correlation method.... >","\n")
  # Identify correlated variables
  corNms <- character()
  myDF <- NULL
  mynms <- c(out_flag_var, inter_variables, num_var, date_trans_var)
  mynms <- setdiff(mynms, zeroNms)
  if(length(mynms) > 1){
    myDF <- cor (XX[mynms], use="pairwise.complete.obs")
    myDF <- as.matrix (myDF)
    myDF <- ifelse (is.na(myDF), 0, myDF)
    myCor <- findCorrelation (myDF, cutoff = corr)
    corNms <- mynms[myCor]
  }

# AUC based variable selections - AUC for binary classification
  {
    master_variable <- setdiff(master_variable, corNms)
  varSel_auc <- character()
  cat("autoDataprep < variable selection - AUC method.... >","\n")
    targetv <- XX[target][,1]
    targetv <- ifelse(is.na(targetv), 0, targetv)
    varSel_summary <- calc_auc_df(XX, targetv, master_variable, impvalue = aucv)
    varSel_auc <- varSel_summary[[1]]
    varSel_auc1 <-  varSel_summary[[2]]
    low_auc_var <- setdiff(master_variable, varSel_auc)
    master_variable <- varSel_auc
    }

  mydata <- cbind(data[c(unique_var, target)] , XX)
  mydata <- mydata[,!duplicated(colnames(mydata))]
  cl[[1]] <- as.name("autoDataprep")
  fit <- list(call = cl,
              final_var_list = master_variable,
              overall_variable = all_variable,
              complete_data = mydata,
              master_data = mydata[unique(c(unique_var, target, master_variable))],
              auc_var = varSel_auc,
              cor_var = corNms,
              zerovariance = zeroNms,
              outlier_summary = out_flag_var,
              raw_list = list("Zero_variance" = myNZV, "Num_Correlation" = myDF, "AUC_value" = varSel_auc1),
              var_list = list("All_columns" = all_names, "Numeric_col" = num_var, "Character_col"= char_var,  "Factor_col"= factor_var,
                              "Logical_col" = logical_var, "Date_col" = date_var, "Unique_col" = unique_var, "MAR_col" = mar_variable,
                              "Dummy_col" = dumvarnam, "Dropped_col" = drop_var, "Low_auc_col" = low_auc_var,
                              "first_set_var" = varbucket))

  cat("autoDataprep < Data preparation is completed >","\n")
  class(fit) <- "autoDataprep"
  rm(list = setdiff(ls(), c("fit")))
  invisible(gc())
  return(fit)
}
