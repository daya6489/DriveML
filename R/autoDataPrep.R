#' Automatic data preparation for ML algorithms
#'
#' Final data preparation before ML algorithms. Function provides final data set and highlights of the data preparation
#'
#' @param data [data.frame | Required] dataframe or data.table
#' @param target [integer | Required] dependent variable (binary or multiclass)
#' @param missimpute [text | Optional] missing value impuation using mlr misimpute function. Please refer to the "details" section to know more
#' @param auto_mar [character | Optional] identify any missing variable which are completely missing at random or not (default FALSE). If TRUE this will call autoMAR()
#' @param mar_object [character | Optional] object created from autoMAR function
#' @param dummyvar [logical | Optional] categorical feature engineering i.e. one hot encoding (default is TRUE)
#' @param char_var_limit [integer | Optional] default limit is 12 for a dummy variable preparation. e.g. if gender variable has two different value "M" and "F", then gender has 2 levels
#' @param aucv [integer | Optional] cut off value for AUC based variable selection
#' @param corr [integer | Optional] cut off value for correlation based variable selection
#' @param outlier_flag [logical | Optional] to add outlier features (default is FALSE)
#' @param interaction_var [logical | Optional] bulk interactions transformer for numerical features
#' @param frequent_var [logical | Optional] frequent transformer for categorical features
#' @param uid [character | Optional] unique identifier column if any to keep in the final data set
#' @param onlykeep [character | Optional] only consider selected variables for data preparation
#' @param drop [character | Optional] exclude variables from the dataset
#' @param verbose [logical | Optional] display executions steps on console(default is FALSE)
#' @details
#'
#' Missing imputation using impute function from MLR
#'
#' MLR package have a appropriate way to impute missing value using multiple methods.
#' #' \itemize{
#'   \item mean value for integer variable
#'   \item median value for numeric variable
#'   \item mode value for character or factor variable
#' }
#'optional: You might be interested to impute missing variable using ML method. List of algorithms will be handle missing variables in MLR package
#' listLearners("classif", check.packages = TRUE, properties = "missings")[c("class", "package")]
#'
#' Feature engineering
#' \itemize{
#'   \item missing not completely at random variable using autoMAR function
#'   \item date transfomer like year, month, quarter, week
#'   \item frequent transformer counts each categorical value in the dataset
#'   \item interaction transformer using multiplication
#'   \item one hot dummy coding for categorical value
#'   \item outlier flag and capping variable for numerical value
#' }
#'
#' Feature reduction
#' \itemize{
#'   \item zero variance using nearZeroVar caret function
#'   \item pearson's correlation value
#'   \item auc with target variable
#' }
#' @return list output contains below objects
#'
#' \describe{
#'   \item{\code{complete_data}}{complete dataset including new derived features based on the functional understanding of the dataset}
#'   \item{\code{master_data}}{filtered dataset based on the input parameters}
#'   \item{\code{final_var_list}}{list of master variables}
#'   \item{\code{auc_var}}{list of auc variables}
#'   \item{\code{cor_var}}{list of correlation variables}
#'   \item{\code{overall_var}}{all variables in the dataset}
#'   \item{\code{zerovariance}}{variables with zero variance in the dataset}
#'}
#' @examples
#' #Auto data prep
#' traindata <- autoDataprep(heart, target = "target_var", missimpute = "default",
#' dummyvar = TRUE, aucv = 0.02, corr = 0.98, outlier_flag = TRUE,
#' interaction_var = TRUE, frequent_var = TRUE)
#' train <- traindata$master
#' @import data.table stats
#' @importFrom sampling strata
#' @importFrom SmartEDA ExpData
#' @importFrom mlr impute imputeMode imputeMean imputeMedian imputeConstant imputeLearner makeLearner configureMlr removeConstantFeatures
#' @seealso
#' \code{\link[mlr:impute]{impute}}
#' @export

autoDataprep <- function(data, target = NULL, missimpute = "default",
auto_mar = FALSE, mar_object = NULL, dummyvar = TRUE, char_var_limit = 12,
aucv = 0.02, corr = 0.99, outlier_flag = FALSE, interaction_var = FALSE,
frequent_var = FALSE, uid = NULL, onlykeep = NULL, drop = NULL, verbose = FALSE){
  if (!is.data.frame(data)) stop ("Input data is not a data frame" )
  if (is.null(target)) stop ("target variable is missing" )
  cl <- match.call()
  setDT(data)
  data[, paste0(target) := ifelse(is.na( get(target)), 0, get(target))]
  setDF(data)
  datstrain <- ExpData(data, type = 1)
  datstrain1 <- ExpData(data, type = 2)
  if (!is.null( onlykeep)) data <- data[ unique( c( onlykeep, uid))]

  mar_variable <- character()
  if (!is.null(mar_object) & isFALSE(auto_mar)) {
    stop ( "IF mar_object is given then auto_mar option should be TRUE" )
  }
  if (auto_mar == TRUE) {
    if (!is.null(mar_object)){
      if (class(mar_object) != "autoMAR") stop ("mar object should be autoMAR output")
      mar_variable <- mar_object$auc_features$Variable
    } else {
          xx <- subset(data, select = setdiff(names(data), c(uid, drop)))
          amrobj <- autoMAR(xx, aucv = 0.9, mar_method = "glm")
          mar_variable <- amrobj$auc_features$Variable
        }
    }
  names(data) <- trimws( gsub("[][!#$%()*?,.:;<=>@^|~.{}]", "", names(data)))
  all_names <- names(data)
  data <- data[, !duplicated(colnames(data))]
  if (!is.null(drop)) {
    data <- subset(data, select = setdiff(all_names, drop))
  }
  num_var <- unique(names(data)[sapply(data, is.numeric)],
                    names(data)[sapply(data, is.integer)])
  char_var <- names(data)[sapply(data, is.character)]
  factor_var <- names(data)[sapply(data, is.factor)]
  logical_var <- names(data)[sapply(data, is.logical)]
  date_var <- names(data)[unlist(lapply(data, function(x)
    class(x) == "Date" | class(x) == "POSIXct" ))]
  date_flag <- unique_var <- character()
  if (!is.null(uid)){
    unique_var <- unique(c(unique_var, uid))
  }
  if (length(date_var) > 0){
    invisible(lapply(date_var, function(x) set(data, which(is.na(data[[x]])), j = x, value = 0)))
    date_flag <- date_var[sapply(data[date_var], function(x) length(unique(x)) > 1 & length(unique(x)) <= 5)]
    if (length(date_flag) > 0){
      data[date_flag] <- lapply(data[date_flag], function(x) as.character(x))
    }
  }
  ch_fact_var <- c(factor_var, char_var, logical_var)
  factor_flag <- num_var_cnt <- num_flag <- character()
  if (length(ch_fact_var) > 0) {
    factor_flag <- ch_fact_var[sapply(data[ch_fact_var], function(x) length(unique(x)) %in% c(2 : char_var_limit))]
  }

  if (length(num_var) > 0){
    num_var_cnt <- sapply(data[num_var], function(x){
      x[is.na(x)] <- 0
      length(unique(x))
      })
    num_flag <- num_var[num_var_cnt == 2 | num_var_cnt == 3]
    num_var <- num_var[num_var_cnt > 3]
    num_var <- setdiff(num_var, unique_var)
  }

  final_dum_var <- setdiff(unique(c(factor_flag, date_flag, num_flag)), c(unique_var, target))
  mar_list_var <- unique(c(setdiff(c(final_dum_var, num_var, target, date_var), unique_var), mar_variable))
  XX <- data[mar_list_var]
  if (ncol(XX) < 2) stop (cat(" Data preparation for ML model is incomplete due to less number of features", "\n", "Please check the input data / function parameters..."))
  invisible(lapply(names(XX), function(x) set(XX, which(is.infinite(XX[[x]])), j = x, value = NA)))
  invisible(lapply(names(XX), function(x) set(XX, which(is.nan(XX[[x]])), j = x, value = NA)))

  if (length(mar_variable) >= 1){
    if (verbose == TRUE) cat("autoDataprep < MAR variable computation.... >", "\n")
    setDT(XX)
    XX[, (paste0("Y_", mar_variable)) := lapply(mar_variable, function(x) ifelse(is.na(get(x)), 1, 0))]
    setDF(XX)
  }

  if (anyNA(XX)){
    if (verbose == TRUE) cat("autoDataprep < missing imputation.... >", "\n")
    mis_imp_df <- "default"
    mis_chk <- mis_imp_df[mis_imp_df %in% missimpute]
    if (length(mis_chk) > 0) {
      imp <- impute(XX, classes = list(factor = imputeMode(), integer = imputeMean(),
                                       numeric = imputeMedian(),
                                       character = imputeConstant(const = 99999)))
    } else {
      lenimp <- length(missimpute)
      if (lenimp == 1) imp <- impute(XX, classes = missimpute$classes)
      if (lenimp == 2) imp <- impute(XX, target = target, classes = missimpute$classes)
    }
    XX <- imp$data
  }

  out_summary <- date_summary <- out_summary1 <- dumvarnam <- NULL
  if (outlier_flag == TRUE) {
    if (verbose == TRUE) cat("autoDataprep < Outlier treatment based on Tukey method....>", "\n")
    out_summary <- generateFeature(XX, num_var, type = "Outlier", method = "flag")
    out_summary1 <- generateFeature(XX, num_var, type = "Outlier", method = "caping")
    if (ncol(out_summary) > 0) XX <- cbind(XX, out_summary)
    if (ncol(out_summary1) > 0) XX <- cbind(XX, out_summary1)
    }
  out_flag_var <- unique(c(names(out_summary), names(out_summary1)))
  date_trans_var <- inter_variables <- freq_variables <- freq_vklist <- character()
  if (length(date_var) > 0) {
    if (verbose == TRUE) cat("autoDataprep < Date transformer....>", "\n")
    date_summary <- generateFeature(XX, date_var, type = "date")
    date_trans_var <- names(date_summary)
    if (length(date_trans_var) > 0) XX <- cbind(XX, date_summary)
    rm(date_summary)
  }
  if (frequent_var == TRUE) {
    freq_vklist <- setdiff(factor_flag, c(target, num_flag))
    if (length(freq_vklist) > 0){
      if (verbose == TRUE) cat("autoDataprep < Frequent transformer....>", "\n")
      if (length(freq_vklist) > 10) freq_vklist <- freq_vklist[sample(1:length(freq_vklist), 10)]
      freq_var_data <- generateFeature(XX, varlist = freq_vklist, type = "Frequent", method = "Frequency")
      if (ncol(freq_var_data) > 0) XX <- cbind(XX, freq_var_data)
      freq_variables <- names(freq_var_data)
    }
  }
  if (interaction_var == TRUE) {
    if (length(num_var) > 1){
      if (verbose == TRUE) cat("autoDataprep < Interactions transformer....>", "\n")
      interaction_var <- num_var
      if (length(num_var) > 10) interaction_var <- num_var[sample(1 : length(num_var), 10)]
      inter_var_data <- generateFeature(XX, varlist = interaction_var, type = "Interaction", method = "multiply")
      inter_variables <- names(inter_var_data)
      if (length(inter_variables) > 0 ) XX <- cbind(XX, inter_var_data)
    }
  }
  varbucket <- list("A" = factor_flag, "B" = date_flag, "C" = num_flag, "D" = num_var, "E" = final_dum_var, "F" = mar_variable, "G" = date_var, "H" = interaction_var, "I" = freq_vklist)
  if (dummyvar == TRUE){
    if (verbose == TRUE) cat("autoDataprep < Categorical variable - one hot encoding....>", "\n")
    final_dum_var <- setdiff(final_dum_var, c(target, num_flag))
    if (length(final_dum_var) > 0){
      sparseMtx <- generateFeature(XX, varlist = final_dum_var, type = "Dummy", method = NULL)
      XX <- cbind(XX, sparseMtx)
      dumvarnam <- c(colnames(sparseMtx), num_flag)
    }
  }

  all_variable <- setdiff(unique(c(num_var, dumvarnam, out_flag_var, freq_variables, inter_variables, date_trans_var)), c(unique_var, target))
  drop_var <- setdiff(all_names, c(unique_var, num_var, factor_flag, final_dum_var, date_var))
  if (length(mar_variable) >= 1)
    all_variable <- setdiff(unique(c(num_var, dumvarnam, freq_variables, inter_variables, date_trans_var, out_flag_var, paste0("Y_", mar_variable))), c(unique_var, target))

  if (verbose == TRUE) cat("autoDataprep < variable reduction - zero variance method.... >", "\n")
  master_variable <- all_variable
  varSel_auc <- corNms <- zeroNms <- character()
  mynms <- c(out_flag_var, dumvarnam, date_trans_var)
  if (length(mynms) > 0) {
    configureMlr(show.info = FALSE)
    XXvar <- removeConstantFeatures(XX[mynms])
    zeroNms <- setdiff(mynms, names(XXvar))
    master_variable <- setdiff(master_variable, zeroNms)
  }

  if (verbose == TRUE) cat("autoDataprep < variable selection - pearson correlation method.... >", "\n")
  myDF <- NULL
  mynms <- c(out_flag_var, inter_variables, num_var, date_trans_var)
  mynms <- setdiff(mynms, zeroNms)
  if (length(mynms) > 1){
    suppressWarnings(myDF <- cor (XX[mynms], use = "pairwise.complete.obs"))
    myDF <- as.matrix (myDF)
    myDF <- ifelse (is.na(myDF), 0, myDF)
    myCor <- findCorrelation (myDF, cutoff = corr)
    corNms <- mynms[myCor]
  }
  master_variable <- setdiff(master_variable, corNms)
  if (verbose == TRUE) cat("autoDataprep < variable selection - AUC method.... >", "\n")
  targetv <- XX[target][, 1]
  targetv <- ifelse(is.na(targetv), 0, targetv)
  varSel_summary <- calc_auc_df(XX, targetv, master_variable, impvalue = aucv)
  varSel_auc <- varSel_summary[[1]]
  varSel_auc1 <-  varSel_summary[[2]]
  low_auc_var <- setdiff(master_variable, varSel_auc)
  master_variable <- varSel_auc

  mydata <- cbind(data[c(unique_var, target)], XX)
  mydata <- mydata[, !duplicated(colnames(mydata))]
  cl[[1]] <- as.name("autoDataprep")
  fit <- list(call = cl,
              datasummary = list(type1 = datstrain, type2 = datstrain1),
              complete_data = mydata,
              master_data = mydata[unique(c(unique_var, target, master_variable))],
              raw_list = list("Num_Correlation" = myDF, "AUC_value" = varSel_auc1),
              var_list = list("All_columns" = all_names, "Numeric_col" = num_var, "Character_col" = char_var,  "Factor_col" = factor_var,
                              "Logical_col" = logical_var, "Date_col" = date_var, "Unique_col" = unique_var, "MAR_col" = mar_variable,
                              "Dummy_col" = dumvarnam, "Dropped_col" = drop_var, "Low_auc_col" = low_auc_var,
                              "first_set_var" = varbucket, "final_var_list" = master_variable, "cor_var" = corNms,
                              "auc_var" = varSel_auc, "overall_variable" = all_variable, "zerovariance" = zeroNms,
                              "outlier_summary" = out_flag_var))
  attr(fit, "class") <- "autoDataprep"
  invisible(gc(verbose = FALSE))
  return(fit)
}
