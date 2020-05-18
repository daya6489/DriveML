#' Function to identify and generate the Missing at Random features (MAR)
#'
#' This function will automatically identify the missing patterns and flag the variables if they are not missing at random based on the AUC method
#'
#' @param data [data.frame | Required] dataframe or data.table
#' @param aucv [integer  | Optional] auc cut-off value for the not missing at random variable selection
#' @param strataname [text  | Optional] vector of stratification variables
#' @param stratasize [integer  | Optional] vector of stratum sample sizes (in the order in which the strata are given in the input dataset).
#' @param mar_method [text  | Optional] missing at random classification method ("glm", "rf"). Default GLM is used (GLM runs faster for high dimensional data)
#' @return list output including missing variable summary and number of MAR flag variables
#' @examples
#' # create missing at random features
#' marobj <- autoMAR (heart, aucv = 0.9, strataname = NULL, stratasize = NULL, mar_method = "glm")
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export autoMAR

autoMAR <- function(data, aucv = 0.9, strataname = NULL,
                    stratasize = NULL, mar_method = "glm"){
  if (!is.data.frame(data)) stop("Input data is not a data frame")
  if (!is.data.table(data)) setDT(data)
  cl <- match.call()
  if (mar_method  == "rf") stop("Random forest method work in progress ...")
  # Checking the presence of missing values
  tmp <- data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(data)]
  mfetaure <- names(tmp)[apply(tmp, 2, function(x) sum(x) > 0)]
  if (length(mfetaure) <= 1) {
    message("less than or equal to one missing value coloumn found in the dataframe")
    fit <- list(MAR_summary = "No missing columns", MAR_output = data)
    class(fit) <- "autoMAR"
    invisible(gc(verbose = FALSE))
    return (fit)
  }
  # Missing feauter list
  ss <- misspattern(data, mfetaure, print = TRUE)
  mfetaure_n <- ss[[1]]
  summary <- ss[[2]]
  if (length(mfetaure_n) <= 1){
    td <- summary
    aucmv1 <- NULL
  } else {
      setDF(data)
      XX <- data[mfetaure_n]
      setDT(XX)
      XX[, (paste0("Y_", mfetaure_n)) := lapply(mfetaure_n, function(x) ifelse(is.na(get(x)), 1, 0))]
      YY <- subset(XX, select = paste0("Y_", mfetaure_n))
      mfetaure_n <- gsub("Y_", "", names(YY)[sapply(YY, function(x) length(x[x == 1])) > 1])
      XX <- XX[, mfetaure_n, with = FALSE]
      # Feature selection and imputations
      invisible(lapply(names(XX), function(x) set(XX, which(is.infinite(XX[[x]])), j = x, value = NA)))
      invisible(lapply(names(XX), function(x) set(XX, which(is.nan(XX[[x]])), j = x, value = NA)))
      setDF(XX)
      imp <- impute(XX, classes = list(factor = imputeMode(),
                                        integer = imputeMean(),
                                        numeric = imputeMedian(),
                                        character = imputeConstant("999")))
      XX <- imp$data
      cat_var <- unique(c(names(XX)[sapply(XX, is.factor)], names(XX)[sapply(XX, is.character)]))
      factor_flag <- character()
      if (length(cat_var) > 0) factor_flag <- cat_var[sapply(XX[cat_var], function(x) length(unique(x)) %in% c(2 : 20))]
		if (length(factor_flag) > 0){
          dmvarf <- XX[factor_flag]
          dmvarf[] <- lapply(dmvarf, function(x) as.factor(paste0(x)))
          sparsemtx <- createDummyFeatures(dmvarf, cols = factor_flag)
          pp <- c(" ", "<", ">", "-", "\\.", "\\+", "&", ", ", "=", "/")
          qq <- c("_", "lt", "gt", "to", "", "p", "", "", "", "")
          for (j in 1 : length(pp)){
            colnames (sparsemtx) <- gsub (pp[j], qq[j], colnames (sparsemtx))
            }
          colnames (sparsemtx) <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", "", colnames (sparsemtx))
          sparsemtx <- sparsemtx[, !duplicated(names(sparsemtx))]
          dumvarnam <- colnames(sparsemtx)
          XX <- cbind(XX, sparsemtx)
      }
      setDT(XX)
    pb <- txtProgressBar(min = 0, max = length(mfetaure_n), style = 3)
    if (length(mfetaure_n) > 1){
      mydata <- NULL
      aucvalue <- sapply(1 : length(mfetaure_n), function(p){
        x <- mfetaure_n[p]
        if (length(cat_var) > 0) {
          if (x %in% factor_flag) {
            rm_dvar <- dumvarnam[grep(x, dumvarnam)]
            ivlist <- c(setdiff(mfetaure_n, cat_var), setdiff(dumvarnam, rm_dvar))
          } else {
            ivlist <- c(setdiff(mfetaure_n, cat_var), dumvarnam)
          }
        } else {
          ivlist <- setdiff(mfetaure_n, x)
        }
        actual <-  YY[, get(paste0("Y_", x))]
        varSel_auc <- calc_auc_df(XX, actual, ivlist, impvalue = 0.03)
        varSel_auc <- varSel_auc[[1]]
        if (length(varSel_auc) < 1) varSel_auc <- ivlist[1]
        if (length(varSel_auc) > 25) varSel_auc <- varSel_auc[1 : 25]
        XX$flag_Y <- actual
        if (!is.null(strataname)) {
          if (is.null(stratasize)) stop("The argument stratasize should be a value")
          Starta <- strata(data, stratanames = strataname, stratasize, method = "srswor")
          rowref <- Starta$ID_unit
          XX <- XX[rowref, ]
        }
        splitIndex <- sample.split(XX$flag_Y, SplitRatio = .8)
        sample_train <- XX[splitIndex == TRUE, c("flag_Y", varSel_auc), with = FALSE]
        sample_test  <- XX[splitIndex == FALSE, c("flag_Y", varSel_auc), with = FALSE]
        setDF(sample_train)
        switch (mar_method,
                "glm" = {
                  Model_desc <- paste0("#GLM model #Num IV variables = ", length(varSel_auc))
                  suppressWarnings( tt <- glm(flag_Y ~., data = sample_train, family = binomial(link = "logit")) )
                },
                "rf" = {
                  warning("Work in progress ....")
                  })
        pred_class <- predict(tt, sample_test, type = "response")
        aucval <- calc_auc(as.numeric(paste0(sample_test$flag_Y)), pred_class)
        mydata1 <- c(x, aucval, Model_desc)
        mydata <- rbind(mydata, mydata1)
        Sys.sleep(0.1)
        setTxtProgressBar(pb, p)
        mydata
      })
      close(pb)
      aucvalue1 <- as.data.frame(t(aucvalue))
      names(aucvalue1) <- c("Variable", "AUC", "Model_desc")
      aucvalue1$AUC <- as.numeric(paste0(aucvalue1$AUC))
      aucvalue1$Variable <- as.character(paste0(aucvalue1$Variable))
      setDT(aucvalue1)
      setkey(aucvalue1, Variable)
      setkey(summary, Variable)
      td <- aucvalue1[summary]
      td <- td[, c("Variable", "Missing_cnt", "Missing_per", "AUC", "Model_desc"), with = FALSE]
      aucmv <- aucvalue1[AUC >= aucv, ]$Variable
      if (length(aucmv) > 0) {
        aucmv1 <- aucvalue1[Variable %in% aucmv, c("Variable", "AUC")]
        nr <- nrow(aucmv1)
      } else {
        aucmv1 <- NULL
        nr <- 0L
      }
    }
  }
  cl[[1]] <- as.name("autoMAR")
  fit <- list(call = cl,
              MARsummary = td,
              num_samples = nrow(data),
              num_variables = ncol(data),
              num_miss_variables = length(mfetaure),
              num_nmar_variables = nr,
              auc_features = aucmv1)
  class(fit) <- "autoMAR"
  invisible(gc())
  return (fit)
}

globalVariables(c("AUC"))
