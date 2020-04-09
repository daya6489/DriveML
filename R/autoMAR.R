#' Function to identify and generate the Missing at Random features (MAR)
#'
#' this function will automatically identify the missing pattern and flag the variable if they are not missing at random based on AUC method
#'
#' @param data dataframe or data.table
#' @param aucv AUC cut-off value for the not missing at random variable selection
#' @param strataname vector of stratification variables
#' @param stratasize vector of stratum sample sizes (in the order in which the strata are given in the input data set).
#' @param mar_method missing at random classification method ("glm", "rf"). Default GLM is used (GLM is run faster for high dimension data)
#' @return List output including missing variable summary and number of MAR flag variables
#' @examples
#' autoMAR (heart, aucv = 0.9, strataname = NULL, stratasize = NULL, mar_method = "glm")
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export autoMAR

autoMAR <- function(data, aucv = 0.9, strataname = NULL, stratasize = NULL, mar_method = "glm")
  {
  if(!is.data.frame(data)) stop("Input data is not a data frame")
  if (!is.data.table(data)) setDT(data)

  cl <- match.call()
  if(mar_method  =="rf") stop("Random forest method work in progress ...")
  # Checking the presence of missing values
  tmp <- data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(data)]
  mfetaure <- names(tmp)[apply(tmp, 2, function(x) sum(x) > 0)]
  rm(tmp)

  if(length(mfetaure) <= 1) {
    message("less than or equal to one missing value coloumn found in the dataframe")
    return(list(MAR_summary = "No missing columns", MAR_output = data))
  }
  cat("****",paste0("Total ",length(mfetaure), " variables having NA values out of ", length(names(data))),"****","\n")

  # Missing feauter list
  ss <- misspattern(data, mfetaure, print = T)
  mfetaure_n <- ss[[1]]
  summary <- ss[[2]]

  if(length(mfetaure_n) <= 1){
    cat("There are no missing missing at random features in the input dataframe","\n")
    td <- summary
    aucmv1 <- NULL
  } else
    {
      cat("Selected ",length(mfetaure_n),"unique variables for MAR computation","\n")
      setDF(data)
      XX <- data[mfetaure_n]
      setDT(XX)
      XX[, (paste0("Y_", mfetaure_n)) := lapply(mfetaure_n, function(x) ifelse(is.na(get(x)), 1, 0))]
      YY <- subset(XX, select = paste0("Y_",mfetaure_n))
      mfetaure_n <- gsub("Y_", "", names(YY)[sapply(YY, function(x) length(x[x==1])) > 1])
      XX <- XX[, mfetaure_n, with = F]

      # Feature selection and imputations
      invisible(lapply(names(XX),function(x) set(XX, which(is.infinite(XX[[x]])), j = x,value =NA)))
      invisible(lapply(names(XX),function(x) set(XX, which(is.nan(XX[[x]])), j = x,value =NA)))
      setDF(XX)
      imp <- impute(XX, classes = list(factor = imputeMode(),
                                        integer = imputeMean(),
                                        numeric = imputeMedian(),
                                        character = imputeConstant("999")))
      XX <- imp$data
      cat_var <- unique(c(names(XX)[sapply(XX, is.factor)],names(XX)[sapply(XX, is.character)]))
      factor_flag <- character()
      if(length(cat_var) > 0) factor_flag <- cat_var[sapply(XX[cat_var], function(x) length(unique(x)) %in% c(2 : 20))]
      setDT(XX)

    # Preparing dummy variables for categorical variables
      if(length(factor_flag) > 0){
          dmvarf <- formula(paste(" ~ ",paste0(factor_flag,collapse = " + ")))
          myDummy <- dummyVars (dmvarf, XX)
          sparseMtx <- predict (myDummy, XX)
          setDT (XX)
          pp <- c(" ", "<", ">", "-", "\\.", "\\+", "&", ",", "=", "/")
          qq <- c("_", "lt", "gt", "to", "", "p", "", "", "", "")

          for (j in 1 : length(pp)){
            colnames (sparseMtx) <- gsub (pp[j], qq[j], colnames (sparseMtx))}

          colnames (sparseMtx) <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", "", colnames (sparseMtx))
          sparseMtx <- as.data.frame(sparseMtx)
          sparseMtx <- sparseMtx[,!duplicated(names(sparseMtx))]
          dumvarnam <- colnames(sparseMtx)
          XX <- cbind(XX,sparseMtx)
      }
    ## Data modeling and AUC calculations
    pb <- txtProgressBar(min = 0, max = length(mfetaure_n), style = 3)
    mydata <- NULL;
    if(length(mfetaure_n) > 1){
      aucvalue <- sapply(1: length(mfetaure_n), function(p){
        x <- mfetaure_n[p]
        if(length(cat_var) > 0) {
          if(x %in% factor_flag) {
            rm_dvar <- dumvarnam[grep(x, dumvarnam)]
            ivlist <- c(setdiff(mfetaure_n, cat_var), setdiff(dumvarnam, rm_dvar))
          } else {
            ivlist <- c(setdiff(mfetaure_n, cat_var), dumvarnam)
          }
        } else {
          ivlist <- setdiff(mfetaure_n, x)
        }

        actual <-  YY[, get(paste0("Y_",x))]
        varSel_auc <- calc_auc_df(XX, actual, ivlist, impvalue = 0.03)
        varSel_auc <- varSel_auc[[1]]
        if(length(varSel_auc) < 1) varSel_auc <- ivlist[1]
        if(length(varSel_auc) > 25) varSel_auc <- varSel_auc[1:25]

        XX$flag_Y <- actual

        ## Stratification
        if(!is.null(strataname)) {
          if(is.null(stratasize)) stop("The argument stratasize should be a value")
          Starta <- strata(data, stratanames=strataname, stratasize, method="srswor")
          rowref <- Starta$ID_unit
          XX <- XX[rowref, ]
        }

        ## split data into training and testing chunks
        set.seed(121)
        splitIndex <- createDataPartition(XX$flag_Y, p = .80, list = FALSE, times = 1)
        sample_train <- XX[ splitIndex, c("flag_Y", varSel_auc), with = F]
        sample_test  <- XX[-splitIndex, c("flag_Y", varSel_auc), with = F]
        setDF(sample_train)

        switch (mar_method,
                "glm" = {
                  Model_desc <- paste0("#GLM model #Num IV variables = ", length(varSel_auc))
                  tt = glm(flag_Y~.,data = sample_train,family = binomial(link = "logit"))
                },
                "rf" = { cat("Work in progress ....")}
        )

        pred_class <- predict(tt, sample_test, type = 'response')
        aucval <- calc_auc(as.numeric(paste0(sample_test$flag_Y)), pred_class)
        mydata1 <- c(x, aucval, Model_desc)
        mydata <- rbind(mydata,mydata1)
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
      td <- td[, c("Variable", "Missing_cnt", "Missing_per", "AUC", "Model_desc"), with = F]
      aucmv <- aucvalue1[AUC >= aucv, ]$Variable

      if(length(aucmv) > 0) {
        aucmv1 <- aucvalue1[Variable %in% aucmv, c("Variable", "AUC")]
        nr <- nrow(aucmv1)
      } else {
        aucmv1 <- NULL
        nr <- 0L
        cat("\n","Not found any MAR features in this data set","\n")
      }
    }
  }

  cl[[1]] <- as.name("autoMAR")
  fit <- list(call = cl,
              MARsummary = td,
              num.samples = nrow(data),
              num.variables = ncol(data),
              num.miss.variables = length(mfetaure),
              num.nmar.variables = nr,
              auc.features = aucmv1)
  class(fit) <- "autoMAR"
  rm(list = setdiff(ls(), c("fit")))
  invisible(gc())
  return(fit)
}

globalVariables(c("AUC"))
