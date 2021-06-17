#' Automated column transformer
#'
#' This function automatically scans through each variable and generate features based on the type listed in the "details"
#'
#' @param data [data.frame  | Required] dataframe or data.table
#' @param varlist [text  | Required] variable list to generate the additional features
#' @param type [text  | Required] variable transformation with type - 'Dummy','Outlier','Frequent' or 'Interaction'
#' @param method [text  | Required] input for variabe transformation for type = 'Frequent' then the method should be 'Frequency' or 'Percent'. Please refer to the "details" section to know more
#' @details
#'
#' This function is for generating features based on different transformation methods such as interaction, outliers, Dummy coding, etc.
#'
#' Interaction type
#' \itemize{
#'   \item multiply - multiplication
#'   \item add - addition
#'   \item substract - substraction
#'   \item divide - division
#' }
#' Frequency type
#' \itemize{
#'   \item Frequency - frequency
#'   \item Percent - percentage
#' }
#' Outlier type
#' \itemize{
#'   \item Flag - flag outlier values like 1 or 0
#'   \item Capping - impute outlier value by 95th or 5th percentile value
#' }
#' Date type
#' \itemize{
#'   \item Year
#'   \item Month
#'   \item Quarter
#'   \item Week
#' }
#' @return generated transformed features
#' @examples
#' # Generate interaction features
#' generateFeature(heart, varlist = c("cp", "chol", "trestbps"), type = "Interaction",
#' method = "add")
#' generateFeature(heart, varlist = c("cp", "chol", "trestbps"), type = "Interaction",
#' method = "multiply")
#'
#' # Generate frequency features
#' generateFeature(heart, varlist = c("cp", "thal"), type = "Frequent", method = "Percent")
#' generateFeature(heart, varlist = c("cp", "thal"), type = "Frequent", method = "Frequency")
#'
#' @importFrom mlr createDummyFeatures
#' @importFrom utils combn
#' @importFrom SmartEDA ExpCTable ExpNumStat
#' @export generateFeature

generateFeature <- function(data, varlist, type = "Frequent", method = NULL) {
  if (missing(data)) stop ("Input data is missing")
  if (missing(type)) stop ("input type is missing")
  data <- setDF(data)
  ## Outlier Transformer
  if (type == "Dummy"){
    if (missing(varlist)) stop ("Input numerical variable list is missing")
    XX <- data[varlist]
    XX[] <- lapply(XX, function(x) as.factor(paste0(x)))
    sparsemtx <- createDummyFeatures(XX, cols = varlist)
    pp <- c(" ", "<", ">", "-", "\\.", "\\+", "&", ",", "=", "/", "\\$")
    qq <- c("_", "lt", "gt", "to", "", "p", "", "", "", "", "_d")
    for (j in 1 : length(pp))
      colnames (sparsemtx) <- gsub (pp[j], qq[j], colnames (sparsemtx))
    colnames(sparsemtx) <- trimws(gsub("[][!#$%()*?,.:;<=>@^|~.{}]", "_q", colnames(sparsemtx)))
    colnames(sparsemtx) <- paste0('d_', colnames(sparsemtx))
    sparsemtx <- sparsemtx[, !duplicated(names(sparsemtx))]
    invisible(gc(verbose = FALSE))
    return(sparsemtx)
  }
  ## Outlier Transformer
  if (type == "Outlier"){
    if (is.null(method)) method <- "flag"
    if (missing(varlist)) stop ("Input numerical variable list is missing")
    XX <- data[varlist]
    setDT(XX)
    mydata <-  XX[, lapply(.SD, function(x) {
      LOut_val <- quantile(x, probs = 0.25, na.rm = TRUE)[[1]] - (1.5 * IQR(x, na.rm = TRUE))
      Lcap <- quantile(x, probs = 0.10, na.rm = TRUE)[[1]]
      UOut_val <- quantile(x, probs = 0.75, na.rm = TRUE)[[1]] + (1.5 * IQR(x, na.rm = TRUE))
      Ucap <- quantile(x, probs = 0.90, na.rm = TRUE)[[1]]
      if (LOut_val != UOut_val){
        if (method == "caping"){
        ifelse(x < LOut_val, Lcap,
                ifelse(x > UOut_val, Ucap, x))
        } else {
          ifelse(x < LOut_val, 1,
                 ifelse(x > UOut_val, 1, 0))
          }
        }
      }
      ), .SDcols = varlist]
  cvnam <- paste0("Out_", method, "_")
  if (ncol(mydata) > 0)  {
    mydata <- as.data.frame(mydata)
    names(mydata) <- paste0(cvnam, names(mydata))
    cvnam <- names(mydata)
    cvnam <- cvnam[sapply(mydata, function(x) length(unique(x)) > 1)]
    mydata <- mydata[cvnam]
  } else {
    mydata <- NULL
  }
  invisible(gc(verbose = FALSE))
  return(mydata)
}
  if (type == "Frequent"){
    if (missing(varlist)) stop ("Input numerical variable list is missing")
    find_list <- function(x, y) match(TRUE, sapply(y, `%in%`, x = x))
    abc <- data[varlist]
    cc <- ExpCTable(abc)
    cc <- cc[cc$Valid != "TOTAL", ]
    catvar <- unique(cc$Variable)
    if (length(varlist) > 15) varlist <- catvar[sapply(catvar, function(x) nrow(cc[cc$Variable == x, ]) > 2)]
    if (length(varlist) > 0){
      if (is.null(method)) method <- "Frequency"
      if (method != "Frequency" & method != "Percent") stop ("Invalid method is selected")
      abc <- abc[varlist]
      setDT(abc)
      for (j in 1 : length(varlist)){
        valid <- cc[cc$Variable == varlist[j], c("Valid")]
        value <- cc[cc$Variable == varlist[j], c(method)]
        idx_value  <- sapply(abc[[j]], find_list, valid)
        abc[, (paste0("Freq_t_", varlist[j])) := value[idx_value]]
      }
      frq_var <- names(abc)[grep("Freq_t_", names(abc))]
      if (length(frq_var) > 0) {
        setDF(abc)
        abc <- abc[frq_var]
        aftermis <- names(abc)[sapply(abc, function(x) sum(is.na(x)) == 0)]
        abc <-  abc[aftermis]
      } else {
        abc <- NULL
      }
    }
    invisible(gc(verbose = FALSE))
    return(abc)
  }

  if (type == "Interaction"){
      if (missing(varlist)) {
        sumsata <- ExpNumStat(data, by = "A")
        varlist <- unique(as.character(paste0(sumsata$Vname)))
        }
      if (length(varlist) > 1){
        if (method != "multiply" & method != "subtract" & method != "divide" & method != "add" ) stop ("Invalid method is selected")
        if (is.null(method)) method <- "multiply"
        XX <- data[varlist]
        XX[,] = sapply(XX[,], as.numeric)
        listcalc <- combn(varlist, 2)
        inter_name <- paste0("Inter_", apply(listcalc, 2, function(x) paste(x, collapse = "_")))
        YY <- matrix(nrow = nrow(XX), ncol = ncol(listcalc))
        YY <- as.data.table(YY)
        for (j in 1 : ncol(listcalc)){
          k <- j + 0L
          comlist <- listcalc[, k]
          switch (method,
                  "multiply" = (YY[, (names(YY)[k]) := (XX[comlist][, 1] * XX[comlist][, 2])]),
                  "add" = (YY[, (names(YY)[k]) := (XX[comlist][, 1] + XX[comlist][, 2])]),
                  "subtract" = (YY[, (names(YY)[k]) := (XX[comlist][, 1] - XX[comlist][, 2])]),
                  "divide" = (YY[, (names(YY)[k]) := (XX[comlist][, 1] / XX[comlist][, 2])])
          )
        }
        names(YY) <- inter_name
        YY <- YY[, which(unlist(lapply(YY, function(x) all(!is.na(x))))), with = F]

      }
    invisible(gc(verbose = FALSE))
      return(YY)
   } else
        if (type == "date"){
          if (missing(varlist)) stop ("Input numerical variable list is missing")
          XX <- data[varlist]
          setDT(XX)
          # Adding flag variable for date column
          XX[, (paste0("datet_y_", varlist)) := lapply(varlist, function(x) year(get(x)))]
          XX[, (paste0("datet_q_", varlist)) := lapply(varlist, function(x) quarter(get(x)))]
          XX[, (paste0("datet_m_", varlist)) := lapply(varlist, function(x) month(get(x)))]
          XX[, (paste0("datet_w_", varlist)) := lapply(varlist, function(x) week(get(x)))]
          final_var <- names(XX)[grep("datet_", names(XX))]
          setDF(XX)
          invisible(gc(verbose = FALSE))
          return(XX[final_var])
        } else
         stop ("Selected type is not available")
}
