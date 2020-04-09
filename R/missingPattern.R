#' Missing pattern analysis for missing data
#'
#' this function for summarise the missing variable, missing pattern identification, classifying the columns based on pattern of missing values.
#'
#' @param data [data.frame | Required] data set with missing values
#' @param mfeature [character | Required] only missing variable name
#' @param drop [numeric | optional] drop variable percentage. Example, if drop = 0.9, function will automatically drop 90per missing columns from the data set
#' @param print [character | optional] defualt print is FALSE
#' @return final variable list, summary of missing data analysis
#'
#' @importFrom data.table is.data.table setDT setDF data.table
#' @importFrom utils combn
#' @export misspattern

misspattern <- function(data, mfeature, drop = 0.99, print = F)
  {
  if(is.null(mfeature)) stop("There is no input for missing features")
  setDF(data)
  mpattern <- data[mfeature]
  setDT(mpattern)

  tmp <- t(mpattern[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:ncol(mpattern)])
  number <- as.integer(tmp)
  names(number) <- rownames(tmp)
  number_1 <- names(number[number <= 2])

  percent <- number/nrow(mpattern)
  outframe <- data.table(Variable = mfeature, Missing_cnt= number, Missing_per = round(percent,2))
  percent <- percent[order(percent)]
  percent_eq_1 <- percent[percent >= drop]

  if(length(percent_eq_1) > 0)
  {
    mfeature <- setdiff(mfeature, names(percent_eq_1))
    mpattern <- data[mfeature]
    setDT(mpattern)
    number <- number[mfeature]
    percent <- percent[mfeature]
    percent <- percent[order(percent)]
  }

  # Variable summary
  varbucket <- lapply(unique(percent), function(x) percent[percent == x])
  final_varlist <- unique(names(unlist(varbucket[lapply(varbucket, function(x) length(x)) == 1])))
  loop_var <- varbucket[lapply(varbucket, function(x) length(x)) > 1]

  if(length(loop_var) > 0) {
    for(i in 1 : length(loop_var)) {
      mfeature <- names(loop_var[[i]])
      var_method <- 'random'
      if(var_method == 'random') {
        mfeature1 <- mfeature[sample(1:length(mfeature), 1)]
        final_varlist <- unique(c(final_varlist,mfeature1))
      } else {

        if(length(mfeature) > 5) {
          set.seed(121)
          mfeature <- mfeature[sample(1:length(mfeature), 5)]
        }
        keep_fet <- c()
        if (isTRUE(print)) cat("*")
        tmp <- mpattern[, apply(.SD, 1,function(x) sum(is.na(x))), .SDcols = mfeature]

        if(length(tmp[tmp>1]) > 1){
          x <- 1; t <- 0
          comblist <- combn(mfeature, 2)
          xi <- ncol(comblist)
          dropvar_list <- NULL

          while(xi >= 1){
            t <- t + 1
            varlist <- comblist[, x]
            valueper <- percent[varlist]
            valuenum <- number[varlist]
            diffv <- abs(diff(valueper))
            if(diffv <= 0.05) {
              cp <- mpattern[tmp > 1, varlist, with = F]
              missing_count <- cp[, apply(.SD, 1, function(x) length(x[is.na(x)])), .SDcols = varlist]
              simpattern_prop <- length(missing_count[missing_count == 2])/length(missing_count)

              drop_var <- names(valuenum[valuenum == max(valuenum)])[1]
              mfeature <- setdiff(mfeature, drop_var)
              if(simpattern_prop < 0.99) keep_fet <- c(keep_fet, drop_var)

              drop_list <- data.frame(Var1 = as.character(names(valuenum[1])),
                                      Var2 = as.character(names(valuenum[2])),
                                      mis_matching_prop = simpattern_prop * 100,
                                      drop_var = drop_var)
              dropvar_list <- rbind(dropvar_list, drop_list)

              if(length(mfeature) == 1) break
              comblist <- combn(mfeature, 2)
              xi <- ncol(comblist)
              if(xi == 1) {
                x <- 0
              } else {
                x <- 1
              }
            }
            x <- x + 1
          }
          final_varlist <- unique(c(final_varlist,mfeature,keep_fet))
        } else {
          final_varlist <- unique(c(final_varlist,mfeature))
          }
      }
    }
  }
  return(list(totvar = setdiff(final_varlist, number_1), summary = outframe))
}

globalVariables(c("Drop_var", "Variable", "missing_count"))
