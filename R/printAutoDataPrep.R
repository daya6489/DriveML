#' Print Method for the autoDataprep Class
#'
#' Print the result of autoDataprep object
#'
#' @param x [Object | Required] an object of class autoDataprep
#' @export printautoDataprep

printautoDataprep <- function(x) {
  cat("Data preparation result", "\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat(" *** Data preparation summary ***", "\n")
  cat("Total no. of columns available in the data set: ", length(x$var_list$All_columns), "\n")
  cat("No. of numeric columns:                         ", length(x$var_list$Numeric_col), "\n")
  cat("No. of factor / character columns:              ", length(x$var_list$Character_col) + length(x$var_list$Factor_col), "\n")
  cat("No. of date columns:                            ", length(x$var_list$Date_col), "\n")
  cat("No. of logical columns:                         ", length(x$var_list$Logical_col), "\n")
  cat("No. of unique columns:                          ", length(x$var_list$Unique_col), "\n")
  cat("No. of MAR columns:                             ", length(x$var_list$MAR_col), "\n")
  cat("No. of dummy variables created:                 ", length(x$var_list$Dummy_col), "\n")
  cat("\n", "*** Variable reduction ***","\n")
  cat("Step 1 - Checked and removed useless variables:        ", length(x$var_list$Dropped_col), "\n")
  cat("Step 2 - No. of variables before fetature engineering: ", length(x$overall_variable), "\n")
  cat("Step 3 - No. of zero variance columns (Constant):      ", length(x$zerovariance), "\n")
  cat("Step 4 - No. of high correlated or bijection columns:  ", length(x$cor_var), "\n")
  cat("Step 5 - No. of low AUC valued columns:                ", length(x$var_list$Low_auc_col), "\n")
  cat("*Final number of columns considered for ML model:      ", length(x$final_var_list), "\n")
  cat("\n", "*** Data preparation highlights ***", "\n")
  cat("Missing replaced with", x$call$imp, "\n")
  cat("Character / factor columns removed if unique label greater than ", x$call$char_var_limit, "\n")
  cat("Created flag variables for date columns if unique number of dates are less than 5", "\n")
}
