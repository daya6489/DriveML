#' Display autoMLmodel output in HTML format using Rmarkdown
#'
#' This function will generate R markdown report for DriveML model object
#'
#' @param mlobject [autoMLmodel Object | Required] autoMLmodel function output
#' @param mldata [autoDataprep Object | Optional] autoDataprep function output
#' @param op_file [character | Required] output file name (.html)
#' @param op_dir [character | Optional] output path. Default path is the current working directory
#' @details
#' Using this function we can easily present the model outcome in standard HTML format without writing Rmarkdown scripts
#'
#' @return HTML R Markdown output
#'
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @examples
#' ## Creating HTML report
#' \donttest{
#'  autoMLReport(heart.model, mldata = NULL, op_file = "sample.html", op_dir = tempdir())
#'  }
#' @export autoMLReport

autoMLReport <- function (mlobject, mldata = NULL, op_file = NULL, op_dir = NULL){
  if (missing(mlobject)) stop("DriveML model object is missing")
  if (class(mlobject) != "autoMLmodel") {
    stop("mlobject should be autoMLmodel output")
  }
  if(is.null(op_dir)) {
    op_dir <- getwd()
  }
  if (is.null(op_file)) stop("Output file name is missing")
  if (!is.null(mldata)) {
    pathname <- paste0("rmd_template/report_tmp_v1.Rmd")
    args <- as.list(match.call())
    report_dir <- system.file(pathname, package = "DriveML")
    suppressWarnings(render(input = report_dir, output_file = op_file,
                            output_dir = op_dir, intermediates_dir = op_dir,
                            params = list(mlobject = mlobject, mldata = mldata)))
  } else {
    pathname <- paste0("rmd_template/report_tmp.Rmd")
    args <- as.list(match.call())
    report_dir <- system.file(pathname, package = "DriveML")
    suppressWarnings(render(input = report_dir, output_file = op_file,
                            output_dir = op_dir, intermediates_dir = op_dir,
                            params = list(mlobject = mlobject)))
  }

  report_path <- file.path(op_dir, op_file)
  browseURL(report_path)
  if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]]))
    message(paste0("\n\nReport is generated at \"", report_path, "\"."))
}
