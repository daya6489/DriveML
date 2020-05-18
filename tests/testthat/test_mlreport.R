context("autoML report")

path_name <- getwd()
file_name <- "testthat_expreport.html"
file_dir <- file.path(path_name, file_name)

test_that("test ml report generating", {
  skip_on_cran()
  mymodel <- autoMLmodel(heart, testSplit = 0.2, target = "target_var", models = "xgboost", tuneIters = 5, pdp = F)
  expect_error(autoMLReport(mymodel, mldata = heart, op_file = file_name, op_dir = path_name))
  if (file.exists(file_dir)) file.remove(file_dir)
})
