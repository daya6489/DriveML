context("Data preparation check")

test_that("test normal function", {
  df <- heart
  expect_error(autoDataprep(df))
  expect_error(autoDataprep(df, target = ""))
  expect_error(autoDataprep(df[,c("target_var")], target = "target_var"))
  expect_message(autoDataprep(df[,c("cp", "target_var")], target = "target_var", auto_mar = T))
})

test_that("test output object", {
  dataprep <- autoDataprep(heart, target = "target_var")
  expect_output(str(dataprep), "list")
  expect_output(str(dataprep), "13 obs")
  expect_equal(nrow(dataprep$complete_data), 303)
  expect_output(str(dataprep), "autoDataprep")
})

test_that("test function parameters", {
  dataprep = autoDataprep(heart, target = "target_var", outlier_flag = FALSE, frequent_var = FALSE)
  cp1 <- cp <- 1L
  if(dataprep$call$outlier_flag == FALSE){
    cp <- length(dataprep$outlier_summary)
  }
  expect_equal(cp, 0)

  if(dataprep$call$frequent_var == FALSE){
    cp1 <- length(dataprep$var_list$first_set_var$F)
  }
  expect_equal(cp1, 0)

})
