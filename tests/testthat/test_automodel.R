context("autoML model")

test_that("test normal function", {
  expect_error(autoMLmodel(heart, testSplit = 0.2))
  expect_error(autoMLmodel(heart, testSplit = 1, target = "target_var", tuneIters = 1))

})

test_that("test output object", {
  mymodel <- autoMLmodel(heart, testSplit = 0.2, target = "target_var", models = "xgboost", tuneIters = 5, pdp = T)
  expect_output(str(mymodel), "list")
  expect_equal(length(mymodel), 6)
  expect_output(str(mymodel), "autoMLmodel")

  xgb <- mymodel$trainedModels$xgboost$model
  xgb_feat <- xgb$features
  expect_is(predict(xgb, newdata = heart[xgb_feat]), "Prediction")
  expect_is(mymodel$modelexp$pdp$plots$cp, "ggplot")
  expect_is(mymodel$datasummary$train, "data.frame")
})

