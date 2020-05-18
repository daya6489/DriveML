context("PDP plot")

hearML <- autoMLmodel(heart,  target = "target_var",  testSplit = 0.2,
                      tuneIters = 5,  tuneType = "random",
                      models = "xgboost", varImp = 20,  liftGroup = 50, positive = 1, seed = 1991)

test_that("test pdp plot", {
 expect_message(autoPDP(heart, hearML, feature = "chol", sample = 0.8, seed = 1234))
 expect_error(autoPDP(heart, hearML, feature, sample = 0.8, seed = 1234))

})

test_that("test pdp plot output", {
  pp = autoPDP(heart, hearML, feature = "chol", sample = 0.8, seed = 1234)
  expect_output(str(pp), "list")
  expect_output(str(pp), "ggplot")
})
