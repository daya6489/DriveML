context("generate features")

test_that("test feature generating function", {
  expect_error(ccd = generateFeature(heart, varlist , type = "Frequent", method = NULL))
  expect_error(generateFeature(heart$sex, varlist , type = "Frequent", method = NULL))
})

test_that("test ouptut", {
  ccd = generateFeature(heart, varlist = c("thal"), type = "Frequent", method = NULL)
  expect_is(ccd, "data.frame")
})
