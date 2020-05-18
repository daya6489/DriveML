context("autoMAR")

test_that("test autoMAR function", {
  expect_message(autoMAR(heart))
  expect_error(autoMAR(heart$sex))
})

test_that("test output object", {
  heart$age[1:10] <- NA
  heart$cp[1:10] <- NA
  heart$thal[200:203] <- NA
  mdata = autoMAR(heart)
  expect_output(str(mdata), "List of 7")
  expect_output(str(mdata), "autoMAR")
})

