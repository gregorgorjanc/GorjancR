library(ggrfuncs)

context("Selection intensity")

test_that("Ne is correct", {
  expect_equal(SelInt(1.00), 0)
  expect_equal(round(SelInt(0.05), 2), 2.06)
  expect_equal(round(SelInt(0.01), 2), 2.67)
})
