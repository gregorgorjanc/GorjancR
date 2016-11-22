library(GorjancR)

context("Effective population size")

test_that("Ne is correct", {
  expect_equal(Ne(nM=50, nF=50), 100)
  expect_equal(Ne(nM=20, nF=30),  48)
  expect_equal(Ne(nM=50, nF=50, VarProgM=0, VarProgF=0), 200)
})
