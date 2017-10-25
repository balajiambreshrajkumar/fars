library(fars)
context("FARS IO opeartions")

test_that("make_filename returns filename of the right format", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename(), "accident_NA.csv.bz2")
})
