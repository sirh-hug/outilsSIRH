context("duplicate_rows works correctly")
library(testthat)

X <- data.frame(from = as.Date.character("2020-04-16") ,
                to = as.Date.character("2020-04-16") + 3)

test_that("errors", {
  expect_error( duplicate_rows(matrix(1:9,nrow = 3)) ) # df not a data.frame
  expect_error( duplicate_rows(data.frame(from=1,to=1), from, to) ) # from & to not dates
  expect_error( duplicate_rows(data.frame(from= Sys.Date() ,to=1), from, to) ) # not date
  expect_error( duplicate_rows(data.frame(from= 1 ,to=Sys.Date()), from, to) ) # not date
  expect_error( duplicate_rows(X, from)) # missing parameter
})

test_that("dimensions", {
  expect_equal( dim(duplicate_rows(X, from, to)), c(4,3) )
  expect_identical( class(duplicate_rows(X, from, to)), class(X) )
})
