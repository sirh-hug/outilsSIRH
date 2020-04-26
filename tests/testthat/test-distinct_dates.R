testthat::context("distinct_dates works fine")

X0 <- data.frame(
  id = c(1:1, 2:2),
  from = as.Date.character("2020-01-01") + c(0, 1, 3, 6),
  to = as.Date.character("2020-01-02") + c(0, 5, 3, 10),
  x = letters[1:4]
)

test_that("errors", {
  expect_error( distinct_dates(X0,id,from) )
  expect_error( distinct_dates(X0,id,from) )
  expect_error( distinct_dates( data.frame(id = 1, from = 1, to = 1), id, from, to ) )
})

test_that("dimensions", {
  expect_equal(dim(distinct_dates(X0, id, from, to, add_days = FALSE)), c(3,6))
  expect_equal(dim(distinct_dates(X0, id, from, to, add_days = TRUE)), c(3,8))
  expect_equal(NROW(distinct_dates(X0, id, from, to, tolerance_n_days = 1)), 3)
  expect_equal(NROW(distinct_dates(X0, id, from, to, tolerance_n_days = 2)), 2)
})


