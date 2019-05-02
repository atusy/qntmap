# © 2018 JAMSTEC
context("mean.R")

test_that("vmean() and hmean() on data frame", {
  x <- iris[1L:5L, -5L]
  expect_equal(vmean(x), colMeans(x))
  expect_equal(hmean(x), unname(rowMeans(x)))
})

test_that("vmean() and hmean() on qntmap class object", {
  x <- iris[1L:5L, -5L]
  qm <- structure(
    list(a = list(wt = x), b = list(wt = x)),
    class = c("qntmap", "list"),
    step = 1
  )
  vm <- vmean(qm)
  hm <- hmean(qm)
  lapply(list(vm, hm), expect_named, c("px", "um", "a", "b"))
  expect_equal(nrow(vm), ncol(qm$a$wt))
  expect_equal(nrow(hm), nrow(qm$a$wt))
})
