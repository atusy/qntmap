# © 2018 JAMSTEC
context("mean.R")

test_that("mean.qntmap", {
  x <- list(A = list(wt = as.data.frame(matrix(c(1, 2), nrow = 2L, ncol = 10L))))
  i <- c(matrix(c("a", "b"), nrow = 2L, ncol = 10L))
  cls <- structure(list(cluster = i), class = "qm_cluster")
  d <- c(a = 2, b = 1)
  x$B$wt <- x$A$wt * 2
  
  # index is a string "foo"
  y <- mean.qntmap(x, index = "foo")
  
  ## Result is data frame with name "Element" and "foo"
  expect_s3_class(y, "data.frame")
  expect_named(y, c("Element", "foo"))
  
  ## Mean of A is twice of B
  temp <- setNames(y$foo, y$Element)
  expect_equal(temp[["A"]] * 2, temp[["B"]])
  
  # Index is a character vector i
  y <- mean.qntmap(x, index = i)
  
  expect_named(y, c("Element", "a", "b"))
  expect_equal(y$a, c(1, 2))
  
  # Index is "foo", cluster is `i`, and density is `d` 
  y <- mean.qntmap(x, index = "foo", cluster = cls, density = d)
  expect_equal(
    setNames(y$foo, y$Element), 
    vapply(x, function(x, ...) weighted.mean(unlist(x$wt), ...), w = d[cls$cluster], 0)
  )
})

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
