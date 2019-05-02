# © 2018 JAMSTEC
context("mean.R")
x <- as.data.frame(matrix(c(1, 2), nrow = 2L, ncol = 10L))
qm <- structure(
  list(A = list(wt = x), B = list(wt = x * 2)),
  class = c("qntmap", "list"),
  step = 1
)
i <- c(matrix(c("a", "b"), nrow = 2L, ncol = 10L))
cls <- structure(list(cluster = i), class = "qm_cluster")
d <- c(a = 2, b = 1)

test_that("mean.qntmap", {
  # index is a string "foo"
  y <- mean.qntmap(qm, index = "foo")
  
  ## Result is data frame with name "Element" and "foo"
  expect_s3_class(y, "data.frame")
  expect_named(y, c("Element", "foo"))
  
  ## Mean of A is twice of B
  temp <- setNames(y$foo, y$Element)
  expect_equal(temp[["A"]] * 2, temp[["B"]])
  
  # Index is a character vector i
  y <- mean.qntmap(qm, index = i)
  
  expect_named(y, c("Element", "a", "b"))
  expect_equal(y$a, c(1, 2))
  
  # Index is "foo", cluster is `i`, and density is `d` 
  y <- mean.qntmap(qm, index = "foo", cluster = cls, density = d)
  expect_equal(
    setNames(y$foo, y$Element), 
    vapply(qm, function(x, ...) weighted.mean(unlist(x$wt), ...), w = d[cls$cluster], 0)
  )
})



"vmean and hmean"

test_that("vmean() and hmean() on qntmap class object", {
  hm <- hmean(qm)
  vm <- vmean(qm)
  
  # Check structures
  lapply(list(hm, vm), expect_named, c("px", "um", "A", "B"))
  
  # Check classes
  expect_s3_class(hm, "qm_hmean")
  expect_s3_class(vm, "qm_vmean")
  
  # Check values
  expect_equal(hm$A, unname(rowMeans(x)))
  expect_equal(vm$A, unname(colMeans(x)))
  
  # Weight by densities
  hm2 <- hmean(qm, cluster = cls, density = d)
  vm2 <- vmean(qm, cluster = cls, density = d)
  expect_equal(hm2, hm)
  expect_length(unique(vm2$A), 1L)
  expect_length(unique(vm2$B), 1L)
  expect_equal(vm2$A[[1]], weighted.mean(c(1, 2), c(2, 1)))
  expect_equal(vm2$B[[1]], weighted.mean(c(1, 2) * 2, c(2, 1)))
})
