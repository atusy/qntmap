# © 2018 JAMSTEC
x <- matrix(1:2, 2L, 10L)
qm <- structure(
  cbind(expand.grid(y = 1:2, x = 1:10), A = c(x), B = c(x) * 2L),
  step = 1,
  class = c("qntmap", "data.frame")
)
i <- rep(c("a", "b"), 10L)
cls <- data.frame(cluster = i)
d <- c(a = 2, b = 1)

"vmean and hmean"

test_that("vmean() and hmean() on qntmap class object", {
  hm <- hmean(qm)
  vm <- vmean(qm)
  
  # Check structures
  lapply(list(hm, vm), expect_named, c("px", "um", "A", "B"))
  
  # Check classes
  lapply(list(hm, vm), expect_s3_class, c("qm_profile", "data.frame"))
  
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
