# © 2018 JAMSTEC
x <- matrix(1:2, 2L, 10L)
qm <- structure(
  cbind(expand.grid(y = 1:2, x = 1:10), A = x, B = x * 2L),
  step = 1,
  class = c("qntmap", "data.frame")
)
i <- rep(c("a", "b"), 10L)
cls <- data.frame(cluster = i)
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
    c(A = weighted.mean(c(1, 2), d), B = weighted.mean(c(2, 4), d))
  )
})
