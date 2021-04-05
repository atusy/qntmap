# © 2018 JAMSTEC
context("test-pick.R")

d <- cbind(expand.grid(y = 1:2, x = 1:2), A = 1:4)

test_that("Pixels to be picked can be specified by parameters x, y, and phase", {
  object <- pick(d, 1:2, 1:2)
  expected = data.frame(phase = c("P1", "P2"), A = c(1, 4))
  expect_named(object, names(expected))
  Map(expect_equal, object, expected)
})

test_that("Pixels to be picked can be specified by data frame", {
  expect_equal(
    pick(d, i = data.frame(phase = "p", x = 1, y = 1)),
    data.frame(phase = c("p"), A = 1)
  )
})

test_that("Specify phase manually", {
  expect_equal(pick(d, 1, 1, "a"), data.frame(phase = "a", A = 1))
})

test_that("Error if not lengths of x, y, phase are not same", {
  expect_error(pick(x = 1, y = 1:2, phase = "a"))
  expect_error(pick(x = 1, y = 1:2, phase = c("a", "b")))
  expect_error(pick(x = 1:2, y = 1, phase = c("a", "b")))
})
