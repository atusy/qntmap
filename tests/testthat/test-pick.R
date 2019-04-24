# © 2018 JAMSTEC
context("test-pick.R")

test_that("Pixels to be picked can be specified by parameters x, y, and phase", {
  xm <- structure(list(A = data.frame(1:2, 2:3)), class = c("qm_xmap", "list"))
  expect_equal(pick(xm, 1:2, 1:2), data.frame(phase = c("P1", "P2"), A = c(1, 3)))
})

test_that("Pixels to be picked can be specified by data frame", {
  xm <- structure(list(A = data.frame(1:2, 2:3)), class = c("qm_xmap", "list"))
  expect_equal(
    pick(xm, i = data.frame(phase = "p", x = 1, y = 1)),
    data.frame(phase = c("p"), A = 1)
  )
})

test_that("Specify phase manually", {
  xm <- structure(list(A = data.frame(1:2, 2:3)), class = c("qm_xmap", "list"))
  expect_equal(pick(xm, 1, 1, "a"), data.frame(phase = "a", A = 1))
})

test_that("Error if not lengths of x, y, phase are not same", {
  expect_error(pick(x = 1, y = 1:2, phase = "a"))
  expect_error(pick(x = 1, y = 1:2, phase = c("a", "b")))
  expect_error(pick(x = 1:2, y = 1, phase = c("a", "b")))
})
