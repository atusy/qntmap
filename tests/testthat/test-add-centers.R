# © 2018 JAMSTEC

cn <- data.frame(phase = "a", X1 = 1, X2 = 1)
xm <- data.frame(x = 1, y = 1, X1 = 1, X2 = 1)

test_that("Add centers without saving csv", {
  expect_equal(
    add_centers(cn, xm, 1, 1, saveas = FALSE),
    data.frame(phase = c("a", "P1"), X1 = c(1, 1), X2 = c(1, 1))
  )
})

test_that("Add centers saving csv", {
  setwd(tempdir())
  add_centers(cn, xm, 1, 1)
  expect_equal(
    read.csv(formals(add_centers)$saveas, stringsAsFactor = FALSE),
    data.frame(phase = c("a", "P1"), X1 = c(1, 1), X2 = c(1, 1))
  )
})
