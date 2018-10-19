# © 2018 JAMSTEC
context("test-add_ceters.R")

test_that("Add centers without saving csv", {
  centers <- data.frame(phase = 'a', X1 = 1, X2 = 1)
  xmap <- list(X1 = data.frame(1), X2 = data.frame(1))
  class(xmap) <- c('qm_xmap', list)
  expect_equal(
    add_centers(centers, xmap, 1, 1, saveas = FALSE),
    data.frame(phase = c('a', 'P1'), X1 = c(1, 1), X2 = c(1, 1))
  )
})

test_that("Add centers saving csv", {
  centers <- data.frame(phase = 'a', X1 = 1, X2 = 1)
  xmap <- list(X1 = data.frame(1), X2 = data.frame(1))
  class(xmap) <- c('qm_xmap', list)
  setwd(tempdir())
  add_centers(centers, xmap, 1, 1)
  expect_equal(
    read.csv(formals(add_centers)$saveas, stringsAsFactor = FALSE),
    data.frame(phase = c('a', 'P1'), X1 = c(1, 1), X2 = c(1, 1))
  )
})
