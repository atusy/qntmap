# © 2018 JAMSTEC
context("fix-params.R")

if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("Reconstruct AG from tidy parameters", {
  fixed <- fix_AG(params)
  nm <- intersect(names(AG), names(fixed))
  expect_equal(AG[nm], fixed[nm])
})

test_that("Reconstruct B from tidy parameters", {
  fixed <- fix_B(params)
  nm <- intersect(names(B), names(fixed))
  expect_equal(B[nm], fixed[nm])
})

if (interactive()) setwd(here::here())
