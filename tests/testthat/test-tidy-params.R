if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("Structure of a returned value by tidy_params()", {
  params <- tidy_params(AG = AG, B = B, qnt = qnt)
  expect_s3_class(params, "data.frame")
  expect_named(params, c("oxide", "element", "phase", "alpha", "beta", "gamma", "wt"))
  expect_equal(
    unname(sapply(params, class)),
    c("character", "character", "character", "numeric", "numeric", "numeric", "numeric")
  )
})
