if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("fine_outlier works when data have cols...", {
  outlier <- find_outlier(epma[c(
    "id", "elint", "phase",
    "pk_t", "beam", "dwell", "beam_map",
    "mapint", "mapint.L", "mapint.H",
    "pkint", "pkint.L", "pkint.H"
  )])
  expect_identical(
    sort(setdiff(names(outlier), names(epma))),
    sort(c("outlier", "pkint.L_est", "pkint.H_est", "mapint.L_est", "beta", "mapint.H_est"))
  )
})

