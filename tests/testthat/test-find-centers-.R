if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("find centers", {
  cn <- find_centers(xmap, qnt, saveas = FALSE)
  expect_named(cn, c("phase", "Si", "Mg"))
  expect_s3_class(cn, "data.frame")
})

test_that("find centers when MgO not quantified", {
  qnt2 <- qnt
  qnt2$elm <- qnt$elm[qnt$elm$elem == "SiO2", ]
  qnt2$cmp <- lapply(qnt$cmp, `[`, "SiO2")
  cn <- find_centers(xmap, qnt2, saveas = FALSE) 
  expect_named(cn, c("phase", "Si", "Mg"))
  expect_s3_class(cn, "data.frame")
})

test_that("find centers when Qtz is fine grained", {
  cn <- find_centers(xmap, qnt, phase = -"Qtz", saveas = FALSE)
  cn2 <- find_centers(xmap, qnt, phase = -Qtz, saveas = FALSE)
  expect_named(cn, c("phase", "Si", "Mg"))
  expect_s3_class(cn, "data.frame")
  expect_identical(cn, cn2)
  
  # fine_phase is deprecated, but works
  f <- function() find_centers(xmap, qnt, fine_phase = "Qtz", saveas = FALSE)
  expect_warning(f())
  expect_identical(cn, suppressWarnings(f()))
})

if (interactive()) setwd(here::here())
