if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("cluster_xmap returns", {
  cls <- cluster_xmap(xmap, centers, saving = FALSE)
  expect_s3_class(cls, c("qm_cluster", "data.frame"))
  expect_s3_class(attributes(cls)$center, "data.frame")
})

"Specify cluster_xmap(xte = )"

test_that("Specifying xte = xm and missing xte are identical", {
  set.seed(1)
  nm <- c("cluster", "membership", "Ol", "Qtz")
  expect_identical(
    cluster_xmap(xmap, centers, saving = FALSE)[nm],
    cluster_xmap(
      xmap, centers, xte = xmap[setdiff(names(xmap), c("x", "y"))], saving = FALSE
    )[nm]
  )
})

test_that("Specifying xte != xm is supported", {
  set.seed(1)
  cls <- cluster_xmap(
    xmap, centers,
    xte = xmap[-1L, setdiff(names(xmap), c("x", "y"))],
    saving = FALSE
  )
  expect_identical(nrow(cls), nrow(xmap) - 1L)
  expect_s3_class(attributes(cls)$center, "data.frame")
})


if (interactive()) setwd(here::here())
