context("cluster.R")

if(interactive()) setwd(here::here("tests/testthat"))

dirs <- file.path("minimal", c(".map/1", ".qnt"))
xm <- read_xmap(dirs[1], saving = FALSE)
qn <- read_qnt(dirs[2], saving = FALSE)
cn <- find_centers(xm, qn, saveas = FALSE)

'Specify cluster_xmap(xte = )'

test_that("Specifying xte = xm and missing xte are identical", {
  set.seed(1)
  expect_identical(
    cluster_xmap(xm, cn, saving = FALSE),
    cluster_xmap(
      xm, cn, 
      xte = as.data.frame(lapply(xm, unlist, use.names = FALSE)), 
      saving = FALSE)
  )
})

test_that("Specifying xte != xm is supported", {
  set.seed(1)
  expect_identical(
    nrow(cluster_xmap(
      xm, cn, 
      xte = as.data.frame(lapply(xm, unlist, use.names = FALSE))[-1L, ], 
      saving = FALSE
    )$membership),
    NROW(xm[[1]]) * NCOL(xm[[1]]) - 1L
  )
})


if(interactive()) setwd(here::here())
