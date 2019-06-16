if (interactive()) setwd(here::here("tests/testthat"))

dirs <- file.path("minimal", c(".map/1", ".qnt"))
xm <- read_xmap(dirs[1])
qn <- read_qnt(dirs[2])
cn <- find_centers(xm, qn, saveas = FALSE)

test_that("cluster_xmap returns", {
  cls <- cluster_xmap(xm, cn, saving = FALSE)
  expect_s3_class(cls, c("qm_cluster", "data.frame"))
  expect_s3_class(attributes(cls)$center, "data.frame")
})

"Specify cluster_xmap(xte = )"

test_that("Specifying xte = xm and missing xte are identical", {
  set.seed(1)
  expect_identical(
    cluster_xmap(xm, cn, saving = FALSE),
    cluster_xmap(
      xm, cn,
      xte = xm[setdiff(names(xm), c("x", "y"))],
      saving = FALSE)
  )
})

test_that("Specifying xte != xm is supported", {
  set.seed(1)
  cls <- cluster_xmap(
    xm, cn,
    xte = xm[-1L, setdiff(names(xm), c("x", "y"))],
    saving = FALSE
  )
  expect_identical(nrow(cls), nrow(xm) - 1L)
  expect_s3_class(attributes(cls)$center, "data.frame")
})


if (interactive()) setwd(here::here())
