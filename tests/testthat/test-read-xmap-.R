context("test-read-xmap-.R")

if(interactive()) setwd(here("tests/testthat"))

wd <- "minimal/.map/1"

prepare <- function() {
  unlink(file.path(wd, "xmap.RDS"))
}

test_that("list structure", {
  prepare()
  xm <- read_xmap(wd, renew = TRUE, saving = FALSE)

  expect_named(xm, c("Si", "Mg"))
  expect_false(file.exists(file.path(wd, "xmap.RDS")))
})

test_that("save = TRUE", {
  prepare()
  xm <- read_xmap(wd, renew = TRUE, saving = TRUE)
  rds <- file.path(wd, "xmap.RDS")
  expect_true(file.exists(rds))

  xm2 <- read_xmap(wd, renew = FALSE, saving = FALSE)
  expect_identical(xm, xm2)
})

prepare()


if(interactive()) setwd(here())
