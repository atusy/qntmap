context("test-tidy-epma.R")

if(interactive()) setwd(here("tests/testthat"))
xmap <- read_xmap("minimal/.map/1", saving = FALSE)
qnt <- read_qnt("minimal/.qnt", saving = FALSE)

test_that("cluster = NA", {
  cluster <- NA
  epma <- tidy_epma(qnt, xmap)
  expect_true(is.data.frame(epma))
  expect_true(all(is.na(epma$cls)))
  expect_true(all(is.na(epma$mem)))
})

test_that("cluster specified", {
  cluster <- cluster_xmap(xmap, find_centers(xmap, qnt), saving = FALSE)
  epma <- tidy_epma(qnt, xmap, cluster)
  expect_false(any(is.na(epma$cls)))
  expect_true(is.character(epma$cls))
  expect_false(any(is.na(epma$mem)))
  expect_true(is.numeric(epma$mem))
})
