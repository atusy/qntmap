context("test-tidy-epma.R")

if(interactive()) setwd(here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1", saving = FALSE)
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
cls <- cluster_xmap(xmap, find_centers(xmap, qnt), saving = FALSE)

test_that("cluster = NULL", {
  epma <- tidy_epma(qnt, xmap)
  expect_true(is.data.frame(epma))
  expect_true(all(is.na(epma$cls)))
  expect_true(all(is.na(epma$mem)))
})

test_that("cluster specified", {
  epma <- tidy_epma(qnt, xmap, cls)
  expect_false(any(is.na(epma$cls)))
  expect_true(is.character(epma$cls))
  expect_false(any(is.na(epma$mem)))
  expect_true(is.numeric(epma$mem))
})


test_that("tidy_epma_for_quantify", {
  epma <- tidy_epma_for_quantify(
    tidy_epma(qnt, xmap, cls),
    maps_x = 100L,
    maps_y = 100L,
    elements = qnt$elm$elem,
    distinguished = FALSE,
    fine_phase = NULL,
    fine_th = .9
  )
  expect_true(all(epma$net >= 0))
  expect_true(is.character(epma$stg))
  expect_equal(epma$phase3, epma$phase2)
  expect_true(all(is.finite(epma$mem)))
})

if(interactive()) setwd(here())
