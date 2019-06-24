context("test-find-AG.R")

if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

# lm_AG

test_that("lm_AG(): returns data frame named at least by 'a', 'a_se', 'g', and 'g_se'", {
  AG <- lm_AG(epma2)
  nm <- c("g", "g_se", "a", "a_se")
  expect_true("data.frame" %in% class(AG))
  expect_named(AG, nm)
  expect_named(lm_AG(epma2, elm), c("elm", nm))
  expect_named(lm_AG(epma2, elm, phase3), c("elm", "phase3", nm))
})

# find_AG()

test_that("find_AG(): returns a data frame without NA", {
  AG <- find_AG(epma2)
  expect_equal(nrow(AG), 4)
  expect_named(AG, c("elm", "phase3", "g", "g_se", "a", "a_se"))
  expect_true(all(!is.na(AG$g)))
  expect_true(all(!is.na(AG$g_se)))
  expect_true(all(!is.na(AG$a)))
  expect_true(all(!is.na(AG$a_se)))
})



test_that("find_AG(): In case map should be divided into smaller maps (e.g., guidenet map)", {
  AG <- find_AG(epma2, not_quantified = "A")
  expect_equal(nrow(AG), 6)
  expect_named(AG, c("elm", "phase3", "g", "g_se", "a", "a_se"))
  expect_true(all(!is.na(AG$g)))
  expect_true(all(!is.na(AG$g_se)))
  expect_true(all(!is.na(AG$a)))
  expect_true(all(!is.na(AG$a_se)))
})

if (interactive()) setwd(here::here())
