context("test-find-B.R")

if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("lm_B(): If `isTRUE(all(mem == 0))`, then coef and vcov return NA", {
  B <- lm_B(mutate(epma2, mem = 0))
  expect_true(all(is.na(B$b)))
  expect_true(all(is.na(B$b_se)))
  B <- lm_B(mutate(epma2, mem = 0), elm)
  expect_true(all(is.na(B$b)))
  expect_true(all(is.na(B$b_se)))
  B <- lm_B(mutate(epma2, mem = 0), elm, stg)
  expect_true(all(is.na(B$b)))
  expect_true(all(is.na(B$b_se)))
})

test_that("lm_B(): returns data frame named at least by 'b' and 'b_se'", {
  B <- lm_B(epma2)
  nm <- c("b", "b_se")
  expect_true("data.frame" %in% class(B))
  expect_named(lm_B(epma2, elm), c("elm", nm))
  expect_named(lm_B(epma2, elm, stg), c("elm", "stg", nm))
})

# find_B()

test_that(
  "find_B() has arguments same as find_outlier except remove_outlier in find_B", {
    expect_identical(formals(find_B)[-2L], formals(find_outlier)[])
})

test_that("find_B(): returns a data frame", {
  B <- find_B(epma2)
  expect_equal(nrow(B), 2)
  expect_named(B, c("elm", "stg", "b", "b_se"))
})

test_that("find_B(): In case map should be divided into smaller maps (e.g., guidenet map)", {
  B <- find_B(dplyr::bind_rows(epma2, dplyr::mutate(epma2, stg = "12")))
  B_11 <- B[B$stg == "11", ]
  B_12 <- B[B$stg == "12", ]
  nm <- setdiff(names(B), "stg")
  rownames(B_12) <- rownames(B_11) <- NULL 
  expect_identical(B_11[nm], B_12[nm])
})

if (interactive()) setwd(here::here())
