if (interactive()) setwd(here::here("tests/testthat"))

if (!exists("xmap")) source("setup.R")

test_that("find_AG() returns data frame", {
  AG <- find_AG(epma2)
  B <- find_B(epma2)
  AB <- find_AB(AG, B)
  expect_true("data.frame" %in% class(AB))
  expect_named(AB, c("elm", "phase3", "stg", "ab", "ab_se"))
  expect_equal(nrow(AG), nrow(AB))
})

test_that("expand_AB() returns AB for each pixels depending on stg", {
  AB_expanded <- expand_AB(
    data.frame(
      elm = "A",
      phase3 = c("a", "b"),
      stg = c("11", "11", "12", "12"),
      ab = 1:4,
      ab_se = 5:8
    ),
    stg = c("11", "12")
  )

  ans <- list(
    A = list(
      ab = data.frame(a = c(1L, 3L), b = c(2L, 4L)),
      ab_se = data.frame(a = c(5L, 7L), b = c(6L, 8L))
    )
  )

  expect_identical(AB_expanded, ans)
})

if (interactive()) setwd(here::here())
