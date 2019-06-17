context("test-find-AG.R")

if (interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1")
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
cluster <- cluster_xmap(xmap, find_centers(xmap, qnt, saveas = FALSE), saving = FALSE)
epma <- tidy_epma_for_quantify(
  qnt, xmap, cluster,
  maps_x = attr(xmap, "pixel")[1],
  maps_y = attr(xmap, "pixel")[2],
  elements = qnt$elm$elem
)


test_that("find_AG() returns data frame", {
  AG <- find_AG(epma)
  B <- find_B(epma)
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
