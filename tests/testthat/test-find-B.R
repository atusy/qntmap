context("test-find-B.R")

if (interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1", saving = FALSE)
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
cluster <- cluster_xmap(xmap, find_centers(xmap, qnt, saveas = FALSE), saving = FALSE)
epma <- tidy_epma_for_quantify(
  tidy_epma(qnt, xmap, cluster),
  maps_x = attr(xmap, "pixel")[1],
  maps_y = attr(xmap, "pixel")[2],
  elements = qnt$elm$elem
)

test_that("lm_B(): If `isTRUE(all(mem == 0))`, then coef and vcov return NA", {
  B <- lm_B(mutate(epma, mem = 0))
  expect_true(all(is.na(B$b)))
  expect_true(all(is.na(B$b_se)))
  B <- lm_B(mutate(epma, mem = 0), elm)
  expect_true(all(is.na(B$b)))
  expect_true(all(is.na(B$b_se)))
  B <- lm_B(mutate(epma, mem = 0), elm, stg)
  expect_true(all(is.na(B$b)))
  expect_true(all(is.na(B$b_se)))
})

test_that("lm_B(): returns data frame named at least by 'b' and 'b_se'", {
  B <- lm_B(epma)
  nm <- c("b", "b_se")
  expect_true("data.frame" %in% class(B))
  expect_named(lm_B(epma, elm), c("elm", nm))
  expect_named(lm_B(epma, elm, stg), c("elm", "stg", nm))
})

# find_B()

test_that("find_B(): returns a data frame", {
  B <- find_B(epma)
  expect_equal(nrow(B), 2)
  expect_named(B, c("elm", "stg", "b", "b_se"))
})

test_that("find_B(): In case map should be divided into smaller maps (e.g., guidenet map)", {
  B <- find_B(
    tibble::add_row(
      epma,
      mem = 0,
      mapint = 0, pkint = 0,
      stg = "dummy",
      elm = unique(epma$elm),
      dwell = epma$dwell[1],
      beam_map = epma$beam_map[1]
    )
  )
  nm <- setdiff(names(B), "stg")
  B_11 <- B[B$stg == "11", nm]
  B_11 <- B_11[order(B_11$elm), ]
  B_dummy <- B[B$stg == "dummy", nm]
  B_dummy <- B_dummy[order(B_dummy$elm), ]
  rownames(B_11) <- rownames(B_dummy) <- NULL
  expect_identical(B_11, B_dummy)
})

if (interactive()) setwd(here::here())
