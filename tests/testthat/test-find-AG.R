context("test-find-AG.R")

if(interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1", saving = FALSE)
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
cluster <- cluster_xmap(xmap, find_centers(xmap, qnt, saveas = FALSE), saving = FALSE)
epma <- tidy_epma_for_quantify(
  tidy_epma(qnt, xmap, cluster),
  maps_x = attr(xmap, "pixel")[1],
  maps_y = attr(xmap, "pixel")[2],
  elements = qnt$elm$elem
)

# lm_AG

test_that("lm_AG(): returns data frame named at least by 'a', 'a_se', 'g', and 'g_se'", {
  AG <- lm_AG(epma)
  nm <- c("g", "g_se", "a", "a_se")
  expect_true("data.frame" %in% class(AG))
  expect_named(AG, nm)
  expect_named(lm_AG(epma, elm), c("elm", nm))
  expect_named(lm_AG(epma, elm, phase3), c("elm", "phase3", nm))
})

# find_AG()

test_that("find_AG(): returns a data frame without NA", {
  AG <- find_AG(epma)
  expect_equal(nrow(AG), 4)
  expect_named(AG, c("elm", "phase3", "g", "g_se", "a", "a_se"))
  expect_true(all(!is.na(AG$g)))
  expect_true(all(!is.na(AG$g_se)))
  expect_true(all(!is.na(AG$a)))
  expect_true(all(!is.na(AG$a_se)))
})



test_that("find_AG(): In case map should be divided into smaller maps (e.g., guidenet map)", {
  AG <- find_AG(epma, not_quantified = "A")
  expect_equal(nrow(AG), 6)
  expect_named(AG, c("elm", "phase3", "g", "g_se", "a", "a_se"))
  expect_true(all(!is.na(AG$g)))
  expect_true(all(!is.na(AG$g_se)))
  expect_true(all(!is.na(AG$a)))
  expect_true(all(!is.na(AG$a_se)))
})

if(interactive()) setwd(here::here())
