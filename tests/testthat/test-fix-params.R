context("fix-params.R")

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

AG <- find_AG(epma)
B <- find_B(epma)
params <- tidy_params(AG, B, qnt)

test_that("Reconstruct AG from tidy parameters", {
  expect_equal(AG, fix_AG(params)[names(AG)])
})

test_that("Reconstruct B from tidy parameters", {
  expect_equal(B, fix_B(params)[names(B)])
})

if(interactive()) setwd(here::here())
