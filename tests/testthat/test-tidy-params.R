context("tidy_params()")

if (interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1")
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

test_that("Structure of a returned value by tidy_params()", {
  params <- tidy_params(AG = AG, B = B, qnt = qnt)
  expect_s3_class(params, "data.frame")
  expect_named(params, c("oxide", "element", "phase", "alpha", "beta", "gamma", "wt"))
  expect_equal(
    unname(sapply(params, class)),
    c("character", "character", "character", "numeric", "numeric", "numeric", "numeric")
  )
})
