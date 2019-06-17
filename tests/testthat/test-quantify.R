if (interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1")
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
cluster <- cluster_xmap(xmap, find_centers(xmap, qnt, saveas = FALSE), saving = FALSE)


# maps_x = attr(xmap, "pixel")[1L]
# maps_y = attr(xmap, "pixel")[2L]
# fine_phase = NULL
# fine_th = 0.9
# fix = NULL
# se = FALSE
# saving = FALSE

epma <- tidy_epma_for_quantify(
  qnt, xmap, cluster,
  maps_x = attr(xmap, "pixel")[1L],
  maps_y = attr(xmap, "pixel")[2L],
  elements = qnt$elm$elem
)

params <- tidy_params(find_AG(epma), find_B(epma), qnt)



context("quantify.R - check_ABG()") # © 2018 JAMSTEC

test_that("check_ABG() returns FALSE", {
  # when params = list()
  expect_false(check_ABG(list()))

  # when parameters are fixed only by wt
  expect_false(check_ABG(data.frame(oxide = "a", phase = "a", wt = 0)))
})

test_that("check_ABG() returns error", {
  # when params is a data frame without required columns
  expect_error(check_ABG(data.frame()))

  # when params is a data frame without required rows
  expect_error(check_ABG(params, xmap = alist(Mg = , Si = , Ti = ), cls = cluster))
  expect_error(check_ABG(params, xmap = xmap, cls = list(cluster = c("A"))))
})

test_that("check_ABG() returns TRUE", {
  # when params is an expected data frame
  expect_true(check_ABG(params, xmap = xmap, cls = cluster))

  # even if xmap contains SE/CP/TP
  for (i in .electron)
    expect_true(check_ABG(params, xmap = c(xmap, setNames(i, i)), cls = cluster))
})



"quantify(se = TRUE)"

test_that("quantify() returns a qntmap class object", {
  .qmap <- quantify(xmap, qnt, cluster, se = TRUE, saving = TRUE)
  dir_qmap <- "minimal/.map/1/qntmap"
  expect_s3_class(.qmap, c("qntmap", "data.frame"))
  expect_named(.qmap, c("x", "y", "SiO2", "SiO2.se", "MgO", "MgO.se", "Total", "Total.se"))
  expect_true(all(
    c("MgO_se.csv", "MgO.csv", "parameters.csv", "qntmap.RDS",
      "SiO2_se.csv", "SiO2.csv", "Total_se.csv", "Total.csv") %in%
      dir(dir_qmap)
  ))
  
  # Partially inhertis attributes from qm_xmap object
  attrs <- c('pixel', 'step')
  expect_identical(
    attributes(.qmap)[attrs], 
    attributes(xmap)[attrs]
  )
  unlink(dir_qmap, recursive = TRUE)
})



"quantify(se = FALSE)"

test_that("quantify() doubles result if alpha is doubled by fix parameter", { # © 2018 JAMSTEC
  .k <- 2L
  .params <- params
  .params$alpha <- params$alpha * .k
  csv <- "params.csv"
  data.table::fwrite(.params, csv)
  .qmap1 <- quantify(xmap, qnt, cluster, se = FALSE,  saving = FALSE)
  .qmap2 <- quantify(xmap, NULL, cluster, se = FALSE,  saving = FALSE, fix = csv)

  ratios <- unique(unlist(.qmap2[-(1:2)] / .qmap1[-(1:2)]))
  
  expect_true(all(ratios[!is.nan(ratios)] - 2 < 1e-10))

  unlink(csv)
})




test_that("csv given to fix parameter does nothing if all values in column are NA", { # © 2018 JAMSTEC
  .params <- params
  .params[
    c("elint", "alpha", "beta", "gamma", "alpha_se", "beta_se", "gamma_se")
  ] <- NULL
  .params$wt <- NA_real_
  csv <- "params.csv"
  data.table::fwrite(.params, csv)

  # se = FALSE
  expect_identical(
    quantify(xmap, qnt, cluster, se = FALSE, saving = FALSE),
    quantify(xmap, qnt, cluster, se = FALSE, saving = FALSE, fix = csv)
  )

  unlink(csv)

})

test_that("quantify() gives 200 wt% for SiO2 in Qtz by fix parameter", {
  .params <- params
  .params$wt <- NA_real_
  .params$wt[.params$phase == "Qtz" & .params$element == "Si"] <- 200
  csv <- "params.csv"
  data.table::fwrite(.params, csv)

  .mean <- mean(
    quantify(xmap, qnt, cluster, se = FALSE, saving = FALSE, fix = csv), 
    index = cluster$cluster
  )
  expect_equal(200, round(.mean$Qtz[.mean$Element == "SiO2"], -2))

  unlink(csv)
})

if (interactive()) here::here()
