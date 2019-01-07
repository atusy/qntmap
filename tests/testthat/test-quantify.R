context("quantify.R")

if(interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1", saving = FALSE)
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
cluster <- cluster_xmap(xmap, find_centers(xmap, qnt), saving = FALSE)
epma <- tidy_epma_for_quantify(
  tidy_epma(qnt, xmap, cluster),
  maps_x = attr(xmap, "pixel")[1],
  maps_y = attr(xmap, "pixel")[2],
  elements = qnt$elm$elem
)

params <- tidy_params(find_AG(epma), find_B(epma), qnt)



"check_ABG()"

test_that("check_ABG() returns FALSE", {
  # when params = list()
  expect_false(check_ABG(list()))
  
  # when parameters are fixed only by wt
  expect_false(check_ABG(data.frame(element = "a", phase = "a", wt = 0)))
})

test_that("check_ABG() returns error", {
  # when params is a data frame without required columns
  expect_error(check_ABG(data.frame()))
  
  # when params is a data frame without required rows
  expect_error(check_ABG(params, xmap = alist(Mg = , Si =, Ti =), cls = cls))
  expect_error(check_ABG(params, xmap = xmap, cls = list(cluster = c("A"))))
})

test_that("check_ABG() returns TRUE", {
  # when params is an expected data frame
  expect_true(check_ABG(params, xmap = xmap, cls = cluster))

  # even if xmap contains SE/CP/TP
  for(i in .electron)
    expect_true(check_ABG(params, xmap = c(xmap, setNames(i, i)), cls = cluster))
})



"quantify()"

test_that("quantify() returns a qntmap class object", {
  .qmap <- quantify(xmap, qnt, cluster, saving = TRUE)
  dir_qmap <- "minimal/.map/1/qntmap"
  expect_s3_class(.qmap, c("qntmap", "list"))
  expect_named(.qmap, c("SiO2", "MgO", "Total"))
  for (i in .qmap) expect_named(i, c("wt", "se"))
  for (i in .qmap) expect_type(i, "list")
  for (i in .qmap) for (j in i) expect_s3_class(j, "data.frame")
  expect_true(all(
    c("MgO_se.csv", "MgO_wt.csv", "parameters.csv", "qntmap.RDS", 
      "SiO2_se.csv", "SiO2_wt.csv", "Total_se.csv", "Total_wt.csv") %in%
      dir(dir_qmap)
  ))
  unlink(dir_qmap, recursive = TRUE)
})

test_that("quantify() doubles result if alpha is doubled by fix parameter", {
  .k <- 2L
  .params <- params
  .params$alpha <- params$alpha * .k
  csv <- "params.csv"
  data.table::fwrite(.params, csv)
  .qmap1 <- quantify(xmap, qnt, cluster, saving = FALSE)
  .qmap2 <- quantify(xmap, NULL, cluster, saving = FALSE, fix = csv)
  
  ratios <- unlist(.qmap2[[1]][["wt"]], use.names = FALSE) /
    unlist(.qmap1[[1]][["wt"]], use.names = FALSE)
  
  expect_true(all(sapply(ratios, all.equal, 2)))
  
  unlink(csv)
})

test_that("csv given to fix parameter does nothing if all values in column are NA", {
  .params <- params
  .params[
    c("elint", "alpha", "beta", "gamma", "alpha_se", "beta_se", "gamma_se")
    ] <- NULL
  .params$wt <- NA_real_
  csv <- "params.csv"
  data.table::fwrite(.params, csv)
  
  expect_identical(
    quantify(xmap, qnt, cluster, saving = FALSE), 
    quantify(xmap, qnt, cluster, saving = FALSE, fix = csv)
  )
  
  unlink(csv)

})

test_that("quantify() gives 200 wt% for SiO2 in Qtz by fix parameter", {
  .params <- params
  .params$wt <- NA_real_
  .params$wt[.params$phase == "Qtz" & .params$elint == "Si"] <- 200
  csv <- "params.csv"
  data.table::fwrite(.params, csv)

  .mean <- mean(quantify(xmap, qnt, cluster, saving = FALSE, fix = csv), index = cluster$cluster)
  expect_equal(200, round(.mean$Qtz[.mean$Element == "SiO2"], -2))
  
  unlink(csv)
})

if(interactive()) here::here()
