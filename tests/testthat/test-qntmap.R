context("qntmap.R")

if(interactive()) setwd(here::here("tests/testthat"))

xmap <- read_xmap("minimal/.map/1", saving = FALSE)
xmap2 <- xmap
xmap2$Ti <- xmap$Si
qnt <- read_qnt("minimal/.qnt", saving = FALSE)
qnt2 <- list()
qnt2$elm <- dplyr::bind_rows(qnt$elm, dplyr::mutate_at(qnt$elm, c("elem", "elint"), paste0, "_dummy"))
qnt2$cnd <- qnt$cnd
qnt2$cmp <- lapply(qnt$cmp, function(x) cbind(x, setNames(x, paste0(names(x), "_dummy"))))
cluster <- cluster_xmap(xmap, find_centers(xmap, qnt, saveas = FALSE), saving = FALSE)

test_that("Map analyzed more elements than spots", {
  
  centers <- find_centers(xmap2, qnt, saveas = FALSE)
  expect_s3_class(centers, "data.frame")
  
  cluster <- cluster_xmap(xmap2, centers, saving = FALSE)
  expect_s3_class(cluster, "qm_cluster")

  qmap <- quantify(xmap = xmap2, qnt = qnt, cluster = cluster)
  expect_s3_class(qmap, "qntmap")
})

test_that("Map analyzed less elements than spots", {
  centers <- find_centers(xmap, qnt2, saveas = FALSE)
  expect_s3_class(centers, "data.frame")
  
  cluster <- cluster_xmap(xmap, centers, saving = FALSE)
  expect_s3_class(cluster, "qm_cluster")
  
  qmap <- quantify(xmap = xmap, qnt = qnt2, cluster = cluster)
  expect_s3_class(qmap, "qntmap")
})

test_that("Map and spots are analyzing elements not analyzed in each other", {
  centers <- find_centers(xmap2, qnt2, saveas = FALSE)
  expect_s3_class(centers, "data.frame")
  
  cluster <- cluster_xmap(xmap2, centers, saving = FALSE)
  expect_s3_class(cluster, "qm_cluster")
  
  qmap <- quantify(xmap = xmap2, qnt = qnt2, cluster = cluster)
  expect_s3_class(qmap, "qntmap")
})

if(interactive()) setwd(here::here())
