context("test-read-qnt-.R")

wd <- "minimal/.qnt"
prepare <- function() {
  files <- c("qnt.RDS", "phase_list0.csv", "phase_list.csv")
  unlink(c(files, file.path(wd, files)))
}

test_that("list structure", {
  prepare()
  qnt <- read_qnt(wd, phase_list = NULL, renew = TRUE, saving = FALSE)
  
  # names
  expect_named(qnt, c("elm", "cnd", "cmp"))
  expect_named(qnt$elm, c("elem", "elint", "bgp_pos", "bgm_pos", "pk_t", "bg_t"))
  expect_named(qnt$cnd, c("id", "x", "y", "z", "comment", "beam", "phase"))
  expect_true(all(c("bgm", "bgp", "net", "wt") %in% names(qnt$cmp)))
})

test_that("saving = FALSE", {
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, renew = TRUE, saving = FALSE)
  expect_false(file.exists(file.path(wd, "phase_list0.csv")))
  expect_false(file.exists(file.path(wd, "qnt.RDS")))
})

test_that("saving = TRUE", {
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, renew = TRUE, saving = TRUE)
  rds <- file.path(wd, "qnt.RDS")
  expect_true(file.exists(rds))
  expect_equal(qnt, readRDS(rds))
})


test_that("renew = FALSE", {
  prepare()

  saveRDS(list(), file.path(wd, "qnt.RDS"))
  expect_length(read_qnt(wd, phase_list = NULL, renew = FALSE, saving = FALSE), 0)
})

test_that("renew = TRUE", {
  prepare()

  saveRDS(list(), file.path(wd, "qnt.RDS"))
  expect_length(read_qnt(wd, phase_list = NULL, renew = TRUE, saving = FALSE), 3)
})

test_that("phase_list = NULL", {
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, renew = TRUE, saving = FALSE)
  expect_equal(qnt$cnd$comment, qnt$cnd$phase)
})

test_that("phase_list = 'example.csv'", {
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, renew = TRUE, saving = TRUE)

  csv <- "phase_list0.csv"
  p <- read.csv(csv)
  p$phase <- "a"
  p$use <- rep_len(c(TRUE, FALSE), nrow(p))
  write.csv(p, csv, row.names = FALSE)

  qnt2 <- read_qnt(wd, phase_list = "phase_list0.csv", renew = TRUE, saving = FALSE)

  expect_false(identical(qnt$cnd$phase, qnt2$cnd$phase))
  expect_true(any(is.na(qnt2$cnd$phase)))
  expect_true(all(qnt2$cnd$phase %in% c("a", NA)))
})

prepare()
