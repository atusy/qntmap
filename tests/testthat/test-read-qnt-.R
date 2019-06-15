if (interactive()) setwd(here::here("tests/testthat"))

wd <- "minimal/.qnt"
prepare <- function() {
  files <- c("qnt.RDS", "phase_list0.csv", "phase_list.csv")
  unlink(c(files, file.path(wd, files)))
}

"Returned value"

test_that("list structure", {
  prepare()
  qnt <- read_qnt(wd, phase_list = NULL, saving = FALSE)

  # names
  expect_named(qnt, c("elm", "cnd", "cmp"))
  expect_named(qnt$elm, c("elem", "elint", "bgp_pos", "bgm_pos", "pk_t", "bg_t"))
  expect_named(qnt$cnd, c("id", "x", "y", "z", "comment", "beam", "phase"))
  expect_true(all(c("bgm", "bgp", "net", "wt") %in% names(qnt$cmp)))
})

"Param: phase_list"

test_that("phase_list = NULL", {
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, saving = FALSE)
  expect_equal(qnt$cnd$comment, qnt$cnd$phase)
})

test_that("phase_list = 'example.csv'", {
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, saving = TRUE)

  p <- read.csv("phase_list0.csv")
  p$phase <- "a"
  p$use <- rep_len(c(TRUE, FALSE), nrow(p))
  csv <- "example.csv"
  write.csv(p, csv, row.names = FALSE)

  qnt2 <- read_qnt(wd, phase_list = csv, saving = FALSE)

  expect_false(identical(qnt$cnd$phase, qnt2$cnd$phase))
  expect_true(any(is.na(qnt2$cnd$phase)))
  expect_true(all(qnt2$cnd$phase %in% c("a", NA)))
  unlink(csv)
})

test_that("phase_list = 'does-not-exist'", {
  prepare()
  expect_error(read_qnt(wd, phase_list = "does-not-exist", saving = FALSE))
})


"Param: params" # © 2018 JAMSTEC

test_that("params = hoge.csv", { # © 2018 JAMSTEC
  prepare()

  qnt <- read_qnt(wd, phase_list = NULL, saving = FALSE)

  qnt$elm$elem <- qnt$elm$elint <- c(Si = "SiO2", Mg = "MgO")[qnt$elm$elint]
  qnt$elm[sapply(qnt$elm, is.numeric)] <- 1000

  csv <- "params_qnt.csv"
  write.csv(
    dplyr::transmute(
      qnt$elm,
      Oxide       = elem,
      Element     = elint,
      `Bg+ [mm]`  = bgp_pos,
      `Bg- [mm]`  = bgm_pos,
      `Peak [sec]` = pk_t,
      `Bg [sec]`  = bg_t
    ),
    csv, row.names = FALSE
  )

  qnt2 <- read_qnt(
    wd, phase_list = NULL, conditions = csv, saving = FALSE
  )

  expect_equal(qnt$elm, qnt2$elm)

  unlink(csv)
})


prepare()

if (interactive()) setwd(here())
