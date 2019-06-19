context("test-read-xmap-.R")

if (interactive()) setwd(here::here("tests/testthat"))

wd <- "minimal/.map/1"

prepare <- function() {
  unlink(file.path(wd, "xmap.RDS"))
}

test_that("read_cnd", {

  # files_cnd <- dir(wd, pattern = formals(read_xmap)$.cnd, full.names = TRUE)
  # expect_gt(length(files_cnd), 1)
  # expect_true(all(file.exists(files_cnd)))
  # expect_true(is.function(read_xmap_cnd))
  # expect_equal(
  #   patterns_xmap_cnd,
  #   c(elm = "XM_ELEMENT|XM_ELEM_NAME%0|XM_ELEM_IMS_SIGNAL_TYPE%0",
  #     dwell = "XM_DWELL_TIME|XM_AP_SA_DWELL_TIME%0", current = "CM_CURRENT|XM_DATA_PROBE_CURRENT",
  #     start = "XM_MAP_START|XM_AP_SA_STAGE_POS%0_1", pixel = "XM_POINTS|XM_AP_SA_PIXELS%0",
  #     step = "XM_STEP_SIZE|XM_AP_SA_PIXEL_SIZE%0", instrument = "CM_INSTRUMENT|XM_ANALYSIS_INSTRUMENT"
  #   )
  # )
  # expect_equal(
  #   collapse_patterns(patterns_xmap_cnd),
  #   "^\\$(XM_ELEMENT|XM_ELEM_NAME%0|XM_ELEM_IMS_SIGNAL_TYPE%0|XM_DWELL_TIME|XM_AP_SA_DWELL_TIME%0|CM_CURRENT|XM_DATA_PROBE_CURRENT|XM_MAP_START|XM_AP_SA_STAGE_POS%0_1|XM_POINTS|XM_AP_SA_PIXELS%0|XM_STEP_SIZE|XM_AP_SA_PIXEL_SIZE%0|CM_INSTRUMENT|XM_ANALYSIS_INSTRUMENT)[:blank:]"
  # )
  # expect_true(file.exists("minimal/.map/1/1.cnd"))

  expect_equal(
    read_xmap_cnd("minimal/.map/1/1.cnd", patterns = NULL),
    c("$CM_INSTRUMENT      JXA-DUMMY", "$CM_CURRENT         1e-07",
      "$XM_ELEMENT         Si", "$XM_DWELL_TIME      100 msec", "$XM_STEP_SIZE       1000 1000  um",
      "$XM_MAP_START       0 0 0 mm", "$XM_POINTS          100 100"
    )
  )
  cnd <- read_xmap_cnd("minimal/.map/1/1.cnd", patterns = patterns_xmap_cnd)
  expect_equal(
    cnd,
    list(instrument = "JXA-DUMMY", current = "1e-07", elm = "Si",
      dwell = c("100", "msec"), step = c("1000", "1000", "um"),
      start = c("0", "0", "0", "mm"), pixel = c("100", "100"))
  )
})

test_that("qm_xmap is data frame", {
  prepare()
  xm <- read_xmap(wd)

  expect_named(xm, c("x", "y", "Si", "Mg"))
  expect_false(file.exists(file.path(wd, "xmap.RDS")))
})

test_that("conditions = hoge", { # © 2018 JAMSTEC
  prepare()
  cd <- getwd()
  xm <- read_xmap(wd)
  setwd("minimal")
  xm2 <- read_xmap(conditions = "conditions_xmap.csv")
  setwd(cd)
  attr_xm <- attributes(xm)
  attr_xm2 <- attributes(xm2)
  expect_true(setequal(names(attr_xm), names(attr_xm2)))
  nm <- setdiff(names(attr_xm), "start")
  expect_equal(attr_xm[nm], attr_xm2[nm])
  expect_equal(xm[seq_along(xm)], xm2[seq_along(xm2)])
})

prepare()

test_that("correct_deadtime()", {
  devtools::load_all()
  x <- data.frame(x = 10000, y = 10000, CP = 10000, Si = 10000)
  y <- structure(correct_deadtime(x, 1, 100), deadtime = 1, dwell = 100)
  
  # Calculated correctly
  expect_equal(y$Si, 10001)
  
  # Elements belonging to "x", "y", and .electron are unaffected
  expect_equal(x[1:3], y[1:3])
  
  # deadtime = 0 will revert correction
  z <- correct_deadtime(y, 0)
  expect_equal(z$Si, x$Si)
  
  # deadtime correction works on the value already affected
  expect_identical(correct_deadtime(y, 1), y)
})

if (interactive()) setwd(here())
