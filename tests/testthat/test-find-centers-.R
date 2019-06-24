if (interactive()) setwd(here::here("tests/testthat"))

dirs <- file.path("minimal", c(".map/1", ".qnt"))
xm <- read_xmap(dirs[1])
qn <- read_qnt(dirs[2], saving = FALSE)

test_that("find centers", {
  cn <- find_centers(xm, qn, saveas = FALSE)
  expect_named(cn, c("phase", "Si", "Mg"))
  expect_s3_class(cn, "data.frame")
})

test_that("find centers when MgO not quantified", {
  qn2 <- qn
  qn2$elm <- qn$elm[qn$elm$elem == "SiO2", ]
  qn2$cmp <- lapply(qn$cmp, `[`, "SiO2")
  cn <- find_centers(xm, qn2, saveas = FALSE) 
  expect_named(cn, c("phase", "Si", "Mg"))
  expect_s3_class(cn, "data.frame")
})

test_that("find centers when Qtz is fine grained", {
  cn <- find_centers(xm, qn, phase = -"Qtz", saveas = FALSE)
  cn2 <- find_centers(xm, qn, phase = -Qtz, saveas = FALSE)
  expect_named(cn, c("phase", "Si", "Mg"))
  expect_s3_class(cn, "data.frame")
  expect_identical(cn, cn2)
  
  # fine_phase is deprecated, but works
  f <- function() find_centers(xm, qn, fine_phase = "Qtz", saveas = FALSE)
  expect_warning(f())
  expect_identical(cn, suppressWarnings(f()))
})

if (interactive()) setwd(here::here())
