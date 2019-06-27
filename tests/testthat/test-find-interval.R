d <- data.frame(x = c(11:13), y = 11:13)
nm <- names(d)

test_that("Returns input asis if vars are not selected", {
  expect_identical(find_interval(d), d)
})

test_that("New vars are suffixed by .L and .H", {
  expect_named(find_interval(d, "x"), c(nm, "x.L", "x.H"))
  expect_named(find_interval(d, "x", "y"), c(nm, "x.L", "y.L", "x.H", "y.H"))
})

test_that("Intervals are right values", {
  expect_equal(
    find_interval(d, "x")[c("x.L", "x.H")],
    data.frame(
      x.L = qnbinom(.025, d$x, .5),
      x.H = qnbinom(.975, d$x, .5)
    )
  )
  expect_equal(
    find_interval(d, "x", type = "credible")[c("x.L", "x.H")],
    data.frame(
      x.L = qgamma(.025, d$x),
      x.H = qgamma(.975, d$x)
    )
  )
})
