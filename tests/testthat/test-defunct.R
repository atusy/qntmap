context("defunct.R")

test_that("Defunct functions after 0.3.1", {
  expect_error(qntmap_quantify())
  expect_error(qltmap_cls_centers())
  expect_error(qltmap_load())
  expect_error(qntmap_cls_pois())
  expect_error(qnt_load())
})
