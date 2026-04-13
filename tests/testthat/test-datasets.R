test_that("packaged datasets have the expected dimensions", {
  data("bromhexine", package = "missPLS", envir = environment())
  data("tetracycline", package = "missPLS", envir = environment())
  data("octane", package = "missPLS", envir = environment())
  data("ozone_complete", package = "missPLS", envir = environment())

  expect_s3_class(bromhexine, "misspls_dataset")
  expect_identical(dim(bromhexine$x), c(23L, 64L))
  expect_length(bromhexine$y, 23L)

  expect_s3_class(tetracycline, "misspls_dataset")
  expect_identical(dim(tetracycline$x), c(107L, 101L))
  expect_length(tetracycline$y, 107L)

  expect_s3_class(octane, "misspls_dataset")
  expect_identical(dim(octane$x), c(68L, 493L))
  expect_length(octane$y, 68L)

  expect_s3_class(ozone_complete, "misspls_dataset")
  expect_identical(dim(ozone_complete$x), c(203L, 12L))
  expect_length(ozone_complete$y, 203L)
})
