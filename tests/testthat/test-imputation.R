test_that("imputation wrappers return reproducible outputs", {
  sim <- simulate_pls_data(n = 25, p = 10, true_ncomp = 2, seed = 1)
  miss <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)

  imp_knn_1 <- impute_pls_data(miss$x_incomplete, method = "knn", seed = 3)
  imp_knn_2 <- impute_pls_data(miss$x_incomplete, method = "knn", seed = 3)
  expect_s3_class(imp_knn_1, "misspls_imputation")
  expect_identical(imp_knn_1$datasets[[1]], imp_knn_2$datasets[[1]])

  imp_svd <- impute_pls_data(miss$x_incomplete, method = "svd", seed = 4, svd_rank = 3, svd_maxiter = 50)
  expect_s3_class(imp_svd, "misspls_imputation")
  expect_false(anyNA(imp_svd$datasets[[1]]))

  imp_mice <- impute_pls_data(miss$x_incomplete, method = "mice", seed = 5, m = 2)
  expect_s3_class(imp_mice, "misspls_imputation")
  expect_length(imp_mice$datasets, 2L)
})
