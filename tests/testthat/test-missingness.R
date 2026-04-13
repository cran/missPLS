test_that("missingness generation is deterministic and avoids all-missing rows", {
  sim <- simulate_pls_data(n = 30, p = 12, true_ncomp = 2, seed = 1)
  miss_1 <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)
  miss_2 <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)

  expect_identical(miss_1$x_incomplete, miss_2$x_incomplete)
  expect_false(any(apply(is.na(miss_1$x_incomplete), 1L, all)))
  expect_gt(mean(is.na(miss_1$x_incomplete)), 0)
})

test_that("MAR missingness is deterministic", {
  sim <- simulate_pls_data(n = 30, p = 12, true_ncomp = 2, seed = 1)
  miss_1 <- add_missingness(sim$x, sim$y, mechanism = "MAR", missing_prop = 10, seed = 3)
  miss_2 <- add_missingness(sim$x, sim$y, mechanism = "MAR", missing_prop = 10, seed = 3)

  expect_identical(miss_1$x_incomplete, miss_2$x_incomplete)
  expect_false(any(apply(is.na(miss_1$x_incomplete), 1L, all)))
})

test_that("simulation and missingness can rely on external RNG state", {
  set.seed(101)
  sim_1 <- simulate_pls_data(n = 20, p = 10, true_ncomp = 2)
  set.seed(101)
  sim_2 <- simulate_pls_data(n = 20, p = 10, true_ncomp = 2)

  expect_identical(sim_1$x, sim_2$x)
  expect_true(is.na(sim_1$seed))

  set.seed(202)
  miss_1 <- add_missingness(sim_1$x, sim_1$y, mechanism = "MCAR", missing_prop = 10)
  set.seed(202)
  miss_2 <- add_missingness(sim_1$x, sim_1$y, mechanism = "MCAR", missing_prop = 10)

  expect_identical(miss_1$x_incomplete, miss_2$x_incomplete)
  expect_true(is.na(miss_1$seed))
})
