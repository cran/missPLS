test_that("complete-data selection matches direct plsRglm outputs", {
  sim <- simulate_pls_data(n = 30, p = 12, true_ncomp = 2, seed = 1)
  PLS_lm <- missPLS:::PLS_lm

  sel_aic <- select_ncomp(sim$x, sim$y, method = "complete", criterion = "AIC", max_ncomp = 4, seed = 2)
  fit <- PLS_lm(dataY = sim$y, dataX = sim$x, nt = 4, verbose = FALSE)
  direct_aic <- unname(which.min(fit$InfCrit[, "AIC"]) - 1L)
  expect_identical(sel_aic$selected_ncomp, direct_aic)

  sel_q2 <- select_ncomp(sim$x, sim$y, method = "complete", criterion = "Q2-10fold", max_ncomp = 4, seed = 2, folds = 5)
  PLS_lm_kfoldcv <- missPLS:::PLS_lm_kfoldcv
  set.seed(2)
  cv <- plsRglm::kfolds2CVinfos_lm(
    plsRglm::cv.plsR(object = sim$y, dataX = sim$x, nt = 4, K = 5, verbose = FALSE),
    MClassed = FALSE,
    verbose = FALSE
  )[[1]]
  direct_q2 <- missPLS:::.q2_selected_ncomp(cv[-1, "Q2_Y"], threshold = 0.0975)
  expect_identical(sel_q2$selected_ncomp, direct_q2)
})

test_that("complete-data Q2 uses the documented cv.plsR interface", {
  sim <- simulate_pls_data(n = 30, p = 12, true_ncomp = 2, seed = 1)
  PLS_lm <- missPLS:::PLS_lm
  PLS_lm_kfoldcv <- missPLS:::PLS_lm_kfoldcv

  set.seed(11)
  via_cv_plsR <- plsRglm::kfolds2CVinfos_lm(
    plsRglm::cv.plsR(object = sim$y, dataX = sim$x, nt = 4, K = 5, verbose = FALSE),
    MClassed = FALSE,
    verbose = FALSE
  )[[1]]

  set.seed(11)
  via_kfold <- plsRglm::kfolds2CVinfos_lm(
    plsRglm::PLS_lm_kfoldcv(dataY = sim$y, dataX = sim$x, nt = 4, K = 5, verbose = FALSE),
    MClassed = FALSE,
    verbose = FALSE
  )[[1]]

  expect_identical(via_cv_plsR, via_kfold)
})

test_that("incomplete-data selection returns a stable schema", {
  sim <- simulate_pls_data(n = 30, p = 12, true_ncomp = 2, seed = 1)
  miss <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)
  sel <- select_ncomp(
    x = miss$x_incomplete,
    y = sim$y,
    method = "nipals_standard",
    criterion = "Q2-LOO",
    max_ncomp = 4,
    seed = 4
  )

  expect_named(sel, c("selection_method", "criterion", "selected_ncomp", "criterion_value",
    "max_ncomp", "seed", "n_imputations", "status", "notes"))
  expect_equal(nrow(sel), 1L)
})
