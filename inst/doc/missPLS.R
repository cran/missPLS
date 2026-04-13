## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(missPLS)

sim <- simulate_pls_data(n = 30, p = 12, true_ncomp = 2, seed = 1)
str(sim, max.level = 1)

## -----------------------------------------------------------------------------
miss <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)

select_ncomp(
  x = miss$x_incomplete,
  y = sim$y,
  method = "nipals_standard",
  criterion = "Q2-LOO",
  max_ncomp = 4,
  seed = 3
)

## -----------------------------------------------------------------------------
imp <- impute_pls_data(miss$x_incomplete, method = "knn", seed = 4)

select_ncomp(
  x = imp,
  y = sim$y,
  method = "complete",
  criterion = "Q2-10fold",
  max_ncomp = 4,
  seed = 5,
  folds = 5
)

## -----------------------------------------------------------------------------
diag_bromhexine <- diagnose_real_data("bromhexine")
head(diag_bromhexine$predictor_correlations)
head(diag_bromhexine$response_correlations)

## -----------------------------------------------------------------------------
results <- run_simulation_study(
  dimensions = list(c(25, 10)),
  true_ncomp = 2,
  missing_props = 10,
  mechanisms = "MCAR",
  reps = 2,
  seed = 1,
  max_ncomp = 4,
  criteria = c("Q2-LOO", "AIC"),
  incomplete_methods = "nipals_standard",
  imputation_methods = "knn",
  folds = 5
)

summarize_simulation_study(results)

