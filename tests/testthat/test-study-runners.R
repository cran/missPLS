test_that("simulation study returns the expected schema and summarises cleanly", {
  results <- run_simulation_study(
    dimensions = list(c(25, 10)),
    true_ncomp = 2,
    missing_props = 10,
    mechanisms = "MCAR",
    reps = 3,
    seed = 1,
    max_ncomp = 4,
    criteria = c("Q2-LOO", "AIC"),
    incomplete_methods = "nipals_standard",
    imputation_methods = "knn",
    folds = 5
  )

  expect_named(results, c(
    "study", "dataset", "n", "p", "true_ncomp", "target_ncomp", "mechanism",
    "missing_prop", "method", "criterion", "selected_ncomp", "matched_target",
    "runtime_sec", "seed", "status", "notes"
  ))

  summary_df <- summarize_simulation_study(results)
  expect_true(nrow(summary_df) > 0)

  complete_rows <- results[results$method == "Complete" & results$status == "ok", , drop = FALSE]
  q2_rate <- mean(complete_rows$matched_target[complete_rows$criterion == "Q2-LOO"])
  aic_rate <- mean(complete_rows$matched_target[complete_rows$criterion == "AIC"])
  expect_true(is.finite(q2_rate))
  expect_true(is.finite(aic_rate))
  expect_true(q2_rate >= aic_rate)
})

test_that("real-data study smoke test runs on bromhexine", {
  results <- run_real_data_study(
    dataset = "bromhexine",
    seed = 1,
    missing_props = 5,
    mechanisms = "MCAR",
    reps = 1,
    baseline_reps = 3,
    max_ncomp = 4,
    criteria = c("Q2-10fold"),
    incomplete_methods = "nipals_standard",
    imputation_methods = "knn",
    folds = 5
  )

  expect_true(nrow(results) > 0)
  expect_true(any(results$dataset == "bromhexine"))
})

test_that("study runners do not impose a default seed", {
  set.seed(303)
  results <- run_simulation_study(
    dimensions = list(c(20, 10)),
    true_ncomp = 2,
    missing_props = numeric(0),
    mechanisms = character(0),
    reps = 1,
    seed = NULL,
    max_ncomp = 3,
    criteria = "AIC"
  )

  expect_true(nrow(results) > 0)
  expect_true(all(is.na(results$seed)))
})
