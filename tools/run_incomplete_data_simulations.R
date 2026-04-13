#!/usr/bin/env Rscript

source("tools/repro_helpers.R")
load_misspls()

reps <- as.integer(Sys.getenv("MISSPLS_REPS", "1000"))
output_root <- Sys.getenv("MISSPLS_OUTPUT_DIR", unset = "")
if (!nzchar(output_root)) {
  output_root <- tempdir()
}
output_dir <- make_output_dir("incomplete-data", root_dir = output_root)

results <- missPLS::run_simulation_study(
  dimensions = list(c(500L, 100L), c(500L, 20L), c(100L, 20L), c(80L, 25L),
                    c(60L, 33L), c(40L, 50L), c(20L, 100L)),
  true_ncomp = c(2L, 4L, 6L),
  missing_props = seq(5, 50, 5),
  mechanisms = c("MCAR", "MAR"),
  reps = reps,
  seed = 20250407L,
  max_ncomp = 8L,
  criteria = c("Q2-LOO", "Q2-10fold", "AIC", "AIC-DoF", "BIC", "BIC-DoF"),
  incomplete_methods = c("nipals_standard", "nipals_adaptative"),
  imputation_methods = c("mice", "knn", "svd"),
  folds = 10L
)

summary_df <- missPLS::summarize_simulation_study(results)
save_outputs(output_dir, "incomplete-data", results, summary_df)
write_manifest(output_dir, list(reps = reps, script = "run_incomplete_data_simulations.R"))
write_match_plot(output_dir, "incomplete-data", summary_df, "Incomplete-data simulation match rates")
write_discrepancy_report(output_dir, "Incomplete data simulations", summary_df)
