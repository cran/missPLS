#!/usr/bin/env Rscript

source("tools/repro_helpers.R")
load_misspls()

baseline_reps <- as.integer(Sys.getenv("MISSPLS_BASELINE_REPS", "100"))
output_root <- Sys.getenv("MISSPLS_OUTPUT_DIR", unset = "")
if (!nzchar(output_root)) {
  output_root <- tempdir()
}
output_dir <- make_output_dir("real-data", root_dir = output_root)

datasets <- c("bromhexine", "tetracycline", "ozone_complete", "octane")
results <- do.call(rbind, lapply(seq_along(datasets), function(i) {
  missPLS::run_real_data_study(
    dataset = datasets[[i]],
    seed = 20250407L + i,
    missing_props = seq(5, 50, 5),
    mechanisms = c("MCAR", "MAR"),
    reps = 1L,
    baseline_reps = baseline_reps,
    max_ncomp = 12L,
    criteria = c("Q2-LOO", "Q2-10fold", "AIC", "AIC-DoF", "BIC", "BIC-DoF"),
    incomplete_methods = c("nipals_standard", "nipals_adaptative"),
    imputation_methods = c("mice", "knn", "svd"),
    folds = 10L
  )
}))

summary_df <- missPLS::summarize_simulation_study(results)
save_outputs(output_dir, "real-data", results, summary_df)
write_manifest(output_dir, list(
  baseline_reps = baseline_reps,
  script = "run_real_data_studies.R",
  datasets = datasets
))
write_match_plot(output_dir, "real-data", summary_df, "Real-data study match rates")
write_discrepancy_report(output_dir, "Real data studies", summary_df)
