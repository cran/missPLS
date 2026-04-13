#' Run a simulation study
#'
#' Run the simulation workflows used in the article and thesis.
#'
#' @param dimensions List of `(n, p)` integer pairs.
#' @param true_ncomp Vector of true component counts.
#' @param missing_props Missing-data proportions as fractions or percentages.
#' @param mechanisms Missing-data mechanisms.
#' @param reps Number of replicates.
#' @param seed Optional base random seed.
#' @param max_ncomp Maximum number of extracted components.
#' @param criteria Criteria evaluated on complete and imputed data.
#' @param incomplete_methods Incomplete-data NIPALS workflows.
#' @param imputation_methods Imputation methods.
#' @param folds Number of folds used by `"Q2-10fold"`.
#' @param mar_y_bias MAR bias parameter passed to [add_missingness()].
#'
#' @return A data frame with one row per study run.
#' @export
run_simulation_study <- function(
    dimensions = list(c(500L, 100L), c(500L, 20L), c(100L, 20L), c(80L, 25L),
                      c(60L, 33L), c(40L, 50L), c(20L, 100L)),
    true_ncomp = c(2L, 4L, 6L),
    missing_props = seq(5, 50, 5),
    mechanisms = c("MCAR", "MAR"),
    reps = 1L,
    seed = NULL,
    max_ncomp = 8L,
    criteria = c("Q2-LOO", "Q2-10fold", "AIC", "AIC-DoF", "BIC", "BIC-DoF"),
    incomplete_methods = c("nipals_standard", "nipals_adaptative"),
    imputation_methods = c("mice", "knn", "svd"),
    folds = 10L,
    mar_y_bias = 0.8) {
  seed <- .normalize_seed(seed, allow_null = TRUE)
  reps <- as.integer(reps)
  max_ncomp <- as.integer(max_ncomp)

  rows <- vector("list", 0L)
  row_id <- 0L
  counter <- 0L

  for (dim_pair in dimensions) {
    n <- as.integer(dim_pair[[1L]])
    p <- as.integer(dim_pair[[2L]])
    dataset_name <- sprintf("sim_n%d_p%d", n, p)

    for (target in as.integer(true_ncomp)) {
      for (rep_id in seq_len(reps)) {
        counter <- counter + 1L
        sim_seed <- .seed_with_offset(seed, counter * 1000L)
        sim <- simulate_pls_data(n = n, p = p, true_ncomp = target, seed = sim_seed)

        for (criterion in criteria) {
          selection_seed <- .seed_with_offset(sim_seed, match(criterion, criteria))
          timing <- system.time(
            sel <- select_ncomp(
              x = sim$x,
              y = sim$y,
              method = "complete",
              criterion = criterion,
              max_ncomp = max_ncomp,
              seed = selection_seed,
              folds = folds
            )
          )
          row_id <- row_id + 1L
          rows[[row_id]] <- .study_row(
            study = "simulation",
            dataset = dataset_name,
            n = n,
            p = p,
            true_ncomp = target,
            target_ncomp = target,
            mechanism = NA_character_,
            missing_prop = 0,
            method = "Complete",
            criterion = criterion,
            selected_ncomp = sel$selected_ncomp,
            matched_target = !is.na(sel$selected_ncomp) && sel$selected_ncomp == target,
            runtime_sec = unname(timing["elapsed"]),
            seed = selection_seed,
            status = sel$status,
            notes = sel$notes
          )
        }

        if (!length(missing_props) || !length(mechanisms)) {
          next
        }

        for (mechanism in mechanisms) {
          for (missing_prop in missing_props) {
            miss_seed <- .seed_with_offset(sim_seed, as.integer(round(.normalize_missing_prop(missing_prop) * 1000)) +
              if (mechanism == "MAR") 50000L else 0L)
            miss <- add_missingness(
              x = sim$x,
              y = sim$y,
              mechanism = mechanism,
              missing_prop = missing_prop,
              seed = miss_seed,
              mar_y_bias = mar_y_bias
            )

            for (selection_method in incomplete_methods) {
              for (criterion in criteria) {
                selection_seed <- .seed_with_offset(miss_seed, row_id + 17L)
                timing <- system.time(
                  sel <- select_ncomp(
                    x = miss$x_incomplete,
                    y = sim$y,
                    method = selection_method,
                    criterion = criterion,
                    max_ncomp = max_ncomp,
                    seed = selection_seed,
                    folds = folds
                  )
                )
                row_id <- row_id + 1L
                rows[[row_id]] <- .study_row(
                  study = "simulation",
                  dataset = dataset_name,
                  n = n,
                  p = p,
                  true_ncomp = target,
                  target_ncomp = target,
                  mechanism = mechanism,
                  missing_prop = .normalize_missing_prop(missing_prop) * 100,
                  method = .selection_method_label(selection_method),
                  criterion = criterion,
                  selected_ncomp = sel$selected_ncomp,
                  matched_target = !is.na(sel$selected_ncomp) && sel$selected_ncomp == target,
                  runtime_sec = unname(timing["elapsed"]),
                  seed = selection_seed,
                  status = sel$status,
                  notes = sel$notes
                )
              }
            }

            for (imp_method in imputation_methods) {
              imputation_seed <- .seed_with_offset(miss_seed, 100000L + row_id)
              imp <- tryCatch(
                impute_pls_data(
                  x = miss$x_incomplete,
                  method = imp_method,
                  seed = imputation_seed,
                  m = max(1L, as.integer(round(.normalize_missing_prop(missing_prop) * 100)))
                ),
                error = function(e) e
              )

              for (criterion in criteria) {
                if (inherits(imp, "error")) {
                  sel <- .selection_result(
                    selection_method = paste0("complete_", imp_method),
                    criterion = criterion,
                    selected_ncomp = NA_integer_,
                    criterion_value = NA_real_,
                    max_ncomp = max_ncomp,
                    seed = imputation_seed,
                    status = "error",
                    notes = conditionMessage(imp)
                  )
                  elapsed <- NA_real_
                } else {
                  selection_seed <- .seed_with_offset(imputation_seed, match(criterion, criteria))
                  timing <- system.time(
                    sel <- select_ncomp(
                      x = imp,
                      y = sim$y,
                      method = "complete",
                      criterion = criterion,
                      max_ncomp = max_ncomp,
                      seed = selection_seed,
                      folds = folds
                    )
                  )
                  elapsed <- unname(timing["elapsed"])
                }

                row_id <- row_id + 1L
                rows[[row_id]] <- .study_row(
                  study = "simulation",
                  dataset = dataset_name,
                  n = n,
                  p = p,
                  true_ncomp = target,
                  target_ncomp = target,
                  mechanism = mechanism,
                  missing_prop = .normalize_missing_prop(missing_prop) * 100,
                  method = .imputation_method_label(imp_method),
                  criterion = criterion,
                  selected_ncomp = sel$selected_ncomp,
                  matched_target = !is.na(sel$selected_ncomp) && sel$selected_ncomp == target,
                  runtime_sec = elapsed,
                  seed = if (inherits(imp, "error")) imputation_seed else selection_seed,
                  status = sel$status,
                  notes = sel$notes
                )
              }
            }
          }
        }
      }
    }
  }

  do.call(rbind, rows)
}
