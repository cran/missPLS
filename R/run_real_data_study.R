#' Run a real-data study
#'
#' @param dataset A packaged dataset name or `misspls_dataset` object.
#' @param seed Optional base random seed.
#' @param missing_props Missing-data proportions as fractions or percentages.
#' @param mechanisms Missing-data mechanisms.
#' @param reps Number of replicate missingness draws for each mechanism and
#'   proportion.
#' @param baseline_reps Number of repeated complete-data `Q2-10fold` selections
#'   used to determine `t**`.
#' @param max_ncomp Maximum number of extracted components.
#' @param criteria Criteria evaluated on incomplete and imputed data.
#' @param incomplete_methods Incomplete-data NIPALS workflows.
#' @param imputation_methods Imputation methods.
#' @param folds Number of folds used by `"Q2-10fold"`.
#' @param mar_y_bias MAR bias parameter passed to [add_missingness()].
#'
#' @return A data frame with one row per study run.
#' @export
run_real_data_study <- function(
    dataset,
    seed = NULL,
    missing_props = seq(5, 50, 5),
    mechanisms = c("MCAR", "MAR"),
    reps = 1L,
    baseline_reps = 100L,
    max_ncomp = 12L,
    criteria = c("Q2-LOO", "Q2-10fold", "AIC", "AIC-DoF", "BIC", "BIC-DoF"),
    incomplete_methods = c("nipals_standard", "nipals_adaptative"),
    imputation_methods = c("mice", "knn", "svd"),
    folds = 10L,
    mar_y_bias = 0.8) {
  dataset <- .coerce_dataset(dataset)
  seed <- .normalize_seed(seed, allow_null = TRUE)
  reps <- as.integer(reps)
  baseline_reps <- as.integer(baseline_reps)

  rows <- vector("list", 0L)
  row_id <- 0L

  baseline_selections <- vector("list", baseline_reps)
  for (i in seq_len(baseline_reps)) {
    selection_seed <- .seed_with_offset(seed, i)
    timing <- system.time(
      baseline_selections[[i]] <- select_ncomp(
        x = dataset$x,
        y = dataset$y,
        method = "complete",
        criterion = "Q2-10fold",
        max_ncomp = max_ncomp,
        seed = selection_seed,
        folds = folds
      )
    )
    row_id <- row_id + 1L
    rows[[row_id]] <- .study_row(
      study = "real_data",
      dataset = dataset$name,
      n = nrow(dataset$x),
      p = ncol(dataset$x),
      true_ncomp = NA_integer_,
      target_ncomp = baseline_selections[[i]]$selected_ncomp,
      mechanism = NA_character_,
      missing_prop = 0,
      method = "Complete",
      criterion = "Q2-10fold",
      selected_ncomp = baseline_selections[[i]]$selected_ncomp,
      matched_target = NA,
      runtime_sec = unname(timing["elapsed"]),
      seed = selection_seed,
      status = baseline_selections[[i]]$status,
      notes = baseline_selections[[i]]$notes
    )
  }

  target_ncomp <- .mode_integer(vapply(baseline_selections, function(x) x$selected_ncomp, integer(1L)))

  for (mechanism in mechanisms) {
    for (missing_prop in missing_props) {
      for (rep_id in seq_len(reps)) {
        miss_seed <- .seed_with_offset(seed, 1000L * rep_id + match(mechanism, mechanisms) * 100L)
        miss <- add_missingness(
          x = dataset$x,
          y = dataset$y,
          mechanism = mechanism,
          missing_prop = missing_prop,
          seed = miss_seed,
          mar_y_bias = mar_y_bias
        )

        for (selection_method in incomplete_methods) {
          for (criterion in criteria) {
            selection_seed <- .seed_with_offset(miss_seed, row_id + 11L)
            timing <- system.time(
              sel <- select_ncomp(
                x = miss$x_incomplete,
                y = dataset$y,
                method = selection_method,
                criterion = criterion,
                max_ncomp = max_ncomp,
                seed = selection_seed,
                folds = folds
              )
            )
            row_id <- row_id + 1L
            rows[[row_id]] <- .study_row(
              study = "real_data",
              dataset = dataset$name,
              n = nrow(dataset$x),
              p = ncol(dataset$x),
              true_ncomp = NA_integer_,
              target_ncomp = target_ncomp,
              mechanism = mechanism,
              missing_prop = .normalize_missing_prop(missing_prop) * 100,
              method = .selection_method_label(selection_method),
              criterion = criterion,
              selected_ncomp = sel$selected_ncomp,
              matched_target = !is.na(sel$selected_ncomp) && sel$selected_ncomp == target_ncomp,
              runtime_sec = unname(timing["elapsed"]),
              seed = selection_seed,
              status = sel$status,
              notes = sel$notes
            )
          }
        }

        for (imp_method in imputation_methods) {
          imputation_seed <- .seed_with_offset(miss_seed, 50000L + row_id)
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
              selection_seed <- imputation_seed
            } else {
              selection_seed <- .seed_with_offset(imputation_seed, match(criterion, criteria))
              timing <- system.time(
                sel <- select_ncomp(
                  x = imp,
                  y = dataset$y,
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
              study = "real_data",
              dataset = dataset$name,
              n = nrow(dataset$x),
              p = ncol(dataset$x),
              true_ncomp = NA_integer_,
              target_ncomp = target_ncomp,
              mechanism = mechanism,
              missing_prop = .normalize_missing_prop(missing_prop) * 100,
              method = .imputation_method_label(imp_method),
              criterion = criterion,
              selected_ncomp = sel$selected_ncomp,
              matched_target = !is.na(sel$selected_ncomp) && sel$selected_ncomp == target_ncomp,
              runtime_sec = elapsed,
              seed = selection_seed,
              status = sel$status,
              notes = sel$notes
            )
          }
        }
      }
    }
  }

  do.call(rbind, rows)
}
