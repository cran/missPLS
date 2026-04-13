#' Select the number of PLS components
#'
#' Select the number of components for complete, imputed, or incomplete-data
#' PLS workflows.
#'
#' @param x Predictor matrix, dataset object, or `misspls_imputation` object.
#' @param y Numeric response vector. This may be omitted when `x` already
#'   contains a response.
#' @param method Selection workflow: `"complete"`, `"nipals_standard"`, or
#'   `"nipals_adaptative"`.
#' @param criterion Selection criterion: `"Q2-LOO"`, `"Q2-10fold"`, `"AIC"`,
#'   `"AIC-DoF"`, `"BIC"`, or `"BIC-DoF"`.
#' @param max_ncomp Maximum number of components to consider.
#' @param seed Optional random seed used by the cross-validation and imputation
#'   aggregation steps.
#' @param folds Number of cross-validation folds used by `"Q2-10fold"`.
#' @param threshold Threshold applied to `Q2` criteria.
#'
#' @return A one-row data frame describing the selected component count.
#' @export
#'
#' @examples
#' sim <- simulate_pls_data(n = 25, p = 10, true_ncomp = 2, seed = 1)
#' select_ncomp(sim$x, sim$y, method = "complete", criterion = "AIC", max_ncomp = 4, seed = 2)
select_ncomp <- function(x, y, method = c("complete", "nipals_standard", "nipals_adaptative"),
                         criterion = c("Q2-LOO", "Q2-10fold", "AIC", "AIC-DoF", "BIC", "BIC-DoF"),
                         max_ncomp, seed = NULL, folds = 10L, threshold = 0.0975) {
  criterion <- match.arg(criterion)
  method <- .normalise_selection_method(method)
  seed <- .normalize_seed(seed, allow_null = TRUE)
  folds <- as.integer(folds)

  if (missing(max_ncomp)) {
    resolved <- if (inherits(x, "misspls_imputation")) {
      .resolve_xy(x$datasets[[1L]], y)
    } else {
      .resolve_xy(x, y)
    }
    max_ncomp <- min(8L, nrow(resolved$x) - 1L, ncol(resolved$x))
  }
  max_ncomp <- as.integer(max_ncomp)

  if (inherits(x, "misspls_imputation")) {
    if (!identical(method, "complete")) {
      return(.selection_result(
        selection_method = method,
        criterion = criterion,
        selected_ncomp = NA_integer_,
        criterion_value = NA_real_,
        max_ncomp = max_ncomp,
        seed = seed,
        status = "unavailable",
        notes = "Imputation objects can only be evaluated with `method = \"complete\"`.",
        n_imputations = x$n_imputations
      ))
    }

    selections <- lapply(seq_along(x$datasets), function(i) {
      select_ncomp(
        x = x$datasets[[i]],
        y = y,
        method = "complete",
        criterion = criterion,
        max_ncomp = max_ncomp,
        seed = .seed_with_offset(seed, i),
        folds = folds,
        threshold = threshold
      )
    })
    selections_df <- do.call(rbind, selections)
    ok <- selections_df$status == "ok"
    if (!any(ok)) {
      return(.selection_result(
        selection_method = method,
        criterion = criterion,
        selected_ncomp = NA_integer_,
        criterion_value = NA_real_,
        max_ncomp = max_ncomp,
        seed = seed,
        status = "error",
        notes = paste(unique(selections_df$notes), collapse = "; "),
        n_imputations = x$n_imputations
      ))
    }

    selected_ncomp <- .mode_integer(selections_df$selected_ncomp[ok])
    criterion_value <- mean(selections_df$criterion_value[ok], na.rm = TRUE)
    return(.selection_result(
      selection_method = paste0("complete_", x$method),
      criterion = criterion,
      selected_ncomp = selected_ncomp,
      criterion_value = criterion_value,
      max_ncomp = max_ncomp,
      seed = seed,
      status = "ok",
      notes = sprintf("Mode across %d imputed datasets.", x$n_imputations),
      n_imputations = x$n_imputations
    ))
  }

  resolved <- .resolve_xy(x, y)
  x <- resolved$x
  y <- resolved$y

  if (max_ncomp < 1L) {
    stop("`max_ncomp` must be at least 1.", call. = FALSE)
  }
  max_ncomp <- min(max_ncomp, nrow(x) - 1L, ncol(x))

  result <- tryCatch(
    {
      if (method == "complete") {
        if (anyNA(x)) {
          stop("`x` contains missing values; use an imputation workflow or NIPALS selection.")
        }
        infcrit <- .complete_selection_matrix(
          x = x,
          y = y,
          max_ncomp = max_ncomp,
          criterion = criterion,
          seed = seed,
          folds = folds,
          threshold = threshold
        )
      } else {
        fit <- plsRglm::plsR(
          dataY = y,
          dataX = x,
          nt = max_ncomp,
          typeVC = if (method == "nipals_standard") "standard" else "adaptative",
          MClassed = FALSE,
          verbose = FALSE
        )

        infcrit <- if (criterion == "Q2-10fold") {
          .incomplete_q2_cv(
            x = x,
            y = y,
            max_ncomp = max_ncomp,
            seed = seed,
            folds = folds,
            selection_method = method,
            threshold = threshold
          )
        } else {
          fit$InfCrit
        }
      }

      sel <- .select_from_infcrit(infcrit, criterion = criterion, threshold = threshold)
      .selection_result(
        selection_method = method,
        criterion = criterion,
        selected_ncomp = sel$selected_ncomp,
        criterion_value = sel$criterion_value,
        max_ncomp = max_ncomp,
        seed = seed,
        status = sel$status,
        notes = sel$notes
      )
    },
    error = function(e) {
      .selection_result(
        selection_method = method,
        criterion = criterion,
        selected_ncomp = NA_integer_,
        criterion_value = NA_real_,
        max_ncomp = max_ncomp,
        seed = seed,
        status = "error",
        notes = conditionMessage(e)
      )
    }
  )

  result
}
