#' Impute a predictor matrix
#'
#' Apply one of the imputation strategies used in the article and thesis.
#'
#' @param x Incomplete predictor matrix or data frame.
#' @param method Imputation method: `"mice"`, `"knn"`, or `"svd"`.
#' @param seed Optional random seed forwarded to stochastic imputers when
#'   supported.
#' @param m Number of imputations for `method = "mice"`. By default this is set
#'   to the missing-data percentage rounded to the nearest integer.
#' @param k Number of neighbours for `method = "knn"`.
#' @param svd_rank Target rank for `method = "svd"`.
#' @param svd_maxiter Maximum number of iterations for the fallback SVD routine.
#'
#' @return A `misspls_imputation` object.
#' @export
#'
#' @examples
#' sim <- simulate_pls_data(n = 20, p = 10, true_ncomp = 2, seed = 1)
#' miss <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)
#' imp <- impute_pls_data(miss$x_incomplete, method = "knn", seed = 3)
#' length(imp$datasets)
impute_pls_data <- function(x, method = c("mice", "knn", "svd"), seed = NULL,
                            m, k = 15L, svd_rank = 10L, svd_maxiter = 1000L) {
  method <- match.arg(method)
  seed <- .normalize_seed(seed, allow_null = TRUE)
  x <- .as_numeric_matrix(x)
  predictor_names <- colnames(x) %||% paste0("x", seq_len(ncol(x)))
  colnames(x) <- predictor_names

  if (missing(m)) {
    m <- max(1L, as.integer(round(mean(is.na(x)) * 100)))
  }
  m <- as.integer(m)
  k <- as.integer(k)
  svd_rank <- as.integer(svd_rank)
  svd_maxiter <- as.integer(svd_maxiter)

  impute_svd_fallback <- function(x_in, rank_k, maxiter) {
    obs_mask <- !is.na(x_in)
    x_imp <- x_in
    col_means <- colMeans(x_imp, na.rm = TRUE)
    for (j in seq_len(ncol(x_imp))) {
      x_imp[!obs_mask[, j], j] <- col_means[j]
    }

    rank_k <- min(rank_k, nrow(x_imp), ncol(x_imp))
    for (iter in seq_len(maxiter)) {
      old_missing <- x_imp[!obs_mask]
      sv <- svd(x_imp)
      reconstructed <- sv$u[, seq_len(rank_k), drop = FALSE] %*%
        diag(sv$d[seq_len(rank_k)], nrow = rank_k) %*%
        t(sv$v[, seq_len(rank_k), drop = FALSE])
      x_imp[!obs_mask] <- reconstructed[!obs_mask]
      if (max(abs(x_imp[!obs_mask] - old_missing), na.rm = TRUE) < 1e-8) {
        break
      }
    }

    x_imp
  }

  datasets <- switch(
    method,
    mice = {
      mids <- suppressWarnings(mice::mice(
        data = as.data.frame(x),
        m = m,
        method = "norm",
        printFlag = FALSE,
        seed = if (is.null(seed)) NA_integer_ else seed,
        remove_collinear = FALSE
      ))
      lapply(seq_len(m), function(i) {
        out <- as.matrix(mice::complete(mids, action = i))
        colnames(out) <- predictor_names
        storage.mode(out) <- "double"
        out
      })
    },
    knn = {
      out <- as.matrix(VIM::kNN(as.data.frame(x), k = k, imp_var = FALSE))
      colnames(out) <- predictor_names
      storage.mode(out) <- "double"
      list(out)
    },
    svd = {
      out <- if (requireNamespace("bcv", quietly = TRUE)) {
        suppressWarnings(bcv::impute.svd(x, k = svd_rank, maxiter = svd_maxiter)$x)
      } else {
        impute_svd_fallback(x, rank_k = svd_rank, maxiter = svd_maxiter)
      }
      out <- as.matrix(out)
      colnames(out) <- predictor_names
      storage.mode(out) <- "double"
      list(out)
    }
  )

  structure(
    list(
      method = method,
      seed = .seed_column_value(seed),
      datasets = datasets,
      n_imputations = length(datasets),
      predictor_names = predictor_names
    ),
    class = "misspls_imputation"
  )
}
