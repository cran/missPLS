#' Diagnose a real dataset
#'
#' Compute correlation summaries and VIF-style diagnostics for a packaged
#' real dataset.
#'
#' @param dataset A packaged dataset name or `misspls_dataset` object.
#' @param cor_threshold Absolute-correlation threshold used when reporting
#'   predictor pairs and predictor-response associations.
#'
#' @return A list with correlation and VIF summaries.
#' @export
#'
#' @examples
#' diag_bromhexine <- diagnose_real_data("bromhexine")
#' names(diag_bromhexine)
diagnose_real_data <- function(dataset, cor_threshold = 0.7) {
  dataset <- .coerce_dataset(dataset)
  x <- dataset$x
  y <- dataset$y

  cor_x <- stats::cor(x)
  diag(cor_x) <- NA_real_
  upper_idx <- which(upper.tri(cor_x) & abs(cor_x) >= cor_threshold, arr.ind = TRUE)
  predictor_pairs <- if (nrow(upper_idx)) {
    data.frame(
      predictor_1 = colnames(x)[upper_idx[, 1L]],
      predictor_2 = colnames(x)[upper_idx[, 2L]],
      correlation = cor_x[upper_idx],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      predictor_1 = character(0),
      predictor_2 = character(0),
      correlation = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  response_cor <- stats::cor(x, y)
  response_hits <- which(abs(response_cor) >= cor_threshold)
  response_pairs <- if (length(response_hits)) {
    data.frame(
      predictor = colnames(x)[response_hits],
      correlation = response_cor[response_hits],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      predictor = character(0),
      correlation = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  vif_values <- .vif_values(x)
  vif_df <- data.frame(
    predictor = colnames(x),
    vif = vif_values,
    stringsAsFactors = FALSE
  )

  list(
    dataset = dataset$name,
    n = nrow(x),
    p = ncol(x),
    predictor_correlations = predictor_pairs,
    response_correlations = response_pairs,
    vif = vif_df,
    notes = if (nrow(x) <= ncol(x)) {
      "VIF values are undefined for n <= p and are returned as NA."
    } else {
      "VIF values were computed by regressing each predictor on the remaining predictors."
    }
  )
}
