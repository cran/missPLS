#' Add missing values to a predictor matrix
#'
#' Create MCAR or MAR missingness on the predictor matrix `x`. Missingness is
#' generated column-wise so that each predictor receives approximately the same
#' missing-data proportion, matching the simulation strategy used in the
#' original work.
#'
#' @param x Predictor matrix or data frame.
#' @param y Numeric response vector.
#' @param mechanism Missingness mechanism: `"MCAR"` or `"MAR"`.
#' @param missing_prop Missing-data proportion as a fraction (`0.05`) or a
#'   percentage (`5`).
#' @param seed Optional random seed. If supplied, it is used only for this call.
#' @param mar_y_bias Proportion of missing values assigned to the upper
#'   half of the observed `y` values under the MAR mechanism.
#'
#' @return A list with components `x_incomplete`, `missing_mask`,
#'   `missing_prop`, `mechanism`, and `seed`.
#' @export
#'
#' @examples
#' sim <- simulate_pls_data(n = 20, p = 10, true_ncomp = 2, seed = 1)
#' miss <- add_missingness(sim$x, sim$y, mechanism = "MCAR", missing_prop = 10, seed = 2)
#' mean(is.na(miss$x_incomplete))
add_missingness <- function(x, y, mechanism = c("MCAR", "MAR"), missing_prop, seed = NULL,
                            mar_y_bias = 0.8) {
  mechanism <- match.arg(mechanism)
  seed <- .set_seed_if_supplied(seed)
  prop <- .normalize_missing_prop(missing_prop)
  x <- .as_numeric_matrix(x)
  y <- as.numeric(y)

  if (nrow(x) != length(y)) {
    stop("`x` and `y` must have compatible dimensions.", call. = FALSE)
  }
  if (mar_y_bias <= 0 || mar_y_bias >= 1) {
    stop("`mar_y_bias` must lie strictly between 0 and 1.", call. = FALSE)
  }

  n <- nrow(x)
  p <- ncol(x)
  n_missing_per_col <- max(1L, as.integer(round(n * prop)))
  if (n_missing_per_col >= n) {
    stop("`missing_prop` is too large for the current number of rows.", call. = FALSE)
  }

  x_incomplete <- x
  missing_mask <- matrix(FALSE, nrow = n, ncol = p, dimnames = dimnames(x))

  for (j in seq_len(p)) {
    if (mechanism == "MCAR") {
      rows <- sample.int(n, size = n_missing_per_col, replace = FALSE)
    } else {
      split_point <- stats::median(y, na.rm = TRUE)
      high_rows <- which(y > split_point)
      low_rows <- which(y <= split_point)
      n_high <- min(length(high_rows), max(0L, as.integer(round(n_missing_per_col * mar_y_bias))))
      n_low <- min(length(low_rows), n_missing_per_col - n_high)
      rows <- c(
        if (n_high) sample(high_rows, size = n_high, replace = FALSE) else integer(0),
        if (n_low) sample(low_rows, size = n_low, replace = FALSE) else integer(0)
      )
      if (length(rows) < n_missing_per_col) {
        remainder_pool <- setdiff(seq_len(n), rows)
        rows <- c(rows, sample(remainder_pool, size = n_missing_per_col - length(rows), replace = FALSE))
      }
    }

    x_incomplete[rows, j] <- NA_real_
    missing_mask[rows, j] <- TRUE
  }

  if (any(apply(is.na(x_incomplete), 1L, all))) {
    return(add_missingness(
      x = x,
      y = y,
      mechanism = mechanism,
      missing_prop = prop,
      seed = .seed_with_offset(seed, 1009L),
      mar_y_bias = mar_y_bias
    ))
  }

  list(
    x_incomplete = x_incomplete,
    missing_mask = missing_mask,
    missing_prop = prop,
    mechanism = mechanism,
    seed = .seed_column_value(seed)
  )
}
