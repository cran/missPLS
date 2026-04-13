#' Simulate PLS data
#'
#' Simulate a univariate-response PLS dataset using the Li et al.-style
#' generator available in `plsRglm`.
#'
#' @param n Number of observations.
#' @param p Number of predictors.
#' @param true_ncomp True number of latent components.
#' @param seed Optional random seed. If supplied, it is used only for this call.
#' @param model Simulation model. Only `"li2002"` is currently supported.
#'
#' @return A list with components `x`, `y`, `data`, `true_ncomp`, `seed`, and
#'   `model`.
#' @export
#'
#' @examples
#' sim <- simulate_pls_data(n = 20, p = 10, true_ncomp = 2, seed = 42)
#' str(sim)
simulate_pls_data <- function(n, p, true_ncomp, seed = NULL, model = "li2002") {
  if (!identical(model, "li2002")) {
    stop("Only `model = \"li2002\"` is currently supported.", call. = FALSE)
  }

  n <- as.integer(n)
  p <- as.integer(p)
  true_ncomp <- as.integer(true_ncomp)
  seed <- .set_seed_if_supplied(seed)

  if (n < 2L) {
    stop("`n` must be at least 2.", call. = FALSE)
  }
  if (p < 2L) {
    stop("`p` must be at least 2.", call. = FALSE)
  }
  if (true_ncomp < 2L || true_ncomp > min(6L, p)) {
    stop("`true_ncomp` must be between 2 and `min(6, p)`.", call. = FALSE)
  }

  sim <- t(replicate(n, plsRglm::simul_data_UniYX(p, true_ncomp)))
  colnames(sim) <- c("y", paste0("x", seq_len(p)))

  x <- sim[, -1L, drop = FALSE]
  y <- as.numeric(sim[, 1L])

  list(
    x = x,
    y = y,
    data = data.frame(y = y, x, check.names = FALSE),
    true_ncomp = true_ncomp,
    seed = .seed_column_value(seed),
    model = model
  )
}
