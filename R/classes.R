#' @export
print.misspls_imputation <- function(x, ...) {
  cat("<misspls_imputation>\n")
  cat("  method:", x$method, "\n")
  cat("  n_imputations:", x$n_imputations, "\n")
  cat("  predictors:", length(x$predictor_names), "\n")
  invisible(x)
}

#' @export
print.misspls_dataset <- function(x, ...) {
  cat("<misspls_dataset>\n")
  cat("  name:", x$name, "\n")
  cat("  dimensions:", nrow(x$x), "x", ncol(x$x), "\n")
  cat("  source:", x$source, "\n")
  invisible(x)
}
