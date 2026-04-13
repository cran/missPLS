`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

.normalize_seed <- function(seed, allow_null = FALSE) {
  if (is.null(seed)) {
    if (allow_null) {
      return(NULL)
    }
    stop("`seed` must be a single non-missing integer-like value.", call. = FALSE)
  }

  if (length(seed) != 1L || is.na(seed)) {
    stop("`seed` must be a single non-missing integer-like value.", call. = FALSE)
  }

  as.integer(seed)
}

.seed_with_offset <- function(seed, offset) {
  base_seed <- .normalize_seed(seed, allow_null = TRUE)
  if (is.null(base_seed)) {
    return(NULL)
  }
  max_int <- .Machine$integer.max
  as.integer((base_seed + as.integer(offset)) %% max_int)
}

.set_seed_if_supplied <- function(seed) {
  seed <- .normalize_seed(seed, allow_null = TRUE)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  seed
}

.seed_column_value <- function(seed) {
  seed <- .normalize_seed(seed, allow_null = TRUE)
  if (is.null(seed)) {
    NA_integer_
  } else {
    as.integer(seed)
  }
}

.normalize_missing_prop <- function(missing_prop) {
  if (length(missing_prop) != 1L || is.na(missing_prop)) {
    stop("`missing_prop` must be a single non-missing numeric value.", call. = FALSE)
  }

  if (missing_prop <= 0) {
    stop("`missing_prop` must be strictly positive.", call. = FALSE)
  }

  if (missing_prop > 1) {
    missing_prop <- missing_prop / 100
  }

  if (missing_prop >= 1) {
    stop("`missing_prop` must be smaller than 1 or 100%.", call. = FALSE)
  }

  as.numeric(missing_prop)
}

.as_numeric_matrix <- function(x, arg = "x") {
  if (inherits(x, "data.frame")) {
    x <- as.matrix(x)
  }

  if (!is.matrix(x)) {
    stop(sprintf("`%s` must be a matrix or data frame.", arg), call. = FALSE)
  }

  storage.mode(x) <- "double"
  x
}

PLS_lm <- function(...) {
  args <- list(...)
  if (is.null(args$naive) && !requireNamespace("plsdof", quietly = TRUE)) {
    args$naive <- TRUE
  }
  do.call(plsRglm::PLS_lm, args)
}

PLS_lm_kfoldcv <- function(...) {
  plsRglm::PLS_lm_kfoldcv(...)
}

.coerce_dataset <- function(dataset) {
  if (inherits(dataset, "misspls_dataset")) {
    return(dataset)
  }

  if (is.character(dataset) && length(dataset) == 1L) {
    if (exists(dataset, inherits = TRUE)) {
      obj <- get(dataset, inherits = TRUE)
      if (inherits(obj, "misspls_dataset")) {
        return(obj)
      }
    }

    env <- new.env(parent = emptyenv())
    utils::data(list = dataset, package = "missPLS", envir = env)
    if (!exists(dataset, envir = env, inherits = FALSE)) {
      stop(sprintf("Dataset `%s` was not found.", dataset), call. = FALSE)
    }
    obj <- get(dataset, envir = env, inherits = FALSE)
    if (!inherits(obj, "misspls_dataset")) {
      stop(sprintf("Object `%s` is not a `misspls_dataset`.", dataset), call. = FALSE)
    }
    return(obj)
  }

  if (is.list(dataset) && !is.null(dataset$x) && !is.null(dataset$y)) {
    x <- .as_numeric_matrix(dataset$x, arg = "dataset$x")
    y <- as.numeric(dataset$y)
    if (nrow(x) != length(y)) {
      stop("`dataset$x` and `dataset$y` have incompatible dimensions.", call. = FALSE)
    }
    return(structure(
      list(
        name = dataset$name %||% "custom_dataset",
        x = x,
        y = y,
        data = data.frame(y = y, x, check.names = FALSE),
        response_name = dataset$response_name %||% "y",
        predictor_names = colnames(x) %||% paste0("x", seq_len(ncol(x))),
        source = dataset$source %||% "custom input",
        preprocessing = dataset$preprocessing %||% character(0),
        notes = dataset$notes %||% character(0)
      ),
      class = "misspls_dataset"
    ))
  }

  stop("`dataset` must be a `misspls_dataset`, a dataset name, or a list with `x` and `y`.", call. = FALSE)
}

.resolve_xy <- function(x, y = NULL) {
  if (inherits(x, "misspls_dataset") || (is.character(x) && length(x) == 1L)) {
    dataset <- .coerce_dataset(x)
    return(list(
      x = dataset$x,
      y = dataset$y,
      dataset_name = dataset$name
    ))
  }

  if (is.list(x) && !is.null(x$x) && !is.null(x$y) && is.null(y)) {
    dataset <- .coerce_dataset(x)
    return(list(
      x = dataset$x,
      y = dataset$y,
      dataset_name = dataset$name
    ))
  }

  if (is.null(y)) {
    stop("`y` must be supplied when `x` is not a dataset object.", call. = FALSE)
  }

  x <- .as_numeric_matrix(x, arg = "x")
  y <- as.numeric(y)
  if (nrow(x) != length(y)) {
    stop("`x` and `y` have incompatible dimensions.", call. = FALSE)
  }

  list(x = x, y = y, dataset_name = "custom_dataset")
}

.mode_integer <- function(x) {
  x <- as.integer(stats::na.omit(x))
  if (!length(x)) {
    return(NA_integer_)
  }

  tab <- sort(table(x), decreasing = TRUE)
  tied <- as.integer(names(tab)[tab == tab[[1L]]])
  min(tied)
}

.q2_selected_ncomp <- function(q2_values, threshold = 0.0975) {
  q2_values <- as.numeric(q2_values)
  if (!length(q2_values)) {
    return(0L)
  }

  flags <- !is.na(q2_values) & q2_values > threshold
  if (!flags[1L]) {
    return(0L)
  }

  first_fail <- match(FALSE, flags, nomatch = length(flags) + 1L)
  as.integer(first_fail - 1L)
}

.criteria_column_candidates <- function(criterion) {
  switch(
    criterion,
    "AIC" = c("AIC", "AIC.naive"),
    "AIC-DoF" = c("AIC.dof", "AIC.naive", "AIC"),
    "BIC" = c("BIC", "BIC.naive"),
    "BIC-DoF" = c("BIC.dof", "BIC.naive", "BIC"),
    NULL
  )
}

.select_from_infcrit <- function(infcrit, criterion, threshold = 0.0975) {
  out <- list(
    selected_ncomp = NA_integer_,
    criterion_value = NA_real_,
    status = "ok",
    notes = ""
  )

  if (criterion %in% c("Q2-LOO", "Q2-10fold")) {
    if (!("Q2_Y" %in% colnames(infcrit))) {
      out$status <- "unavailable"
      out$notes <- sprintf("`%s` is not available in the criterion matrix.", criterion)
      return(out)
    }

    q2_values <- infcrit[-1L, "Q2_Y", drop = TRUE]
    selected_ncomp <- .q2_selected_ncomp(q2_values, threshold = threshold)
    out$selected_ncomp <- selected_ncomp
    if (selected_ncomp > 0L) {
      out$criterion_value <- unname(q2_values[selected_ncomp])
    }
    return(out)
  }

  candidates <- .criteria_column_candidates(criterion)
  if (is.null(candidates)) {
    stop(sprintf("Unsupported criterion `%s`.", criterion), call. = FALSE)
  }

  column_name <- candidates[candidates %in% colnames(infcrit)][1L]
  if (is.na(column_name) || !nzchar(column_name)) {
    out$status <- "unavailable"
    out$notes <- sprintf("`%s` is not available in the criterion matrix.", criterion)
    return(out)
  }

  values <- as.numeric(infcrit[, column_name, drop = TRUE])
  if (!any(is.finite(values))) {
    out$status <- "unavailable"
    out$notes <- sprintf("`%s` is present but contains no finite values.", column_name)
    return(out)
  }

  preferred_column <- candidates[[1L]]
  if (!identical(column_name, preferred_column)) {
    out$notes <- sprintf("`%s` was unavailable; `%s` was used instead.", preferred_column, column_name)
  }

  idx <- which.min(replace(values, !is.finite(values), Inf))
  out$selected_ncomp <- as.integer(idx - 1L)
  out$criterion_value <- values[idx]
  out
}

.make_folds <- function(n, k, seed) {
  if (k < 2L) {
    stop("`k` must be at least 2.", call. = FALSE)
  }

  k <- min(as.integer(k), as.integer(n))
  .set_seed_if_supplied(seed)
  idx <- sample.int(n)
  split(idx, rep(seq_len(k), length.out = n))
}

.complete_selection_matrix <- function(x, y, max_ncomp, criterion, seed, folds, threshold) {
  if (criterion %in% c("Q2-LOO", "Q2-10fold")) {
    k <- if (criterion == "Q2-LOO") nrow(x) else folds
    .set_seed_if_supplied(seed)
    cv_fit <- plsRglm::cv.plsR(
      object = y,
      dataX = x,
      nt = max_ncomp,
      K = k,
      verbose = FALSE
    )
    cv_infos <- plsRglm::kfolds2CVinfos_lm(cv_fit, MClassed = FALSE, verbose = FALSE)[[1L]]
    return(cv_infos)
  }

  fit <- PLS_lm(
    dataY = y,
    dataX = x,
    nt = max_ncomp,
    verbose = FALSE
  )
  fit$InfCrit
}

.incomplete_q2_cv <- function(x, y, max_ncomp, seed, folds, selection_method, threshold) {
  method_na <- if (selection_method == "nipals_standard") "missingdata" else "adaptative"
  type_vc <- if (selection_method == "nipals_standard") "standard" else "adaptative"
  fold_index <- .make_folds(nrow(x), folds, seed)
  press <- rep(0, max_ncomp)
  available <- rep(TRUE, max_ncomp)

  full_fit <- plsRglm::plsR(
    dataY = y,
    dataX = x,
    nt = max_ncomp,
    typeVC = type_vc,
    MClassed = FALSE,
    verbose = FALSE
  )
  rss_prev <- as.numeric(full_fit$InfCrit[seq_len(max_ncomp), "RSS_Y", drop = TRUE])

  for (fold in fold_index) {
    train_x <- x[-fold, , drop = FALSE]
    train_y <- y[-fold]
    test_x <- x[fold, , drop = FALSE]
    test_y <- y[fold]

    fit <- plsRglm::plsR(
      dataY = train_y,
      dataX = train_x,
      nt = max_ncomp,
      typeVC = type_vc,
      MClassed = FALSE,
      verbose = FALSE
    )

    computed_nt <- min(max_ncomp, nrow(fit$InfCrit) - 1L)
    for (comp in seq_len(max_ncomp)) {
      if (comp > computed_nt) {
        available[comp] <- FALSE
        next
      }

      pred <- plsRglm::predict.plsRmodel(
        fit,
        newdata = as.data.frame(test_x),
        comps = comp,
        methodNA = method_na,
        verbose = FALSE
      )
      press[comp] <- press[comp] + sum((test_y - as.numeric(pred[, 1L]))^2)
    }
  }

  q2 <- rep(NA_real_, max_ncomp)
  valid <- available & is.finite(rss_prev) & rss_prev > 0
  q2[valid] <- 1 - (press[valid] / rss_prev[valid])

  q2cum <- rep(NA_real_, max_ncomp)
  running_press <- 1
  running_rss <- 1
  for (comp in seq_len(max_ncomp)) {
    if (!is.finite(press[comp]) || !is.finite(rss_prev[comp]) || rss_prev[comp] <= 0) {
      next
    }
    running_press <- running_press * press[comp]
    running_rss <- running_rss * rss_prev[comp]
    q2cum[comp] <- 1 - running_press / running_rss
  }

  out <- cbind(
    Q2cum_Y = c(NA_real_, q2cum),
    LimQ2_Y = c(NA_real_, rep(threshold, max_ncomp)),
    Q2_Y = c(NA_real_, q2),
    PRESS_Y = c(NA_real_, press),
    RSS_Y = full_fit$InfCrit[, "RSS_Y", drop = TRUE]
  )
  rownames(out) <- rownames(full_fit$InfCrit)
  out
}

.selection_result <- function(selection_method, criterion, selected_ncomp, criterion_value,
                              max_ncomp, seed, status = "ok", notes = "", n_imputations = 1L) {
  data.frame(
    selection_method = selection_method,
    criterion = criterion,
    selected_ncomp = as.integer(selected_ncomp),
    criterion_value = as.numeric(criterion_value),
    max_ncomp = as.integer(max_ncomp),
    seed = .seed_column_value(seed),
    n_imputations = as.integer(n_imputations),
    status = status,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

.study_row <- function(study, dataset, n, p, true_ncomp, target_ncomp, mechanism,
                       missing_prop, method, criterion, selected_ncomp,
                       matched_target, runtime_sec, seed, status, notes) {
  data.frame(
    study = study,
    dataset = dataset,
    n = as.integer(n),
    p = as.integer(p),
    true_ncomp = if (is.na(true_ncomp)) NA_integer_ else as.integer(true_ncomp),
    target_ncomp = if (is.na(target_ncomp)) NA_integer_ else as.integer(target_ncomp),
    mechanism = if (is.null(mechanism)) NA_character_ else as.character(mechanism),
    missing_prop = if (is.na(missing_prop)) NA_real_ else as.numeric(missing_prop),
    method = method,
    criterion = criterion,
    selected_ncomp = if (is.na(selected_ncomp)) NA_integer_ else as.integer(selected_ncomp),
    matched_target = if (is.na(matched_target)) NA else as.logical(matched_target),
    runtime_sec = as.numeric(runtime_sec),
    seed = .seed_column_value(seed),
    status = status,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

.imputation_method_label <- function(method) {
  switch(
    method,
    mice = "MICE",
    knn = "KNNimpute",
    svd = "SVDimpute",
    method
  )
}

.selection_method_label <- function(method) {
  switch(
    method,
    complete = "Complete",
    nipals_standard = "NIPALS-PLSR (standard)",
    nipals_adaptative = "NIPALS-PLSR (adaptative)",
    nipals_adaptive = "NIPALS-PLSR (adaptative)",
    method
  )
}

.normalise_selection_method <- function(method) {
  method <- match.arg(
    method,
    choices = c("complete", "nipals_standard", "nipals_adaptative", "nipals_adaptive")
  )
  if (method == "nipals_adaptive") {
    return("nipals_adaptative")
  }
  method
}

.vif_values <- function(x) {
  x <- .as_numeric_matrix(x)
  if (nrow(x) <= ncol(x)) {
    return(rep(NA_real_, ncol(x)))
  }

  out <- rep(NA_real_, ncol(x))
  for (j in seq_len(ncol(x))) {
    response <- x[, j]
    predictors <- x[, -j, drop = FALSE]
    fit <- tryCatch(
      stats::lm.fit(cbind(1, predictors), response),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      next
    }
    rss <- sum(fit$residuals^2)
    tss <- sum((response - mean(response))^2)
    r_squared <- 1 - (rss / tss)
    if (is.finite(r_squared) && r_squared < 1) {
      out[j] <- 1 / (1 - r_squared)
    } else {
      out[j] <- Inf
    }
  }
  out
}
