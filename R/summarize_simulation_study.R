#' Summarize simulation or real-data study results
#'
#' @param results A results data frame returned by [run_simulation_study()] or
#'   [run_real_data_study()].
#'
#' @return A grouped summary data frame.
#' @export
#'
#' @examples
#' sim_results <- run_simulation_study(
#'   dimensions = list(c(30, 12)),
#'   true_ncomp = 2,
#'   missing_props = numeric(0),
#'   mechanisms = character(0),
#'   reps = 2,
#'   seed = 1
#' )
#' summarize_simulation_study(sim_results)
summarize_simulation_study <- function(results) {
  required <- c(
    "study", "dataset", "n", "p", "true_ncomp", "target_ncomp", "mechanism",
    "missing_prop", "method", "criterion", "selected_ncomp", "matched_target",
    "runtime_sec", "seed", "status", "notes"
  )
  missing_cols <- setdiff(required, colnames(results))
  if (length(missing_cols)) {
    stop(sprintf("`results` is missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  group_cols <- c("study", "dataset", "n", "p", "true_ncomp", "target_ncomp",
                  "mechanism", "missing_prop", "method", "criterion")

  group_frame <- results[, group_cols, drop = FALSE]
  for (nm in names(group_frame)) {
    is_missing <- is.na(group_frame[[nm]])
    group_frame[[nm]] <- as.character(group_frame[[nm]])
    group_frame[[nm]][is_missing] <- "<NA>"
  }

  split_ids <- split(seq_len(nrow(results)), do.call(interaction, c(group_frame, drop = TRUE)))
  rows <- lapply(split_ids, function(idx) {
    chunk <- results[idx, , drop = FALSE]
    ok <- chunk$status == "ok"
    first <- chunk[1L, group_cols, drop = FALSE]
    data.frame(
      first,
      n_runs = nrow(chunk),
      n_success = sum(ok, na.rm = TRUE),
      n_matched = sum(chunk$matched_target %in% TRUE, na.rm = TRUE),
      match_rate = if (sum(ok, na.rm = TRUE) > 0) {
        sum(chunk$matched_target %in% TRUE, na.rm = TRUE) / sum(ok, na.rm = TRUE)
      } else {
        NA_real_
      },
      mean_selected_ncomp = if (any(ok)) mean(chunk$selected_ncomp[ok], na.rm = TRUE) else NA_real_,
      median_selected_ncomp = if (any(ok)) stats::median(chunk$selected_ncomp[ok], na.rm = TRUE) else NA_real_,
      mean_runtime_sec = mean(chunk$runtime_sec, na.rm = TRUE),
      status_summary = paste(sort(unique(chunk$status)), collapse = ","),
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, rows)
  summary_df$mechanism[summary_df$mechanism == "<NA>"] <- NA_character_
  summary_df$missing_prop[summary_df$missing_prop == "<NA>"] <- NA_character_
  summary_df$true_ncomp[summary_df$true_ncomp == "<NA>"] <- NA_character_
  summary_df$target_ncomp[summary_df$target_ncomp == "<NA>"] <- NA_character_
  summary_df
}
