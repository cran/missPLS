load_misspls <- function() {
  if (!"package:missPLS" %in% search()) {
    devtools::load_all(".", quiet = TRUE)
  }
}

make_output_dir <- function(name, root_dir) {
  if (missing(root_dir) || !nzchar(root_dir)) {
    stop("`root_dir` must be supplied for writing helpers.", call. = FALSE)
  }

  out_dir <- file.path(root_dir, "reproduction", name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

write_manifest <- function(output_dir, parameters) {
  param_file <- file.path(output_dir, "parameters.txt")
  writeLines(capture.output(str(parameters)), con = param_file)

  session_file <- file.path(output_dir, "session-info.txt")
  writeLines(capture.output(sessionInfo()), con = session_file)
}

write_discrepancy_report <- function(output_dir, label, current_summary, reference = NULL) {
  report_file <- file.path(output_dir, "discrepancy-report.txt")
  lines <- c(
    paste("Report:", label),
    paste("Generated:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    ""
  )

  if (is.null(reference)) {
    lines <- c(
      lines,
      "No encoded published reference table is bundled with the package.",
      "Manual comparison against the article or thesis tables is still required for this run.",
      "",
      "Current summary preview:",
      capture.output(utils::head(current_summary))
    )
  } else {
    lines <- c(
      lines,
      "A reference table was supplied by the caller.",
      "",
      "Current summary preview:",
      capture.output(utils::head(current_summary)),
      "",
      "Reference preview:",
      capture.output(utils::head(reference))
    )
  }

  writeLines(lines, con = report_file)
}

save_outputs <- function(output_dir, prefix, results, summary_df) {
  utils::write.csv(results, file.path(output_dir, paste0(prefix, "-results.csv")), row.names = FALSE)
  utils::write.csv(summary_df, file.path(output_dir, paste0(prefix, "-summary.csv")), row.names = FALSE)
  saveRDS(results, file.path(output_dir, paste0(prefix, "-results.rds")))
  saveRDS(summary_df, file.path(output_dir, paste0(prefix, "-summary.rds")))
}

write_match_plot <- function(output_dir, prefix, summary_df, title) {
  png(file.path(output_dir, paste0(prefix, "-match-rate.png")), width = 1400, height = 900, res = 150)
  on.exit(dev.off(), add = TRUE)

  plot_df <- summary_df[!is.na(summary_df$match_rate), , drop = FALSE]
  if (!nrow(plot_df)) {
    plot.new()
    title(main = paste(title, "(no non-missing match rates)"))
    return(invisible(NULL))
  }

  labels <- paste(plot_df$method, plot_df$criterion, sep = "\n")
  stats::barplot(
    height = plot_df$match_rate,
    names.arg = labels,
    las = 2,
    col = "steelblue",
    ylim = c(0, 1),
    main = title,
    ylab = "Match rate"
  )
}
