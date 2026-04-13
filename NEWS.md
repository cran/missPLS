# missPLS 0.1.0

* Initial package release.
* Added methods-first helpers for simulation, MCAR/MAR missingness generation,
  imputation, component selection, and standardized study summaries.
* Added packaged real datasets for the published bromhexine, tetracycline,
  octane, and ozone analyses.
* Added study runners for simulation and real-data workflows, plus diagnostics
  for correlation and VIF-style screening.
* Added long-run reproduction scripts under `tools/` for complete-data,
  incomplete-data, and real-data article/thesis reruns.
* Added tests, vignette material, and package documentation for the CRAN-ready
  public API.
* Complete-data Q2 cross-validation now follows the documented
  `plsRglm::cv.plsR(object = ...)` interface, while incomplete-data Q2
  selection keeps the package-side NIPALS fallback used by `missPLS`.
