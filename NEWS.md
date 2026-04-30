# missPLS 0.2.1

* Improved test portability across BLAS/LAPACK implementations, including MKL,
  by comparing complete-data Q2 cross-validation summaries with a numeric
  tolerance while keeping exact selected-component parity checks.

# missPLS 0.2.0

* Complete-data selection now degrades cleanly when optional `plsdof` is not
  installed, falling back to the naive information-criterion columns when
  degree-of-freedom based criteria are unavailable.
* Removed fixed default RNG seeds from the public API and avoided implicit
  writes to the user's home filespace; long-run reproduction scripts now
  default to `tempdir()` unless an explicit output directory is supplied.

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
