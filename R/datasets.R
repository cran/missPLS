#' Bromhexine dataset
#'
#' Bromhexine in pharmaceutical syrup used in the article and thesis.
#'
#' @format A `misspls_dataset` list with components:
#' \describe{
#'   \item{name}{Dataset name.}
#'   \item{x}{A numeric `23 x 64` predictor matrix.}
#'   \item{y}{A numeric response vector of length `23`.}
#'   \item{data}{A data frame with response `y` and predictors `x1` to `x64`.}
#'   \item{source}{A short source reference.}
#'   \item{preprocessing}{Dataset preprocessing notes.}
#'   \item{notes}{Additional study notes.}
#' }
#' @source Goicoechea and Olivieri (1999a), calibration and test files bundled in
#'   `extra_docs/pls_data`.
"bromhexine"

#' Tetracycline dataset
#'
#' Tetracycline in serum used in the article and thesis.
#'
#' @format A `misspls_dataset` list with components:
#' \describe{
#'   \item{name}{Dataset name.}
#'   \item{x}{A numeric `107 x 101` predictor matrix.}
#'   \item{y}{A numeric response vector of length `107`.}
#'   \item{data}{A data frame with response `y` and predictors `x1` to `x101`.}
#'   \item{source}{A short source reference.}
#'   \item{preprocessing}{Dataset preprocessing notes.}
#'   \item{notes}{Additional study notes.}
#' }
#' @source Goicoechea and Olivieri (1999b), calibration and test files bundled in
#'   `extra_docs/pls_data`.
"tetracycline"

#' Octane dataset
#'
#' Octane in gasoline from NIR data used in the article and thesis.
#'
#' @format A `misspls_dataset` list with components:
#' \describe{
#'   \item{name}{Dataset name.}
#'   \item{x}{A numeric `68 x 493` predictor matrix.}
#'   \item{y}{A numeric response vector of length `68`.}
#'   \item{data}{A data frame with response `y` and predictors `x1` to `x493`.}
#'   \item{source}{A short source reference.}
#'   \item{preprocessing}{Dataset preprocessing notes.}
#'   \item{notes}{Additional study notes.}
#' }
#' @source Goicoechea and Olivieri (2003), calibration and test files bundled in
#'   `extra_docs/pls_data`.
"octane"

#' Complete-case ozone dataset
#'
#' Los Angeles ozone pollution complete-case dataset used in the article and
#' thesis.
#'
#' @format A `misspls_dataset` list with components:
#' \describe{
#'   \item{name}{Dataset name.}
#'   \item{x}{A numeric `203 x 12` predictor matrix.}
#'   \item{y}{A numeric response vector of length `203`.}
#'   \item{data}{A data frame with response `y` and predictors `x1` to `x12`.}
#'   \item{source}{A short source reference.}
#'   \item{preprocessing}{Dataset preprocessing notes.}
#'   \item{notes}{Additional study notes.}
#' }
#' @source `mlbench::Ozone`, restricted to the `203` complete observations used
#'   in the published analysis.
"ozone_complete"
