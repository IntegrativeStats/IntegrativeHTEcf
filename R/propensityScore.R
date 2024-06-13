#' Estimated Propensity Score
#'
#' @noRd
#' @param X A numeric matrix object. The model design matrix. Must have named
#'   columns.
#' @param A An integer vector object. The binary treatment.
#' @param wgt A numeric vector object. An optional case weight.
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#' @param models A character vector object. The propensity score models to
#'   fit. Must be in c("ps", "ml.ps")
#'
#' @returns A list with elements "ps": the basic model estimates, and
#'   "ml.ps": the sieve model estimates
#'
#' @include sieveEstimator.R
#' @keywords internal
.propensityScore <- function(X, A, wgt, sieve.degree, method, method.controls, models) {
  stopifnot(
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`A` must be a binary vector" = !missing(A) && .isIntegerVector(A, nrow(X)) &&
      length(unique(A)) <= 2L,
    "`wgt` must be a numeric vector" = !missing(wgt) && .isNumericVector(wgt, nrow(X)),
    "`sieve.degree` must be a scalar numeric" = !missing(sieve.degree) &&
      .isNumericVector(sieve.degree, 1L),
    "`method` must be provided" = !missing(method),
    "`method.controls` must be provided" = !missing(method.controls),
    "`models` can contain only {'ps', 'ml.ps'}" = !missing(models) &&
      .isCharacterVector(models) &&
      all(models %in% c("ps", "ml.ps")) && length(models) <= 2L
  )
  
  res <- list()

  if ("ps" %in% models) {
    res$ps <- tryCatch(.sieveEstimator(X = X, Y = A, wgt = wgt,
                                       subset = rep(TRUE, nrow(X)),
                                       sieve.degree = 1L,
                                       method = method,
                                       method.controls = method.controls),
                       error = function(e) {
                         stop("unable to fit propensity score model\n\t",
                              e$message, call. = FALSE)
                       })
  }
  
  if ("ml.ps" %in% models) {
    res$ml.ps <- tryCatch(.sieveEstimator(X = X, Y = A, wgt = wgt,
                                          subset = rep(TRUE, nrow(X)),
                                          sieve.degree = sieve.degree,
                                          method = method,
                                          method.controls = method.controls),
                          error = function(e) {
                            stop("unable to fit sieve propensity score model\n\t",
                                 e$message, call. = FALSE)
                          })
  }
  
  res
}
