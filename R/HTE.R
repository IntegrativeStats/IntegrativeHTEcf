#' Heterogeneous Treatment Effect
#'
#' @noRd
#' @param psi A numeric vector. The estimated coefficients.
#' @param X A named numeric matrix. The covariates.
#' @param outcome.type A character. The type of outcome ('cont' or 'bin')
#'
#' @returns A numeric vector.
#'
#' @include stopTests.R
#' @keywords internal
.HTE <- function(psi, X, outcome.type) {
  
  stopifnot(
    "`psi` must be a named numeric vector" = !missing(psi) &&
      .isNumericVector(psi) && !is.null(names(psi)) &&
      all(nchar(names(psi)) > 0L),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin")
  )
  
  idx <- match(names(psi), colnames(X))[-1L]
  if (any(is.na(idx))) stop("names of psi do not match provided X", call. = FALSE)
  
  HTE <- drop({psi[1L] + X[, idx, drop = FALSE] %*% psi[-1L]})
  
  if (outcome.type == "bin") {
    eXB <- pmin(exp(HTE) |> drop(), 1e8)
    HTE <- {eXB - 1.0}/{eXB + 1.0}
  }
  
  HTE
}

#' Derivative of Heterogenous Treatment Effect
#'
#' @noRd
#' @param psi A numeric vector. The estimated coefficients.
#' @param X A named numeric matrix. The covariates.
#' @param outcome.type A character. The type of outcome ('cont' or 'bin')
#'
#' @returns A numeric matrix in order of psi.
#' 
#' @include stopTests.R
#' @keywords internal
.dHTE <- function(psi, X, outcome.type) {
  
  stopifnot(
    "`psi` must be a named numeric vector" = !missing(psi) &&
      .isNumericVector(psi) && !is.null(names(psi)) &&
      all(nchar(names(psi)) > 0L),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin")
  )
  
  idx <- match(names(psi), colnames(X))[-1L]
  if (any(is.na(idx))) stop("names of psi do not match provided X", call. = FALSE)
  
  if (outcome.type == "cont") {
    dHTE <- cbind(1.0, X[, idx, drop = FALSE])
  } else {
    Xbeta <- drop({psi[1L] + X[, idx, drop = FALSE] %*% psi[-1L]})
    eXB <- pmin(exp(Xbeta) |> drop(), 1e8)
    dHTE <- cbind(1.0, X[, idx, drop = FALSE]) * {2.0 * eXB} / {eXB + 1.0}^2
  }
  
  dHTE
}

#' Confounding Function
#' 
#' @noRd
#' @param phi A numeric vector. The estimated coefficients.
#' @param X A named numeric matrix. The covariates.
#' @param n.rct A numeric integer. The number of participants in the RCT dataset.
#'
#' @returns A numeric vector of length {n.rct + n.rwe}. 
#' 
#' @include stopTests.R
#' @keywords internal
.lambda <- function(phi, X, n.rct) {
  
  stopifnot(
    "`phi` must be a named numeric vector" = !missing(phi) &&
      .isNumericVector(phi) && !is.null(names(phi)) &&
      all(nchar(names(phi)) > 0L),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`n.rct` must be an integer" = !missing(n.rct) && .isIntegerVector(n.rct, 1L)
  )
  
  idx <- match(names(phi), colnames(X))[-1L]
  if (any(is.na(idx))) stop("names of phi do not match provided X", call. = FALSE)
  
  lambda <- drop({phi[1L] + X[, idx, drop = FALSE] %*% phi[-1L]})
  
  c(numeric(n.rct), lambda)
}

#' Derivative of the Confounding Function
#'
#' @noRd
#' @param phi A numeric vector. The estimated coefficients.
#' @param X A named numeric matrix. The covariates.
#' @param n.rct A numeric integer. The number of participants in the RCT dataset.
#'
#' @returns A numeric matrix of dimension {n.rct + n.rwe} x {ncol(X) + 1}
#' 
#' @include stopTests.R
#' @keywords internal
.dlambda <- function(phi, X, n.rct) {
  
  stopifnot(
    "`phi` must be a named numeric vector" = !missing(phi) &&
      .isNumericVector(phi) && !is.null(names(phi)) &&
      all(nchar(names(phi)) > 0L),
    "`X` must be a named numeric matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`n.rct` must be an integer" = !missing(n.rct) && .isIntegerVector(n.rct, 1L)
  )
  
  idx <- match(names(phi), colnames(X))[-1L]
  if (any(is.na(idx))) stop("names of phi do not match provided X", call. = FALSE)
  
  dlambda <- cbind(1.0, X[, idx, drop = FALSE])
  
  rbind(matrix(0.0, n.rct, ncol(dlambda)), dlambda)
}

#' @noRd
#' @param Y A numeric vector. The outcome.
#' @param A A numeric vector. The treatment.
#' @param ps A numeric vector. The propensit.
#'
#' @returns A numeric vector.
#'
#' @include stopTests.R
#' @keywords internal
.ipw_HTE <- function(Y, A, ps) {
  
  stopifnot(
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y),
    "`A` must be a numeric vector" = !missing(A) && .isNumericVector(A, length(Y)),
    "`ps` must be a numeric vector" = !missing(ps) && .isNumericVector(ps, length(A))
  )

  tx1 <- A > 0.5
  res <- Y
  res[tx1] <- res[tx1] / ps[tx1]
  res[!tx1] <- -res[!tx1] / (1.0 - ps[!tx1])
  res

}
