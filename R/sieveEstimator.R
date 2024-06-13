#' Sieve Estimator Using stats::glm
#'
#' @noRd
#' @param X A numeric matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param wgt A numeric vector object. The case weights.
#' @param subset A logical vector object. The participants to include in the
#'   regression.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#'
#' @returns The predicted outcome for all participants in `X`.
#'
#' @importFrom stats as.formula glm predict.glm
#' @include stopTests.R
.fitglm <- function(X, Y, wgt, subset, method.controls) {
  
  stopifnot(
    "`X` must be a named matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`wgt` must be a numeric vector" = !missing(wgt) &&
      .isNumericVector(wgt, nrow(X)),
    "`subset` must be a logical vector" = !missing(subset) &&
      .isLogicalVector(subset, nrow(X)),
    "`method.controls` must be a list" = !missing(method.controls) &&
      is.list(method.controls)
  )
  
  glm.args <- names(formals(stats::glm))
  if (!all(names(method.controls) %in% glm.args)) {
    stop("`method.controls` is not properly defined", call. = FALSE)
  }
  
  cannot_provide <- c("formula", "data", "weights", "subset")
  if (any(cannot_provide %in% names(method.controls))) {
    warning("Element(s) ",
            paste(intersect(cannot_provide, names(method.controls)), collapse = ", "),
            " cannot be provided as input; input overwritten.")
  }
  
  if (ncol(X) > 0L) {
    form <- paste("Y ~ ", paste(colnames(X), collapse = "+"))
  } else {
    form <- "Y ~ 1"
  }
  method.controls$formula <- stats::as.formula(form)
  data <- as.data.frame(cbind(Y, X))
  colnames(data)[1L] <- "Y"
  method.controls$data <- data[subset, , drop = FALSE]
  method.controls$weights <- wgt[subset]
  
  fit <- tryCatch(do.call(stats::glm, method.controls),
                  error = function(e) {
                    stop("stats::glm encountered errors\n\t",
                         e$message, call. = FALSE)
                  })
  
  stats::predict.glm(object = fit, newdata = data, type = "response") |> c() |> unname()
}

#' Sieve Estimator Using mgcv::gam
#'
#' @noRd
#' @param X A numeric matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param wgt A numeric vector object. The case weights.
#' @param subset A logical vector object. The participants to include in the
#'   regression.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#'
#' @returns The predicted outcome for all participants in `X`.
#'
#' @import mgcv
#' @importFrom stats as.formula
#' @include stopTests.R
.fitgam <- function(X, Y, wgt, subset, method.controls) {
  
  stopifnot(
    "`X` must be a named matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`wgt` must be a numeric vector" = !missing(wgt) &&
      .isNumericVector(wgt, nrow(X)),
    "`subset` must be a logical vector" = !missing(subset) &&
      .isLogicalVector(subset, nrow(X)),
    "`method.controls` must be a list" = !missing(method.controls) &&
      is.list(method.controls)
  )
  
  gam.args <- names(formals(mgcv::gam))
  if (!all(names(method.controls) %in% gam.args)) {
    stop("`method.controls` is not properly defined", call. = FALSE)
  }
  
  cannot_provide <- c("formula", "data", "weights", "subset")
  if (any(cannot_provide %in% names(method.controls))) {
    warning("Element(s) ",
            paste(intersect(cannot_provide, names(method.controls)), collapse = ", "),
            " cannot be provided as input; input overwritten.")
  }
  
  if (ncol(X) > 0L) {
    cont_vars <- apply(X, MARGIN = 2L, FUN = unique, simplify = FALSE)
    dont_smooth <- lengths(cont_vars) < 10L
    add_smooth_function <- c(paste0("s(", colnames(X)[!dont_smooth], ")"), colnames(X)[dont_smooth])
    form <- paste("Y ~ ", paste(add_smooth_function, collapse = "+"))
  } else {
    form <- "Y ~ 1"
  }
  method.controls$formula <- stats::as.formula(form)
  data <- as.data.frame(cbind(Y, X))
  colnames(data)[1L] <- "Y"
  method.controls$data <- data[subset, , drop = FALSE]
  method.controls$weights <- wgt[subset]

  fit <- tryCatch(do.call(mgcv::gam, method.controls),
                  error = function(e) {
                    stop("mgcv::gam encountered errors\n\t",
                         e$message, call. = FALSE)
                  })

  mgcv::predict.gam(object = fit, newdata = data, type = "response") |> c() |> unname()
}

#' Sieve Estimator Using SuperLearning
#'
#' @noRd
#' @param X A numeric matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param wgt A numeric vector object. The case weights.
#' @param subset A logical vector object. The participants to include in the
#'   regression.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#'
#' @returns The predicted outcome for all participants in `X`.
#'
#' @import SuperLearner
#' @include stopTests.R
.fitSL <- function(X, Y, wgt, subset, method.controls) {
  
  stopifnot(
    "`X` must be a named matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && .isNumericVector(Y, nrow(X)),
    "`wgt` must be a numeric vector" = !missing(wgt) &&
      .isNumericVector(wgt, nrow(X)),
    "`subset` must be a logical vector" = !missing(subset) &&
      .isLogicalVector(subset, nrow(X)),
    "`method.controls` must be a list" = !missing(method.controls) &&
      is.list(method.controls)
  )
  
  sl.args <- names(formals(SuperLearner::SuperLearner))
  if (!all(names(method.controls) %in% sl.args)) {
    stop("`method.controls` is not properly defined", call. = FALSE)
  }
    
  cannot_provide <- c("Y", "X", "newX", "obsWeights")
  if (any(cannot_provide %in% names(method.controls))) {
    warning("Element(s) ",
            paste(intersect(cannot_provide, names(method.controls)), collapse = ", "),
            " cannot be provided as input; input overwritten.")
  }
    
  method.controls$Y <- Y[subset]
  method.controls$X <- as.data.frame(cbind(1.0, X[subset, , drop = FALSE]))
  method.controls$newX <- as.data.frame(cbind(1.0, X))
  method.controls$obsWeights <- wgt[subset]
    
  fit <- tryCatch(do.call(SuperLearner::SuperLearner, method.controls),
                  error = function(e) {
                    stop("SuperLearner encountered errors\n\t",
                         e$message, call. = FALSE)
                  })
  # STH CONSIDER VERIFYING NOT ALL ZERO
  fit$SL.predict |> drop()
}

#' Sieve Estimator
#'
#' @noRd
#' @param X A numeric matrix object. The covariates. Columns must be named.
#' @param Y A numeric vector object. The outcome of interest.
#' @param wgt A numeric vector object. The case weights.
#' @param subset A logical vector object. The participants to include in the
#'   regression.
#' @param sieve.degree A scalar numeric object. The degree of the polynomial
#'   used to define the sieve model.
#' @param method A character object. One of `SL`, `gam`, or `glm`.
#' @param method.controls A list object. User specified inputs to the
#'   regression method. Element names must match formal arguments of the
#'   regression method.
#'
#' @returns The predicted outcome for all participants in `X`.
#'
#' @importFrom stats as.formula glm glm.control poly predict.glm
#' @import SuperLearner
#' @include stopTests.R
.sieveEstimator <- function(X, Y, wgt, subset, sieve.degree, method, method.controls) {
  
  stopifnot(
    "`X` must be a named matrix" = !missing(X) &&
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be provided" = !missing(Y),
    "`wgt` must be provided" = !missing(wgt),
    "`subset` must be provided" = !missing(subset),
    "`sieve.degree` must be a scalar numeric" = !missing(sieve.degree) &&
      .isNumericVector(sieve.degree, 1L),
    "`method` must be one of 'SL', 'gam', or 'glm'" = !missing(method) &&
      method %in% c("SL", "gam", "glm"),
    "`method.controls` must be provided" = !missing(method.controls)
  )
  
  if (ncol(X) > 0L) {
    # expand design matrix
    X <- stats::poly(X, degree = sieve.degree, raw = TRUE)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    
    
    # there may be duplicated columns
    X <- X[, !duplicated(t(X)), drop = FALSE]
  }
  
  if (method == "SL") {
    .fitSL(X = X, Y = Y, wgt = wgt, subset = subset, method.controls = method.controls)
  } else if (method == "gam") {
    .fitgam(X = X, Y = Y, wgt = wgt, subset = subset, method.controls = method.controls)
  } else if (method == "glm") {
    .fitglm(X = X, Y = Y, wgt = wgt, subset = subset, method.controls = method.controls)
  } else {
    stop("unrecognized method ", method, call. = FALSE)
  }
}