#' Meta Analysis - IPW Binary Outcome
#' 
#' @noRd
#' @param X A named matrix object. The covariates of the main effects model.
#' @param Y A numeric vector. The binary outcome.
#' @param predict.subset A logical vector. The elements of X for which
#'   predictions should be made.
#' 
#' @returns A list containing `est.meta` the estimate parameters and `att.meta`,
#'   the mean of the estimated ipw outcome for the RWE data.
#'   
#' @importFrom stats model.matrix optim
#' @include HTE.R stopTests.R
#' @keywords internal
.metaAnalysis.bin <- function(X, Y, predict.subset, optim.controls) {
  
  stopifnot(
    "`X` must be a named numeric matrix" = !missing(X) && 
      {.isNamedNumericMatrix(X) || ncol(X) == 0L},
    "`Y` must be a numeric vector" = !missing(Y) && 
      .isNumericVector(Y, nrow(X)),
    "`predict.subset` must be a logical vector" = !missing(predict.subset) && 
      {.isLogicalVector(predict.subset, nrow(X))},
    "`optim.controls` must be a list" = !missing(optim.controls) && 
      is.vector(optim.controls, mode = "list")
  )
  
  par <- numeric(ncol(X) + 1L)
  names(par) <- c("(Intercept)", colnames(X))
    
  .func <- function(par, X, Y) {
    hte <- .HTE(psi = par, X = X, outcome.type = "bin")
    sum({Y - hte}^2)
  }
  
  .dfunc <- function(par, X, Y) {
    hte <- .HTE(psi = par, X = X, outcome.type = "bin")
    dhte <- .dHTE(psi = par, X = X, outcome.type = "bin")
    colSums(- 2.0 * {Y - hte} * dhte)
  }
  
  if (length(optim.controls) == 0L ||
      is.null(optim.controls$method)) {
    optim.controls$method <- "L-BFGS-B"
  }
  
  optim.controls$par <- par
  optim.controls$fn <- .func
  optim.controls$gr <- .dfunc
  optim.controls$X <- X
  optim.controls$Y <- Y
  
  ipw_me_fit <- tryCatch(do.call(stats::optim, optim.controls),
                         error = function(e) {
                           stop("unable to complete meta analysis\n\t",
                                "`optim()` failed with error\n\t",
                                e$message, call. = FALSE)
                           })

  if (ipw_me_fit$convergence != 0L) {
    warning("`optim()` in meta-analysis did not converge: ", 
            ipw_me_fit$convergence, "\n", call. = FALSE)
  }
  if (!is.null(ipw_me_fit$message)) {
    message("Message returned by `optim()` in meta-analysis\n\t",
            ipw_me_fit$message)
  }
    
  tauZ <- .HTE(psi = ipw_me_fit$par, X = X[predict.subset, , drop = FALSE], 
               outcome.type = "bin")

  list("est.meta" = ipw_me_fit$par,
       "att.meta" = mean(tauZ))
}

#' Meta Analysis - IPW Continuous Outcome
#' 
#' @noRd
#' @param formula A formula object. The main effects model.
#' @param data A data.frame object. The covariates of the main effects model.
#' @param predict.subset A logical vector. The elements of X for which
#'   predictions should be made.
#' 
#' @returns A list containing `est.meta` the estimate parameters and `att.meta`,
#'   the mean of the estimated ipw outcome for the RWE data.
#'   
#' @importFrom stats glm predict.glm
#' @include stopTests.R
#' @keywords internal
.metaAnalysis.cont <- function(formula, data, predict.subset) {
  
  stopifnot(
    "`formula` must be a formula object of the form LHS ~ RHS" = !missing(formula) && 
      inherits(formula, "formula") && length(formula) == 3L,
    "`data` must be a data.frame" = !missing(data) && is.data.frame(data),
    "`predict.subset` must be a logical vector" = !missing(predict.subset) && 
      {.isLogicalVector(predict.subset, nrow(data))}
  )
  
 ipw_me_fit <- tryCatch(stats::glm(formula = formula, data = data, 
                                   family = "gaussian"),
                        error = function(e) {
                          stop("unable to complete meta analysis\n\t",
                               e$message, call. = FALSE)
                        })

 tauZ <- stats::predict.glm(ipw_me_fit, 
                            newdata = data[predict.subset, , drop = FALSE], 
                            type = "response")
 
 list("est.meta" = ipw_me_fit$coefficients,
      "att.meta" = mean(tauZ))
}

#' Meta Analysis - IPW Outcome
#' 
#' @noRd
#' @param data.rct A list object. The data for the RCT. Must contain elements
#'   A, Y, X, and ps.
#' @param data.rwe A list object. The data for the RWE. Must contain elements
#'   A, Y, X, and ps.
#' @param contName NULL or a character vector. The treatment effect covariates.
#' 
#' @returns A list containing `est.meta` the estimate parameters and `att.meta`,
#'   the mean of the estimated ipw outcome for the RWE data.
#'   
#' @importFrom stats as.formula glm model.matrix optim predict.glm
#' @include HTE.R stopTests.R
#' @keywords internal
.metaAnalysis <- function(data.rct, data.rwe, contName, outcome.type, optim.controls) {
  
  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', 'A', and 'ps'" =
      !missing(data.rct) && is.vector(data.rct, mode = "list") &&
      all(c("X", "Y", "A", "ps") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'ps'" =
      !missing(data.rwe) && is.vector(data.rwe, mode = "list") &&
      all(c("X", "Y", "A", "ps") %in% names(data.rwe)),
    "`contName` must be NULL or a character vector" = !missing(contName) &&
      {is.null(contName) ||
          {.isCharacterVector(contName) && 
              all(contName %in% colnames(data.rct$X)) &&
              all(contName %in% colnames(data.rwe$X))}},
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin"),
    "`optim.controls` must be provided" = !missing(optim.controls)
  )
  
  ## ipw-adjusted outcome regression based on both RCT and RWE datasets
  ipw_hte_rct <- .ipw_HTE(Y = data.rct$Y, A = data.rct$A, ps = data.rct$ps)
  ipw_hte_rwe <- .ipw_HTE(Y = data.rwe$Y, A = data.rwe$A, ps = data.rwe$ps)
  
  ipw_hte <- c(ipw_hte_rct, ipw_hte_rwe)
  
  if (outcome.type == "cont") {
    response_name <- "IPW_HTE"
    while (response_name %in% contName) {
      response_name <- sample(LETTERS, 26, TRUE) |> paste(collapse = "")
    }
    
    df <- as.data.frame(rbind(data.rct$X[, contName, drop = FALSE], 
                              data.rwe$X[, contName, drop = FALSE]))
    df[[ response_name ]] <- ipw_hte
    
    if (!is.null(contName)) {
      form <- paste(response_name, "~", paste(contName, collapse = " + ")) |>
        stats::as.formula()
    } else {
      form <- paste(response_name, "~ 1") |> stats::as.formula()
    }
    
    .metaAnalysis.cont(formula = form, data = df, 
                       predict.subset = c(rep(FALSE, nrow(data.rct$X)),
                                          rep(TRUE, nrow(data.rwe$X))))
    
  } else {
    .metaAnalysis.bin(X = rbind(data.rct$X[, contName, drop = FALSE], 
                                data.rwe$X[, contName, drop = FALSE]), 
                      Y = ipw_hte,
                      predict.subset = c(rep(FALSE, nrow(data.rct$X)),
                                         rep(TRUE, nrow(data.rwe$X))),
                      optim.controls = optim.controls)
  }
  
}

.conditionalVar <- function(y, yhat, outcome.type) {
  switch(outcome.type,
         "cont" = stats::var(y - yhat),
         "bin" = mean(yhat) * {1.0 - mean(yhat)})
}

#' Estimate Main Effects
#' 
#' @noRd
#' @param data A list object. Must contain elements Y, A, X, and q.
#' @param psi A numeric vector object. The estimated psi.
#' @param mainName NULL or a character vector. The main effects covariate names.
#' @param outcome.type A character object. Must be one of "cont" or "bin".
#' @param method A character object. The regression method.
#' @param method.controls A list object. The user specified inputs for 
#'   SuperLearner::SuperLearner().
#' @param fit.name A character object. The name of the fit; used for messaging.
#' 
#' @return A list containing `me`, the estimated main effects and 
#'   `me.conditional.var`, the estimated conditional variance.
#'   
#' @importFrom stats var
#' @include HTE.R sieveEstimator.R stopTests.R
#' @keywords internal
.mainEffects <- function(data, psi, mainName, 
                         outcome.type, method, method.controls, 
                         fit.name) {
  stopifnot(
    "`data` must be a named list containing elements 'X', 'Y', 'A', and 'q'" =
      !missing(data) && is.vector(data, mode = "list") && 
      all(c("X", "Y", "A", "q") %in% names(data)),
    "`psi` must be a named numeric vector" = !missing(psi) &&
      .isNumericVector(psi) && !is.null(names(psi)),
    "`mainName` must be NULL or a character vector" = !missing(mainName) &&
      {is.null(mainName) ||
          {.isCharacterVector(mainName) && all(mainName %in% colnames(data$X))}},
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin"),
    "`method` must be provided" = !missing(method),
    "`method.controls` must be a named list" = !missing(method.controls) &&
      is.vector(method.controls, mode = "list") && !is.null(names(method.controls)),
    "`fit.name` must be a character" = !missing(fit.name) && 
      .isCharacterVector(fit.name, 1L)
  )

  hte <- .HTE(psi = psi, X = data$X, outcome.type = outcome.type)
  main_effects <- data$Y - data$A * hte

  me <- tryCatch(.sieveEstimator(X = data$X[, mainName, drop = FALSE], 
                                 Y = main_effects, 
                                 wgt = data$q,
                                 sieve.degree = 1L,
                                 subset = rep(TRUE, times = nrow(data$X)),
                                 method = method,
                                 method.controls = method.controls),
                 error = function(e) {
                   stop("unable to fit adjusted outcome model for ", fit.name, 
                        "\n\t", e$message, call. = FALSE)
                   })

  conditional.var <- .conditionalVar(y = main_effects,
                                     yhat = me,
                                     outcome.type = outcome.type)

  list("me" = me, "me.conditional.var" = conditional.var)
}

#' RCT Analysis
#' 
#' @noRd
#' @param data.rct A list object. Must contain elements Y, A, and X.
#' @param data.rwe A list object. Must contain elements Y, A, and X.
#' @param mainName NULL or a character vector. The main effects covariate names.
#' @param contName NULL or a character vector. The contrasts covariate names.
#' @param outcome.type A character object. Must be one of "cont" or "bin".
#' @param method A character object. The regression method.
#' @param method.controls A list object. The user specified inputs for 
#'   the regression method.
#' 
#' @return A list containing "est.rct", the estimated parameters, 
#'   "att.rct", the mean of the HTE, the inverse of the
#'   variance of the residuals of the main effects model fit using RCT, 
#'   "rwe.wgt", the inverse of the variance of the residuals of the main effects
#'   model fit using RWE
#'   
#' @importFrom stats var
#' @include scores.R stopTests.R
#' @keywords internal
.rctAnalysis <- function(data.rct, data.rwe, outcome.type, models) {
  
  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rct) && is.vector(data.rct, mode = "list") &&
      all(c("X", "Y", "A") %in% names(data.rct)),
    "`data.rwe` must be provided" = !missing(data.rwe),
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'" =
      !missing(models) && is.vector(models, mode = "list") &&
      all(c("RWE", "RCT", "outcome", "ps", "sieve.degree", "contName", "cfName") %in% names(models)),
    "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'" =
      is.vector(models$RWE, mode = "list") && is.vector(models$RCT, mode = "list") &&
      all(c("ME", "PS") %in% names(models$RWE)) && all(c("ME", "PS") %in% names(models$RCT)),
    "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'" =
      is.vector(models$outcome, mode = "list") && is.vector(models$ps, mode = "list") &&
      all(c("method", "controls") %in% names(models$outcome)) &&
      all(c("method", "controls") %in% names(models$ps))
  )
  
  ### RCT
  initial_guess <- numeric(length(models$contName) + 1L)
  names(initial_guess) <- c("(Intercept)", models$contName)
  
  # HTE parameters if there are no main effects
  psi_p <- .rootsOfScore(X = data.rct$X[, models$contName, drop = FALSE],
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.rct$Y,
                         A = data.rct$A,
                         outcome.type = outcome.type,
                         mu = numeric(nrow(data.rct$X)),
                         ps = rep(0.5, nrow(data.rct$X)),
                         inv.sig2 = rep(1.0, nrow(data.rct$X)),
                         wgt = rep(1.0, nrow(data.rct$X)))
  
  # estimated main effects using initial psi_p
  me_rct <- .mainEffects(data = data.rct, psi = psi_p, mainName = models$RCT$ME, 
                         outcome.type = outcome.type,
                         method = models$outcome$method,
                         method.controls = models$outcome$controls,
                         fit.name = "RCT dataset")
  
  # re-estimate HTE parameters using estimated main effects
  est_rct <- .rootsOfScore(X = data.rct$X[, models$contName, drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.rct$Y,
                           A = data.rct$A,
                           outcome.type = outcome.type,
                           mu = me_rct$me,
                           ps = rep(0.5, nrow(data.rct$X)),
                           inv.sig2 = rep(1.0, nrow(data.rct$X)),
                           wgt = rep(1.0, nrow(data.rct$X)))
  
  # estimated HTE
  tau <- .HTE(psi = est_rct, X = data.rct$X, outcome.type = outcome.type)
  
  ### RWE
  
  # estimated main effects of RWE using initial psi_p
  me_rwe <- .mainEffects(data = data.rwe, psi = psi_p, 
                         mainName = models$RWE$ME, 
                         outcome.type = outcome.type,
                         method = models$outcome$method,
                         method.controls = models$outcome$controls,
                         fit.name = "RWE dataset")
  
  list("est.rct" = est_rct,
       "att.rct" = mean(tau), 
       "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, nrow(data.rct$X)),
       "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, nrow(data.rwe$X)))
}

#' Integrative Analysis
#' 
#' @noRd
#' @param data.rct A list object. Must contain elements Y, A, and X.
#' @param data.rwe A list object. Must contain elements Y, A, and X.
#' @param outcome.type A character object. Must be one of "cont" or "bin".
#' 
#' @return A list containing "est.int", the estimated contrast parameters
#'   obtained using the full RCT + RWE data; and "att.int", the mean of the 
#'   estimated HTE.
#' 
#' @include stopTests.R
#' @keywords internal
.integrativeAnalysis <- function(data.rct, data.rwe, outcome.type, models) {
  
  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'" =
      !missing(data.rct) && is.vector(data.rct, mode = "list") &&
      all(c("X", "Y", "A", "q", "ps", "inv.sig2") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'" =
      !missing(data.rwe) && is.vector(data.rwe, mode = "list") &&
      all(c("X", "Y", "A", "q", "ps", "inv.sig2") %in% names(data.rwe)),
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'" =
      !missing(models) && is.vector(models, mode = "list") &&
      all(c("RWE", "RCT", "outcome", "ps", "sieve.degree", "contName", "cfName") %in% names(models)),
    "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'" =
      is.vector(models$RWE, mode = "list") && is.vector(models$RCT, mode = "list") &&
      all(c("ME", "PS") %in% names(models$RWE)) && all(c("ME", "PS") %in% names(models$RCT)),
    "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'" =
      is.vector(models$outcome, mode = "list") && is.vector(models$ps, mode = "list") &&
      all(c("method", "controls") %in% names(models$outcome)) &&
      all(c("method", "controls") %in% names(models$ps))
  )

  if (!all(names(data.rct) %in% names(data.rwe))) {
    stop("`data.rwe` does not contain appropriate elements", call. = FALSE)
  }
  data.rwe <- data.rwe[names(data.rct)]
  
  data_integ <- mapply(data.rct, data.rwe,
                       FUN = function(x, y) {
                         if (is.matrix(x)) {
                           idx <- match(colnames(x), colnames(y))
                           rbind(x[, !is.na(idx), drop = FALSE],
                                 y[, idx[!is.na(idx)], drop = FALSE])
                         } else {
                           c(x, y)
                         }
                       }, SIMPLIFY = FALSE)
  
  initial_guess <- numeric(length(models$contName) + length(models$cfName) + 2L)
  names(initial_guess) <- c("(Intercept)", models$contName, "(Intercept)", models$cfName)

  # initial estimates of psi assuming no main effects
  est_intpre <- .rootsOfScore(X = data_integ$X[, models$contName, drop = FALSE],
                              initial.guess = initial_guess,
                              fit.name = "Integrative Estimator",
                              score.func = "integ",
                              X.cf = data.rwe$X[, models$cfName, drop = FALSE],
                              Y = data_integ$Y,
                              A = data_integ$A,
                              outcome.type = outcome.type,
                              wgt = rep(1.0, nrow(data_integ$X)),
                              inv.sig2 = data_integ$inv.sig2,
                              ps = data_integ$ps,
                              mu = numeric(nrow(data_integ$X)))
  
  psi <- est_intpre[ seq_len(length(models$contName) + 1L)]
  phi <- est_intpre[-seq_len(length(models$contName) + 1L)]
  
  # estimate main effects of RCT data using estimated tau
  me_rct <- .mainEffects(data = data.rct, psi = psi, 
                         mainName = models$RCT$ME, 
                         outcome.type = outcome.type,
                         method = models$outcome$method,
                         method.controls = models$outcome$controls,
                         fit.name = "RCT dataset")
  
  # estimate main effects of RWE data using estimated tau and lambda
  lambda <- .lambda(phi = phi, X = data.rwe$X, n.rct = 0L)
  data.rwe$Y <- data.rwe$Y - lambda * {data.rwe$A - data.rwe$ps}
  me_rwe <- .mainEffects(data = data.rwe, psi = psi, 
                         mainName = models$RWE$ME, 
                         outcome.type = outcome.type,
                         method = models$outcome$method,
                         method.controls = models$outcome$controls,
                         fit.name = "RWE dataset")

  # estimate parameters of tau and lambda using estimated main effects
  est_int <- .rootsOfScore(X = data_integ$X[, models$contName, drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "Integrative Estimator",
                           score.func = "integ",
                           X.cf = data.rwe$X[, models$cfName, drop = FALSE],
                           Y = data_integ$Y,
                           A = data_integ$A,
                           outcome.type = outcome.type,
                           inv.sig2 = c(rep(1.0 / me_rct$me.conditional.var, nrow(data.rct$X)),
                                        rep(1.0 / me_rwe$me.conditional.var, nrow(data.rwe$X))),
                           wgt = rep(1.0, nrow(data_integ$X)),
                           ps = data_integ$ps,
                           mu = c(me_rct$me, me_rwe$me))

  # HTE for RWE dataset
  tau <- .HTE(psi = est_int[seq_len(length(models$contName) + 1L)],
              X = data.rwe$X, 
              outcome.type = outcome.type)

  list("est.int" = est_int,
       "att.int" = mean(tau))
}

#' Full Parameter Procedure
#' 
#' @noRd
#' @param data.rct A list object. Must contain elements Y, A, and X.
#' @param data.rwe A list object. Must contain elements Y, A, and X.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param models A list. Must contain elements 'RCT', a list containing the
#'   main effects model (ME) and propensity score model (PS); 'RWE', a list
#'   containing the main effects model (ME) and propensity score model (PS);
#'   'contName' the variables of the treatment effect model; 
#'   'cfName' the confounding variables; sieve.degree',
#'   the degree of the Sieve estimator; 'outcome', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   outcome regression; and 'ps', a list containing the
#'   method (method) and regression control arguments (controls) for the
#'   propensity score regression
#' 
#' @return A list containing 
#' 
#' @include propensityScore.R stopTests.R
#' @keywords internal
.psiEst_IntHTEcf <- function(data.rct, data.rwe, outcome.type, models, optim.controls) {
  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rct) && is.vector(data.rct, mode = "list") &&
      all(c("X", "Y", "A") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rwe) && is.vector(data.rwe, mode = "list") &&
      all(c("X", "Y", "A") %in% names(data.rwe)),
    "`outcome.type` must be one of 'cont' or 'bin'" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("bin", "cont"),
    "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'" =
      !missing(models) && is.vector(models, mode = "list") &&
      all(c("RWE", "RCT", "outcome", "ps", "sieve.degree", "contName", "cfName") %in% names(models)),
    "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'" =
      is.vector(models$RWE, mode = "list") && is.vector(models$RCT, mode = "list") &&
      all(c("ME", "PS") %in% names(models$RWE)) && all(c("ME", "PS") %in% names(models$RCT)),
    "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'" =
      is.vector(models$outcome, mode = "list") && is.vector(models$ps, mode = "list") &&
      all(c("method", "controls") %in% names(models$outcome)) &&
      all(c("method", "controls") %in% names(models$ps)),
    "`optim.controls` must be provided" = !missing(optim.controls)
  )
  
  ## propensity score estimation
  if (data.rct$est.ps) {
    if (length(unique(data.rct$A)) > 1L) {
      ps <- tryCatch(.propensityScore(X = data.rct$X[, models$RCT$PS, drop = FALSE],
                                      A = data.rct$A,
                                      wgt = rep(1.0, nrow(data.rct$X)),
                                      sieve.degree = 1L,
                                      method = models$ps$method,
                                      method.controls = models$ps$controls,
                                      models = "ps"),
                     error = function(e) {
                       stop("unable to estimate parameters for RCT propensity\n\t",
                            e$message, call. = FALSE)
                     })
      data.rct$ps <- ps$ps
    } else {
      message("All treatments are the same in RCT, propensity set as 1")
      data.rct$ps <- rep(1.0, nrow(data.rct$X))
    }
  } else {
    if (is.null(data.rct$ps)) {
      stop("ps must be provided for RCT data", call. = FALSE)
    }
  }
  
  # need to remove this for later mapply -- names of the data.x lists
  # must match
  data.rct$est.ps <- NULL

  if (length(unique(data.rwe$A)) > 1L) {
    ps <- tryCatch(.propensityScore(X = data.rwe$X[, models$RWE$PS, drop = FALSE],
                                    A = data.rwe$A,
                                    wgt = rep(1.0, nrow(data.rwe$X)),
                                    sieve.degree = 1L,
                                    method = models$ps$method,
                                    method.controls = models$ps$controls,
                                    models = "ps"),
                   error = function(e) {
                     stop("unable to estimate parameters for RWE propensity\n\t",
                          e$message, call. = FALSE)
                   })
    data.rwe$ps <- ps$ps
  } else {
    message("All treatments are the same in RWE, propensity set as 1")
    data.rwe$ps <- rep(1.0, nrow(data.rwe$X))
  }
  
  data.rct$q <- rep(1.0, nrow(data.rct$X))
  data.rwe$q <- rep(1.0, nrow(data.rwe$X))
  
  meta <- .metaAnalysis(data.rct = data.rct, 
                        data.rwe = data.rwe, 
                        contName = models$contName,
                        outcome.type = outcome.type,
                        optim.controls = optim.controls)
  
  rct <- .rctAnalysis(data.rct = data.rct, 
                      data.rwe = data.rwe,
                      outcome.type = outcome.type, 
                      models = models)
  
  data.rct$inv.sig2 <- rct$rct.inv.sig2
  data.rwe$inv.sig2 <- rct$rwe.inv.sig2
  
  rct$rct.inv.sig2 <- NULL
  rct$rwe.inv.sig2 <- NULL
  
  integrative <- .integrativeAnalysis(data.rct = data.rct, 
                                      data.rwe = data.rwe, 
                                      outcome.type = outcome.type,
                                      models = models)
  c(meta, rct, integrative)
}