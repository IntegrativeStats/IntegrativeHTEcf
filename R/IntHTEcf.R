#' Integrative Analysis of the Heterogeneous Treatment Effect with Confounding Functions
#'
#' Implements an integrative estimator of the heterogeneity of treatment effect
#'   and confounding function based on a semiparametric efficient score that
#'   combines randomized trial data (RCT) with data from a real-world evidence 
#'   (RWE) study.
#'   
#' @details
#'    
#' Inputs \code{data.rct} and \code{data.rwe} are most easily specified using
#'   the provided convenience function \link{dataInput}(). However, this is
#'   not required. See \link{dataInput}() for details of the returned
#'   object.
#'   
#'@note
#'   When specifying \code{outcome.controls} and \code{ps.controls}, some
#'   input arguments cannot be accessed. Specifically, formal arguments
#'   \code{Y}, \code{X}, \code{newX}, and  \code{obsWeight} of
#'   \code{SuperLearner::SuperLearner()} and \code{formula}, \code{data}, and
#'   \code{weights} of \code{stats::glm()} or \code{mgcv::gam()}
#'   cannot be set through these inputs.
#'
#' @param data.rct The value object returned by \code{dataInput()} for the
#'   data from a randomized clinical trial (RCT). See \link{dataInput} for
#'   further details.
#' @param data.rwe The value object returned by \code{dataInput()} for the
#'   data from a real-world evidence (RWE) study. See \link{dataInput} for
#'   further details. Note that the treatment effect model must be identical
#'   to that of \code{data.rct}.
#' @param cfName NULL, character vector, or an integer. The covariates of the
#'   the confounding function. If NULL, all covariates in \code{data.rwe$X}
#'   specify the function; if a character vector, the column headers of
#'   \code{data.rwe$X} to include in the model. Note that an intercept is
#'   always included; an intercept only model can be specified as
#'   \code{cfName = 1}.
#' @param ... Ignored. Included to require named inputs.
#' @param ps.rct NULL or a numeric vector. Optional input providing a vector of
#'   known propensity scores P(A=1) for the RCT dataset. If not provided,
#'   it will be estimated using the model defined in \code{data.rct$psName}.
#' @param outcome.type A character. The type of outcome. Must be one of
#'   \{"cont", "bin"\} indicating a continuous or binary outcome, respectively.
#' @param outcome.method A character. The regression method to use for
#'   outcome regressions. Must be one of \{"gam", "glm", "SL"\} indicating
#'   \code{mgcv::gam()}, \code{stats::glm()}, and \code{SuperLearner::SuperLearner()},
#'   respectively. Note
#'   that for "gam", smoothing splines (\code{s()}) are automatically applied.
#'   Further, the outcome is modeled after adjustment, i.e., Y - tau(Z)
#'   is the modeled outcome variable.
#' @param outcome.controls A named list. Additional inputs provided to 
#'   `outcome.method`. 
#'   Element names must match the formal arguments of `outcome.method`.
#'   Note that arguments defining the model parameters, data.frame to analyze,
#'   and weights will be set internally.
#' @param ps.method A character. The regression method to use for
#'   propensity score regressions. Must be one of \{"glm", "gam", SL"\} indicating
#'   \code{mgcv::gam()}, \code{stats::glm()}, and \code{SuperLearner::SuperLearner()},
#'   respectively. Note
#'   that for "gam", smoothing splines (\code{s()}) are automatically applied.
#' @param ps.controls A named list. Additional inputs provided to 
#'   `ps.method`. 
#'   Element names must match the formal arguments of `ps.method`.
#'   Note that arguments defining the model parameters, data.frame to analyze,
#'   and weights will be set internally.
#' @param n.boot An integer. The number of bootstrap samples to generate
#'   when estimating the confidence intervals. If 0, variance estimates
#'   are not provided.
#' @param optim.controls A list object. For binary outcomes, the meta-analysis
#'   uses stats::optim() to estimate the parameters of the HTE. This input can 
#'   be used to modify the default settings
#'   of that analysis. For example, the default method is "L-BFGS-B"; to 
#'   change this to "Nelder-Mead", optim.controls = list("method" = "Nelder-Mead").
#'   For continuous outcomes, this input is ignored.
#'
#' @return A list object containing 
#' \describe{
#'   \item{\code{est.meta}: }{The HTE estimator based on a meta analysis.}
#'   \item{\code{est.rct}: }{The HTE estimator using the semiparametric 
#'     efficient estimation based only on RCT data.}
#'   \item{\code{est.int}: }{The HTE estimator using the semiparametric 
#'     efficient estimation based on the combined RCT and RWE.}
#'   \item{\code{att.meta}: }{The ATT estimator based on a meta analysis.}
#'   \item{\code{att.rct}: }{The ATT estimator based only on RCT data.}
#'   \item{\code{att.int}: }{The ATT estimator based on the combined RCT and RWE.}
#'   \item{\code{ve.meta}: }{The variance estimates of est.meta.}
#'   \item{\code{ve.rct}: }{The variance estimates of est.rct.}
#'   \item{\code{ve.int}: }{The variance estimates of est.int.}
#'   \item{\code{ve.att.meta}: }{The variance estimates of att.meta.}
#'   \item{\code{ve.att.rct}: }{The variance estimates of att.rct.}
#'   \item{\code{ve.att.int}: }{The variance estimates of att.int.}
#' }
#'
#' @examples
#' data("IntHTEToy.cont")
#' 
#' IntHTEcf(dataInput(IntHTEToy.cont.rct, 
#'                    outcome.model = Y ~ X1*A,
#'                    ps.model = A ~ X1 + X2),
#'          dataInput(IntHTEToy.cont.rwe, 
#'                    outcome.model = Y ~ X2 + X1*A,
#'                    ps.model = A ~ X1 + X2),
#'          cfName = "X1")
#'
#' @export
#' @include allEstimators.R bootFunc.R stopTests.R utils.R
IntHTEcf <- function(data.rct, data.rwe, cfName = NULL, ..., 
                     ps.rct = NULL,
                     outcome.type = c("cont", "bin"),
                     outcome.method = c("gam", "glm", "SL"),
                     outcome.controls = list("family" = "gaussian"),
                     ps.method = c("glm", "gam", "SL"),
                     ps.controls = list("family" = "binomial"),
                     n.boot = 100L,
                     optim.controls = list("method" = "L-BFGS-B")) {
  
  outcome.type <- match.arg(outcome.type)
  outcome.method <- match.arg(outcome.method)
  ps.method <- match.arg(ps.method)
  
  stopifnot(
    "`data.rct` must be provided" = !missing(data.rct),
    "`data.rwe` must be provided" = !missing(data.rwe)
  )
  
  .isDI(data.rct, "data.rct")
  .isDI(data.rwe, "data.rwe")
  
  stopifnot(
    "`data.rct$contName` must match `data.rwe$contName`" =
      isTRUE(all.equal(data.rct$contName, data.rwe$contName)),
    "`cfName` must be a character vector of X column headers" = is.null(cfName) ||
      {.isNumericVector(cfName, 1L) && isTRUE(all.equal(cfName, 1))} ||
      {.isCharacterVector(cfName) && all(cfName %in% colnames(data.rwe$X))},
    "`ps.rct` must be NULL or a numeric vector of length = nrow(data.rct$X)" =
      is.null(ps.rct) || .isNumericVector(ps.rct, nrow(data.rct$X)),
    "`outcome.type` must be one of {'cont', 'bin'}" = .isCharacterVector(outcome.type, 1L) &&
      outcome.type %in% c("cont", "bin"),
    "`outcome.controls` must be a named list" = is.vector(outcome.controls, mode = "list") &&
      {{length(outcome.controls) > 0L && !is.null(names(outcome.controls)) &&
          !any(nchar(names(outcome.controls)) == 0L)} ||
          {length(outcome.controls) == 0L}},
    "`ps.controls` must be a named list" = is.vector(ps.controls, mode = "list") &&
      {{length(ps.controls) > 0L && !is.null(names(ps.controls)) &&
          !any(nchar(names(ps.controls)) == 0L)} ||
          {length(ps.controls) == 0L}},
    "`n.boot` must be a non-negative integer" = .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0L))) && n.boot >= 0,
    "`optim.controls` must be a list" = is.vector(optim.controls, mode = "list")
  )
  
  if (is.null(outcome.controls$family)) outcome.controls$family <- "gaussian"

  if (is.null(ps.controls$family)) ps.controls$family <- "binomial"

  # NULL input means "all covariates in X"; integer input means "intercept only"
  mainName.rct <- .adjustModelCoding(data.rct$mainName, colnames(data.rct$X))
  mainName.rwe <- .adjustModelCoding(data.rwe$mainName, colnames(data.rwe$X))
  contName <- .adjustModelCoding(data.rct$contName, colnames(data.rct$X))
  psName.rct <- .adjustModelCoding(data.rct$psName, colnames(data.rct$X))
  psName.rwe <- .adjustModelCoding(data.rwe$psName, colnames(data.rwe$X))
  cfName <- .adjustModelCoding(cfName, colnames(data.rwe$X))

  data.rct[c("mainName", "contName", "psName")] <- NULL
  data.rwe[c("mainName", "contName", "psName")] <- NULL
  
  # reduce dataset down to only those covariates used in models
  if (is.null(mainName.rct) && is.null(contName) && is.null(psName.rct)) {
    data.rct$X <- matrix(NA, nrow(data.rct$X), 0L)
  } else if (!is.null(mainName.rct) || !is.null(contName) || !is.null(psName.rct)) {
    all_cov <- unique(c(mainName.rct, contName, psName.rct))
    
    if (!all(all_cov %in% colnames(data.rct$X))) {
      stop("not all model covariates are found in provided data\n\t",
           "model covariate: ", paste(all_cov, collapse = ", "), "\n\t",
           "data.rct: ", paste(colnames(data.rct$X), collapse = ", "), "\n\t", call. = FALSE)
    }
    
    data.rct$X <- data.rct$X[, all_cov, drop = FALSE]
    
    # spaces in covariate names might cause issues later
    colnames(data.rct$X) <- .fixNames(colnames(data.rct$X))
    mainName.rct <- .fixNames(mainName.rct)
    contName <- .fixNames(contName)
    psName.rct <- .fixNames(psName.rct)
    
    # if this introduces duplicate column headers, ask user to adjust column
    # headers themselves
    if (length(unique(colnames(data.rct$X))) != ncol(data.rct$X)) {
      stop("duplicate column headers found in X, ",
           "possibly due to required removal of spaces ",
           "please eliminate spaces from column header names in `data.rct$X` ",
           call. = FALSE)
    }
  }
  
  if (is.null(mainName.rwe) && is.null(contName) && is.null(psName.rwe) && is.null(cfName)) {
    data.rwe$X <- matrix(NA, nrow(data.rwe$X), 0L)
  } else if (!is.null(mainName.rwe) || !is.null(contName) || 
             !is.null(psName.rwe) || !is.null(cfName)) {
    all_cov <- unique(c(mainName.rwe, contName, psName.rwe, cfName))
    
    if (!all(all_cov %in% colnames(data.rwe$X))) {
      stop("not all model covariates are found in provided data\n\t",
           "model covariate: ", paste(all_cov, collapse = ", "), "\n\t",
           "data.rwe: ", paste(colnames(data.rwe$X), collapse = ", "), "\n\t", call. = FALSE)
    }
    
    data.rwe$X <- data.rwe$X[, all_cov, drop = FALSE]
    
    # spaces in covariate names might cause issues later
    colnames(data.rwe$X) <- .fixNames(colnames(data.rwe$X))
    mainName.rwe <- .fixNames(mainName.rwe)
    contName <- .fixNames(contName)
    psName.rwe <- .fixNames(psName.rwe)
    cfName <- .fixNames(cfName)
    
    # if this introduces duplicate column headers, ask user to adjust column
    # headers themselves
    if (length(unique(colnames(data.rwe$X))) != ncol(data.rwe$X)) {
      stop("duplicate column headers found in X, ",
           "possibly due to required removal of spaces ",
           "please eliminate spaces from column header names in `data.rwe$X` ",
           call. = FALSE)
    }
  }  
  
  # we do not allow for missing values
  if (any(!stats::complete.cases(data.rct$X, data.rct$Y, data.rct$A)) ||
      any(!stats::complete.cases(data.rwe$X, data.rwe$Y, data.rwe$A))) {
    stop("elements of `data.rct` and `data.rwe` cannot contain missing values",
         call. = FALSE)
  }
  
  data.rct$ps <- ps.rct
  
  # if user provided ps for RWE, warn and remove
  if (!is.null(data.rwe$ps)) {
    warning("`ps` cannot be provided in `data.rwe`; input ignored",
            call. = FALSE)
    data.rwe$ps <- NULL
  }

  # keep only the information we need
  data.rct <- data.rct[names(data.rct) %in% c("X", "Y", "A", "ps")]
  data.rwe <- data.rwe[names(data.rwe) %in% c("X", "Y", "A")]

  # flag indicates if user provided ps for RCT
  # TRUE indicates that these propensity scores must be estimated
  data.rct$est.ps <- is.null(data.rct$ps)
  
  # number of participants in RCT
  n_rct <- length(data.rct$Y)
  
  # number of participants in RWE
  n_rwe <- length(data.rwe$Y)
  
  # STH not sure if this is true for this estimator
  if (n_rct >= n_rwe) {
    warning("methods developed under the assumption that n >> m; ",
            "requested analysis has m/n = ", format(n_rct / n_rwe, digits = 2),
            call. = FALSE)
  }
  
  # ensure that treatments are binary integer 0/1
  if (is.factor(data.rct$A) && is.factor(data.rwe$A)) {
    if (all(levels(data.rct$A) != levels(data.rwe$A))) {
      stop("levels of data.rct$A and data.rwe$A do not match", call. = FALSE)
    }
  } else if (is.factor(data.rct$A)) {
    orig_levels <- levels(data.rct$A)
    if (!all(data.rwe$A %in% orig_levels)) {
      stop("treatment sets of data.rct$A and data.rwe$A do not match", call. = FALSE)
    }
  } else if (is.factor(data.rwe$A)) {
    orig_levels <- levels(data.rwe$A)
    if (!all(data.rct$A %in% orig_levels)) {
      stop("treatment sets of data.rct$A and data.rwe$A do not match", call. = FALSE)
    }
  } else {
    orig_levels <- c(unique(data.rct$A), unique(data.rwe$A)) |> unique() |> sort()
  }
  
  # ensure the treatment in RCT and RWE are the same set
  if (length(orig_levels) != 2L) {
    stop("more than 2 treatments found in data.rct$A and data.rwe$A", call. = FALSE)
  }
  
  data.rct$A <- .fixA(data.rct$A, "data.rct$A", orig.levels = orig_levels)
  data.rwe$A <- .fixA(data.rwe$A, "data.rwe$A", orig.levels = orig_levels)
  
  # Ensure that outcome type does not blatantly conflict with provided data
  # NA values shouldn't be an issue as there is a previous stopping condition
  # for that
  data.rct$Y <- .fixY(data.rct$Y, outcome.type, "data.rct$Y")
  data.rwe$Y <- .fixY(data.rwe$Y, outcome.type, "data.rwe$Y")
  
  models <- list("RWE" = list("ME" = mainName.rwe,
                              "PS" = psName.rwe),
                 "RCT" = list("ME" = mainName.rct,
                              "PS" = psName.rct),
                 "contName" = contName,
                 "cfName" = cfName,
                 "outcome" = list("method" = outcome.method,
                                  "controls" = outcome.controls),
                 "ps" = list("method" = ps.method,
                             "controls" = ps.controls),
                 "sieve.degree" = 1L)
  
  out <- .psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                          outcome.type = outcome.type, models = models,
                          optim.controls = optim.controls)
  
  # Add psi/phi to integrative parameter names
  names(out$est.int) <- paste(c(rep("psi", length(contName) + 1L),
                                rep("phi", length(cfName) + 1L)),
                                names(out$est.int), sep = ".")

  if(n.boot > 1L) {
    ve <- .bootFunc(data.rct = data.rct, data.rwe = data.rwe, 
                    outcome.type = outcome.type, models = models,
                    n.boot = n.boot, optim.controls = optim.controls)
    # Add psi/phi to integrative parameter names
    names(ve$ve.est.int) <- paste(c(rep("psi", length(contName) + 1L),
                                    rep("phi", length(cfName) + 1L)),
                                  names(ve$ve.est.int), sep = ".")
    
    
    cov <- ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")]
    ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")] <- NULL
    
    colnames(cov$cov.est.int) <- paste(c(rep("psi", length(contName) + 1L),
                                         rep("phi", length(cfName) + 1L)),
                                       colnames(cov$cov.est.int), sep = ".")
    rownames(cov$cov.est.int) <- paste(c(rep("psi", length(contName) + 1L),
                                         rep("phi", length(cfName) + 1L)),
                                       rownames(cov$cov.est.int), sep = ".")
    
  } else {
    ve <- list("ve" = NA)
    cov <- list("cov" = NA)
  }
  
  res <- c(out, ve, cov) 
  res$call <- match.call()
  class(res) <- c("IntHTEcf", class(res))
  res
  
}