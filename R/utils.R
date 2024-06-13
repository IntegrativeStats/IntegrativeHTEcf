# Convert A to 0/1 binary
.fixA <- function(A, obj.name, orig.levels) {

  stopifnot(
    "`A` must be a vector or factor" = !missing(A) &&
      {is.vector(A, mode = "character") || is.vector(A, mode = "integer") ||
          is.vector(A, mode = "numeric") || is.factor(A)} &&
      length(A) > 0L,
    "`obj.name` must be a character" = !missing(obj.name) &&
      .isCharacterVector(obj.name, 1L),
    "`orig.levels` must be a vector of length 2" = !missing(orig.levels) &&
      {is.vector(orig.levels, mode = "character") || is.vector(orig.levels, mode = "integer") ||
          is.vector(orig.levels, mode = "numeric")} && length(orig.levels)  == 2L
  )

  tmpA <- A
  if (!is.factor(tmpA)) {
    tmpA <- factor(tmpA, levels = orig.levels)
  }
  tmpA <- {unclass(tmpA) - 1L} |> as.integer()

  if (!all(tmpA %in% c(0L, 1L))) {
    stop("`", obj.name, "` must be binary", call. = FALSE)
  }

  tmpA
}


.fixY <- function(Y, outcome.type, obj.name) {
  stopifnot(
    "`Y` must be a vector or factor" = !missing(Y) &&
      {is.vector(Y, mode = "character") || is.vector(Y, mode = "integer") ||
          is.vector(Y, mode = "numeric") || is.factor(Y)} &&
      length(Y) > 0L,
    "`outcome.type` must be one of {'cont', 'bin'}" = !missing(outcome.type) &&
      .isCharacterVector(outcome.type, 1L) && outcome.type %in% c("cont", "bin"),
    "`obj.name` must be a character" = !missing(obj.name) &&
      .isCharacterVector(obj.name, 1L)
  )

  if (outcome.type == "bin") {
    tmpY <- Y
    if (!is.factor(tmpY)) {
      tmpY <- factor(tmpY)
    }
    tmpY <- {unclass(tmpY) - 1L} |> as.integer()

    if (!all(tmpY %in% c(0L, 1L))) {
      stop("`", obj.name, "` is not binary", call. = FALSE)
    }
    Y <- tmpY
  } else {
    if (is.vector(Y, mode = "character")) {
      stop("`", obj.name, "` must be numeric for `outcome.type` = 'cont'",
           call. = FALSE)
    }
    if (length(unique(Y)) <= 2L) {
      message(rep("* ", 10), "WARNING", rep(" *", 10), "\n",
              "`outcome.type` = 'cont'; however, response provided in ",
              obj.name, " has only ", length(unique(Y)), " unique values")
    }
  }
  Y
}

.fixNames <- function(nms) {
  stopifnot(
    "`nms` must be NULL or character" = !missing(nms) &&
      {is.null(nms) || .isCharacterVector(nms)}
  )

  if (is.null(nms)) {
    NULL
  } else {
    gsub(" ", ".", nms, fixed = TRUE)
  }
}


.adjustModelCoding <- function(model.names, available.covariates) {
  if (is.null(model.names)) {
    model.names <- available.covariates
  } else if (is.numeric(model.names)) {
    model.names <- NULL
  } else {
    if (!all(model.names %in% available.covariates)) {
      stop("unrecognized model covariate provided", call. = FALSE)
    }
  }
  model.names
}

.calculateTstat <- function(eta, inv.Sigma.SS) {
  stopifnot(
    "`eta` must be a numeric vector" = !missing(eta) &&
      .isNumericVector(eta),
    "`inv.Sigma.SS` must be a numeric matrix" = !missing(inv.Sigma.SS) &&
      .isNumericMatrix(inv.Sigma.SS) && ncol(inv.Sigma.SS) == nrow(inv.Sigma.SS) &&
      ncol(inv.Sigma.SS) == length(eta)
  )
  crossprod(eta, inv.Sigma.SS %*% eta) |> drop()
}
