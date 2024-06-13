#' Generate continuous data for example
#'
#' Code used to create package datasets. Provided for the convenience of future
#'   developer, not intended for use by users. Default settings are those
#'   used to generate the data provided with the package. The code is not
#'   robustly tested.
#'
#' @noRd
#' @param seed n ninteger. The random seed for data generation
#' @param n.rct An integer. The size of the clinical trial dataset.
#' @param n.rwe An integer. The size of the real-world evidence dataset.
#'
#' @returns A list containing trivial datasets provided with the package
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rbinom rnorm runif
#' @keywords internal
.generateToyContData <- function(seed = 1234L, n.rct = 100L, n.rwe = 500L) {

  set.seed(seed)

  A_rct <- stats::rbinom(n.rct, 1L, 0.5)
  X_rct <- matrix(stats::runif(n.rct, -1.0, 1.0), ncol = 1L)
  X_rct <- cbind(X_rct, X_rct^2)
  colnames(X_rct) <- c("X1", "X2")
  U_rct <- matrix(stats::rnorm(n.rct, 1.0, 1.0), ncol = 1L)
  colnames(U_rct) <- c("U1")
  Y_rct <- 1.0 + X_rct[, 1L] + 0.5 * X_rct[, 2L] + 0.5 * U_rct[, 1L] +
    0.5 * stats::rnorm(n.rct) + A_rct * {1.0 + 0.75 * X_rct[, 2L]}

  A_rwe <- stats::rbinom(n.rwe, 1L, 0.5)
  sigma0 <- matrix(c(1.0, -0.5, -0.5, 1.0), 2L, 2L)
  sigma1 <- matrix(c(1.0,  0.5,  0.5, 1.0), 2L, 2L)
  XU0 <- MASS::mvrnorm(n.rwe, c(1.0, 0.0), sigma0)
  XU1 <- MASS::mvrnorm(n.rwe, c(1.0, 0.0), sigma1)

  X_rwe <- matrix(XU0[, 1L] * {1.0 - A_rwe} + XU1[, 1L] * A_rwe, ncol = 1L)
  X_rwe <- cbind(X_rwe, X_rwe^2)
  colnames(X_rwe) <- c("X1", "X2")
  U_rwe <- matrix(XU0[, 2L] * {1.0 - A_rwe} + XU1[, 2L] * A_rwe, ncol = 1L)
  Y_rwe <- 1.0 + X_rwe[, 1L] + 0.5 * X_rwe[, 2L] + 0.5 * U_rwe[, 1L] +
    0.5 * stats::rnorm(n.rwe) + A_rwe * {1.0 + 0.75 * X_rwe[, 2L]}

  list("RCT" = data.frame(X_rct, "Y" = Y_rct, "A" = A_rct),
       "RWE" = data.frame(X_rwe, "Y" = Y_rwe, "A" = A_rwe))

}

#' Toy Continuous Outcome Dataset
#'
#' These datasets are provided only to facilitate examples. They are not based
#'   on or representative of any real-world applications.
#'
#' @name IntHTEToy.cont
#' @rdname IntHTEToy.cont
#' @aliases IntHTEToy.cont.rct IntHTEToy.cont.rwe
#'
#' @usage data("IntHTEToy.cont")
#'
#' @format IntHTEToy.cont provides two datasets. The IntHTEToy.cont.rct
#'   100 participant records; IntHTEToy.cont.rwe 500 participant records. Each
#'   dataset provides the following:
#' \itemize{
#' \item Y: A continuous outcome.
#' \item X1: A continuous covariate.
#' \item X2: A continuous covariate.
#' \item A: A binary treatment variable.
#' }
#'
#' @keywords datasets
NULL


#' Generate continuous data for example
#'
#' Code used to create package datasets. Provided for the convenience of future
#'   developer, not intended for use by users. Default settings are those
#'   used to generate the data provided with the package. The code is not
#'   robustly tested.
#'
#' @noRd
#' @param seed n ninteger. The random seed for data generation
#' @param n.rct An integer. The size of the clinical trial dataset.
#' @param n.rwe An integer. The size of the real-world evidence dataset.
#'
#' @returns A list containing trivial datasets provided with the package
#'
#' @importFrom stats rbinom
#' @keywords internal
.generateToyBinData <- function(seed = 2345L, n.rct = 100L, n.rwe = 500L) {

  data <- .generateToyContData(seed, n.rct, n.rwe)
  data$RCT$Y <- stats::rbinom(nrow(data$RCT$X), 1, prob = 1.0 / {1.0 + exp(-data$RCT$Y)})
  data$RWE$Y <- stats::rbinom(nrow(data$RWE$X), 1, prob = 1.0 / {1.0 + exp(-data$RWE$Y)})

  data
}

#' Toy Binary Outcome Dataset
#'
#' These datasets are provided only to facilitate examples. They are not based
#'   on or representative of any real-world applications.
#'
#' @name IntHTEToy.bin
#' @rdname IntHTEToy.bin
#' @aliases IntHTEToy.bin.rct IntHTEToy.bin.rwe
#'
#' @usage data("IntHTEToy.bin")
#'
#' @format IntHTEToy.bin provides two datasets. The IntHTEToy.bin.rct
#'   100 participant records; IntHTEToy.bin.rwe 500 participant records. Each
#'   list provides the following:
#' \itemize{
#' \item Y: A binary outcome.
#' \item X1: A continuous covariate.
#' \item X2: A continuous covariate.
#' \item A: A binary treatment variable.
#' }
#'
#' @keywords datasets
NULL
