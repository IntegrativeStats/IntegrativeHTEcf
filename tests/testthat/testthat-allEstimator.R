test_that("`.metaAnalysis.bin()` returns expected errors", {
  
  expect_error(.metaAnalysis.bin(), "`X` must be a named numeric matrix")
  expect_error(.metaAnalysis.bin(X = matrix("A", 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.metaAnalysis.bin(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named numeric matrix")
  expect_error(.metaAnalysis.bin(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.metaAnalysis.bin(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named numeric matrix")
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  
  expect_error(.metaAnalysis.bin(X = X),
               "`Y` must be a numeric vector")
  expect_error(.metaAnalysis.bin(X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.metaAnalysis.bin(X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.metaAnalysis.bin(X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")
  Y <- 1L:10L
  
  expect_error(.metaAnalysis.bin(X = X, Y = Y), 
               "`predict.subset` must be a logical vector")
  expect_error(.metaAnalysis.bin(X = X, Y = Y, 
                                 predict.subset = matrix(FALSE, 10L, 1L)),
               "`predict.subset` must be a logical vector")
  expect_error(.metaAnalysis.bin(X = X, Y = Y, predict.subset = rep(1L, 10L)),
               "`predict.subset` must be a logical vector")
  expect_error(.metaAnalysis.bin(X = X, Y = Y, predict.subset = rep(TRUE, 9L)),
               "`predict.subset` must be a logical vector")

})

test_that("`.metaAnalysis.bin()` returns expected results", {
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  par <- c("(Intercept)" = 1.0, "X1" = 1.0, "X2" = 1.5, "X3" = 2.0, "X4" = 2.5)
  Y <- .HTE(psi = par, X = X, outcome.type = "bin")
  
  expected <- list("est.meta" = par,
                   "att.meta" = mean(Y))

  expect_equal(.metaAnalysis.bin(X = X, Y = Y, predict.subset = rep(TRUE, n),
                                 optim.controls = list()), 
               expected, tol = 0.001)
})

test_that("`.metaAnalysis.bin()` returns expected results; single covariate", {
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1L))
  colnames(X) <- c("X1")
  par <- c("(Intercept)" = 1.0, "X1" = 1.0)
  Y <- .HTE(psi = par, X = X, outcome.type = "bin")
  
  expected <- list("est.meta" = par,
                   "att.meta" = mean(Y))
  
  expect_equal(.metaAnalysis.bin(X = X, Y = Y, predict.subset = rep(TRUE, n),
                                 optim.controls = list()), 
               expected, tol = 0.001)
})

test_that("`.metaAnalysis.bin()` returns expected results; no covariate", {
  n <- 1000L
  
  X <- matrix(NA, n, 0)
  par <- c("(Intercept)" = 1.15)
  Y <- .HTE(psi = par, X = X, outcome.type = "bin")

  expected <- list("est.meta" = par,
                   "att.meta" = mean(Y))

  expect_equal(.metaAnalysis.bin(X = X, Y = Y, predict.subset = rep(TRUE, n),
                                 optim.controls = list()), 
               expected, tol = 0.001)
})

test_that("`.metaAnalysis.cont()` returns expected errors", {
  
  expect_error(.metaAnalysis.cont(),
               "`formula` must be a formula object of the form LHS ~ RHS")
  expect_error(.metaAnalysis.cont(formula = "Y ~ X1"),
               "`formula` must be a formula object of the form LHS ~ RHS")
  expect_error(.metaAnalysis.cont(formula = ~ X1),
               "`formula` must be a formula object of the form LHS ~ RHS")
  
  formula <- Y ~ X1 + X2
  
  expect_error(.metaAnalysis.cont(formula = formula),
               "`data` must be a data.frame")
  expect_error(.metaAnalysis.cont(formula = formula, data = matrix(1, 100, 3)),
               "`data` must be a data.frame")
  expect_error(.metaAnalysis.cont(formula = formula, data = numeric(100)),
               "`data` must be a data.frame")
  expect_error(.metaAnalysis.cont(formula = formula, data = list("a" = 100)),
               "`data` must be a data.frame")
  data <- data.frame("X1" = 1:100, "X2" = 200)
  
  expect_error(.metaAnalysis.cont(formula = formula, data = data), 
               "`predict.subset` must be a logical vector")
  expect_error(.metaAnalysis.cont(formula = formula, data = data, 
                                  predict.subset = matrix(FALSE, 10L, 1L)),
               "`predict.subset` must be a logical vector")
  expect_error(.metaAnalysis.cont(formula = formula, data = data, 
                                  predict.subset = rep(1L, 10L)),
               "`predict.subset` must be a logical vector")
  expect_error(.metaAnalysis.cont(formula = formula, data = data, 
                                  predict.subset = rep(TRUE, 9L)),
               "`predict.subset` must be a logical vector")
  
})

test_that("`.metaAnalysis.cont()` returns expected results", {
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  par <- c("(Intercept)" = 1.0, "X1" = 1.0, "X2" = 1.5, "X3" = 2.0, "X4" = 2.5)
  Y <- .HTE(psi = par, X = X, outcome.type = "cont")
  data <- as.data.frame(cbind(Y = Y, X))
  
  ipw_me_fit <- stats::glm(formula = Y ~ X1 + X2 + X3 + X4,
                           data = data,
                           family = gaussian())
  
  expected <- list("est.meta" = ipw_me_fit$coefficients,
                   "att.meta" = mean(ipw_me_fit$fitted.values))
  
  expect_equal(.metaAnalysis.cont(formula = Y ~ X1 + X2 + X3 + X4,
                                  data = data,
                                  predict.subset = rep(TRUE, n)),
               expected)
})

test_that("`.metaAnalysis.cont()` returns expected results; 1 covariate", {
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  par <- c("(Intercept)" = 1.0, "X1" = 1.0)
  Y <- .HTE(psi = par, X = X, outcome.type = "cont")
  data <- as.data.frame(cbind(Y = Y, X))
  
  ipw_me_fit <- stats::glm(formula = Y ~ X1,
                           data = data,
                           family = gaussian())
  
  expected <- list("est.meta" = ipw_me_fit$coefficients,
                   "att.meta" = mean(ipw_me_fit$fitted.values))
  
  expect_equal(.metaAnalysis.cont(formula = Y ~ X1,
                                  data = data,
                                  predict.subset = rep(TRUE, n)),
               expected)
})

test_that("`.metaAnalysis.cont()` returns expected results; no covariate", {
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(0*n), n, 0))
  par <- c("(Intercept)" = 1.0)
  Y <- .HTE(psi = par, X = X, outcome.type = "cont")
  data <- as.data.frame(cbind(Y = Y))
  
  expected <- list("est.meta" = c("(Intercept)" = mean(Y)),
                   "att.meta" = mean(Y))
  
  expect_equal(.metaAnalysis.cont(formula = Y ~ 1,
                                  data = data,
                                  predict.subset = rep(TRUE, n)),
               expected)
})

test_that("`metaAnalysis()` returns expected errors", {
 
  expect_error(.metaAnalysis(),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  expect_error(.metaAnalysis(data.rct = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  expect_error(.metaAnalysis(data.rct = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  expect_error(.metaAnalysis(data.rct = list("X" = 1, "Y" = 2, "A" = 3)),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  data.rct <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("AA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10)
  
  expect_error(.metaAnalysis(data.rct = data.rct),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  expect_error(.metaAnalysis(data.rct = data.rct,
                             data.rwe = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  expect_error(.metaAnalysis(data.rct = data.rct,
                             data.rwe = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  expect_error(.metaAnalysis(data.rct = data.rct,
                             data.rwe = list("X" = 1, "Y" = 2, "A" = 3)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', and 'ps'")
  data.rwe <- data.rct
  colnames(data.rwe$X) <- c("aA", "BB")
  
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe),
               "`contName` must be NULL or a character vector")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = 1L),
               "`contName` must be NULL or a character vector")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = character()),
               "`contName` must be NULL or a character vector")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = "aA"),
               "`contName` must be NULL or a character vector")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = "AA"),
               "`contName` must be NULL or a character vector")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = c("aA", "BB")),
               "`contName` must be NULL or a character vector")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = c("AA", "BB")),
               "`contName` must be NULL or a character vector")
  
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = "BB"),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = "BB", outcome.type = 1.0),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = "BB", outcome.type = "con"),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.metaAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                             contName = "BB", outcome.type = c("cont", "bin")),
               "`outcome.type` must be one of 'cont' or 'bin'")
  
})

test_that("`metaAnalysis()` returns expected result; continuous", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "ps" = ps)
  
  Y_ipw <- .ipw_HTE(Y = data.obj$Y, A = data.obj$A, ps = data.obj$ps)
  Y <- c(Y_ipw, Y_ipw)
  
  df <- data.frame("X2" = c(data.obj$X[ ,2L], data.obj$X[, 2L]),
                   "X4" = c(data.obj$X[, 4L], data.obj$x[, 4L]))
  df$Y <- c(Y_ipw, Y_ipw)
  
  expected <- .metaAnalysis.cont(formula = Y ~ X2 + X4, data = df, 
                                 predict.subset = c(rep(FALSE, n), rep(TRUE, n)))
  
  expect_equal(.metaAnalysis(data.rct = data.obj, data.rwe = data.obj,
                             contName = c("X2", "X4"), outcome.type = "cont", 
                             optim.controls = list()),
               expected)
  
})

test_that("`metaAnalysis()` returns expected result; continuous; 1 covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "ps" = ps)
  
  ## ipw-adjusted outcome regression based on both RCT and RWE datasets
  Y_ipw <- .ipw_HTE(Y = data.obj$Y, A = data.obj$A, ps = data.obj$ps)
  Y <- c(Y_ipw, Y_ipw)
  
  df <- data.frame("X1" = c(data.obj$X[ ,1L], data.obj$X[, 1L]))
  df$Y <- c(Y_ipw, Y_ipw)
  
  expected <- .metaAnalysis.cont(formula = Y ~ X1, data = df, 
                                 predict.subset = c(rep(FALSE, n), rep(TRUE, n)))
  
  expect_equal(.metaAnalysis(data.rct = data.obj, data.rwe = data.obj,
                             contName = c("X1"), outcome.type = "cont",
                             optim.controls = list()),
               expected)
  
})

test_that("`metaAnalysis()` returns expected result; continuous; no covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(NA, n, 0))
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "ps" = ps)
  
  ## ipw-adjusted outcome regression based on both RCT and RWE datasets
  Y_ipw <- .ipw_HTE(Y = data.obj$Y, A = data.obj$A, ps = data.obj$ps)
  Y <- c(Y_ipw, Y_ipw)
  
  df <- as.data.frame(matrix(0, 2L * n, 0L))
  df$Y <- c(Y_ipw, Y_ipw)
  
  expected <- .metaAnalysis.cont(formula = Y ~ 1, data = df, 
                                 predict.subset = c(rep(FALSE, n), rep(TRUE, n)))
  
  expect_equal(.metaAnalysis(data.rct = data.obj, data.rwe = data.obj,
                             contName = NULL, outcome.type = "cont",
                             optim.controls = list()),
               expected)
  
})

test_that("`metaAnalysis()` returns expected result; binary", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L, stats::rbinom(n, 1, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "ps" = ps)
  
  Y_ipw <- .ipw_HTE(Y = data.obj$Y, A = data.obj$A, ps = data.obj$ps)
  Y <- c(Y_ipw, Y_ipw)
  
  expected <- .metaAnalysis.bin(X = rbind(data.obj$X[, c("X2", "X4"), drop = FALSE], 
                                          data.obj$X[, c("X2", "X4"), drop = FALSE]), 
                                c(rep(FALSE, n), rep(TRUE, n)), 
                                Y = Y,
                                optim.controls = list())
  
  expect_equal(.metaAnalysis(data.rct = data.obj, data.rwe = data.obj,
                             contName = c("X2", "X4"), outcome.type = "bin",
                             optim.controls = list()),
               expected)
  
})

test_that("`metaAnalysis()` returns expected result; binary; 1 covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L, stats::rbinom(n, 1, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "ps" = ps)
  
  Y_ipw <- .ipw_HTE(Y = data.obj$Y, A = data.obj$A, ps = data.obj$ps)
  Y <- c(Y_ipw, Y_ipw)
  
  expected <- .metaAnalysis.bin(X = rbind(data.obj$X[, c("X1"), drop = FALSE], 
                                          data.obj$X[, c("X1"), drop = FALSE]), 
                                predict.subset = c(rep(FALSE, n), rep(TRUE, n)), 
                                Y = Y,
                                optim.controls = list())
  
  expect_equal(.metaAnalysis(data.rct = data.obj, data.rwe = data.obj,
                             contName = c("X1"), outcome.type = "bin",
                             optim.controls = list()),
               expected)
  
})

test_that("`metaAnalysis()` returns expected result; binary; no covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(NA, n, 0))
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rbinom(n, 1, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "ps" = ps)
  
  Y_ipw <- .ipw_HTE(Y = data.obj$Y, A = data.obj$A, ps = data.obj$ps)
  Y <- c(Y_ipw, Y_ipw)
  
  expected <- .metaAnalysis.bin(X = matrix(NA, 2L * n, 0L), 
                                predict.subset = c(rep(FALSE, n), rep(TRUE, n)), 
                                Y = Y,
                                optim.controls = list())
  
  
  expect_equal(.metaAnalysis(data.rct = data.obj, data.rwe = data.obj,
                             contName = NULL, outcome.type = "bin",
                             optim.controls = list()),
               expected)
  
})

test_that("`.mainEffects()` returns expected errors", {
  
  expect_error(.mainEffects(),
               "`data` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
  expect_error(.mainEffects(data = data.frame("X" = 1, "Y" = 2, "A" = 3, "q" = 4)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
  expect_error(.mainEffects(data = list("X2" = 1, "Y" = 2, "A" = 3, "q" = 4)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
  expect_error(.mainEffects(data = list("X" = 1, "Y" = 2, "A" = 3)),
               "`data` must be a named list containing elements 'X', 'Y', 'A', and 'q'")
  data <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("AA", "BB"))), 
               "Y" = 1:10, "A" = 1:10, "q" = 1:10)
  
  expect_error(.mainEffects(data = data),
               "`psi` must be a named numeric vector")
  expect_error(.mainEffects(data = data, psi = c("AA", "BB")),
               "`psi` must be a named numeric vector")
  expect_error(.mainEffects(data = data, psi = 1:2),
               "`psi` must be a named numeric vector")
  psi <- c("(Intercept)" = 1.0, "AA" = 3.0, "BB" = 2.0)

  expect_error(.mainEffects(data = data, psi = psi),
               "`mainName` must be NULL or a character vector")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = 1L),
               "`mainName` must be NULL or a character vector")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = character()),
               "`mainName` must be NULL or a character vector")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "aA"),
               "`mainName` must be NULL or a character vector")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = c("aA", "BB")),
               "`mainName` must be NULL or a character vector")

  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB"),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = 1.0),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "con"),
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = c("cont", "bin")),
               "`outcome.type` must be one of 'cont' or 'bin'")

  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "cont"),
               "`method` must be provided")
  
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "cont",
                            method = "gam"),
               "`method.controls` must be a named list")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "cont",
                            method = "gam",
                            method.controls = c("family" = "quasibinomial")),
               "`method.controls` must be a named list")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "cont",
                            method = "gam",
                            method.controls = list(1.0)),
               "`method.controls` must be a named list")
  
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "cont",
                            method = "gam",
                            method.controls = list("a" = 1, "b" = 2)),
               "`fit.name` must be a character")
  expect_error(.mainEffects(data = data, psi = psi,
                            mainName = "BB", outcome.type = "cont",
                            method = "gam",
                            method.controls = list("a" = 1, "b" = 2),
                            fit.name = c("a", "b")),
               "`fit.name` must be a character")

})

test_that("`.mainEffects()` returns expected results; continuous", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(5678L,  stats::rnorm(n, 2, 0.3))
  q <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = q)
  psi <- c("(Intercept)" = 0.1, "X1" = 0.3, "X3" = 0.5)
  
  hte <- .HTE(psi = psi, X = data.obj$X, outcome.type = "cont")
  
  y_hat <- .sieveEstimator(X = data.obj$X[, c("X1", "X3")], 
                           Y = data.obj$Y - data.obj$A * hte, 
                           wgt = data.obj$q,
                           sieve.degree = 1L,
                           subset = rep(TRUE, times = n),
                           method = "gam",
                           method.controls = list("family" = "gaussian"))
  
  residuals <- data.obj$Y - data.obj$A * hte - y_hat
  
  expected <- list("me" = y_hat,
                   "me.conditional.var" = stats::var(residuals))
  
  expect_equal(.mainEffects(data = data.obj, psi = psi, mainName = c("X1", "X3"),
                            outcome.type = "cont", 
                            method = "gam",
                            method.controls = list("family" = "gaussian"),
                            fit.name = "test"),
               expected)
})

test_that("`.mainEffects()` returns expected results; continuous 1 covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(5678L,  stats::rnorm(n, 2, 0.3))
  q <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = q)
  psi <- c("(Intercept)" = 0.1, "X1" = 0.3)
  
  hte <- .HTE(psi = psi, X = data.obj$X, outcome.type = "cont")
  
  y_hat <- .sieveEstimator(X = data.obj$X[, c("X1"), drop = FALSE], 
                           Y = data.obj$Y - data.obj$A * hte, 
                           wgt = data.obj$q,
                           sieve.degree = 1L,
                           subset = rep(TRUE, times = n),
                           method = "gam", 
                           method.controls = list("family" = "gaussian"))
  
  residuals <- data.obj$Y - data.obj$A * hte - y_hat
  
  expected <- list("me" = y_hat,
                   "me.conditional.var" = stats::var(residuals))
  
  expect_equal(.mainEffects(data = data.obj, psi = psi, mainName = c("X1"),
                            outcome.type = "cont", 
                            method = "gam", 
                            method.controls = list("family" = "gaussian"),
                            fit.name = "test"),
               expected)
})

test_that("`.mainEffects()` returns expected results; continuous no covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(NA, n, 0))
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(5678L,  stats::rnorm(n, 2, 0.3))
  q <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = q)
  psi <- c("(Intercept)" = 0.1)
  
  hte <- .HTE(psi = psi, X = data.obj$X, outcome.type = "cont")
  
  y_hat <- .sieveEstimator(X = data.obj$X, 
                           Y = data.obj$Y - data.obj$A * hte, 
                           wgt = data.obj$q,
                           sieve.degree = 1L,
                           subset = rep(TRUE, times = n),
                           method = "gam",
                           method.controls = list("family" = "gaussian"))
  
  residuals <- data.obj$Y - data.obj$A * hte - y_hat
  
  expected <- list("me" = y_hat,
                   "me.conditional.var" = stats::var(residuals))
  
  expect_equal(.mainEffects(data = data.obj, psi = psi, mainName = NULL,
                            outcome.type = "cont", 
                            method = "gam",
                            method.controls = list("family" = "gaussian"),
                            fit.name = "test"),
               expected)
})

test_that("`.mainEffects()` returns expected results; binary", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(5678L,  stats::rbinom(n, 1, 0.3))
  q <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = q)
  psi <- c("(Intercept)" = 0.1, "X1" = 0.3, "X3" = 0.5)
  
  hte <- .HTE(psi = psi, X = data.obj$X, outcome.type = "bin")
  
  y_hat <- .sieveEstimator(X = data.obj$X[, c("X1", "X3")], 
                           Y = data.obj$Y - data.obj$A * hte, 
                           wgt = data.obj$q,
                           sieve.degree = 1L,
                           subset = rep(TRUE, times = n),
                           method = "gam",
                           method.controls = list("family" = gaussian()))
  
  residuals <- data.obj$Y - data.obj$A * hte - y_hat
  
  expected <- list("me" = y_hat,
                   "me.conditional.var" = mean(y_hat)*(1.0 - mean(y_hat)))
  
  expect_equal(.mainEffects(data = data.obj, psi = psi, mainName = c("X1", "X3"),
                            outcome.type = "bin", 
                            method = "gam", 
                            method.controls = list("family" = "gaussian"),
                            fit.name = "test"),
               expected)
})

test_that("`.mainEffects()` returns expected results; binary 1 covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(5678L,  stats::rbinom(n, 1, 0.3))
  q <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = q)
  psi <- c("(Intercept)" = 0.1, "X1" = 0.3)
  
  hte <- .HTE(psi = psi, X = data.obj$X, outcome.type = "bin")
  
  y_hat <- .sieveEstimator(X = data.obj$X[, c("X1"), drop = FALSE], 
                           Y = data.obj$Y - data.obj$A * hte, 
                           wgt = data.obj$q,
                           sieve.degree = 1L,
                           subset = rep(TRUE, times = n),
                           method = "gam",
                           method.controls = list("family" = "gaussian"))
  
  residuals <- data.obj$Y - data.obj$A * hte - y_hat
  
  expected <- list("me" = y_hat,
                   "me.conditional.var" = mean(y_hat)*(1.0 - mean(y_hat)))
  
  expect_equal(.mainEffects(data = data.obj, psi = psi, mainName = c("X1"),
                            outcome.type = "bin", 
                            method = "gam",
                            method.controls = list("family" = "gaussian"),
                            fit.name = "test"),
               expected)
})

test_that("`.mainEffects()` returns expected results; binary no covariate", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(NA, n, 0))
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(5678L,  stats::rbinom(n, 1, 0.3))
  q <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = q)
  psi <- c("(Intercept)" = 0.1)
  
  hte <- .HTE(psi = psi, X = data.obj$X, outcome.type = "bin")
  
  y_hat <- .sieveEstimator(X = data.obj$X, 
                           Y = data.obj$Y - data.obj$A * hte, 
                           wgt = data.obj$q,
                           sieve.degree = 1L,
                           subset = rep(TRUE, times = n),
                           method = "gam",
                           method.controls = list("family" = "gaussian"))
  
  residuals <- data.obj$Y - data.obj$A * hte - y_hat
  
  expected <- list("me" = y_hat,
                   "me.conditional.var" = mean(y_hat)*(1.0 - mean(y_hat)))
  
  expect_equal(.mainEffects(data = data.obj, psi = psi, mainName = NULL,
                            outcome.type = "bin", 
                            method = "gam",
                            method.controls = list("family" = "gaussian"),
                            fit.name = "test"),
               expected)
})

test_that("`.rctAnalysis()` returns expected errors", {
  
  expect_error(.rctAnalysis(),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.rctAnalysis(data.rct = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.rctAnalysis(data.rct = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.rctAnalysis(data.rct = list("X" = 1, "Y" = 2)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  data.rct <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("AA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10)
  
  expect_error(.rctAnalysis(data.rct = data.rct),
               "`data.rwe` must be provided")
  data.rwe <- data.rct
  colnames(data.rwe$X) <- c("aA", "BB")
  
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont"),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont", models = NA),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                          "ps" = 4, "sieve.degree" = 5)),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6, "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = c("ME" = 1, "PS" = 2), 
                                          "RCT" = 2, 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = 2, 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = c("ME" = 1, "PS" = 2), 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = list("ME" = 1, "PS" = 2), 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                       outcome.type = "cont",
                       models = list("RWE" = list("ME" = 1, "PS" = 2),
                                     "RCT" = list("ME" = 1, "PS" = 2),
                                     "outcome" = c("method" = 1, "controls" = 2),
                                     "ps" = 4, "sieve.degree" = 5, "contName" = 6,
                                     "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                       outcome.type = "cont",
                       models = list("RWE" = list("ME" = 1, "PS" = 2),
                                     "RCT" = list("ME" = 1, "PS" = 2),
                                     "outcome" = list("method" = 1, "controls" = 2),
                                     "ps" = 4, "sieve.degree" = 5, "contName" = 6,
                                     "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  expect_error(.rctAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                       outcome.type = "cont",
                       models = list("RWE" = list("ME" = 1, "PS" = 2),
                                     "RCT" = list("ME" = 1, "PS" = 2),
                                     "outcome" = list("method" = 1, "controls" = 2),
                                     "ps" = c("method" = 1, "controls" = 2),
                                     "sieve.degree" = 5, "contName" = 6,
                                     "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
})

test_that("`rctAnalysis()` returns expected results; continuous", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps)
  initial_guess <- numeric(5L)
  names(initial_guess) <- c("(Intercept)", "X1", "X2", "X3", "X4")
  
  psi_p <- .rootsOfScore(X = data.obj$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.obj$Y,
                         A = data.obj$A,
                         outcome.type = "cont",
                         mu = numeric(n),
                         ps = rep(0.5, n),
                         inv.sig2 = rep(1.0, n),
                         wgt = rep(1.0, n))
  
  me_rct <- .mainEffects(data = data.obj, psi = psi_p, mainName = c("X1", "X2", "X3", "X4"), 
                         outcome.type = "cont",
                         method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  est_rct <- .rootsOfScore(X = data.obj$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.obj$Y,
                           A = data.obj$A,
                           outcome.type = "cont",
                           mu = me_rct$me,
                           ps = rep(0.5, n),
                           inv.sig2 = rep(1.0, n),
                           wgt = rep(1.0, n))
  
  tau <- .HTE(psi = est_rct, X = data.obj$X, outcome.type = "cont")
  
  me_rwe <- me_rct
  
  expected <- list("est.rct" = est_rct,
                   "att.rct" = mean(tau),
                   "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, n),
                   "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, n))
  
  models <- list("RCT" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1", "X2", "X3", "X4"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1", "X2", "X3", "X4"),
                 "sieve.degree" = 1L)
  
  expect_equal(.rctAnalysis(data.obj, data.obj, "cont", models),
               expected)
  
})

test_that("`rctAnalysis()` returns expected results; continuous; 1 variable", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps)
  initial_guess <- numeric(2L)
  names(initial_guess) <- c("(Intercept)", "X1")
  
  psi_p <- .rootsOfScore(X = data.obj$X[, c("X1"), drop = FALSE],
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.obj$Y,
                         A = data.obj$A,
                         outcome.type = "cont",
                         mu = numeric(n),
                         ps = rep(0.5, n),
                         inv.sig2 = rep(1.0, n),
                         wgt = rep(1.0, n))
  
  me_rct <- .mainEffects(data = data.obj, psi = psi_p, mainName = c("X1"), 
                         outcome.type = "cont",
                         method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  est_rct <- .rootsOfScore(X = data.obj$X[, c("X1"), drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.obj$Y,
                           A = data.obj$A,
                           outcome.type = "cont",
                           mu = me_rct$me,
                           ps = rep(0.5, n),
                           inv.sig2 = rep(1.0, n),
                           wgt = rep(1.0, n))
  
  tau <- .HTE(psi = est_rct, X = data.obj$X, outcome.type = "cont")
  
  me_rwe <- me_rct
  
  expected <- list("est.rct" = est_rct,
                   "att.rct" = mean(tau),
                   "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, n),
                   "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, n))
  
  models <- list("RCT" = list("ME" = c("X1"), 
                              "PS" = c("X1")),
                 "RWE" = list("ME" = c("X1"), 
                              "PS" = c("X1")),
                 "contName" = c("X1"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1"),
                 "sieve.degree" = 1L)
  
  expect_equal(.rctAnalysis(data.obj, data.obj,  "cont", models),
               expected)
  
})

test_that("`rctAnalysis()` returns expected results; continuous; no variable", {
  
  n <- 1000L
  X <- matrix(NA_real_, n, 0L)
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps)
  initial_guess <- numeric(1L)
  names(initial_guess) <- c("(Intercept)")
  
  psi_p <- .rootsOfScore(X = data.obj$X,
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.obj$Y,
                         A = data.obj$A,
                         outcome.type = "cont",
                         mu = numeric(n),
                         ps = rep(0.5, n),
                         inv.sig2 = rep(1.0, n),
                         wgt = rep(1.0, n))
  
  me_rct <- .mainEffects(data = data.obj, psi = psi_p, mainName = NULL, 
                         outcome.type = "cont",
                         method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  est_rct <- .rootsOfScore(X = data.obj$X,
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.obj$Y,
                           A = data.obj$A,
                           outcome.type = "cont",
                           mu = me_rct$me,
                           ps = rep(0.5, n),
                           inv.sig2 = rep(1.0, n),
                           wgt = rep(1.0, n))
  
  tau <- .HTE(psi = est_rct, X = data.obj$X, outcome.type = "cont")
  
  me_rwe <- me_rct
  
  expected <- list("est.rct" = est_rct,
                   "att.rct" = mean(tau),
                   "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, n),
                   "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, n))
  
  models <- list("RCT" = list("ME" = NULL, 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = NULL, 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = NULL,
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1", "X2", "X3", "X4"),
                 "sieve.degree" = 1L)
  
  
  
  expect_equal(.rctAnalysis(data.obj, data.obj, "cont", models),
               expected)
  
})

test_that("`rctAnalysis()` returns expected results; binary", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rbinom(n, 1, 0.6))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps)
  initial_guess <- numeric(5L)
  names(initial_guess) <- c("(Intercept)", "X1", "X2", "X3", "X4")
  
  psi_p <- .rootsOfScore(X = data.obj$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.obj$Y,
                         A = data.obj$A,
                         outcome.type = "bin",
                         mu = numeric(n),
                         ps = rep(0.5, n),
                         inv.sig2 = rep(1.0, n),
                         wgt = rep(1.0, n))
  
  me_rct <- .mainEffects(data = data.obj, psi = psi_p, mainName = c("X1", "X2", "X3", "X4"), 
                         outcome.type = "bin",
                         method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  est_rct <- .rootsOfScore(X = data.obj$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.obj$Y,
                           A = data.obj$A,
                           outcome.type = "bin",
                           mu = me_rct$me,
                           ps = rep(0.5, n),
                           inv.sig2 = rep(1.0, n),
                           wgt = rep(1.0, n))
  
  tau <- .HTE(psi = est_rct, X = data.obj$X, outcome.type = "bin")
  
  me_rwe <- me_rct
  
  expected <- list("est.rct" = est_rct,
                   "att.rct" = mean(tau),
                   "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, n),
                   "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, n))
  
  models <- list("RCT" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1", "X2", "X3", "X4"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1", "X2", "X3", "X4"),
                 "sieve.degree" = 1L)
  
  
  
  expect_equal(.rctAnalysis(data.obj, data.obj, "bin", models),
               expected)
  
})

test_that("`rctAnalysis()` returns expected results; binary; 1 variable", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rbinom(n, 1, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps)
  initial_guess <- numeric(2L)
  names(initial_guess) <- c("(Intercept)", "X1")
  
  psi_p <- .rootsOfScore(X = data.obj$X[, c("X1"), drop = FALSE],
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.obj$Y,
                         A = data.obj$A,
                         outcome.type = "bin",
                         mu = numeric(n),
                         ps = rep(0.5, n),
                         inv.sig2 = rep(1.0, n),
                         wgt = rep(1.0, n))
  
  me_rct <- .mainEffects(data = data.obj, psi = psi_p, mainName = c("X1"), 
                         outcome.type = "bin",
                         method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  est_rct <- .rootsOfScore(X = data.obj$X[, c("X1"), drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.obj$Y,
                           A = data.obj$A,
                           outcome.type = "bin",
                           mu = me_rct$me,
                           ps = rep(0.5, n),
                           inv.sig2 = rep(1.0, n),
                           wgt = rep(1.0, n))
  
  tau <- .HTE(psi = est_rct, X = data.obj$X, outcome.type = "bin")
  
  me_rwe <- me_rct
  
  expected <- list("est.rct" = est_rct,
                   "att.rct" = mean(tau),
                   "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, n),
                   "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, n))
  
  models <- list("RCT" = list("ME" = c("X1"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X1"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1", "X2", "X3", "X4"),
                 "sieve.degree" = 1L)
  
  expect_equal(.rctAnalysis(data.obj, data.obj, "bin", models),
               expected)
  
})

test_that("`rctAnalysis()` returns expected results; binary; no variable", {
  
  n <- 1000L
  X <- matrix(NA_real_, n, 0L)
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rbinom(n, 1, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps)
  initial_guess <- numeric(1L)
  names(initial_guess) <- c("(Intercept)")
  
  psi_p <- .rootsOfScore(X = data.obj$X,
                         initial.guess = initial_guess,
                         fit.name = "Preliminary Estimator of psi",
                         score.func = "basic",
                         Y = data.obj$Y,
                         A = data.obj$A,
                         outcome.type = "bin",
                         mu = numeric(n),
                         ps = rep(0.5, n),
                         inv.sig2 = rep(1.0, n),
                         wgt = rep(1.0, n))
  
  me_rct <- .mainEffects(data = data.obj, psi = psi_p, mainName = NULL, 
                         outcome.type = "bin",
                         method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  est_rct <- .rootsOfScore(X = data.obj$X,
                           initial.guess = initial_guess,
                           fit.name = "RCT estimator",
                           score.func = "basic",
                           Y = data.obj$Y,
                           A = data.obj$A,
                           outcome.type = "bin",
                           mu = me_rct$me,
                           ps = rep(0.5, n),
                           inv.sig2 = rep(1.0, n),
                           wgt = rep(1.0, n))
  
  tau <- .HTE(psi = est_rct, X = data.obj$X, outcome.type = "bin")
  
  me_rwe <- me_rct
  
  expected <- list("est.rct" = est_rct,
                   "att.rct" = mean(tau),
                   "rct.inv.sig2" = rep(1.0 / me_rct$me.conditional.var, n),
                   "rwe.inv.sig2" = rep(1.0 / me_rwe$me.conditional.var, n))
  
  models <- list("RCT" = list("ME" = NULL, 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = NULL, 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = NULL,
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1", "X2", "X3", "X4"),
                 "sieve.degree" = 1L)
  
  expect_equal(.rctAnalysis(data.obj, data.obj, "bin", models),
               expected)
  
})

test_that("`.integrativeAnalysis()` returns expected errors", {
  
  expect_error(.integrativeAnalysis(),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  expect_error(.integrativeAnalysis(data.rct = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  expect_error(.integrativeAnalysis(data.rct = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  expect_error(.integrativeAnalysis(data.rct = list("X" = 1, "Y" = 2)),
               "`data.rct` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  data.rct <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("AA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10, "q" = 1:10, "inv.sig2" = 1:10)
  
  expect_error(.integrativeAnalysis(data.rct = data.rct),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  expect_error(.integrativeAnalysis(data.rct = data.rct,
                                    data.rwe = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  expect_error(.integrativeAnalysis(data.rct = data.rct,
                                    data.rwe = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  expect_error(.integrativeAnalysis(data.rct = data.rct,
                                    data.rwe = list("X" = 1, "Y" = 2)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', 'A', 'q', 'ps', and 'inv.sig2'")
  data.rwe <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("aA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10, "q" = 1:10, "inv.sig2" = 1:10)
  
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe),
               "`outcome.type` must be provided")
  
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont"),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont", models = NA),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                          "ps" = 4, "sieve.degree" = 5)),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6, "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = c("ME" = 1, "PS" = 2), 
                                          "RCT" = 2, 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = 2, 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = c("ME" = 1, "PS" = 2), 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = list("ME" = 1, "PS" = 2), 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2),
                                          "RCT" = list("ME" = 1, "PS" = 2),
                                          "outcome" = c("method" = 1, "controls" = 2),
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2),
                                          "RCT" = list("ME" = 1, "PS" = 2),
                                          "outcome" = list("method" = 1, "controls" = 2),
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  expect_error(.integrativeAnalysis(data.rct = data.rct, data.rwe = data.rwe,
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2),
                                          "RCT" = list("ME" = 1, "PS" = 2),
                                          "outcome" = list("method" = 1, "controls" = 2),
                                          "ps" = c("method" = 1, "controls" = 2),
                                          "sieve.degree" = 5, "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
})

test_that("`integrativeAnalysis()` returns expected results; continuous", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  invs <- rep(1.0, n)
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps, "ps" = ps, "inv.sig2" = invs)
  
  data_int <- list("X" = rbind(X, X),
                   "A" = c(A, A),
                   "Y" = c(Y, Y),
                   "q" = c(ps, ps),
                   "ps" = c(ps, ps),
                   "inv.sig2" = c(invs, invs))  
  
  initial_guess <- numeric(7L)
  names(initial_guess) <- c("(Intercept)", "X1", "X2", "X3", "X4", "(Intercept)", "X1")
  
  est_intpre <- .rootsOfScore(X = data_int$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                              initial.guess = initial_guess,
                              fit.name = "Integrative Estimator",
                              score.func = "integ",
                              X.cf = data.obj$X[, "X1", drop = FALSE],
                              Y = data_int$Y,
                              A = data_int$A,
                              outcome.type = "cont",
                              wgt = rep(1.0, 2L*n),
                              ps = data_int$ps,
                              mu = rep(0.0, 2L*n),
                              inv.sig2 = data_int$inv.sig2)
  
  psi <- est_intpre[1L:5L]
  phi <- est_intpre[6L:7L]
  
  me_rct <- .mainEffects(data = data.obj, psi = psi, mainName = c("X1", "X2", "X3", "X4"), 
                         outcome.type = "cont", method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  lambda <- .lambda(phi = phi, X = data.obj$X, n.rct = n)
  data.obj$Y <- data.obj$Y - lambda[-seq_len(nrow(data.obj$X))]
  me_rwe <- .mainEffects(data = data.obj, psi = psi, mainName = c("X1", "X2", "X3", "X4"), 
                         outcome.type = "cont", method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RWE dataset")
  
  est_int <- .rootsOfScore(X = data_int$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "Integrative Estimator",
                           score.func = "integ",
                           X.cf = data.obj$X[, "X1", drop = FALSE],
                           Y = data_int$Y,
                           A = data_int$A,
                           outcome.type = "cont",
                           wgt = rep(1.0, 2L * n),
                           ps = data_int$ps,
                           mu = c(me_rct$me, me_rwe$me),
                           inv.sig2 = 1.0 / c(rep(me_rct$me.conditional.var, n), 
                                              rep(me_rwe$me.conditional.var, n)))
  
  tau <- .HTE(psi = est_int[1L:5L], X = data.obj$X, outcome.type = "cont")
  
  expected <- list("est.int" = est_int,
                   "att.int" = mean(tau))
  
  data.obj$Y <- Y
  
  models <- list("RCT" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1", "X2", "X3", "X4"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1"),
                 "sieve.degree" = 1L)
  
  expect_equal(.integrativeAnalysis(data.rct = data.obj, data.rwe = data.obj,
                                    outcome.type = "cont", models = models),
               expected)
  
  
})

test_that("`integrativeAnalysis()` returns expected results; continuous; 0 cfName", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  invs <- rep(1.0, n)
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y, "q" = ps, "ps" = ps, "inv.sig2" = invs)
  
  data_int <- list("X" = rbind(X, X),
                   "A" = c(A, A),
                   "Y" = c(Y, Y),
                   "q" = c(ps, ps),
                   "ps" = c(ps, ps),
                   "inv.sig2" = c(invs, invs))  
  
  initial_guess <- numeric(6L)
  names(initial_guess) <- c("(Intercept)", "X1", "X2", "X3", "X4", "(Intercept)")
  
  est_intpre <- .rootsOfScore(X = data_int$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                              initial.guess = initial_guess,
                              fit.name = "Integrative Estimator",
                              score.func = "integ",
                              X.cf = matrix(NA_real_, n, 0L),
                              Y = data_int$Y,
                              A = data_int$A,
                              outcome.type = "cont",
                              wgt = rep(1.0, 2L * n),
                              ps = data_int$ps,
                              mu = numeric(2L*n),
                              inv.sig2 = data_int$inv.sig2)
  
  psi <- est_intpre[1L:5L]
  phi <- est_intpre[6L:6L]
  
  me_rct <- .mainEffects(data = data.obj, psi = psi, mainName = c("X1", "X2", "X3", "X4"), 
                         outcome.type = "cont", method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RCT dataset")
  
  lambda <- .lambda(phi = phi, X = data.obj$X, n.rct = n)
  data.obj$Y <- data.obj$Y - lambda[-seq_len(nrow(data.obj$X))]
  me_rwe <- .mainEffects(data = data.obj, psi = psi, mainName = c("X1", "X2", "X3", "X4"), 
                         outcome.type = "cont", method = "gam",
                         method.controls = list("family" = "gaussian"),
                         fit.name = "RWE dataset")
  
  est_int <- .rootsOfScore(X = data_int$X[, c("X1", "X2", "X3", "X4"), drop = FALSE],
                           initial.guess = initial_guess,
                           fit.name = "Integrative Estimator",
                           score.func = "integ",
                           X.cf = matrix(NA_real_, n, 0L),
                           Y = data_int$Y,
                           A = data_int$A,
                           outcome.type = "cont",
                           inv.sig2 = 1.0 / c(rep(me_rct$me.conditional.var, n), 
                                              rep(me_rwe$me.conditional.var, n)),
                           ps = data_int$ps,
                           mu = c(me_rct$me,  me_rwe$me),
                           wgt = rep(1.0, 2L * n))
  
  tau <- .HTE(psi = est_int[1L:5L], X = data.obj$X, outcome.type = "cont")
  
  expected <- list("est.int" = est_int,
                   "att.int" = mean(tau))
  
  data.obj$Y <- Y
  
  models <- list("RCT" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X1", "X2", "X3", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1", "X2", "X3", "X4"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = NULL,
                 "sieve.degree" = 1L)
  
  expect_equal(.integrativeAnalysis(data.rct = data.obj, data.rwe = data.obj,
                                    outcome.type = "cont", models = models),
               expected)
  
  
})


test_that("`.psiEst_IntHTEcf()` returns expected errors", {
  
  expect_error(.psiEst_IntHTEcf(),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.psiEst_IntHTEcf(data.rct = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.psiEst_IntHTEcf(data.rct = list("X" = 1, "Y" = 2)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  data.rct <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("AA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10, "q" = 1:10)
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct,
                                    data.rwe = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct,
                                    data.rwe = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct,
                                    data.rwe = list("X" = 1, "Y" = 2)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  data.rwe <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("aA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10, "q" = 1:10)
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "`outcome.type` must be one of 'cont' or 'bin'")
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont"),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont", models = NA),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                          "ps" = 4, "sieve.degree" = 5)),
               "`models` must be a list with elements 'RWE', 'RCT', 'outcome', 'ps', 'sieve.degree', 'contName', and 'cfName'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = 1, "RCT" = 2, "outcome" = 3,
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6, "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = c("ME" = 1, "PS" = 2), 
                                          "RCT" = 2, 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = 2, 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = c("ME" = 1, "PS" = 2), 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'RWE' and 'RCT' of 'models' must be lists with elements 'ME' and 'PS'")
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2), 
                                          "RCT" = list("ME" = 1, "PS" = 2), 
                                          "outcome" = 3,
                                          "ps" = 4, 
                                          "sieve.degree" = 5, 
                                          "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2),
                                          "RCT" = list("ME" = 1, "PS" = 2),
                                          "outcome" = c("method" = 1, "controls" = 2),
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2),
                                          "RCT" = list("ME" = 1, "PS" = 2),
                                          "outcome" = list("method" = 1, "controls" = 2),
                                          "ps" = 4, "sieve.degree" = 5, "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
  expect_error(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe,
                            outcome.type = "cont",
                            models = list("RWE" = list("ME" = 1, "PS" = 2),
                                          "RCT" = list("ME" = 1, "PS" = 2),
                                          "outcome" = list("method" = 1, "controls" = 2),
                                          "ps" = c("method" = 1, "controls" = 2),
                                          "sieve.degree" = 5, "contName" = 6,
                                          "cfName" = 7)),
               "elements 'outcome' and 'ps' of 'models' must be lists with elements 'method' and 'controls'")
})

test_that("`.psiEst_IntHTEcf()` returns expected results", {
  
  n <- 1000L
  
  withr::with_seed(1234, {
    X <- matrix(stats::runif(3*n), n, 3)
    X <- cbind(X, stats::rbinom(n, 1, 0.3))
    colnames(X) <- c("X1", "X2", "X3", "X4")
    A <- stats::rbinom(n, 1, 0.4)
    Y <- stats::rnorm(n, 2, 0.3)
    ps <- stats::runif(n)
        
    data.obj <- list("X" = X, "A" = A, "Y" = Y)
        
  })
  
  data.obj$ps <- .propensityScore(X = X, A = A, wgt = rep(1.0, n), 
                                  sieve.degree = 1L,
                                  method = "gam",
                                  method.controls = list("family" = binomial),
                                  models = "ps")$ps

  data.obj$q <- rep(1.0, n)
  
  meta <- .metaAnalysis(data.rct = data.obj, 
                        data.rwe = data.obj, 
                        contName = c("X1", "X3"),
                        outcome.type = "cont",
                        optim.controls = list())
  
  models <- list("RCT" = list("ME" = c("X2", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X2", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1", "X3"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "gam", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1"),
                 "sieve.degree" = 1L)
  
  rct <- .rctAnalysis(data.rct = data.obj, 
                      data.rwe = data.obj, 
                      outcome.type = "cont",
                      models = models)
  data.rct <- data.obj
  data.rwe <- data.obj
  data.rct$inv.sig2 <- rct$rct.inv.sig2
  data.rwe$inv.sig2 <- rct$rwe.inv.sig2
  rct$rct.inv.sig2 <- NULL
  rct$rwe.inv.sig2 <- NULL
  
  integrative <- .integrativeAnalysis(data.rct = data.rct, 
                                      data.rwe = data.rwe, 
                                      outcome.type = "cont",
                                      models = models)
  
  expected <- c(meta, rct, integrative)

  expect_equal(.psiEst_IntHTEcf(data.rct = c(data.obj, "est.ps" = TRUE), 
                                data.rwe = data.obj,
                                outcome.type = "cont", models = models,
                                optim.controls = list()),
                                expected)
  
  
})

test_that("`.psiEst_IntHTEcf()` returns expected results; no ps", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y)
  data.rct <- data.obj
  data.rwe <- data.obj
  
  data.rct$ps <- rep(0.4, n)
  data.rwe$ps <- .propensityScore(X = X, A = A, wgt = rep(1.0, n), 
                                  sieve.degree = 1L,
                                  method = "gam",
                                  method.controls = list("family" = binomial),
                                  models = "ps")$ps
  
  data.rct$q <- rep(1.0, n)
  data.rwe$q <- rep(1.0, n)
  
  meta <- .metaAnalysis(data.rct = data.rct, 
                        data.rwe = data.rwe, 
                        contName = c("X1", "X3"),
                        outcome.type = "cont",
                        optim.controls = list())
  
  models <- list("RCT" = list("ME" = c("X2", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X2", "X4"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X1", "X3"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "gam", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1"),
                 "sieve.degree" = 1L)
  
  rct <- .rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                      outcome.type = "cont", models = models)
  
  data.rct$inv.sig2 <- rct$rct.inv.sig2
  data.rwe$inv.sig2 <- rct$rwe.inv.sig2
  rct$rct.inv.sig2 <- NULL
  rct$rwe.inv.sig2 <- NULL
  
  integrative <- .integrativeAnalysis(data.rct = data.rct, 
                                      data.rwe = data.rwe, 
                                      outcome.type = "cont",
                                      models = models)
  
  expected <- c(meta, rct, integrative)
  
  data.rct <- data.obj
  data.rwe <- data.obj
  
  data.rct$ps <- rep(0.4, n)
  data.rct$est.ps <- FALSE
  
  expect_equal(.psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe,
                                outcome.type = "cont", models = models,
                                optim.controls = list()),
               expected)
})

test_that("`.psiEst_IntHTEcf()` returns expected results; no ps", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  X[, "X1"] <- X[, "X3"]
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y)
  data.rct <- data.obj
  data.rwe <- data.obj
  
  data.rct$ps <- rep(0.4, n)
  data.rwe$ps <- .propensityScore(X = X[, c("X1", "X2", "X4")], A = A, wgt = rep(1.0, n), 
                                  sieve.degree = 1L,
                                  method = "gam",
                                  method.controls = list("family" = binomial),
                                  models = "ps")$ps
  
  data.rct$q <- rep(1.0, n)
  data.rwe$q <- rep(1.0, n)
  
  meta <- .metaAnalysis(data.rct = data.rct, 
                        data.rwe = data.rwe, 
                        contName = c("X1", "X3"),
                        outcome.type = "cont",
                        optim.controls = list())
  
  models <- list("RCT" = list("ME" = c("X2", "X4"), 
                              "PS" = c("X1", "X2", "X4")),
                 "RWE" = list("ME" = c("X2", "X4"), 
                              "PS" = c("X1", "X2", "X4")),
                 "contName" = c("X1", "X3"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "gam", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1"),
                 "sieve.degree" = 1L)
  
  rct <- .rctAnalysis(data.rct = data.rct, data.rwe = data.rwe, 
                      outcome.type = "cont", models = models)
  
  data.rct$inv.sig2 <- rct$rct.inv.sig2
  data.rwe$inv.sig2 <- rct$rwe.inv.sig2
  rct$rct.inv.sig2 <- NULL
  rct$rwe.inv.sig2 <- NULL
  
  integrative <- .integrativeAnalysis(data.rct = data.rct, 
                                      data.rwe = data.rwe, 
                                      outcome.type = "cont",
                                      models = models)
  
  expected <- c(meta, rct, integrative)
  
  data.rct <- data.obj
  data.rwe <- data.obj
  
  data.rct$ps <- rep(0.4, n)
  data.rct$est.ps <- FALSE
  
  expect_message(out <- .psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe,
                                         outcome.type = "cont", models = models,
                                         optim.controls = list()),
                 "parameters set to 0.0")
                 
  
  expect_equal(out,  expected)
})