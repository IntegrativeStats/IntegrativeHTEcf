test_that("`.fitSL()` returns expected errors", {
  
  expect_error(.fitSL(),
               "`X` must be a named matrix")
  expect_error(.fitSL(X = matrix("A", 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.fitSL(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named matrix")
  expect_error(.fitSL(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.fitSL(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named matrix")
  
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  expect_error(.fitSL(X = X),
               "`Y` must be a numeric vector")
  expect_error(.fitSL(X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.fitSL(X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")
  
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L)),
               "`wgt` must be a numeric vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")
  
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L)),
               "`subset` must be a logical vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep("a", 10)),
               "`subset` must be a logical vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(1.0, 10)),
               "`subset` must be a logical vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = matrix(TRUE, 10L, 1L)),
               "`subset` must be a logical vector")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 9L)),
               "`subset` must be a logical vector")
  
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L)),
               "`method.controls` must be a list")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               method.controls = c("family" = "binomial")),
               "`method.controls` must be a list")
  expect_error(.fitSL(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               method.controls = 1.0),
               "`method.controls` must be a list")
})

test_that("`fitSL()` returns expected results", {
  
  expect_error(.fitSL(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                      Y = rep(1, 10), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L),
                      method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")
  
  expect_error(.fitSL(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                      Y = rep(1, 10), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L),
                      method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")
  
  form <- Y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y, X = as.data.frame(cbind(1, X2)),
                                                     newX = as.data.frame(cbind(1, X2)),
                                                     obsWeights = wgt,
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)
  
  expect_equal(.fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                      method.controls = list("family" = gaussian(),
                                             "SL.library" = "SL.glm")), expected)
  
  expect_warning(.fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                        method.controls = list("family" = "gaussian",
                                               "obsWeights" = wgt,
                                               "SL.library" = "SL.glm")),
                 "Element(s) obsWeights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y[1:{n/2}],
                                                     X = cbind(1, X2[1:{n/2}, ]),
                                                     newX = cbind(1, X2),
                                                     obsWeights = wgt[1:{n/2}],
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)
  
  expect_equal(.fitSL(X = X2, Y = Y, wgt = wgt,
                      subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                      method.controls = list("family" = "gaussian",
                                             "SL.library" = "SL.glm")), expected)
})



test_that("`.fitSL()` returns expected results for SL; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X
  
  form <- Y ~ 1
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y, X = as.data.frame(cbind(1, X2)),
                                                     newX = as.data.frame(cbind(1, X2)),
                                                     obsWeights = wgt,
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)
  
  expect_equal(.fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                      method.controls = list("family" = gaussian(),
                                             "SL.library" = "SL.glm")), 
               expected)
  
  expect_warning(.fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                        method.controls = list("family" = "gaussian",
                                               "obsWeights" = wgt,
                                               "SL.library" = "SL.glm")),
                 "Element(s) obsWeights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y[1:{n/2}], X = matrix(1.0, n/2, 1),
                                                     newX = matrix(1.0, n, 1),
                                                     obsWeights = wgt[1:{n/2}],
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)
  
  expect_equal(.fitSL(X = X2, Y = Y, wgt = wgt,
                      subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                      method.controls = list("family" = "gaussian",
                                             "SL.library" = "SL.glm")), expected)
})

test_that("`.fitSL()` returns expected results; single covariate", {
  expect_error(.fitSL(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                      Y = rep(1, 10), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L),
                      method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")
  
  expect_error(.fitSL(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                      Y = rep(1, 10), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L),
                      method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")
  
  form <- Y ~ x1 + x2
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y, X = as.data.frame(cbind(1, X2)),
                                                     newX = as.data.frame(cbind(1, X2)),
                                                     obsWeights = wgt,
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)
  
  expect_equal(.fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                      method.controls = list("family" = gaussian(),
                                             "SL.library" = "SL.glm")), 
               expected)
  
  expect_warning(.fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                        method.controls = list("family" = "gaussian",
                                               "obsWeights" = wgt,
                                               "SL.library" = "SL.glm")),
                 "Element(s) obsWeights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- suppressWarnings(SuperLearner::SuperLearner(Y = Y[1:{n/2}],
                                                     X = cbind(1, X2[1:{n/2}, ]),
                                                     newX = cbind(1, X2),
                                                     obsWeights = wgt[1:{n/2}],
                                                     family = gaussian(),
                                                     SL.library = "SL.glm"))
  expected <- drop(fit$SL.predict)
  
  expect_equal(.fitSL(X = X2, Y = Y, wgt = wgt,
                      subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                      method.controls = list("family" = "gaussian",
                                             "SL.library" = "SL.glm")), 
               expected)
})

test_that("`.fitgam()` returns expected errors", {
  
  expect_error(.fitgam(),
               "`X` must be a named matrix")
  expect_error(.fitgam(X = matrix("A", 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.fitgam(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named matrix")
  expect_error(.fitgam(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.fitgam(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named matrix")
  
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  expect_error(.fitgam(X = X),
               "`Y` must be a numeric vector")
  expect_error(.fitgam(X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.fitgam(X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")
  
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L)),
               "`wgt` must be a numeric vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")
  
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L)),
               "`subset` must be a logical vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = rep("a", 10)),
               "`subset` must be a logical vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = rep(1.0, 10)),
               "`subset` must be a logical vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = matrix(TRUE, 10L, 1L)),
               "`subset` must be a logical vector")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 9L)),
               "`subset` must be a logical vector")
  
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L)),
               "`method.controls` must be a list")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L),
                      method.controls = c("family" = "binomial")),
               "`method.controls` must be a list")
  expect_error(.fitgam(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                      subset = rep(TRUE, 10L),
                      method.controls = 1.0),
               "`method.controls` must be a list")
})

test_that("`fitgam()` returns expected results", {
  
  expect_error(.fitgam(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")
  
  expect_error(.fitgam(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")

  form <- Y ~ s(x1) + s(x2) + s(x3) + s(x4) + s(x5) + s(x6) + s(x7) + s(x8) + s(x9) + x10 + s(x11) + s(x12) + s(x13)
  
  df <- cbind(Y, X2) |> as.data.frame()
  colnames(df)[1L] <- "Y"
  
  fit <- mgcv::gam(formula = form, data = df, weights = wgt,
                   family = gaussian())
  expected <- drop(fit$fitted.values)
  
  expect_equal(.fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                       method.controls = list("family" = gaussian())), 
               expected)
  
  expect_warning(.fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                         method.controls = list("family" = "gaussian",
                                                "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- mgcv::gam(formula = form, data = df[1L:{n/2L}, ], weights = wgt[1L:{n/2L}],
                   family = gaussian())
  expected <- mgcv::predict.gam(fit, df, type = "response") |> as.numeric()
  
  expect_equal(.fitgam(X = X2, Y = Y, wgt = wgt,
                       subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                       method.controls = list("family" = "gaussian")), 
               expected)
})



test_that("`.fitgam()` returns expected results; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X
  
  df <- cbind(Y, X2) |> as.data.frame()
  colnames(df)[1L] <- "Y"
  
  form <- Y ~ 1
  fit <- mgcv::gam(formula = form, data = df, weights = wgt,
                   family = gaussian())
  expected <- drop(fit$fitted.values)
  
  expect_equal(.fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                       method.controls = list("family" = gaussian())), 
               expected)
  
  expect_warning(.fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                         method.controls = list("family" = "gaussian",
                                                "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- mgcv::gam(formula = form, data = df[1L:{n/2L}, , drop = FALSE], weights = wgt[1L:{n/2L}],
                   family = gaussian())
  expected <- mgcv::predict.gam(fit, df, type = "response") |> as.numeric()

  expect_equal(.fitgam(X = X2, Y = Y, wgt = wgt,
                       subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                       method.controls = list("family" = "gaussian")), 
               expected)
})

test_that("`.fitgam()` returns expected results; single covariate", {
  expect_error(.fitgam(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")
  
  expect_error(.fitgam(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")

  df <- cbind(Y, X2) |> as.data.frame()
  colnames(df)[1L] <- "Y"
  
  form <- Y ~ s(x1) + s(x2)
  fit <- mgcv::gam(formula = form, data = df, weights = wgt,
                   family = gaussian())
  expected <- drop(fit$fitted.values)
  
  expect_equal(.fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                       method.controls = list("family" = gaussian())), 
               expected)
  
  expect_warning(.fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                         method.controls = list("family" = "gaussian",
                                                "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- mgcv::gam(formula = form, data = df[1L:{n/2L}, ], weights = wgt[1L:{n/2L}],
                   family = gaussian())
  expected <- mgcv::predict.gam(fit, df, type = "response") |> as.numeric()
  
  expect_equal(.fitgam(X = X2, Y = Y, wgt = wgt,
                       subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                       method.controls = list("family" = "gaussian")), 
               expected)
})

test_that("`.fitglm()` returns expected errors", {
  
  expect_error(.fitglm(),
               "`X` must be a named matrix")
  expect_error(.fitglm(X = matrix("A", 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.fitglm(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named matrix")
  expect_error(.fitglm(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.fitglm(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named matrix")
  
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  expect_error(.fitglm(X = X),
               "`Y` must be a numeric vector")
  expect_error(.fitglm(X = X, Y = rep("a", 10)),
               "`Y` must be a numeric vector")
  expect_error(.fitglm(X = X, Y = matrix(1.0, 10L, 1L)),
               "`Y` must be a numeric vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 9L)),
               "`Y` must be a numeric vector")
  
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L)),
               "`wgt` must be a numeric vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")
  
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L)),
               "`subset` must be a logical vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = rep("a", 10)),
               "`subset` must be a logical vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = rep(1.0, 10)),
               "`subset` must be a logical vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = matrix(TRUE, 10L, 1L)),
               "`subset` must be a logical vector")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 9L)),
               "`subset` must be a logical vector")
  
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L)),
               "`method.controls` must be a list")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = c("family" = "binomial")),
               "`method.controls` must be a list")
  expect_error(.fitglm(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = 1.0),
               "`method.controls` must be a list")
})

test_that("`fitglm()` returns expected results", {
  
  expect_error(.fitglm(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")
  
  expect_error(.fitglm(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")
  
  form <- Y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13
  
  df <- cbind(Y, X2) |> as.data.frame()
  colnames(df)[1L] <- "Y"
  
  fit <- mgcv::gam(formula = form, data = df, weights = wgt,
                   family = gaussian())
  expected <- drop(fit$fitted.values)
  
  expect_equal(.fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                       method.controls = list("family" = gaussian())), 
               expected)
  
  expect_warning(.fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                         method.controls = list("family" = "gaussian",
                                                "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- mgcv::gam(formula = form, data = df[1L:{n/2L}, ], weights = wgt[1L:{n/2L}],
                   family = gaussian())
  expected <- mgcv::predict.gam(fit, df, type = "response") |> as.numeric()
  
  expect_equal(.fitglm(X = X2, Y = Y, wgt = wgt,
                       subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                       method.controls = list("family" = "gaussian")), 
               expected)
})



test_that("`.fitglm()` returns expected results; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X
  
  df <- cbind(Y, X2) |> as.data.frame()
  colnames(df)[1L] <- "Y"
  
  form <- Y ~ 1
  fit <- mgcv::gam(formula = form, data = df, weights = wgt,
                   family = gaussian())
  expected <- drop(fit$fitted.values)
  
  expect_equal(.fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                       method.controls = list("family" = gaussian())), 
               expected)
  
  expect_warning(.fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                         method.controls = list("family" = "gaussian",
                                                "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- mgcv::gam(formula = form, data = df[1L:{n/2L}, , drop = FALSE], weights = wgt[1L:{n/2L}],
                   family = gaussian())
  expected <- mgcv::predict.gam(fit, df, type = "response") |> as.numeric()
  
  expect_equal(.fitglm(X = X2, Y = Y, wgt = wgt,
                       subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                       method.controls = list("family" = "gaussian")), 
               expected)
})

test_that("`.fitglm()` returns expected results; single covariate", {
  expect_error(.fitglm(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = c("family" = binomial())),
               "`method.controls` is not properly defined")
  
  expect_error(.fitglm(X = matrix(1.0, 10, 3, dimnames = list(NULL, c("x1", "x2", "x3"))),
                       Y = rep(1, 10), wgt = rep(1.0, 10L),
                       subset = rep(TRUE, 10L),
                       method.controls = list("NA.action" = TRUE)),
               "`method.controls` is not properly defined")
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")
  
  df <- cbind(Y, X2) |> as.data.frame()
  colnames(df)[1L] <- "Y"
  
  form <- Y ~ x1 + x2
  fit <- mgcv::gam(formula = form, data = df, weights = wgt,
                   family = gaussian())
  expected <- drop(fit$fitted.values)
  
  expect_equal(.fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                       method.controls = list("family" = gaussian())), 
               expected)
  
  expect_warning(.fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                         method.controls = list("family" = "gaussian",
                                                "weights" = wgt)),
                 "Element(s) weights cannot be provided as input; input overwritten", fixed = TRUE)
  
  fit <- mgcv::gam(formula = form, data = df[1L:{n/2L}, ], weights = wgt[1L:{n/2L}],
                   family = gaussian())
  expected <- mgcv::predict.gam(fit, df, type = "response") |> as.numeric()
  
  expect_equal(.fitglm(X = X2, Y = Y, wgt = wgt,
                       subset = c(rep(TRUE, n/2), rep(FALSE, n/2)),
                       method.controls = list("family" = "gaussian")), 
               expected)
})





test_that("`.sieveEstimator()` returns expected errors", {
  
  expect_error(.sieveEstimator(),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = matrix("A", 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named matrix")
  expect_error(.sieveEstimator(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named matrix")
  
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  expect_error(.sieveEstimator(X = X),
               "`Y` must be provided")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L)),
               "`wgt` must be provided")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L)),
               "`subset` must be provided")
  
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L), sieve.degree = "a"),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L), sieve.degree = TRUE),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L), sieve.degree = c(1.0, 2.0)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               subset = rep(TRUE, 10L),
                               sieve.degree = matrix(1, 1L, 1L)),
               "`sieve.degree` must be a scalar numeric")

  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               sieve.degree = 2L,
                               subset = rep(TRUE, 10L)),
               "`method` must be one of 'SL', 'gam', or 'glm'")
  
  expect_error(.sieveEstimator(X = X, Y = rep(1.0, 10L), wgt = rep(1.0, 10L),
                               sieve.degree = 2L,
                               subset = rep(TRUE, 10L), method = "gam"),
               "`method.controls` must be provided")
})

test_that("`sieveEstimator()` returns expected results for SL", {
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")
  
  expected <- .fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                     method.controls = list("family" = gaussian(),
                                            "SL.library" = "SL.glm"))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "SL",
                               method.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm")), expected)
})



test_that("`sieveEstimator()` returns expected results for SL; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X
  
  expected <- .fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                     method.controls = list("family" = gaussian(),
                                            "SL.library" = "SL.glm"))

  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "SL",
                               method.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm")), expected)
  
})

test_that("`sieveEstimator()` returns expected results for SL; single covariate", {
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")
  
  expected <- .fitSL(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                     method.controls = list("family" = gaussian(),
                                            "SL.library" = "SL.glm"))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "SL", 
                               method.controls = list("family" = gaussian(),
                                                      "SL.library" = "SL.glm")), expected)
})

test_that("`sieveEstimator()` returns expected results for gam", {
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")
  
  expected <- .fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                      method.controls = list("family" = gaussian()))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "gam",
                               method.controls = list("family" = gaussian())), 
               expected)
})



test_that("`sieveEstimator()` returns expected results for gam; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X
  
  expected <- .fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                      method.controls = list("family" = gaussian()))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "gam",
                               method.controls = list("family" = gaussian())), 
               expected)
  
})

test_that("`sieveEstimator()` returns expected results for gam; single covariate", {
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")
  
  expected <- .fitgam(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                      method.controls = list("family" = gaussian()))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "gam", 
                               method.controls = list("family" = gaussian())), 
               expected)
})

test_that("`sieveEstimator()` returns expected results for glm", {
  
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)[, 1L:13L]
  colnames(X2) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
                    "x10", "x11", "x12", "x13")
  
  expected <- .fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                      method.controls = list("family" = gaussian()))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "glm",
                               method.controls = list("family" = gaussian())), 
               expected)
})



test_that("`sieveEstimator()` returns expected results for glm; no covariate", {
  n <- 1000
  X <- matrix(NA_real_, n, 0L)
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  X2 <- X
  
  expected <- .fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                      method.controls = list("family" = gaussian()))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "glm",
                               method.controls = list("family" = gaussian())), 
               expected)
  
})

test_that("`sieveEstimator()` returns expected results for glm; single covariate", {
  n <- 1000
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1))
  colnames(X) <- c("X1")
  Y <- withr::with_seed(2345L, runif(n))
  wgt <- withr::with_seed(4567L, runif(n))
  
  X2 <- stats::poly(X, degree = 2, raw = TRUE)
  colnames(X2) <- c("x1", "x2")
  
  expected <- .fitglm(X = X2, Y = Y, wgt = wgt, subset = rep(TRUE, n), 
                      method.controls = list("family" = gaussian()))
  
  expect_equal(.sieveEstimator(X = X, Y = Y, wgt = wgt, subset = rep(TRUE, n),
                               sieve.degree = 2,
                               method = "glm", 
                               method.controls = list("family" = gaussian())), 
               expected)
})