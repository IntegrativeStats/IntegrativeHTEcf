test_that("`.propensityScore()` returns expected errors", {
  
  expect_error(.propensityScore(), "`X` must be a named numeric matrix")
  expect_error(.propensityScore(X = matrix("A", 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.propensityScore(X = c(1.0, 2.0, 3.0)),
               "`X` must be a named numeric matrix")
  expect_error(.propensityScore(X = matrix(1.0, 10L, 3L)),
               "`X` must be a named numeric matrix")
  expect_error(.propensityScore(X = as.data.frame(matrix(1.0, 10L, 3L))),
               "`X` must be a named numeric matrix")
  
  X <- matrix(1, 10L, 3L, dimnames = list(NULL, c("X1", "X2", "X3")))
  expect_error(.propensityScore(X = X),
               "`A` must be a binary vector")
  expect_error(.propensityScore(X = X, A = rep("a", 10)),
               "`A` must be a binary vector")
  expect_error(.propensityScore(X = X, A = matrix(1.0, 10L, 1L)),
               "`A` must be a binary vector")
  expect_error(.propensityScore(X = X, A = rep(1.0, 9L)),
               "`A` must be a binary vector")
  expect_error(.propensityScore(X = X, A = c(1, 2, 3, 1, 2)),
               "`A` must be a binary vector")
  expect_error(.propensityScore(X = X, A = c(1, 2, 1, 2, NA_real_)),
               "`A` must be a binary vector")
  
  
  expect_error(.propensityScore(X = X, A = rep(1L, 10L)),
               "`wgt` must be a numeric vector")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep("a", 10)),
               "`wgt` must be a numeric vector")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = matrix(1.0, 10L, 1L)),
               "`wgt` must be a numeric vector")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 9L)),
               "`wgt` must be a numeric vector")
  
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = "a"),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = TRUE),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = c(1.0, 2.0)),
               "`sieve.degree` must be a scalar numeric")
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = matrix(1, 1L, 1L)),
               "`sieve.degree` must be a scalar numeric")
  
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L),
               "`method` must be provided")
  
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L, method = "glm"),
               "`method.controls` must be provided")

  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L, method = "glm",
                                method.controls = list("family" = gaussian())),
               "`models` can contain only {'ps', 'ml.ps'}", fixed = TRUE)
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L, method = "glm",
                                method.controls = list("family" = gaussian()),
                                models = 1),
               "`models` can contain only {'ps', 'ml.ps'}", fixed = TRUE)
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L, method = "glm",
                                method.controls = list("family" = gaussian()),
                                models = "PS"),
               "`models` can contain only {'ps', 'ml.ps'}", fixed = TRUE)
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L, method = "glm",
                                method.controls = list("family" = gaussian()),
                                models = c("ps", "ml_ps")),
               "`models` can contain only {'ps', 'ml.ps'}", fixed = TRUE)
  expect_error(.propensityScore(X = X, A = rep(1L, 10L), wgt = rep(1.0, 10L),
                                sieve.degree = 2L, method = "glm",
                                method.controls = list("family" = gaussian()),
                                models = c("ps", "ml.ps", "ps")),
               "`models` can contain only {'ps', 'ml.ps'}", fixed = TRUE)
  
})


test_that("`.propensityScore()` returns the expected results", {
  
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, rbinom(n, 1, 0.4))
  wgt <- rep(1.0, n)
  
  method.controls <- list("family" = binomial())
  
  res <- list()
  res$ps <- .sieveEstimator(X = X, Y = A, wgt = wgt,
                            subset = rep(TRUE, nrow(X)),
                            sieve.degree = 1L,
                            method = "glm", 
                            method.controls = method.controls)
  res$ml.ps <- .sieveEstimator(X = X, Y = A, wgt = wgt,
                               subset = rep(TRUE, nrow(X)),
                               sieve.degree = 2L,
                               method = "glm", 
                               method.controls = method.controls)
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm",
                                method.controls = method.controls,
                                models = c("ps", "ml.ps")),
               res)
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ps")),
               list("ps" = res$ps))
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ml.ps")),
               list("ml.ps" = res$ml.ps))
})

test_that("`.propensityScore()` returns the expected results; single covariate", {
  
  n <- 1000L
  
  X <- withr::with_seed(1234L, matrix(runif(1*n), n, 1L))
  colnames(X) <- c("X1")
  A <- withr::with_seed(2345L, rbinom(n, 1, 0.4))
  wgt <- rep(1.0, n)
  
  method.controls <- list("family" = binomial())
  
  res <- list()
  res$ps <- .sieveEstimator(X = X, Y = A, wgt = wgt,
                            subset = rep(TRUE, nrow(X)),
                            sieve.degree = 1L,
                            method = "glm", 
                            method.controls = method.controls)
  res$ml.ps <- .sieveEstimator(X = X, Y = A, wgt = wgt,
                               subset = rep(TRUE, nrow(X)),
                               sieve.degree = 2L,
                               method = "glm", 
                               method.controls = method.controls)
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ps", "ml.ps")),
               res)
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ps")),
               list("ps" = res$ps))
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ml.ps")),
               list("ml.ps" = res$ml.ps))
})


test_that("`.propensityScore()` returns the expected results; no covariate", {
  
  n <- 1000L
  
  X <- matrix(NA_real_, n, 0L)
  A <- withr::with_seed(2345L, rbinom(n, 1, 0.4))
  wgt <- rep(1.0, n)
  
  method.controls <- list("family" = binomial())
  
  res <- list()
  res$ps <- .sieveEstimator(X = X, Y = A, wgt = wgt,
                            subset = rep(TRUE, nrow(X)),
                            sieve.degree = 1L,
                            method = "glm", 
                            method.controls = method.controls)
  res$ml.ps <- .sieveEstimator(X = X, Y = A, wgt = wgt,
                               subset = rep(TRUE, nrow(X)),
                               sieve.degree = 2L,
                               method = "glm", 
                               method.controls = method.controls)
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ps", "ml.ps")),
               res)
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ps")),
               list("ps" = res$ps))
  
  expect_equal(.propensityScore(X = X, A = A, wgt = wgt, sieve.degree = 2L,
                                method = "glm", 
                                method.controls = method.controls,
                                models = c("ml.ps")),
               list("ml.ps" = res$ml.ps))
})