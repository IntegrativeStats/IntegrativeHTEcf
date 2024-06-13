test_that("IntHTEcf() returns expected stopifnot errors", {
  
  expect_error(IntHTEcf(), "`data.rct` must be provided")
  data.rct <- list("X" = matrix(1:10, 10, 4, dimnames = list(NULL, c("X1", "X2", "X3", "X4"))),
                   "Y" = 1:10, "A" = rep(0L, 10), "mainName" = 1L, "contName" = 1L, "psName" = 1L)

  expect_error(IntHTEcf(data.rct = data.rct),
               "`data.rwe` must be provided")
  data.rwe <- data.rct
  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = NA),
               "`cfName` must be a character vector of X column headers")  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = 2),
               "`cfName` must be a character vector of X column headers")  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = c(1, 2)),
               "`cfName` must be a character vector of X column headers")  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = c("AA", "BB")),
               "`cfName` must be a character vector of X column headers")  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = list("BB")),
               "`cfName` must be a character vector of X column headers")  

  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "aA", outcome.type = "Cont"),
               "'arg' should be one of “cont”, “bin”")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "aA", outcome.type = c("Cont", "bin")),
               "'arg' must be of length 1")
  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "aA", outcome.type = "cont",
                        outcome.method = "GAM"),
               "'arg' should be one of “gam”, “glm”, “SL”")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "aA", outcome.type = "cont",
                        outcome.method = c("gam", "glm")),
               "'arg' must be of length 1")
  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = c("family" = "quasibinomial")),
               "`outcome.controls` must be a named list")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(1.0)),
               "`outcome.controls` must be a named list")

  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(1.0),
                        ps.method = "GLM"),
               "'arg' should be one of “glm”, “gam”, “SL”")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(1.0),
                        ps.method = c("SL", "glm")),
               "'arg' must be of length 1")
  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(),
                        ps.method = "glm",
                        ps.controls = c("family" = "quasibinomial")),
               "`ps.controls` must be a named list")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(),
                        ps.method = "glm",
                        ps.controls = list(1.0)),
               "`ps.controls` must be a named list")

  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(),
                        ps.method = "glm",
                        ps.controls = list(), n.boot = c(100, 200)),
               "`n.boot` must be a non-negative integer")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(),
                        ps.method = "glm",
                        ps.controls = list(), n.boot = 100.01),
               "`n.boot` must be a non-negative integer")
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                        cfName = "X1", outcome.type = "cont",
                        outcome.method = "gam",
                        outcome.controls = list(),
                        ps.method = "glm",
                        ps.controls = list(), n.boot = -1L),
               "`n.boot` must be a non-negative integer")
})

test_that("`IntHTEcf()` results in expected internal errors", {
  
  data.rct <- list("X" = matrix(1:10, 10, 4, dimnames = list(NULL, c("X1", "X2", "X3", "X4"))),
                   "Y" = 1:10, "A" = rep(0L, 10), "mainName" = 1L, "contName" = "X1", "psName" = 1L)
  data.rwe <- list("X" = matrix(1:100, 100, 4, dimnames = list(NULL, c("X1", "X2", "X3", "X4"))),
                   "Y" = 1:100, "A" = rep(0L, 100), "mainName" = 1L, "contName" = "X1", "psName" = 1L)
  
  data.rct$X[1L, 1L] <- NA_real_
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$X[1L, 1L] <- 1
  
  data.rct$Y[1L] <- NA_real_
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$Y[1L] <- 1
  
  data.rct$A[1L] <- NA_real_
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rct$A[1L] <- 0L
  
  data.rwe$X[1L, 1L] <- NA_real_
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$X[1L, 1L] <- 1
  
  data.rwe$Y[1L] <- NA_real_
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$Y[1L] <- 1
  
  data.rwe$A[1L] <- NA_real_
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "elements of `data.rct` and `data.rwe` cannot contain missing values")
  data.rwe$A[1L] <- 0L
  
  data.rwe$ps <- rep(1, length(data.rwe$A))
  expect_error(tryCatch(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
                        warning = function(e) {
                          expect_equal(e$message, "`ps` cannot be provided in `data.rwe`; input ignored")
                          data.rwe$ps <- NULL
                          tryCatch(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
                                   warning = function(e) {
                                     expect_equal(e$message,
                                                  paste("methods developed under the assumption that n >> m;",
                                                        "requested analysis has m/n = 1"), fixed = TRUE)
                                     suppressWarnings(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe))
                                   })
                        }),
               "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)
  data.rct$A <- rep(0L, length(data.rct$A))
  data.rwe$ps <- NULL
  
  data.rwe$A <- seq_along(data.rwe$A)
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe),
               "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)
  data.rwe$A <- c(rep(0, 50), rep(1, 50))
  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "`data.rct$Y` is not binary",
               fixed = TRUE)
  data.rct$Y <- c(rep(0L, 5L), rep(1L, 5L))
  
  data.rct$A <- rep(0L, 10L)
  data.rwe$A <- c(rep(2, 50), rep(3, 50))
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "more than 2 treatments found in data.rct$A and data.rwe$A", fixed = TRUE)
  data.rwe$A <- c(rep(0, 50), rep(1, 50))
  
  expect_error(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "bin"),
               "`data.rwe$Y` is not binary",
               fixed = TRUE)
  data.rwe$Y <- c(rep(0L, 50L), rep(1L, 50L))
  
  tryCatch(IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont", n.boot = 0L),
           message = function(m) {
             expect_equal(m$message,
                          paste0("* * * * * * * * * * WARNING * * * * * * * * * *\n",
                                 "`outcome.type` = 'cont'; however, response provided in data.rct$Y has only 2 unique values\n"),
                          fixed = TRUE)
             NULL
           })
  
})

test_that("`IntHTEcf()` returns expected results", {
  
  n <- 1000L
  m <- n * 10L
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n * 3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$mainName <- c("X1", "X2", "X3")
    data.rct$contName <- c("X1", "X2", "X3")
    data.rct$psName <- c("X1", "X2", "X3")
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- c("X2", "X3")
    data.rwe$contName <- c("X1", "X2", "X3")
    data.rwe$psName <- c("X2", "X3")
  })
  
  models <- list("RWE" = list("ME" = c("X2", "X3"),
                              "PS" = c("X2", "X3")),
                 "RCT" = list("ME" = c("X1", "X2", "X3"),
                              "PS" = c("X1", "X2", "X3")),
                 "contName" = c("X1", "X2", "X3"),
                 "cfName" = c("X1", "X3"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- TRUE
    out <- .psiEst_IntHTEcf(data.rct = data.rct[c("X", "Y", "A", "est.ps")], 
                            data.rwe = data.rwe[c("X", "Y", "A")], 
                            outcome.type = "cont", models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 4L),
                                  rep("phi", 3L)),
                                names(out$est.int), sep = ".")
    out$ve <- NA
    out$cov <- NA
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
    
  })
  
  test_object <- withr::with_seed(2345L, 
                                  IntHTEcf(data.rct, data.rwe, cfName = c("X1", "X3"), n.boot = 0L))
  test_object$call <- NA

  expect_equal(test_object, out)
  
})


test_that("`IntHTEcf()` returns expected results; one covariate", {
  
  n <- 1000L
  m <- n * 10L
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n*3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$mainName <- c("X2")
    data.rct$contName <- c("X2")
    data.rct$psName <- c("X2")
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- c("X1")
    data.rwe$contName <- c("X2")
    data.rwe$psName <- c("X3")
  })
  
  models <- list("RWE" = list("ME" = c("X1"),
                              "PS" = c("X3")),
                 "RCT" = list("ME" = c("X2"),
                              "PS" = c("X2")),
                 "contName" = c("X2"),
                 "cfName" = c("X1", "X2", "X3"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- TRUE
    out <- .psiEst_IntHTEcf(data.rct = data.rct[c("X", "Y", "A", "est.ps")], 
                            data.rwe = data.rwe[c("X", "Y", "A")], 
                            outcome.type = "cont",
                            models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 2L),
                                  rep("phi", 4L)),
                                names(out$est.int), sep = ".")
    out$ve <- NA
    out$cov <- NA
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
  })
  
  test_object <- withr::with_seed(2345L,
                                  IntHTEcf(data.rct, data.rwe, n.boot = 0L))
  test_object$call <- NA
  
  expect_equal(test_object, out)
  
})


test_that("`IntHTEcf()` returns expected results; no covariate", {
  
  n <- 1000L
  m <- 10L * n
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n*3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$mainName <- 1L
    data.rct$contName <- 1L
    data.rct$psName <- 1L
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), n*10, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- 1L
    data.rwe$contName <- 1L
    data.rwe$psName <- 1L
  })
  
  models <- list("RWE" = list("ME" = NULL,
                              "PS" = NULL),
                 "RCT" = list("ME" = NULL,
                              "PS" = NULL),
                 "contName" = NULL,
                 "cfName" = c("X1", "X2", "X3"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- TRUE
    out <- .psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 1L),
                                  rep("phi", 4L)),
                                names(out$est.int), sep = ".")
    out$ve <- NA
    out$cov <- NA
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
  })
  
  test_object <- withr::with_seed(2345L,
                                  IntHTEcf(data.rct, data.rwe, n.boot = 0L))
  test_object$call <- NA
  
  expect_equal(test_object, out)
})

test_that("`IntHTEcf()` returns expected results", {
  
  n <- 1000L
  m <- n * 10L
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n * 3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$mainName <- c("X1", "X2", "X3")
    data.rct$contName <- c("X1", "X2", "X3")
    data.rct$psName <- c("X1", "X2", "X3")
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- c("X1", "X2", "X3")
    data.rwe$contName <- c("X1", "X2", "X3")
    data.rwe$psName <- c("X1", "X2", "X3")
  })
  
  models <- list("RWE" = list("ME" = c("X1", "X2", "X3"),
                              "PS" = c("X1", "X2", "X3")),
                 "RCT" = list("ME" = c("X1", "X2", "X3"),
                              "PS" = c("X1", "X2", "X3")),
                 "contName" = c("X1", "X2", "X3"),
                 "cfName" = c("X1", "X2", "X3"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- TRUE
    out <- .psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 4L),
                                  rep("phi", 4L)),
                                names(out$est.int), sep = ".")
    ve <- .bootFunc(data.rct = data.rct, data.rwe = data.rwe, 
                    outcome.type = "cont",
                    models = models,
                    n.boot = 3L,
                    optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(ve$ve.est.int) <- paste(c(rep("psi", 4L),
                                    rep("phi", 4L)),
                                  names(ve$ve.est.int), sep = ".")
    cov <- ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")]
    ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")] <- NULL
    
    colnames(cov$cov.est.int) <- paste(c(rep("psi", 4L),
                                         rep("phi", 4L)),
                                       colnames(cov$cov.est.int), sep = ".")
    rownames(cov$cov.est.int) <- paste(c(rep("psi", 4L),
                                         rep("phi", 4L)),
                                       rownames(cov$cov.est.int), sep = ".")
    out <- c(out, ve, cov)
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
    
  })

  test_object <- withr::with_seed(2345L, 
                                  IntHTEcf(data.rct, data.rwe, n.boot = 3L))
  test_object$call <- NA

  expect_equal(test_object, out)
  
}) ###


test_that("`IntHTEcf()` returns expected results; one covariate", {
  
  n <- 1000L
  m <- n * 10L
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n*3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$mainName <- c("X2", "X3")
    data.rct$contName <- c("X2")
    data.rct$psName <- c("X2", "X3")
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- c("X1")
    data.rwe$contName <- c("X2")
    data.rwe$psName <- c("X3")
  })
  
  models <- list("RWE" = list("ME" = c("X1"),
                              "PS" = c("X3")),
                 "RCT" = list("ME" = c("X2", "X3"),
                              "PS" = c("X2", "X3")),
                 "contName" = c("X2"),
                 "cfName" = c("X1", "X2", "X3"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- TRUE
    out <- .psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 2L),
                                  rep("phi", 4L)),
                                names(out$est.int), sep = ".")
    ve <- .bootFunc(data.rct = data.rct, data.rwe = data.rwe, 
                    outcome.type = "cont",
                    models = models,
                    n.boot = 3L,
                    optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(ve$ve.est.int) <- paste(c(rep("psi", 2L),
                                    rep("phi", 4L)),
                                  names(ve$ve.est.int), sep = ".")
    cov <- ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")]
    ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")] <- NULL
    
    colnames(cov$cov.est.int) <- paste(c(rep("psi", 2L),
                                         rep("phi", 4L)),
                                       colnames(cov$cov.est.int), sep = ".")
    rownames(cov$cov.est.int) <- paste(c(rep("psi", 2L),
                                         rep("phi", 4L)),
                                       rownames(cov$cov.est.int), sep = ".")
    out <- c(out, ve, cov)
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
  })
  
  test_object <- withr::with_seed(2345L,
                                  IntHTEcf(data.rct, data.rwe, n.boot = 3L))
  test_object$call <- NA
  
  expect_equal(test_object, out)
  
})


test_that("`IntHTEcf()` returns expected results; no covariate", {
  
  n <- 1000L
  m <- 10L * n
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n*3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$mainName <- 1L
    data.rct$contName <- 1L
    data.rct$psName <- 1L
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- 1L
    data.rwe$contName <- 1L
    data.rwe$psName <- 1L
  })
  
  models <- list("RWE" = list("ME" = NULL,
                              "PS" = NULL),
                 "RCT" = list("ME" = NULL,
                              "PS" = NULL),
                 "contName" = NULL,
                 "cfName" = c("X1", "X2"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- TRUE
    out <- .psiEst_IntHTEcf(data.rct = data.rct, data.rwe = data.rwe, 
                            outcome.type = "cont",
                            models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 1L),
                                  rep("phi", 3L)),
                                names(out$est.int), sep = ".")
    ve <- .bootFunc(data.rct = data.rct, data.rwe = data.rwe, 
                    outcome.type = "cont",
                    models = models,
                    n.boot = 3L,
                    optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(ve$ve.est.int) <- paste(c(rep("psi", 1L),
                                    rep("phi", 3L)),
                                  names(ve$ve.est.int), sep = ".")
    cov <- ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")]
    ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")] <- NULL
    
    colnames(cov$cov.est.int) <- paste(c(rep("psi", 1L),
                                         rep("phi", 3L)),
                                       colnames(cov$cov.est.int), sep = ".")
    rownames(cov$cov.est.int) <- paste(c(rep("psi", 1L),
                                         rep("phi", 3L)),
                                       rownames(cov$cov.est.int), sep = ".")
    out <- c(out, ve, cov)
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
  })
  
  test_object <- withr::with_seed(2345L,
                                  IntHTEcf(data.rct, data.rwe,
                                           cfName = c("X1", "X2"), 
                                           n.boot = 3L))
  test_object$call <- NA
  
  expect_equal(test_object, out)
})

test_that("`IntHTEcf()` returns expected results", {
  
  n <- 1000L
  m <- n * 10L
  
  withr::with_seed(1234L, {
    data.rct <- list()
    data.rct$X <- matrix(stats::rnorm(n * 3), n, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rct$Y <- stats::rnorm(n, 1)
    data.rct$A <- stats::rbinom(n, 1, 0.4)
    data.rct$ps <- rep(0.4, n)
    data.rct$mainName <- c("X1", "X2", "X3")
    data.rct$contName <- c("X1", "X2", "X3")
    data.rct$psName <- c("X1", "X2", "X3")
    
    data.rwe <- list()
    data.rwe$X <- matrix(stats::rnorm(m * 3), m, 3,
                         dimnames = list(NULL, c("X1", "X2", "X3")))
    data.rwe$Y <- stats::rnorm(m, 1.5)
    data.rwe$A <- stats::rbinom(m, 1, 0.4)
    data.rwe$mainName <- c("X1", "X2", "X3")
    data.rwe$contName <- c("X1", "X2", "X3")
    data.rwe$psName <- c("X1", "X2", "X3")
  })
  
  models <- list("RWE" = list("ME" = c("X1", "X2", "X3"),
                              "PS" = c("X1", "X2", "X3")),
                 "RCT" = list("ME" = c("X1", "X2", "X3"),
                              "PS" = c("X1", "X2", "X3")),
                 "contName" = c("X1", "X2", "X3"),
                 "cfName" = c("X1", "X2", "X3"),
                 "outcome" = list("method" = "gam",
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm",
                             "controls" = list("family" = "binomial")),
                 "sieve.degree" = 1L)
  
  withr::with_seed(2345L, {
    
    data.rct$est.ps <- FALSE
    out <- .psiEst_IntHTEcf(data.rct = data.rct[c("X", "Y", "A", "est.ps", "ps")], 
                            data.rwe = data.rwe[c("X", "Y", "A")], 
                            outcome.type = "cont",
                            models = models,
                            optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(out$est.int) <- paste(c(rep("psi", 4L),
                                  rep("phi", 4L)),
                                names(out$est.int), sep = ".")
    ve <- .bootFunc(data.rct = data.rct[c("X", "Y", "A", "est.ps", "ps")], 
                    data.rwe = data.rwe[c("X", "Y", "A")], 
                    outcome.type = "cont",
                    models = models,
                    n.boot = 3L,
                    optim.controls = list())
    # Add psi/phi to integrative parameter names
    names(ve$ve.est.int) <- paste(c(rep("psi", 4L),
                                    rep("phi", 4L)),
                                  names(ve$ve.est.int), sep = ".")
    cov <- ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")]
    ve[c("cov.est.meta", "cov.est.rct", "cov.est.int")] <- NULL
    
    colnames(cov$cov.est.int) <- paste(c(rep("psi", 4L),
                                         rep("phi", 4L)),
                                       colnames(cov$cov.est.int), sep = ".")
    rownames(cov$cov.est.int) <- paste(c(rep("psi", 4L),
                                         rep("phi", 4L)),
                                       rownames(cov$cov.est.int), sep = ".")
    out <- c(out, ve, cov)
    out$call <- NA
    class(out) <- c("IntHTEcf", class(out))
    
  })
  test_object <- withr::with_seed(2345L, 
                                  IntHTEcf(data.rct, data.rwe, 
                                           ps.rct = data.rct$ps, n.boot = 3L))
  test_object$call <- NA
  
  expect_equal(test_object, out)
  
})