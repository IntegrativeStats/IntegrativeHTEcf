test_that("`.bootFunc()` returns expected errors", {
  
  expect_error(.bootFunc(),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.bootFunc(data.rct = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.bootFunc(data.rct = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.bootFunc(data.rct = list("X" = 1, "Y" = 2)),
               "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'")
  data.rct <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("AA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10, "q" = 1:10)
  
  expect_error(.bootFunc(data.rct = data.rct),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.bootFunc(data.rct = data.rct,
                                data.rwe = data.frame("X" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.bootFunc(data.rct = data.rct,
                                data.rwe = list("X2" = 1, "Y" = 2, "A" = 3, "ps" = 4)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = list("X" = 1, "Y" = 2)),
               "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'")
  data.rwe <- list("X" = matrix(1, 10, 2, dimnames = list(NULL, c("aA", "BB"))), 
                   "Y" = 1:10, "A" = 1:10, "ps" = 1:10, "q" = 1:10)
  
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe, outcome.type = "cont"),
               "`models` must be provided")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2),
                                       "RCT" = list("ME" = 1, "PS" = 2),
                                       "outcome" = list("method" = 1, "controls" = 2),
                                       "ps" = c("method" = 1, "controls" = 2),
                                       "sieve.degree" = 5, "contName" = 6,
                                       "cfName" = 7)),
               "`n.boot` must be a positive integer > 1")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2),
                                       "RCT" = list("ME" = 1, "PS" = 2),
                                       "outcome" = list("method" = 1, "controls" = 2),
                                       "ps" = c("method" = 1, "controls" = 2),
                                       "sieve.degree" = 5, "contName" = 6,
                                       "cfName" = 7), n.boot = "1"),
               "`n.boot` must be a positive integer > 1")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2),
                                       "RCT" = list("ME" = 1, "PS" = 2),
                                       "outcome" = list("method" = 1, "controls" = 2),
                                       "ps" = c("method" = 1, "controls" = 2),
                                       "sieve.degree" = 5, "contName" = 6,
                                       "cfName" = 7), n.boot = c(1, 1)),
               "`n.boot` must be a positive integer > 1")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2),
                                       "RCT" = list("ME" = 1, "PS" = 2),
                                       "outcome" = list("method" = 1, "controls" = 2),
                                       "ps" = c("method" = 1, "controls" = 2),
                                       "sieve.degree" = 5, "contName" = 6,
                                       "cfName" = 7), n.boot = 1.001),
               "`n.boot` must be a positive integer > 1")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2),
                                       "RCT" = list("ME" = 1, "PS" = 2),
                                       "outcome" = list("method" = 1, "controls" = 2),
                                       "ps" = c("method" = 1, "controls" = 2),
                                       "sieve.degree" = 5, "contName" = 6,
                                       "cfName" = 7), n.boot = -1L),
               "`n.boot` must be a positive integer > 1")
  expect_error(.bootFunc(data.rct = data.rct, data.rwe = data.rwe,
                         outcome.type = "cont",
                         models = list("RWE" = list("ME" = 1, "PS" = 2),
                                       "RCT" = list("ME" = 1, "PS" = 2),
                                       "outcome" = list("method" = 1, "controls" = 2),
                                       "ps" = c("method" = 1, "controls" = 2),
                                       "sieve.degree" = 5, "contName" = 6,
                                       "cfName" = 7), n.boot = 1L),
               "`n.boot` must be a positive integer > 1")
})

test_that(".bootFunc() returns expected results", {
  
  n <- 1000L
  X <- withr::with_seed(1234L, matrix(runif(3*n), n, 3))
  X <- cbind(X, withr::with_seed(3456L, stats::rbinom(n, 1, 0.3)))
  colnames(X) <- c("X1", "X2", "X3", "X4")
  A <- withr::with_seed(2345L, stats::rbinom(n, 1, 0.4))
  Y <- withr::with_seed(3456L,  stats::rnorm(n, 2, 0.3))
  ps <- withr::with_seed(4567L, stats::runif(n))
  
  data.obj <- list("X" = X, "A" = A, "Y" = Y)
  
  models <- list("RCT" = list("ME" = c("X1", "X3"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "RWE" = list("ME" = c("X1", "X3"), 
                              "PS" = c("X1", "X2", "X3", "X4")),
                 "contName" = c("X2", "X4"),
                 "outcome" = list("method" = "gam", 
                                  "controls" = list("family" = "gaussian")),
                 "ps" = list("method" = "glm", 
                             "controls" = list("family" = "binomial")),
                 "cfName" = c("X1"),
                 "sieve.degree" = 1L)
  withr::with_seed(1234, {
    out <- list()
    for (i in 1L:2L) {
      sample_rct <- sample(n, size = n, replace = TRUE)
      sample_rwe <- sample(n, size = n, replace = TRUE)
    
      boot_rct <- list("X" = X[sample_rct, , drop = FALSE],
                       "Y" = Y[sample_rct],
                       "A" = A[sample_rct])  
      boot_rwe <- list("X" = X[sample_rwe, , drop = FALSE],
                       "Y" = Y[sample_rwe],
                       "A" = A[sample_rwe])  
      boot_rct$est.ps <- TRUE
      out[[i]] <- .psiEst_IntHTEcf(data.rct = boot_rct, data.rwe = boot_rwe, 
                                   outcome.type = "cont", models = models,
                                   optim.controls = list()) |> unlist()
    }
 
    out <- do.call(rbind, out)
    ve <- numeric(ncol(out))
    for (i in 1L:ncol(out)) {
      ve[i] <- stats::var(out[, i])
    }
    nms <- c("est.meta", "att.meta", "est.rct", "att.rct", "est.int", "att.int")
    
    res <- list()
    for (i in nms) {
      tst <- grepl(i, colnames(out))
      res[[i]] <- ve[tst]
      names(res[[i]]) <- gsub(paste0(i, "."), "", colnames(out)[tst], fixed = TRUE)
    }
    names(res) <- paste0("ve.", names(res))
    
    nms <- c("est.meta", "est.rct", "est.int")
    for (i in nms) {
      tst <- grepl(i, colnames(out))
      res[[paste0("cov.", i)]] <- stats::cov(out[, tst, drop = FALSE])
      colnames(res[[paste0("cov.", i)]]) <- gsub(paste0(i, "."), "", colnames(out)[tst], fixed = TRUE)
      rownames(res[[paste0("cov.", i)]]) <- gsub(paste0(i, "."), "", colnames(out)[tst], fixed = TRUE)
    }
    res
  })
  
  expect_equal(withr::with_seed(1234,
                                .bootFunc(data.rct = c(data.obj, "est.ps" = TRUE), data.rwe = data.obj, 
                                          outcome.type = "cont", 
                                          models = models, n.boot = 2L,
                                          optim.controls = list())),
               res)
})