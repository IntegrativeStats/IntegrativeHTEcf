test_that(".HTE() returns expected errors", {
  expect_error(.HTE(), "`psi` must be a named numeric vector")
  expect_error(.HTE(c("a", "b", "c", "d")), "`psi` must be a named numeric vector")
  expect_error(.HTE(list("a", "b", "c", "d")), "`psi` must be a named numeric vector")
  expect_error(.HTE(1:3), "`psi` must be a named numeric vector")
  expect_error(.HTE(c("a" = 1, "b" = 2, 3)), "`psi` must be a named numeric vector")
  
  psi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  expect_error(.HTE(psi = psi), "`X` must be a named numeric matrix")
  expect_error(.HTE(psi = psi, X = matrix("A", 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.HTE(psi = psi, X = matrix(1, 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.HTE(psi = psi, X = matrix(1, 3, 3, dimnames = list(NULL, c("a", "b", "")))), 
               "`X` must be a named numeric matrix")
  
  X <- matrix(1, 10, 3, dimnames = list(NULL, c("a", "B", "C")))
  expect_error(.HTE(psi = psi, X = X), "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.HTE(psi = psi, X = X, outcome.type = 2), 
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.HTE(psi = psi, X = X, outcome.type = "co"), 
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.HTE(psi = psi, X = X, outcome.type = c("cont", "bin")), 
               "`outcome.type` must be one of 'cont' or 'bin'")
  
  expect_error(.HTE(psi = psi, X = X, outcome.type = "cont"),
               "names of psi do not match provided X")
  
})

test_that("`.HTE()` returns expected results for continuous", {
  psi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  X <- matrix(1.5, 10, 3, dimnames = list(NULL, c("A", "B", "C")))
  X[, 2L] <- 2.5
  X[, 3L] <- 3.5
  expected <- rep(19.0, time = 10)

  expect_equal(.HTE(psi = psi, X = X, outcome.type = "cont"), expected)
})

test_that("`.HTE()` returns expected results for continuous; single covariate", {
  psi <- c("(Intercept)" = 2, "A" = 1)
  X <- matrix(1.5, 10, 1, dimnames = list(NULL, c("A")))
  expected <- rep(3.5, time = 10)
  
  expect_equal(.HTE(psi = psi, X = X, outcome.type = "cont"), expected)
})

test_that("`.HTE()` returns expected results for continuous; no covariate", {
  psi <- c("(Intercept)" = 2)
  X <- matrix(0, 10, 0)
  expected <- rep(2.0, time = 10)
  
  expect_equal(.HTE(psi = psi, X = X, outcome.type = "cont"), expected)
})

test_that("`.HTE()` returns expected results for binary", {
  psi <- c("(Intercept)" = .2, "A" = 1, "B" = 2, "C" = 3)
  X <- matrix(.15, 10, 3, dimnames = list(NULL, c("A", "B", "C")))
  X[, 2L] <- .25
  X[, 3L] <- .35
  expected <- rep((exp(1.9) - 1.0) / (exp(1.9) + 1.0), time = 10)
  expect_equal(.HTE(psi = psi, X = X, outcome.type = "bin"), expected)
})

test_that("`.HTE()` returns expected results for binary; single covariate", {
  psi <- c("(Intercept)" = .2, "A" = 1)
  X <- matrix(.15, 10, 1, dimnames = list(NULL, c("A")))
  expected <- rep((exp(.35) - 1.0) / (exp(.35) + 1.0), time = 10)
  expect_equal(.HTE(psi = psi, X = X, outcome.type = "bin"), expected)
})

test_that("`.HTE()` returns expected results for binary; no covariate", {
  psi <- c("(Intercept)" = .2)
  X <- matrix(.15, 10, 0)
  expected <- rep((exp(.2) - 1.0) / (exp(.2) + 1.0), time = 10)
  expect_equal(.HTE(psi = psi, X = X, outcome.type = "bin"), expected)
})

test_that(".dHTE() returns expected errors", {
  expect_error(.dHTE(), "`psi` must be a named numeric vector")
  expect_error(.dHTE(c("a", "b", "c", "d")), "`psi` must be a named numeric vector")
  expect_error(.dHTE(list("a", "b", "c", "d")), "`psi` must be a named numeric vector")
  expect_error(.dHTE(1:3), "`psi` must be a named numeric vector")
  expect_error(.dHTE(c("a" = 1, "b" = 2, 3)), "`psi` must be a named numeric vector")
  
  psi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  expect_error(.dHTE(psi = psi), "`X` must be a named numeric matrix")
  expect_error(.dHTE(psi = psi, X = matrix("A", 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.dHTE(psi = psi, X = matrix(1, 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.dHTE(psi = psi, X = matrix(1, 3, 3, dimnames = list(NULL, c("a", "b", "")))), 
               "`X` must be a named numeric matrix")
  
  X <- matrix(1, 10, 3, dimnames = list(NULL, c("a", "B", "C")))
  expect_error(.dHTE(psi = psi, X = X), "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.dHTE(psi = psi, X = X, outcome.type = 2), 
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.dHTE(psi = psi, X = X, outcome.type = "co"), 
               "`outcome.type` must be one of 'cont' or 'bin'")
  expect_error(.dHTE(psi = psi, X = X, outcome.type = c("cont", "bin")), 
               "`outcome.type` must be one of 'cont' or 'bin'")
  
  expect_error(.dHTE(psi = psi, X = X, outcome.type = "cont"),
               "names of psi do not match provided X")
  
})

test_that("`.dHTE()` returns expected results for continuous", {
  psi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  X <- matrix(1.5, 10, 3, dimnames = list(NULL, c("A", "B", "C")))
  X[, 2L] <- 2.5
  X[, 3L] <- 3.5
  expected <- cbind(1.0, X)
  
  expect_equal(.dHTE(psi = psi, X = X, outcome.type = "cont"), expected)
})

test_that("`.dHTE()` returns expected results for continuous; single covariate", {
  psi <- c("(Intercept)" = 2, "A" = 1)
  X <- matrix(1.5, 10, 1, dimnames = list(NULL, c("A")))
  expected <- cbind(1.0, X)
  
  expect_equal(.dHTE(psi = psi, X = X, outcome.type = "cont"), expected)
})

test_that("`.dHTE()` returns expected results for continuous; no covariate", {
  psi <- c("(Intercept)" = 2)
  X <- matrix(0, 10, 0)
  expected <- cbind(1.0, X)
  
  expect_equal(.dHTE(psi = psi, X = X, outcome.type = "cont"), expected)
})

test_that("`.dHTE()` returns expected results for binary", {
  psi <- c("(Intercept)" = .2, "A" = 1, "B" = 2, "C" = 3)
  X <- matrix(.15, 10, 3, dimnames = list(NULL, c("A", "B", "C")))
  X[, 2L] <- .25
  X[, 3L] <- .35
  expected <- cbind(1.0, X) * rep((2*exp(1.9)) / (exp(1.9) + 1.0)^2, time = 10)
  expect_equal(.dHTE(psi = psi, X = X, outcome.type = "bin"), expected)
})

test_that("`.dHTE()` returns expected results for binary; single covariate", {
  psi <- c("(Intercept)" = .2, "A" = 1)
  X <- matrix(.15, 10, 1, dimnames = list(NULL, c("A")))
  expected <- cbind(1.0, X) * rep((2.0*exp(.35)) / (exp(.35) + 1.0)^2, time = 10)
  expect_equal(.dHTE(psi = psi, X = X, outcome.type = "bin"), expected)
})

test_that("`.dHTE()` returns expected results for binary; no covariate", {
  psi <- c("(Intercept)" = .2)
  X <- matrix(.15, 10, 0)
  expected <- cbind(1.0, X) * rep((2*exp(.2) ) / (exp(.2) + 1.0)^2, time = 10)
  expect_equal(.dHTE(psi = psi, X = X, outcome.type = "bin"), expected)
})

test_that(".lambda() returns expected errors", {
  expect_error(.lambda(), "`phi` must be a named numeric vector")
  expect_error(.lambda(c("a", "b", "c", "d")), "`phi` must be a named numeric vector")
  expect_error(.lambda(list("a", "b", "c", "d")), "`phi` must be a named numeric vector")
  expect_error(.lambda(1:3), "`phi` must be a named numeric vector")
  expect_error(.lambda(c("a" = 1, "b" = 2, 3)), "`phi` must be a named numeric vector")
  
  phi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  expect_error(.lambda(phi = phi), "`X` must be a named numeric matrix")
  expect_error(.lambda(phi = phi, X = matrix("A", 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.lambda(phi = phi, X = matrix(1, 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.lambda(phi = phi, X = matrix(1, 3, 3, dimnames = list(NULL, c("a", "b", "")))), 
               "`X` must be a named numeric matrix")
  
  X <- matrix(1, 10, 3, dimnames = list(NULL, c("a", "B", "C")))
  expect_error(.lambda(phi = phi, X = X), "`n.rct` must be an integer")
  expect_error(.lambda(phi = phi, X = X, n.rct = c("a")), 
               "`n.rct` must be an integer")
  expect_error(.lambda(phi = phi, X = X, n.rct = list(1:3)), 
               "`n.rct` must be an integer")

  expect_error(.lambda(phi = phi, X = X, n.rct = 10L),
               "names of phi do not match provided X")
  
})

test_that("`.lambda()` returns expected results for continuous", {
  
  psi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  X <- matrix(1.5, 10, 3, dimnames = list(NULL, c("A", "B", "C")))
  X[, 2L] <- 2.5
  X[, 3L] <- 3.5
  
  expected <- c(rep(0, 10), rep(19.0, nrow(X)))

  expect_equal(.lambda(phi = psi, X = X, n.rct = 10L), expected)
})

test_that("`.lambda()` returns expected results for continuous; single covariate", {
  psi <- c("(Intercept)" = 2, "A" = 1)
  X <- matrix(1.5, 10, 1, dimnames = list(NULL, c("A")))
  expected <- c(rep(0, 2), rep(3.5, nrow(X)))
  
  expect_equal(.lambda(phi = psi, X = X, n.rct = 2L), expected)
})

test_that("`.lambda()` returns expected results for continuous; no covariate", {
  psi <- c("(Intercept)" = 2)
  X <- matrix(0, 10, 0)
  expected <- rep(2.0, nrow(X))
  
  expect_equal(.lambda(phi = psi, X = X, n.rct = 0L), expected)
})

test_that(".dlambda() returns expected errors", {
  expect_error(.dlambda(), "`phi` must be a named numeric vector")
  expect_error(.dlambda(c("a", "b", "c", "d")), "`phi` must be a named numeric vector")
  expect_error(.dlambda(list("a", "b", "c", "d")), "`phi` must be a named numeric vector")
  expect_error(.dlambda(1:3), "`phi` must be a named numeric vector")
  expect_error(.dlambda(c("a" = 1, "b" = 2, 3)), "`phi` must be a named numeric vector")
  
  phi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  expect_error(.dlambda(phi = phi), "`X` must be a named numeric matrix")
  expect_error(.dlambda(phi = phi, X = matrix("A", 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.dlambda(phi = phi, X = matrix(1, 3, 3)), "`X` must be a named numeric matrix")
  expect_error(.dlambda(phi = phi, X = matrix(1, 3, 3, dimnames = list(NULL, c("a", "b", "")))), 
               "`X` must be a named numeric matrix")
  
  X <- matrix(1, 10, 3, dimnames = list(NULL, c("a", "B", "C")))
  expect_error(.dlambda(phi = phi, X = X), "`n.rct` must be an integer")
  expect_error(.dlambda(phi = phi, X = X, n.rct = c("a")), 
               "`n.rct` must be an integer")
  expect_error(.dlambda(phi = phi, X = X, n.rct = list(1:3)), 
               "`n.rct` must be an integer")
  
  expect_error(.dlambda(phi = phi, X = X, n.rct = 10L),
               "names of phi do not match provided X")
  
})

test_that("`.dlambda()` returns expected results for continuous", {
  
  psi <- c("(Intercept)" = 2, "A" = 1, "B" = 2, "C" = 3)
  X <- matrix(1.5, 10, 3, dimnames = list(NULL, c("A", "B", "C")))
  X[, 2L] <- 2.5
  X[, 3L] <- 3.5
  
  expected <- rbind(matrix(0, 10, 4), cbind(1.0, X))
  
  expect_equal(.dlambda(phi = psi, X = X, n.rct = 10L), expected)
})

test_that("`.dlambda()` returns expected results for continuous; single covariate", {
  psi <- c("(Intercept)" = 2, "A" = 1)
  X <- matrix(1.5, 10, 1, dimnames = list(NULL, c("A")))
  expected <- rbind(matrix(0.0, 2, 2), cbind(1.0, X))
  
  expect_equal(.dlambda(phi = psi, X = X, n.rct = 2L), expected)
})

test_that("`.dlambda()` returns expected results for continuous; no covariate", {
  psi <- c("(Intercept)" = 2)
  X <- matrix(0, 10, 0)
  expected <- cbind(1.0, X)
  
  expect_equal(.dlambda(phi = psi, X = X, n.rct = 0L), expected)
})

test_that("`.ipw_HTE()` returns expected errors", {
  expect_error(.ipw_HTE(), "`Y` must be a numeric vector")
  expect_error(.ipw_HTE(Y = c("a", "b", "c")), "`Y` must be a numeric vector")
  expect_error(.ipw_HTE(Y = matrix(1, 3, 1)), "`Y` must be a numeric vector")
  
  expect_error(.ipw_HTE(Y = 1:3), "`A` must be a numeric vector")
  expect_error(.ipw_HTE(Y = 1:3, A = c("a", "b", "c")), "`A` must be a numeric vector")
  expect_error(.ipw_HTE(Y = 1:3, A = matrix(1, 3, 1)), "`A` must be a numeric vector")
  expect_error(.ipw_HTE(Y = 1:3, A = 1:4), "`A` must be a numeric vector")
  
  expect_error(.ipw_HTE(Y = 1:3, A = 1:3), "`ps` must be a numeric vector")
  expect_error(.ipw_HTE(Y = 1:3, A = 1:3, ps = c("a", "b", "c")), "`ps` must be a numeric vector")
  expect_error(.ipw_HTE(Y = 1:3, A = 1:3, ps = matrix(1, 3, 1)), "`ps` must be a numeric vector")
  expect_error(.ipw_HTE(Y = 1:3, A = 1:3, ps = 1:4), "`ps` must be a numeric vector")

})

test_that("`.ipw_HTE()` returns expected results", {
  expect_equal(.ipw_HTE(Y = 1:10, A = rep(1, 10), ps = rep(0.4, 10)),
               1:10*2.5)
  expect_equal(.ipw_HTE(Y = 1:10, A = rep(0, 10), ps = rep(0.2, 10)),
               -(1:10*1.25))
  expect_equal(.ipw_HTE(Y = 1:10, A = c(rep(0, 6), rep(1,4)), ps = seq(0.1, 1.0, length.out = 10)),
               c(-1/0.9, -2/0.8, -3/0.7, -4/0.6, -5/0.5,
                 -6/0.4, 7/0.7, 8/0.8, 9/0.9, 10/1.0))
  
})