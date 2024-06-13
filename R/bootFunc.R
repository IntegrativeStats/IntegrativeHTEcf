#' @noRd
#' @param data.rct A list object. Must contain elements Y, A, and X.
#' @param data.rwe A list object. Must contain elements Y, A, and X.
#' @param outcome.type A character object. Must be one of "cont" or "bin".
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
#' @param n.boot An integer object. The number of bootstrap samples.
#' 
#' @return A list containing 
#' 
#' @importFrom stats var
#' @include allEstimators.R stopTests.R
#' @keywords internal
.bootFunc <- function(data.rct, data.rwe, outcome.type, models, n.boot, optim.controls) {
  
  stopifnot(
    "`data.rct` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rct) && is.vector(data.rct, mode = "list") &&
      all(c("X", "Y", "A") %in% names(data.rct)),
    "`data.rwe` must be a named list containing elements 'X', 'Y', and 'A'" =
      !missing(data.rwe) && is.vector(data.rwe, mode = "list") &&
      all(c("X", "Y", "A") %in% names(data.rwe)),
    "`outcome.type` must be provided" = !missing(outcome.type),
    "`models` must be provided" = !missing(models),
    "`n.boot` must be a positive integer > 1" = !missing(n.boot) &&
      .isNumericVector(n.boot, 1L) &&
      isTRUE(all.equal(n.boot, round(n.boot, 0L))) && n.boot > 1,
    "`optim.controls` must be. provided" = !missing(optim.controls)
  )
  
  n <- nrow(data.rct$X)
  m <- nrow(data.rwe$X)
  out <- list()
  iter <- 1L
  while (iter <= n.boot) {
    
    sample_rct <- sample(n, size = n, replace = TRUE)
    sample_rwe <- sample(m, size = m, replace = TRUE)
    
    boot_rct <- lapply(data.rct, 
                       FUN = function(x, subset) {
                         if (is.matrix(x)) { 
                           x[subset, ]
                         } else {
                           x[subset]
                         }}, 
                       subset = sample_rct)
    boot_rct$est.ps <- data.rct$est.ps
    boot_rwe <- lapply(data.rwe, 
                       FUN = function(x, subset) {
                         if (is.matrix(x)) { 
                           x[subset, ]
                         } else {
                           x[subset]
                         }}, 
                       subset = sample_rwe)
    
    out[[iter]] <- tryCatch(.psiEst_IntHTEcf(data.rct = boot_rct, data.rwe = boot_rwe, 
                                             outcome.type = outcome.type, 
                                             models, optim.controls = optim.controls) |> unlist(),
                            error = function(e) { 
                              stop("Error encountered during bootstrap\n\t",
                                   e$message, call. = FALSE)
                            })
    iter <- iter + 1L
  }
  
  out <- do.call(rbind, out)
  
  ve <- apply(out, MARGIN = 2L, stats::var)
  
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
  
}