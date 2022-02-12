#' fac.design
#'
#' randomized block design of Factorial design
#'
#' @param levels,nVars,r,center,factors,varNames,seed,std
#'
#' @return std.order, run.order, level of factor
#'
#' @examples
#' levels<-c(2,2,3)
#' r=2
#' std=TRUE
#'
#' @export
#'
#'

fac.design<-function (levels, nVars = 0, r=1,center = TRUE, factors = "none", varNames = NULL, seed=0, std=FALSE){

  if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
  }
  set.seed(seed)

  if (length(levels) == 1) {
    std.order<-1:(as.numeric(levels)^as.numeric(nVars)*r)
    run.order<-sample(1:(as.numeric(levels)^as.numeric(nVars)*r))

    levels <- rep(levels, nVars)

    if (missing(nVars))
      stop("nVars is needed when levels is a scalar")
  }
  else {
    std.order<-1:(prod(levels)*r)
    run.order<-sample(1:(prod(levels)*r))

    nVars <- length(levels)
  }

  if (any(levels < 1)) stop("All levels must be greater than 1")

  N <- prod(levels)
  factorVec <- rep(0, nVars)

  if (missing(factors)) {
    factors <- NULL
  }
  else if (is.character(factors)) {
    if (factors == "all") {
      factors <- 1:nVars
      factorVec <- rep(1, nVars)
    }
    else stop("factors error")
  }
  else {
    nFactors <- length(factors)
    for (i in factors) factorVec[i] <- 1
  }

  design <- matrix(0, N, nVars)

  .Call("GetFactorial", design, as.integer(levels), as.integer(center),
        as.integer(factorVec), PACKAGE = "AlgDesign")

  design <- data.frame(design)

  if (!missing(factors)) {
    for (i in factors) {
      design[, i] <- factor(design[, i])
    }
  }

  #design<-design[,c(nVars:1)]

  if (!missing(varNames) && length(varNames == nVars))
    colnames(design) <- varNames
  else colnames(design) <- paste("X", 1:nVars, sep = "")

  result<-data.frame(std.order,run.order,design)

  if(std) print(result)
  else print(result[order(result$run.order),])

}
