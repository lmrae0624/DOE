#' random.one
#'
#' random number table(one way)
#'
#' @param fac,r,seed,std
#'
#' @return std.order, run.order, r, level of factor
#'
#' @examples
#' A<-c("a","b","c")
#'
#' @export
#'
#' @importFrom
#' stats runif
#'

random.one<-function(fac, r=1, seed=0, std=FALSE){

  seed<-seed;
  if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
  }
  set.seed(seed)

  n<-length(fac)
  k<-1:(n*r)

  dat<-factor(rep(c(fac), each=r))

  std.order<-1:(n*r)
  run.order<-sample(1:(n*r))

  r<-seq(1,r)

  dat1<-data.frame(std.order, run.order, r, dat)
  names(dat1)[4]<-c(paste(deparse(substitute(fac))))

  if(std) print(dat1)
  else print(dat1[order(run.order),])
}
