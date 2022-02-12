#' random.latin
#'
#' random number table(latin,graeco)
#'
#' @param n,greco,seed,std
#'
#' @return std.order, run.order, level of factor
#'
#' @examples
#' n <- 3
#'
#' @export
#'
#' @importFrom
#' stats runif
#'
random.latin<-function(n, greco=FALSE, seed=0, std=FALSE){

  seed<-seed;
  if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
  }
  set.seed(seed)

  std.order<-1:(n^2)
  run.order<-sample(1:n^2)

  if(greco==FALSE){
    A<-sample(rep(c(1:n),n))
    B<-sample(rep(c(1:n),n))
    C<-sample(rep(c(1:n),n))

    z<-data.frame(A,B,C)
    z<-data.frame(std.order, run.order, z[order(A,B,C),])

    if(std) print(z)
    else print(z[order(run.order),])
  }

  else{
    A<-sample(rep(c(1:n),n))
    B<-sample(rep(c(1:n),n))
    C<-sample(rep(c(1:n),n))
    D<-sample(rep(c(1:n),n))

    z<-data.frame(A,B,C,D)
    z<-data.frame(std.order, run.order, z[order(A,B,C,D),])

    if(std) print(z)
    else print(z[order(run.order),])
  }
}
