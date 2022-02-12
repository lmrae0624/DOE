#' random.multi
#'
#' multiple way random number table
#'
#' @param fac,r,seed,std
#'
#' @return std.order, run.order, r, factorlevel
#'
#' @examples
#' fac<-c(2,3)
#'
#' @export
#'
#' @importFrom
#' stats runif
#'
#'
random.multi <-function(fac, r=1, seed=0, std=FALSE){

  seed<-seed
  if (seed == 0) {
    genera<-runif(1)
    seed <-.Random.seed[3]
  }
  set.seed(seed)

  n<-length(fac)


    fact<-NULL
  fr1<-1:fac[1]
  k<-0
  a<-fac[1]
  b<-fac[2]
  for(i in 1:a){
    for(j in 1:b){
      k<-k+1
      fact[k]<-paste(fr1[i],j)
    }
  }

    if(n >2) {
    for(m in 3:n){
      k<-0
      fr1<-fact
      fact<-NULL
      a<-a*b
      b<-fac[m]
      for(i in 1:a){
        for(j in 1:b){
          k<-k+1
          fact[k]<-paste(fr1[i],j)
        }
      }
    }
  }

  design<-function(fac, r, seed=0, std=FALSE){
    dat<-data.frame(fac,r)
    dat<-dat[order(dat[,1]),]

    f<-as.character(fac)
    r<-as.numeric(dat[,2])
    fn <- length(f)

    ffac <- rep(f[1], r[1]) # fac
    for (i in 2:fn) ffac<-c(ffac, rep(f[i], r[i]))

    std.order<-1:length(ffac)
    run.order<-sample(1:length(ffac))

    dca<-data.frame(std.order, run.order, ffac)
    xx<-dca[order(dca[,3],dca[,1]),] ##factor, std.order

    # dca[,1]<-as.numeric(dca[,1]) #std.order

    r1<-seq(1,r[1])
    for (i in 2:length(r)) {
      r1<-c(r1,seq(1,r[i]))
    }

    yy<-data.frame(std.order=xx[,1], run.order=xx[,2], r=r1, fac=ffac)

    names(yy)[4]<-c(paste(deparse(substitute(fac))))

    return(yy)
  }

  plan<-design(fac=fact, r, seed, std)
  fac<-as.character(plan[,4])
  nplan<-nrow(plan)

  A<-rep(" ",nplan*n)
  dim(A)<-c(nplan,n)
  colnames(A)<-LETTERS[1:n]

  for(i in 1:nplan) {
    A[i,]<-unlist(strsplit(fac[i], " "))
  }

  A<-as.data.frame(A)

  result<-data.frame(plan[,1:3],A)

  if(std) print(result)
  else print(result[order(result$run.order),])
}
