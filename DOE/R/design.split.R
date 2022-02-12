#' design.split
#'
#' design of split's random number table
#'
#' @param trt1,trt2,r,seed,randomization,std
#'
#' @return std.order, run.order, block, trt1, trt2
#'
#' @examples
#' t1<-c(1,2,3)
#' t2<-c(1,2,3)
#'
#' @export
#'
#' @importFrom
#' stats runif
#'

design.split <-function (trt1, trt2,r=NULL,seed = 0,randomization=TRUE,std=FALSE ){
    n1<-length(trt1)
    n2<-length(trt2)
    if (seed == 0) {
      genera<-runif(1)
      seed <-.Random.seed[3]
    }
    set.seed(seed)
    number<-10

    design.rcbd <-
      function (trt, r,seed=0,randomization=TRUE,std=FALSE ){
        number<-10
        ntr <- length(trt)
        if (seed == 0) {
          genera<-runif(1)
          seed <-.Random.seed[3]
        }
        set.seed(seed)
        parameters<-list(design="rcbd",trt=trt,r=r,seed=seed,randomization)
        mtr <-trt
        if(randomization)mtr <- sample(trt, ntr)
        block <- c(rep(1, ntr))
        for (y in 2:r) {
          if(r==1){
            block <- c(rep(1, ntr))
            if(randomization)mtr <- c(mtr)
          }else{
            block <- c(block, rep(y, ntr))
            if(randomization)mtr <- c(mtr, sample(trt, ntr))
          }
        }
        plots <- block*number+(1:ntr)
        book <- data.frame(plots,block = as.factor(block), trt = as.factor(mtr))

        outdesign<-list(book=book)
        return(outdesign)

      }
    plan<-design.rcbd(trt1,r, seed,randomization,std)
    book<-plan$book
    j<-0
    nplot<-nrow(book)
    d<-NULL

    if(randomization){
      for(i in 1:nplot){
        d<-rbind(d,sample(trt2,n2))
      }
    }else{
      d<-rbind(d,trt2[1:n2])
    }
    aa<-data.frame(book,trt2=d[,1])
    for(j in 2:n2) aa<-rbind(aa,data.frame(book,trt2=d[,j]))
    aa<-aa[order(aa[,1]),]
    splots<-rep(gl(n2,1),nplot)

    s1<-n1*n2*r
    std.order<-sample(s1)
    run.order<-sample(s1)
    rtr1<-length(std.order)
    rtr2<-length(run.order)
    if(std==FALSE){
      std.order<-sample(std.order,rtr1)
      run.order<-sort(run.order)
    }else{
      std.order<-sort(std.order)
      run.order<-sample(run.order,rtr2)
    }
    book <- data.frame(std.order=as.factor(std.order),run.order=as.factor(run.order),aa)
    rownames(book)<-1:(nrow(book))
    book<-book[,-3]
    outdesign<-list(book=book)
    return(outdesign)
  }
