#' nnova1
#'
#' one way anova (CI plot / no repetition)
#'
#' @param formula,data,alpha,na.rm,verdose
#'
#' @return plot, anova, CI
#'
#' @examples
#' a1<-c(49,73,58,38,42)
#' a2<-c(31,40,43,44,34,20)
#' a3<-c(46,41,58,31,65)
#' a4<-c(45,73,76)
#' a <- c(a1,a2,a3,a4)
#' group<-c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4,4,4)
#' group<-as.factor(group)
#' z<-data.frame(a,group)
#'
#' @export
#'
#' @importFrom
#' ggplot2 ggplot
nnova1<-function(formula,data,alpha,na.rm=TRUE,verdose=TRUE){
  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }

  if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

  y = data[[dp[[2L]]]]
  group = data[[dp[[3L]]]]
  if (!is.factor(group)) stop("The group variable must be a factor.")
  if (!is.numeric(y)) stop("The response must be a numeric variable.")

  n <- length(y)
  x.levels <- levels(factor(group))
  y.mean <- y.n <- NULL
  L<-NULL
  U<-NULL
  lower<-NULL
  upper<-NULL

  b<-anova(lm(y~group))
  s<-b[1,1]
  d<-b[2,1]
  f<-b[2,3]
  print(b)

  for(i in x.levels){
    mi<-NULL
    error<-NULL
    mi[i]<-length(which(group==i))
    error[i]<-qt(1-alpha/2,d)*sqrt(f/mi[i])
    y.mean[i]<-mean(y[group==i])
    L[i]<-round(y.mean[i]-error[i],2)
    U[i]<-round(y.mean[i]+error[i],2)
    lower[i]<-as.numeric(L[i])
    upper[i]<-as.numeric(U[i])
  }

  y.mean<-round(y.mean,2)
  CI<-data.frame(x.levels,lower,y.mean,upper)
  print(CI)
  ggplot(CI,aes(x=CI$x.levels, y=CI$y.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=CI$upper, ymin=CI$lower)) +
    labs(x="LEVEL",y="CI")+geom_text(aes(label = CI$y.mean),vjust = 1.5)

}
