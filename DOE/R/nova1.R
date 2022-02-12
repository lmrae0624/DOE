#' nova1
#'
#' one way anova (CI plot / repetition)
#'
#' @param formula,data,alpha,na.rm,verdose
#'
#' @return anova, ci plot
#'
#' @examples
#' a<-as.factor(c(rep(1,9),rep(2,9),rep(3,9)))
#' b<-as.factor(c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
#' c<-as.factor(rep(c(1,2,3),9))
#' y<-c(74,86,76,72,91,87,48,65,56,61,78,71,62,81,77,55,72,63,50,70,60,49,68,64,52,69,60)
#' z<-data.frame(a,b,c,y)
#'
#' @export
#'
#' @importFrom
#' ggplot2 ggplot
#'
nova1<-function(formula,data,alpha,na.rm=TRUE,verdose=TRUE){
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
  error<-qt(1-alpha/2,d)*sqrt(f/s)

  for(i in x.levels){
    y.mean[i]<-mean(y[group==i])
    L[i]<-round(y.mean[i]-error,2)
    U[i]<-round(y.mean[i]+error,2)
    lower[i]<-as.numeric(L[i])
    upper[i]<-as.numeric(U[i])
  }

  CI<-data.frame(x.levels,lower,y.mean,upper)
  print(CI)

  ggplot(CI,aes(x=CI$x.levels, y=CI$y.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=CI$upper, ymin=CI$lower)) +
    labs(x="LEVEL",y="CI")+geom_text(aes(label = CI$y.mean),vjust = 1.5)
}
