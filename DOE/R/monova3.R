#' monova3
#'
#' two way anova (CI plot / no repetition / fixed model)
#'
#' @param formula,data,alpha,k
#'
#' @return ci plot
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
#' b<-as.factor(rep(c(1,2,3),4))
#' y<-c(97.6,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' ggplot2 ggplot
#'
monova3<-function(formula,data,alpha,k){
  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(stats::model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)
  Y.name<-formula.t[2]

  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }

  j<-suppressWarnings(anova(lm(formula)))
  s<-j[3,1]
  f<-j[3,3]
  l<-j[1,1]+1
  m<-j[2,1]+1


  a.levels<-levels(factor(a))
  a.mean <- NULL
  La<-NULL
  Ua<-NULL
  a.lower<-NULL
  a.upper<-NULL
  a.error<-qt(1-alpha/2,s)*sqrt(f/m)

  for(i in a.levels){
    a.mean[i]<-mean(y[a==i])
    La[i]<-round(a.mean[i]-a.error,2)
    Ua[i]<-round(a.mean[i]+a.error,2)
    a.lower[i]<-as.numeric(La[i])
    a.upper[i]<-as.numeric(Ua[i])
  }



  b.levels<-levels(factor(b))
  b.mean<-NULL
  Lb<-NULL
  Ub<-NULL
  b.lower<-NULL
  b.upper<-NULL
  b.error<-qt(1-alpha/2,s)*sqrt(f/l)

  for(i in b.levels){
    b.mean[i]<-mean(y[b==i])
    Lb[i]<-round(b.mean[i]-b.error,2)
    Ub[i]<-round(b.mean[i]+b.error,2)
    b.lower[i]<-as.numeric(Lb[i])
    b.upper[i]<-as.numeric(Ub[i])
  }

  a.CI<-data.frame(a.levels,a.lower,a.mean,a.upper)
  print(a.CI)

  b.CI<-data.frame(b.levels,b.lower,b.mean,b.upper)
  print(b.CI)

  k1<-ggplot(a.CI,aes(x=a.CI$a.levels,y=a.CI$a.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=a.CI$a.upper, ymin=a.CI$a.lower))+geom_text(aes(label = round(a.CI$a.mean,2)),vjust = 1.5)
  k2<-ggplot(b.CI,aes(x=b.CI$b.levels,y=b.CI$b.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=b.CI$b.upper, ymin=b.CI$b.lower))+geom_text(aes(label = round(b.CI$b.mean,2)),vjust = 1.5)

  grid.arrange(k1, k2,  ncol=2)
}
