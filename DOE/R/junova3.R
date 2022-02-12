#' junova3
#'
#' three way anova (no repetition / main effect)
#'
#' @param formula,data,alpha
#'
#' @return ci, plot
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
#' stats anova
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 geom_errorbar
#' ggplot2 geom_text
#' ggplot2 labs
#' ggplot2 facet_grid
#' gridExtra grid.arrange
junova3<-function(formula,data,alpha){
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
  s<-j[4,1]
  f<-j[4,3]
  l<-j[1,1]+1
  m<-j[2,1]+1
  n<-j[3,1]+1

  a.levels<-levels(factor(a))
  a.mean <- NULL
  La<-NULL
  Ua<-NULL
  a.lower<-NULL
  a.upper<-NULL
  a.error<-qt(1-alpha/2,s)*sqrt(f/(n*m))

  for(i in a.levels){
    a.mean[i]<-round(mean(y[a==i]),2)
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
  b.error<-qt(1-alpha/2,s)*sqrt(f/(n*l))

  for(i in b.levels){
    b.mean[i]<-round(mean(y[b==i]),2)
    Lb[i]<-round(b.mean[i]-b.error,2)
    Ub[i]<-round(b.mean[i]+b.error,2)
    b.lower[i]<-as.numeric(Lb[i])
    b.upper[i]<-as.numeric(Ub[i])
  }

  c.levels<-levels(factor(c))
  c.mean<-NULL
  Lc<-NULL
  Uc<-NULL
  c.lower<-NULL
  c.upper<-NULL
  c.error<-qt(1-alpha/2,s)*sqrt(f/(m*l))

  for(i in c.levels){
    c.mean[i]<-round(mean(y[c==i]),2)
    Lc[i]<-round(c.mean[i]-c.error,2)
    Uc[i]<-round(c.mean[i]+c.error,2)
    c.lower[i]<-as.numeric(Lc[i])
    c.upper[i]<-as.numeric(Uc[i])
  }

  t<-mean(y)
  ne<- (l*m*n)/(l+m+n-2)
  abc.error<-qt(1-alpha/2,s)*sqrt(f/ne)
  abc.mean<-NULL
  L.abc<-NULL
  U.abc<-NULL
  abc.lower<-NULL
  abc.upper<-NULL
  abc.data<-NULL

  for(A in a.levels){
    for(B in b.levels){
      for(C in c.levels){
        abc.mean<-round(mean(y[a==A])+mean(y[b==B])+mean(y[c==C])-2*t, 2)
        L.abc<-round(abc.mean-abc.error,2)
        U.abc<-round(abc.mean+abc.error,2)
        abc.lower<-as.numeric(L.abc)
        abc.upper<-as.numeric(U.abc)
        abc.data<- rbind(abc.data,data.frame(A,B,C,abc.lower,abc.mean,abc.upper))
      }
    }
  }


  a.CI<-data.frame(a.levels,a.lower,a.mean,a.upper)
  print(a.CI)
  cat("\n")

  b.CI<-data.frame(b.levels,b.lower,b.mean,b.upper)
  print(b.CI)
  cat("\n")

  c.CI<-data.frame(c.levels,c.lower,c.mean,c.upper)
  print(c.CI)
  cat("\n")

  print(abc.data)
  abc.data<-arrange(abc.data,B,C)
  k1<-ggplot(a.CI,aes(x=a.CI$a.levels,y=a.CI$a.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=a.CI$a.upper, ymin=a.CI$a.lower))+geom_text(aes(label = round(a.CI$a.mean,2)),vjust = 1.5)
  k2<-ggplot(b.CI,aes(x=b.CI$b.levels,y=b.CI$b.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=b.CI$b.upper, ymin=b.CI$b.lower))+geom_text(aes(label = round(b.CI$b.mean,2)),vjust = 1.5)
  k3<-ggplot(c.CI,aes(x=c.CI$c.levels,y=c.CI$c.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=c.CI$c.upper, ymin=c.CI$c.lower))+geom_text(aes(label = round(c.CI$c.mean,2)),vjust = 1.5)
  k4<-ggplot(abc.data, aes(x =interaction(abc.data$B, abc.data$C,lex.order =TRUE), y=abc.data$abc.mean)) +
    geom_point(size=1.5)+geom_errorbar(aes(ymax=abc.data$abc.upper, ymin=abc.data$abc.lower))+
    labs(y="AxBxC CI")+labs(x = " ")+
    facet_grid(.~ abc.data$A, switch = 'x')
  grid.arrange(k1, k2, k3,k4, ncol=3,nrow=2)

}
