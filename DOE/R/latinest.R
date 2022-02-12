#' latin.est
#'
#' population mean estimation (latin,graeco)
#'
#' @param formula,data,alpha,latin,ro,co
#'
#' @return lower,maen,upper,plot
#'
#' @examples
#' a <- as.factor(c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
#' b <- as.factor(rep(c(1,2,3,4,5),5))
#' c <- as.factor(c("C1","C2","C3","C4","C5","C2","C3","C4","C5","C1","C3","C4","C5","C1","C2","C4","C5","C1","C2","C3", "C5","C1","C2","C3","C4"))
#' y <- c(68,64,71,71,72,74,70,80,74,80,63,65,70,69,68,64,58,69,66,65,70,72,76,70,78)
#' z<-data.frame(a,b,c,y)
#'
#' @export
#'
#' @importFrom
#' stats model.frame
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 errorbar
#' ggplot2 geom_text
#' gridExtra grid.arrange
#'

latin.est<-function(formula,data,alpha,latin=TRUE,ro,co){
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
  if(latin==FALSE){
    j<-suppressWarnings(anova(lm(formula)))
    cat("\n")
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
    a.error<-qt(1-alpha/2,s)*sqrt(f/ro)

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
    b.error<-qt(1-alpha/2,s)*sqrt(f/ro)

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
    c.error<-qt(1-alpha/2,s)*sqrt(f/ro)

    for(i in c.levels){
      c.mean[i]<-round(mean(y[c==i]),2)
      Lc[i]<-round(c.mean[i]-c.error,2)
      Uc[i]<-round(c.mean[i]+c.error,2)
      c.lower[i]<-as.numeric(Lc[i])
      c.upper[i]<-as.numeric(Uc[i])
    }

    d.levels<-levels(factor(d))
    d.mean<-NULL
    Ld<-NULL
    Ud<-NULL
    d.lower<-NULL
    d.upper<-NULL
    d.error<-qt(1-alpha/2,s)*sqrt(f/ro)

    for(i in d.levels){
      d.mean[i]<-round(mean(y[d==i]),2)
      Ld[i]<-round(d.mean[i]-d.error,2)
      Ud[i]<-round(d.mean[i]+d.error,2)
      d.lower[i]<-as.numeric(Ld[i])
      d.upper[i]<-as.numeric(Ud[i])
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

    d.CI<-data.frame(d.levels,d.lower,d.mean,d.upper)
    print(d.CI)
    cat("\n")

    k1<-ggplot(a.CI,aes(x=a.CI$a.levels,y=a.CI$a.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=a.CI$a.upper, ymin=a.CI$a.lower))+geom_text(aes(label = round(a.CI$a.mean,2)),vjust = 1.5)
    k2<-ggplot(b.CI,aes(x=b.CI$b.levels,y=b.CI$b.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=b.CI$b.upper, ymin=b.CI$b.lower))+geom_text(aes(label = round(b.CI$b.mean,2)),vjust = 1.5)
    k3<-ggplot(c.CI,aes(x=c.CI$c.levels,y=c.CI$c.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=c.CI$c.upper, ymin=c.CI$c.lower))+geom_text(aes(label = round(c.CI$c.mean,2)),vjust = 1.5)
    k4<-ggplot(d.CI,aes(x=d.CI$d.levels,y=d.CI$d.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=d.CI$d.upper, ymin=d.CI$d.lower))+geom_text(aes(label = round(d.CI$d.mean,2)),vjust = 1.5)
    grid.arrange(k1, k2, k3, k4, ncol=2,nrow=2)
  }else{
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
    a.error<-qt(1-alpha/2,s)*sqrt(f/ro)

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
    b.error<-qt(1-alpha/2,s)*sqrt(f/ro)

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
    c.error<-qt(1-alpha/2,s)*sqrt(f/ro)

    for(i in c.levels){
      c.mean[i]<-round(mean(y[c==i]),2)
      Lc[i]<-round(c.mean[i]-c.error,2)
      Uc[i]<-round(c.mean[i]+c.error,2)
      c.lower[i]<-as.numeric(Lc[i])
      c.upper[i]<-as.numeric(Uc[i])
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
    k1<-ggplot(a.CI,aes(x=a.CI$a.levels,y=a.CI$a.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=a.CI$a.upper, ymin=a.CI$a.lower))+geom_text(aes(label = round(a.CI$a.mean,2)),vjust = 1.5)
    k2<-ggplot(b.CI,aes(x=b.CI$b.levels,y=b.CI$b.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=b.CI$b.upper, ymin=b.CI$b.lower))+geom_text(aes(label = round(b.CI$b.mean,2)),vjust = 1.5)
    k3<-ggplot(c.CI,aes(x=c.CI$c.levels,y=c.CI$c.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=c.CI$c.upper, ymin=c.CI$c.lower))+geom_text(aes(label = round(c.CI$c.mean,2)),vjust = 1.5)
    grid.arrange(k1, k2, k3, ncol=3,nrow=1)
  }

}
