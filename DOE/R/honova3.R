#' honova3
#'
#' two way anova (CI plot / no repetition / mixed model)
#'
#' @param formula,data,alpha,k
#'
#' @return ci plot
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3)))
#' b<-as.factor(rep(c(1,2,3),3))
#' y<-c(13.1,12.9,13.4,12.4,12.7,12.5,12.3,12,12.2)
#' z<-data.frame(a,b,y)
#' j<-suppressWarnings(anova(lm(y~a+b)))
#'
#' @export
#'
#' @importFrom
#' ggplot2 ggplot
#'
honova3<-function(formula,data,alpha,k){
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
  q<-j[2,3]
  s<-j[3,1]
  f<-j[3,3]
  l<-j[1,1]+1
  m<-j[2,1]+1
  df<-round(((q+(l-1)*f)^2)/((q^2/(m-1))+((l-1)*f)^2/s),2)
  dd<-df-floor(df)
  tt<-qt(1-0.05/2,floor(df))-dd*(qt(1-0.05/2,floor(df))-qt(1-0.05/2,df))
  ac<-sqrt((q+(l-1)*f)/(l*m))
  a.error<-tt*ac

  a.levels<-levels(factor(a))
  a.mean <- NULL
  La<-NULL
  Ua<-NULL
  a.lower<-NULL
  a.upper<-NULL
  a.error<-qt(1-alpha/2,s)*sqrt(f/m)

  for(i in a.levels){
    a.mean[i]<-round(mean(y[a==i]),2)
    La[i]<-round(a.mean[i]-a.error,2)
    Ua[i]<-round(a.mean[i]+a.error,2)
    a.lower[i]<-as.numeric(La[i])
    a.upper[i]<-as.numeric(Ua[i])
  }

  a.CI<-data.frame(a.levels,a.lower,a.mean,a.upper)
  print(a.CI)

  ggplot(a.CI,aes(x=a.CI$a.levels,y=a.CI$a.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=a.CI$a.upper, ymin=a.CI$a.lower))+geom_text(aes(label = round(a.CI$a.mean,2)),vjust = 1.5)

}
