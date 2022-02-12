#' ksanova
#'
#' two way anova (no repetition / main effect and some interaction)
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
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 geom_errorbar
#' ggplot2 labs
#' ggplot2 facet_grid
#' ggplot2 xlab
#' ggplot2 ylab
#' gridExtra grid.arrange
#' gplots plotmeans

ksanova<-function(formula,data,alpha){
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
  E<-j["Residuals","Df"]
  VE<-j["Residuals","Mean Sq"]
  l<-j["a","Df"]+1
  m<-j["b","Df"]+1
  n<-j["c","Df"]+1


  a.levels<-levels(factor(a))
  b.levels<-levels(factor(b))
  c.levels<-levels(factor(c))


  t<-mean(y)

  c.mean<-NULL
  Lc<-NULL
  Uc<-NULL
  c.lower<-NULL
  c.upper<-NULL
  c.error<-qt(1-alpha/2,E)*sqrt(VE/(m*l))

  for(i in c.levels){
    c.mean[i]<-round(mean(y[c==i]),2)
    Lc[i]<-round(c.mean[i]-c.error,2)
    Uc[i]<-round(c.mean[i]+c.error,2)
    c.lower[i]<-as.numeric(Lc[i])
    c.upper[i]<-as.numeric(Uc[i])
  }

  ab.ne<- (l*m*n)/(l*m+n-1)
  ab.error<-qt(1-alpha/2,E)*sqrt(VE/ab.ne)
  ab.mean<-NULL
  L.ab<-NULL
  U.ab<-NULL
  ab.lower<-NULL
  ab.upper<-NULL
  ab.data<-NULL

  for(A in a.levels){
    for(B in b.levels){
      for(C in c.levels){
        ab<-subset(data, a%in%c(A) & b%in%c(B))

        ab.mean<-round(mean(ab$y)+mean(y[c==C])-t, 2)
        L.ab<-round(ab.mean-ab.error,2)
        U.ab<-round(ab.mean+ab.error,2)
        ab.lower<-as.numeric(L.ab)
        ab.upper<-as.numeric(U.ab)
        ab.data<- rbind(ab.data,data.frame(A,B,C,ab.lower,ab.mean,ab.upper))
      }
    }
  }
  c.CI<-data.frame(c.levels,c.lower,c.mean,c.upper)
  print(c.CI)
  cat("\n")
  print(ab.data)
  as<-ggplot(ab.data, aes(x =interaction(ab.data$B, ab.data$C,lex.order =TRUE), y=ab.data$ab.mean)) +
    geom_point(size=1.5) + geom_errorbar(aes(ymax=ab.data$ab.upper, ymin=ab.data$ab.lower)) +
    labs(y="AxB CI")+labs(x = " ")+
    facet_grid(.~ ab.data$A ,switch = 'x')
  grid.arrange(as,ncol=1,nrow=2)
  par(mfrow=c(2,2))
  interaction.plot(x.factor = data$a, xlab="A", ylab="Y",trace.factor = data$b, trace.label="B",
                   response = data$y,fun = mean,col=c("red","green","blue"), pch = 1:3,
                   fixed = TRUE,lty = 1,lwd = 1)
  plotmeans(y~c,bars=FALSE, xlab="C", pch = 1:3)

  as<-ggplot(ab.data, aes(x =interaction(ab.data$B, ab.data$C,lex.order =TRUE), y=ab.data$ab.mean)) +
    geom_point(size=1.5) + geom_errorbar(aes(ymax=ab.data$ab.upper, ymin=ab.data$ab.lower)) +
    labs(y="AxB CI")+labs(x = " ")+
    facet_grid(.~ ab.data$A ,switch = 'x')
  grid.arrange(as,ncol=1,nrow=2)

  plotmeans(y~c,bars=FALSE, xlab="C", pch = 1:3)
  interaction.plot(x.factor = data$a, xlab="A", ylab="Y",trace.factor = data$b, trace.label="B",
                   response = data$y,fun = mean,col=c("red","green","blue"), pch = 1:3,
                   fixed = TRUE,lty = 1,lwd = 1)

}
