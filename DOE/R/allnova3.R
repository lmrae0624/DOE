#' allnova3
#'
#' three way anova (no repetition / total model)
#'
#' @param formula,data,alpha
#'
#' @return ci, plot, anova
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
#' stats lm
#' stats qt
#' ggplot2 facet_grid
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 geom_errorbar
#' ggplot2 ymax
#' ggplot2 ymin
#' ggplot2 labs
#' dplyr select
#'
allnova3<-function(formula,data,alpha){
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
  l<-j[1,1]+1
  m<-j[2,1]+1
  n<-j[3,1]+1
  E<-j[7,1]
  VE<-j[7,3]


  t<-mean(y)
  ne<-(l*m*n)/(l*m + l*n + m*n -l-m-n+1)

  a.levels<-levels(factor(a))
  b.levels<-levels(factor(b))
  c.levels<-levels(factor(c))

  abc.error<-qt(1-alpha/2,E)*sqrt(VE/ne)
  abc.mean<-NULL
  L.abc<-NULL
  U.abc<-NULL
  abc.lower<-NULL
  abc.upper<-NULL
  abc.data<-NULL

  for(A in a.levels){
    for(B in b.levels){
      for(C in c.levels){
        xij<-summarise(select(filter(z,c((z$a==A) & (z$b==B))),a,b,y),mean_y=mean(y))
        xik<-summarise(select(filter(z,c((z$a==A) & (z$c==C))),a,c,y),mean_y=mean(y))
        xjk<-summarise(select(filter(z,c((z$b==B) & (z$c==C))),b,c,y),mean_y=mean(y))

        abc.mean<-round(xij$mean_y + xik$mean_y + xjk$mean_y -mean(y[a==A])-mean(y[b==B])-mean(y[c==C])+t, 2)

        L.abc<-round(abc.mean-abc.error,2)
        U.abc<-round(abc.mean+abc.error,2)
        abc.lower<-as.numeric(L.abc)
        abc.upper<-as.numeric(U.abc)
        abc.data<- rbind(abc.data,data.frame(A,B,C,abc.lower,abc.mean,abc.upper))
      }
    }
  }

  print(abc.data)

  ggplot(abc.data, aes(x =interaction(abc.data$B, abc.data$C,lex.order =TRUE), y=abc.data$abc.mean)) +
    geom_point(size=1.5)+geom_errorbar(aes(ymax=abc.data$abc.upper, ymin=abc.data$abc.lower))+
    labs(y="AxBxC CI")+labs(x = " ")+
    facet_grid(.~ abc.data$A, switch = 'x')
}
