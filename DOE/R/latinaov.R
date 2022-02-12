#' lain.aov
#'
#' anova of latin and graeco
#'
#' @param formula,data,alpha,latin,ro,co
#'
#' @return anova
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

latin.aov<-function(formula,data,alpha,latin=TRUE,ro,co){
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
    print(j)
  }else{
    j<-suppressWarnings(anova(lm(formula)))
    print(j)

  }

}
