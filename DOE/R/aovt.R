#' aov.t
#'
#' analysis of variance
#'
#' @param formula,ranfac
#'
#' @return aov(df,sumsq,meansq)
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
#' b<-as.factor(c(rep(c(1,2,3),4)))
#' y<-c(97.6,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' stats formula
#' stats aov
#' stats anova
#' stats lm
#'
aov.t<-function(formula, ranfac=NULL){

  if(is.null(ranfac)){
    anv<-summary(aov(formula))
    anv<-do.call(rbind.data.frame, anv)
    print(anv)
  }else{
    anv<-anova(lm(formula))
    fac.name<-rownames(anv)

    fac<-subset(fac.name, fac.name!='Residuals')
    ranin<-fac[grep(ranfac, fac)]
    for(r in ranin) fac<-subset(fac, fac!=r)


    fac.inter<-fac[grep(":", fac)]
    for(i in fac.inter) fac.main<-subset(fac, fac!=i)


    for(m in fac.main){
      raninter<-paste(m,":",ranfac, sep='')
      raninter2<-paste(ranfac,":",m, sep='')

      raninter<-subset(fac.name, fac.name==raninter)
      raninter2<-subset(fac.name, fac.name==raninter2)

      if(length(raninter)==0) raninter<-raninter2

      if(length(raninter)!=0){
        anv[m,"F value"]<-anv[m,"Mean Sq"]/anv[raninter,"Mean Sq"]
        anv[m,"Pr(>F)"]<-1-pf(anv[m,"F value"], anv[m,"Df"], anv[raninter,"Df"])
      }
    }
    for(i in fac.inter){
      inter.main<-strsplit(i, split= ":")

      for(im in inter.main[[1]]) fac.main<-subset(fac.main, fac.main!=im) #???þ??? ????

      if(length(fac.main)!=0)
        for(f in fac.main) {
          dele<-ranin[grep(f, ranin)]
          for(d in dele) ranin<-subset(ranin, ranin!=d)
        }

      for(im in inter.main[[1]]) ranin<-ranin[grep(im, ranin)]
      raninter<-subset(fac.name, fac.name==ranin)

      if(length(raninter)!=0){
        anv[i,"F value"]<-anv[i,"Mean Sq"]/anv[raninter,"Mean Sq"]
        anv[i,"Pr(>F)"]<-1-pf(anv[i,"F value"], anv[f,"Df"], anv[raninter,"Df"])
      }
    }
    print(anv)
  }
}
