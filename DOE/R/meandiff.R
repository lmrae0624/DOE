#' mean.diff
#'
#' population mean difference
#'
#' @param data,formula,method,ranfac
#'
#' @return diff,lwr.ci,upr,ci,p-value,ci plot
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
#' DescTools PostHocTest
#' stats aov
#' stats lm
#' stats anova
#' graphics plot

mean.diff<-function(data, formula, method="lsd", ranfac=NULL){

  anv<-suppressWarnings(aov(formula, data=data))
  sanv<-anova(lm(formula))

  fac.name<-rownames(sanv)
  fac.main<-subset(fac.name, fac.name!='Residuals')
  fac.inter<-fac.main[grep(":", fac.main)]
  for(i in fac.inter) fac.main<-subset(fac.main, fac.main!=i)

  if(!is.null(ranfac)) fac.main<-subset(fac.main, fac.main!=ranfac)

  if(method=="hsd") pht<-PostHocTest(anv, which=fac.main, method="hsd")
  else if(method=="bonferroni") pht<-PostHocTest(anv, which=fac.main, method="bonferroni")
  else if(method=="scheffe") pht<-PostHocTest(anv, which=fac.main, method="scheffe")
  else if(method=="duncan") pht<-PostHocTest(anv, which=fac.main, method="duncan")
  else if(method=="newmankeuls") pht<-PostHocTest(anv, which=fac.main, method="newmankeuls")
  else pht<-PostHocTest(anv, which=fac.main, method="lsd")

  par(mfrow=c(round(sqrt(length(fac.main)),digits = 0),ceiling(sqrt(length(fac.main)))))
  print(pht)
  plot(pht, col="blue")
}
