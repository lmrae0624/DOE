#' mean.est
#'
#' population mean of main effect
#'
#' @param formula,data,factor,ranfac,alpha
#'
#' @return population mean, graph
#'
#' @examples
#' a<-c(1,1,2,2,3,3)
#' a<-as.factor(c(rep(a,6)))
#' b<-as.factor(c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)))
#' y<-c(305,302,322,325,320,322,335,337,350,348,342,344,366,364,326,324,338,336,372,374,330,330,348,348,376,373,327,330,350,350,348,350,310,308,330,328)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' stats model.frame
#' stats lm
#' stats anova
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_errorbar
#' ggplot2 geom_point
#' ggplot2 geom_text
#' ggplot2 xlab
#' ggplot2 ylab

mean.est <- function(formula,data,factor,ranfac=NULL,alpha=0.05){

  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(stats::model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)
  y.name<-formula.t[2]

  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }

  aov.t<-function(formula, ranfac=NULL){

    if(is.null(ranfac)){
      anv<-summary(aov(formula))
      anv<-do.call(rbind.data.frame, anv)
    }else{
      anv<-anova(lm(formula))
      fac.name<-rownames(anv)

      fac.main<-subset(fac.name, fac.name!='Residuals')
      dele<-fac.main[grep(":", fac.main)]
      for(d in dele) fac.main<-subset(fac.main, fac.main!=d)
      fac.main<-subset(fac.main, fac.main!=ranfac)


      for(f in fac.main){
        raninter<-paste(f,":",ranfac, sep='')
        raninter2<-paste(ranfac,":",f, sep='')

        raninter<-subset(fac.name, fac.name==raninter)
        raninter2<-subset(fac.name, fac.name==raninter2)

        if(length(raninter)==0) raninter<-raninter2

        if(length(raninter)==0){
          anv[f,"F value"]<-anv[f,"Mean Sq"]/anv['Residuals',"Mean Sq"]
          anv[f,"Pr(>F)"]<-1-pf(anv[f,"F value"], anv[f,"Df"], anv['Residuals',"Df"])
        }else{
          anv[f,"F value"]<-anv[f,"Mean Sq"]/anv[raninter,"Mean Sq"]
          anv[f,"Pr(>F)"]<-1-pf(anv[f,"F value"], anv[f,"Df"], anv[raninter,"Df"])
        }
      }
    }
    return(anv)
  }
  anv<-aov.t(formula=formula, ranfac=ranfac)
  fac.name<-rownames(anv)

  E<-anv["Residuals","Df"]
  VE<-anv["Residuals","Mean Sq"]

  if(is.null(ranfac)){ #????????
    lv<-as.numeric(levels(as.factor(data[[factor]])))

    num<-NULL
    ci<-NULL
    lw<-NULL
    up<-NULL

    f.form<-as.formula(paste(y.name, paste(factor, collapse=" + "), sep=" ~ "))
    dat.mean<-aggregate(f.form, data=data, mean)

    for(i in lv){
      num[i]<-length(data[data[factor]==i,y.name])

      ci[i]<-qt(1-alpha/2,E)*sqrt(VE/(num[i]))

      lw[i]<-as.numeric(dat.mean[dat.mean[factor]==i,y.name]-ci[i])
      up[i]<-as.numeric(dat.mean[dat.mean[factor]==i,y.name]+ci[i])
    }

  }else{ #ȥ?ո???
    num<-nrow(data)

    f.df<-anv[factor,"Df"]
    r.df<-anv[ranfac,"Df"]
    Vr<-anv[ranfac,"Mean Sq"]

    fr<-paste(factor,":",ranfac, sep='')
    fr2<-paste(ranfac,":",factor, sep='')
    fr<-subset(fac.name, fac.name==fr)
    fr2<-subset(fac.name, fac.name==fr2)

    if(length(fr)==0) fr<-fr2

    f.form<-as.formula(paste(y.name, paste(factor, collapse=" + "), sep=" ~ "))
    dat.mean<-aggregate(f.form, data=data, mean)
    lv<-as.numeric(levels(as.factor(data[[factor]])))

    lw<-NULL
    up<-NULL

    for(l in lv){
      if(length(fr)==0){
        satt<-((Vr+(f.df)*VE)^2)/((Vr^2/r.df)+(((f.df)*VE)^2/E))

        se<-qt(1-alpha/2,satt)*sqrt((Vr+f.df*VE)/num)
        lw[l]<-as.numeric(dat.mean[dat.mean[factor]==l,y.name]-se)
        up[l]<-as.numeric(dat.mean[dat.mean[factor]==l,y.name]+se)
      }else{
        Vfr<-anv[fr,"Mean Sq"]
        fr.df<-anv[fr,"Df"]

        satt<-((Vr+(f.df+1)*Vfr-VE)^2)/((Vr^2/r.df) + ((f.df*Vfr)^2/fr.df) + ((-VE)^2/E))

        se<-qt(1-alpha/2,satt)*sqrt((Vr+(f.df+1)*Vfr-VE)/num)
        lw[l]<-as.numeric(dat.mean[dat.mean[factor]==l,y.name]-se)
        up[l]<-as.numeric(dat.mean[dat.mean[factor]==l,y.name]+se)
      }
    }
  }

  dat.mean<-dat.mean[[y.name]]
  result<-data.frame(lv, lw, dat.mean, up)

  names(result)[1]<-c(factor)
  names(result)[3]<-c("mean")

  print(result)

  ggplot(result,aes(x=lv, y=dat.mean))+
    geom_errorbar(aes(ymax=result$up, ymin=result$lw),colour="dodgerblue3",size=1) + geom_point(size=3,colour="dodgerblue4")+
    geom_text(aes(label = round(dat.mean,2)),vjust = 1.5) + xlab(names(result[1])) + ylab("Mean")
}
