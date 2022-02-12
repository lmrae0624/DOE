#' optcom
#'
#' Optimum level combination
#'
#' @param data,fac,ranfac,x,alpha
#'
#' @return optimum level combination, confidence level
#'
#' @examples
#' fac<-c("a", "b","c","a*c")
#' a<-as.factor(c(rep(1,9),rep(2,9),rep(3,9)))
#' a<-as.factor(c(rep(1,9),rep(2,9),rep(3,9)))
#' b<-as.factor(c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
#' c<-as.factor(rep(c(1,2,3),9))
#' y<-c(74,86,76,72,91,87,48,65,56,61,78,71,62,81,77,55,72,63,50,70,60,49,68,64,52,69,60)
#' z<-data.frame(a,b,c,y)
#'
#' @export
#'
#' @importFrom
#' stats as.formula
#' stats aggregate
#' stats na.omit
#' stats lm
#' stats anova

optcom <- function(data, fac, ranfac=NULL, x, alpha=0.05){
  form=as.formula(paste(x, paste(fac, collapse=" + "), sep=" ~ "))

  if(!is.null(ranfac)){
    ran<-fac[grep(ranfac, fac)]
    for(r in ran) fac<-subset(fac, fac!=r)
  }

  n.in<-sapply(as.character(fac), function(x, letter = "*"){
    sum(unlist(strsplit(x, split = "")) == letter)
  })

  fac.main<-factor(subset(fac, n.in==0))
  fac.inter<-factor(subset(fac, n.in!=0))

  ci.mean<-NULL

  if(sum(n.in)==0){

    fac.mean<-NULL
    fac.max<-NULL
    for(i in fac.main){
      lv<-levels(factor(data[[i]]))
      for(j in lv){
        dat<-data[data[i]==j,]
        fac.mean[j]<-mean(dat[[x]])
      }
      fac.max[i]<-which.max(fac.mean)
      ci.mean[i]<-max(fac.mean)  ##############################################
    }

    cat("Optimum level combination: \n")
    print(fac.max)

  }else{

    n.in<-sapply(as.character(fac.inter), function(x, letter = "*"){
      sum(unlist(strsplit(x, split = "")) == letter)
    })

    lv.in<-sort(levels(factor(n.in)),decreasing = T)

    nam<-as.data.frame(fac.main)

    main<-data.frame(matrix(nrow=0, ncol=length(fac.main)))
    colnames(main)<-fac.main

    main2<-NULL
    for(l in lv.in){
      max.in<-names(subset(n.in, n.in==l))

      for(m in max.in) fac.inter<-subset(fac.inter, fac.inter!=m)

      max.in<-gsub("\\*", '', max.in)
      sp.in<-strsplit(max.in, split= "")
      inter.n<-length(sp.in)


      for(i in 1:inter.n){
        opt.data<-data.frame()
        for(j in 1:length(fac.main)) opt.data[i,j]<-NA
      }
      rownames(opt.data)<-max.in
      colnames(opt.data)<-fac.main  #Ʋ


      for(i in 1:inter.n){
        fac.n<-factor(sp.in[[i]])
        ci.fac<-paste(sp.in[[i]], collapse="*")

        inter.form<-as.formula(paste(x, paste(sp.in[[i]], collapse=" + "), sep=" ~ "))
        inter.mean<-aggregate(inter.form, data=data, mean)

        ci.mean[ci.fac]<-max(inter.mean[[x]]) ##############################################

        inter.max<-NULL
        for(j in fac.n){
          inter.max[j]<-inter.mean[which.max(inter.mean[[x]]),j]

          for(k in 1:ncol(opt.data))
            if(j==names(opt.data[k])) opt.data[max.in[i],k]<-inter.max[j]
        }
      }

      for(f in fac.main) main2[f]<-unique(na.omit(opt.data[f]))
      main2<-t(as.data.frame(unlist(main2)))
      main<-merge(main,main2,all=TRUE)
      main2<-NULL

      sp.in<-unlist(sp.in)
      for(a in nam$fac.main)
        for(b in sp.in)
          if(a==b) nam[which(nam$fac.main==a),] <- NA
      nam<-na.omit(nam)

      fac.inter2<-NULL
      for(n in nam$fac.main) {
        fac.inter2<-c(fac.inter2, grep(n, fac.inter))
      }
      fac.inter<-unique(fac.inter[fac.inter2])
    } #############################

    main<-main[,order(names(main))]
    for(f in fac.main) main[f]<-unique(na.omit(main[f]))
    main<-unique(main)

    if(nrow(main)>1){
      for(f in fac.main)
        if(main[1,f]!=main[2,f]){
          f.form<-as.formula(paste(x, f, sep=" ~ "))
          f.mean<-aggregate(f.form, data=data, mean)

          ci.mean[f]<-max(f.mean[[x]]) ##############################################
          f.max<-which.max(f.mean[[x]])
          main[,f]<-f.max
        }
      main<-unique(main)
    }


    if(nrow(nam)==0){
      cat("Optimum level combination: \n")
      print(main)

    }else{
      for(n in nam$fac.main){
        nam.form<-as.formula(paste(x, paste(n, collapse=" + "), sep=" ~ "))
        nam.mean<-aggregate(nam.form, data=data, mean)

        ci.mean[n]<-max(nam.mean[[x]]) ##############################################

        nam.max<-which.max(nam.mean[[x]])
        names(nam.max)<-n
        main[names(nam.max)]<-nam.max
      }
      cat("Optimum level combination: \n")
      print(main)
    }
  }

  if(is.null(ranfac)){
    fac.name<-rownames(anova(lm(form,data=data)))
    anvfac<-subset(fac.name, fac.name!=fac.name[grep("Residuals", fac.name)])

    E<-anova(lm(form,data=data))["Residuals","Df"]
    VE<-anova(lm(form,data=data))["Residuals","Mean Sq"]

    total<-nrow(data)

    f.df<-NULL
    for(af in anvfac) f.df[af]<-anova(lm(form,data=data))[af,"Df"]
    ne<-total/sum(f.df)+1

    se<-qt(1-alpha/2,E)*sqrt(VE/ne)

    chl.inter<-paste(fac.main, collapse="*")
    chl<-subset(fac, fac==chl.inter)

    if(length(chl)!=0){
      chl.mean<-max(inter.mean[[x]])
    }else{
      t.mean<-mean(data[[x]])
      chl.mean<-sum(ci.mean)-(length(ci.mean)-1)*t.mean
    }

    lw<-chl.mean-se
    up<-chl.mean+se
    result<-data.frame(lw, chl.mean, up)
    names(result)[2]<-c("mean")


    cat("\nConfidence Interval of",chl.inter,"\n")
    print(result)
  }

}
