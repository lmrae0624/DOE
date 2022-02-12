#' dispersion
#'
#' estimation of dispersion
#'
#' @param formula,data,ranfac,fc,sp,nest,fac
#'
#' @return result of dispersion
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

dispersion <- function(formula, data, ranfac, rc=TRUE, sp=FALSE,nest=TRUE,fac){

  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)

  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }

  n.ran<-length(ranfac)

  if(sp==FALSE){
    for(rrr in 1:n.ran){

      ranmod<-ranfac[rrr]

      anv<-suppressWarnings(anova(lm(formula)))
      VE<-anv["Residuals","Mean Sq"]

      fac.name<-rownames(anv)
      fac.name<-subset(fac.name, fac.name!='Residuals')
      ran.fac<-fac.name[grep(ranmod, fac.name)]

      fac.main<-data.frame(fac.name)
      dele<-fac.name[grep(":", fac.name)]
      for(i in dele) fac.main<-subset(fac.main, fac.main!=i)
      fac.main<-subset(fac.main, fac.main!='Residuals')


      v<-NULL
      if(rc==TRUE){ ###?ݺ??? ??��
        level<-NULL
        for(i in fac.main[,1]){
          level[i]<-anv[i,"Df"]+1
        }

        dft<-sum(anv[,"Df"])
        r<-(dft+1)/prod(level)

        for(i in ran.fac){

          inter<-i[grep(":", i)]

          v[[i]]<-anv[i,"Mean Sq"]

          if(length(inter)!=0){ ###??ȣ?ۿ?
            sp.inter<-strsplit(inter, split= ":")
            sp.inter<-levels(factor(sp.inter[[1]]))
            lv<-NULL

            for(k in fac.main[,1]){
              lv[k]<-level[k]
              for(j in sp.inter) if(k==j) lv[k]<-1
            }

            num<-prod(lv)*r
            disp<-(v[[i]]-VE)/num
          }

          else{ ###??ȿ??
            lv<-length(levels(data[[i]]))
            num<-length(which(data[[i]]==lv))

            disp<-(v[[i]]-VE)/num
          }

          cat("The dispersion of",i,": \n" )
          print(disp)
          cat("\n")
        }
      }

      else{ ###?ݺ??? ??��x
        for(i in ran.fac){
          inter<-i[grep(":", i)]

          if(length(inter)==0){###??ȿ??
            v[[i]]<-anv[i,"Mean Sq"]

            lv<-levels(data[[i]])

            num<-NULL
            disp<-NULL
            for(j in lv){
              num[j]<-length(which(data[[i]]==j))
              disp[j]<-(v[[i]]-VE)/num[j]
            }

            cat("The dispersion of",i,": \n" )
            print(disp)
            cat("\n")
          }
        }
      }
    }
  }else{
    if(nest==TRUE){
      formula.t<-as.character(formula)
      yo<-formula.t[3]
      yo<-strsplit(yo, split= "/")
      yo<-levels(factor(yo[[1]]))

      a<-as.factor(data[[1]])
      b<-as.factor(data[[2]])
      c<-as.factor(data[[3]])
      r<-as.factor(data[[4]])

      mo<-anova(lm(y~a/b/c))
      N<-NULL
      N[1]<-"A"
      N[2]<-"B(A)"
      N[3]<-"C(BA)"
      N[4]<-"E"

      rownames(mo)<-N
      print(mo)
      cat("\n")

      ad<-length(unique(a))
      bd<-length(unique(b))
      cd<-length(unique(c))
      rd<-length(unique(r))

      ea<-round((mo[1,"Mean Sq"]-mo[2,"Mean Sq"])/(bd*cd*rd),4)
      eba<-round((mo[2,"Mean Sq"]-mo[3,"Mean Sq"])/(cd*rd),4)
      ecba<-round((mo[3,"Mean Sq"]-mo[4,"Mean Sq"])/(rd),4)
      ee<-round(mo[4,"Mean Sq"],4)

      cat("The dispersions of each factor")
      cat("\n")
      cat("A : ", ea)
      cat("\n")
      cat("B(A) : ", eba)
      cat("\n")
      cat("C(AB) : ",ecba)
      cat("\n")
      cat("E : ",ee)
    }else{
      if(fac==1){
        r<-as.factor(data[[1]])
        a<-as.factor(data[[2]])
        b<-as.factor(data[[3]])
        rd<-length(unique(r))
        ad<-length(unique(a))
        bd<-length(unique(b))

        B<-suppressWarnings(anova(lm(data[[4]]~r*a*b)))
        W<-NULL
        W<-B[c(1,2,7,3,6,7),]
        for (j in 1:2){
          W[3,j]<-B[4,j]
          W[6,j]<-B[5,j]+B[7,j]
        }
        W[,"Mean Sq"]<-W[,"Sum Sq"]/W[,"Df"]
        W[1:2,"F value"]<-W[1:2,"Mean Sq"]/W[3,"Mean Sq"]
        W[4:5,"F value"]<-W[4:5,"Mean Sq"]/W[6,"Mean Sq"]
        W[3,"F value"]<-W[3,"Mean Sq"]/W[6,"Mean Sq"]

        # Pvalue
        W[1:2,"Pr(>F)"]<-1-pf(W[1:2,"F value"],W[1:2,"Df"],W[3,"Df"])
        W[4:5,"Pr(>F)"]<-1-pf(W[4:5,"F value"],W[4:5,"Df"],W[6,"Df"])
        N<-NULL
        N[1]<- "R"
        N[2]<- "A"
        N[3]<- "E1"
        N[4]<- "B"
        N[5]<- "A:B"
        N[6]<- "E2"
        rownames(W)<-N
        print(W)

        er<-(W["R","Mean Sq"]-W["E1","Mean Sq"])/(ad*bd)
        ee1<-(W["E1","Mean Sq"]-W["E2","Mean Sq"])/bd
        ee2<-W["E2","Mean Sq"]

        cat("\n")
        cat("The dispersions of each factor")
        cat("\n")
        cat("R : ", er)
        cat("\n")
        cat("E1 : ", ee1)
        cat("\n")
        cat("E2 : ",ee2)
      }else if(fac==2){

        r<-as.factor(data[[1]])
        a<-as.factor(data[[2]])
        b<-as.factor(data[[3]])
        c<-as.factor(data[[4]])
        rd<-length(unique(r))
        ad<-length(unique(a))
        bd<-length(unique(b))
        cd<-length(unique(c))

        B<-suppressWarnings(anova(lm(data[[5]]~r*a*b*c)))
        W<-NULL
        W<-B[c(1,2,3,7,16,4,9,10,14,16),]
        for (j in 1:2){
          W[5,j]<-B[5,j]+B[6,j]+B[11,j]
          W[10,j]<-B[8,j]+B[12,j]+B[13,j]+B[15,j]
        }
        W[,"Mean Sq"]<-W[,"Sum Sq"]/W[,"Df"]
        W[1:4,"F value"]<-W[1:4,"Mean Sq"]/W[5,"Mean Sq"]
        W[6:9,"F value"]<-W[6:9,"Mean Sq"]/W[10,"Mean Sq"]
        W[5,"F value"]<-W[5,"Mean Sq"]/W[10,"Mean Sq"]
        # Pvalue
        W[1:4,"Pr(>F)"]<-1-pf(W[1:4,"F value"],W[1:4,"Df"],W[5,"Df"])
        W[6:9,"Pr(>F)"]<-1-pf(W[6:9,"F value"],W[6:9,"Df"],W[10,"Df"])

        N<-NULL
        N[1]<- "R"
        N[2]<- "A"
        N[3]<- "B"
        N[4]<- "A:B"
        N[5]<- "E1"
        N[6]<- "C"
        N[7]<- "A:C"
        N[8]<- "B:c"
        N[9]<- "A:B:C"
        N[10]<- "E2"
        rownames(W)<-N
        print(W)

        er<-(W["R","Mean Sq"]-W["E1","Mean Sq"])/(ad*bd*cd)
        ee1<-(W["E1","Mean Sq"]-W["E2","Mean Sq"])/cd
        ee2<-W["E2","Mean Sq"]

        cat("\n")
        cat("The dispersions of each factor")
        cat("\n")
        cat("R : ", er)
        cat("\n")
        cat("E1 : ", ee1)
        cat("\n")
        cat("E2 : ",ee2)
      }else{

        r<-as.factor(data[[1]])
        a<-as.factor(data[[2]])
        b<-as.factor(data[[3]])
        c<-as.factor(data[[4]])
        rd<-length(unique(r))
        ad<-length(unique(a))
        bd<-length(unique(b))
        cd<-length(unique(c))

        B<-suppressWarnings(anova(lm(data[[5]]~r*a*b*c)))
        W<-NULL
        W<-B[c(1,2,16,3,7,16,4,9,10,14,16),]
        for (j in 1:2){
          W[3,j]<-B[5,j]
          W[6,j]<-B[6,j]+B[11,j]
          W[11,j]<-B[8,j]+B[12,j]+B[13,j]+B[15,j]
        }
        W[,"Mean Sq"]<-W[,"Sum Sq"]/W[,"Df"]
        W[1:2,"F value"]<-W[1:2,"Mean Sq"]/W[3,"Mean Sq"]
        W[4:5,"F value"]<-W[4:5,"Mean Sq"]/W[6,"Mean Sq"]
        W[7:10,"F value"]<-W[7:10,"Mean Sq"]/W[11,"Mean Sq"]
        # Pvalue
        W[1:2,"Pr(>F)"]<-1-pf(W[1:2,"F value"],W[1:2,"Df"],W[3,"Df"])
        W[4:5,"Pr(>F)"]<-1-pf(W[4:5,"F value"],W[4:5,"Df"],W[6,"Df"])
        W[7:10,"Pr(>F)"]<-1-pf(W[7:10,"F value"],W[7:10,"Df"],W[11,"Df"])
        #W[,5]<-round(W[,5],4)
        N<-NULL
        N[1]<- "R"
        N[2]<- "A"
        N[3]<- "E1"
        N[4]<- "B"
        N[5]<- "A:B"
        N[6]<- "E2"
        N[7]<- "C"
        N[8]<- "A:C"
        N[9]<- "B:C"
        N[10]<-"A:B:C"
        N[11]<- "E3"
        rownames(W)<-N
        print(W)

        er<-(W["R","Mean Sq"]-W["E1","Mean Sq"])/(ad*bd*cd)
        ee1<-(W["E1","Mean Sq"]-W["E2","Mean Sq"])/(bd*cd)
        ee2<-(W["E2","Mean Sq"]-W["E3","Mean Sq"])/cd
        ee3<-W["E3","Mean Sq"]

        cat("\n")
        cat("The dispersions of each factor")
        cat("\n")
        cat("R : ", er)
        cat("\n")
        cat("E1 : ", ee1)
        cat("\n")
        cat("E2 : ",ee2)
        cat("\n")
        cat("E3 : ",ee3)
      }

    }
  }
}
