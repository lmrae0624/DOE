#' disperaov
#'
#' aov of dispersion
#'
#' @param formula,data,nest,fac
#'
#' @return aov of dispersion
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

disperaov <- function(formula, data, nest=TRUE,fac){
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
    }
  }
}
