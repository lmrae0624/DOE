#' qual.aov
#'
#' total qualitative data
#'
#' @param formula,data,r,i,j,fac
#'
#' @return table, anova
#'
#' @examples
#' A<-c(1,1,2,2,3,3,4,4)
#' B<-rep(c("plus","minus"),4)
#' Y<-c(190.00,10.00,178.00,22.00,194.00,6.00,170.00,30.00)
#' k<-data.frame(A,B,Y)
#'
#' @export
#'
#' @importFrom
#' stats model.frame
#' stats anova
#' stats lm

qual.aov<-function(formula,data,r,i,j,fac){
  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)
  Y.name<-formula.t[2]

  data.n<-strsplit(formula.t[3]," \\* ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }
  if(fac==1){
    ay<-as.numeric(data[[3]])
    v<-matrix(ay,i,j)
    print(v)

    A<-as.numeric(data[[1]])
    B<-as.numeric(data[[2]])

    ct<-(sum(v[2,])^2/((length(unique(A))*(r))))
    st<-sum(v[2,])-ct
    sa<-sum(v[2,]^2)/r-ct
    se<-st-sa

    m<-anova(lm(formula))
    e<-NULL
    e<-m[c(1,2,3),]
    e[1,2]<-round(sa,2)
    e[1,3]<-e[1,2]/e[1,1]
    e[2,1]<-r*length(unique(A))-e[1,1]-1
    e[2,2]<-se
    e[2,3]<-e[2,2]/e[2,1]
    e[1,4]<-e[1,3]/e[2,3]
    e[2,4]<-""
    e[2,5]<-""
    e[3,1]<-r*length(unique(A))-1
    e[3,2]<-st
    e[3,3]<-""
    f<-as.numeric(e[1,4])
    g<-as.numeric(e[1,1])
    h<-as.numeric(e[2,1])
    e[1,5]<-1-pf(f,g,h)

    n<-NULL
    n[1]<- "A"
    n[2]<- "E"
    n[3]<- "T"
    rownames(e)<-n

    print(e)
  }else{
    ay<-as.numeric(data[[4]])
    v<-matrix(ay,i,j)
    print(v)

    A<-as.numeric(data[[1]])
    B<-as.numeric(data[[2]])

    ct<-sum(c(v[,2],v[,4],v[,6],v[,8]))^2/(length(unique(A))*length(unique(B))*r)
    st<-sum(c(v[,2],v[,4],v[,6],v[,8]))-ct
    sa<-sum(c(sum(v[,2]),sum(v[,4]),sum(v[,6]),sum(v[,8]))^2)/(r*length(unique(B)))-ct
    sb<-sum(sum(c(v[1,2],v[1,4],v[1,6],v[1,8]))^2,sum(c(v[2,2],v[2,4],v[2,6],v[2,8]))^2)/(r*length(unique(A)))-ct
    sab<-sum(v[1,2]^2,v[1,4]^2,v[1,6]^2,v[1,8]^2,v[2,2]^2,v[2,4]^2,v[2,6]^2,v[2,8]^2)/r-ct
    se1<-sab-sa-sb
    se2<-st-sab
    se22<-st-sa-sb

    m<-anova(lm(formula))
    e<-NULL
    e<-m[c(1,2,3,4),]
    e[1,2]<-round(sa,3)
    e[1,3]<-e[1,2]/e[1,1]
    e[2,2]<-round(sb,3)
    e[2,3]<-e[2,2]/e[2,1]
    e[3,1]<-(length(unique(A))-1)*(length(unique(B))-1)
    e[3,2]<-se1
    e[3,3]<-e[3,2]/e[3,1]
    e[1,4]<-e[1,3]/e[3,3]
    e[2,4]<-e[2,3]/e[3,3]
    e[4,1]<-(r*length(unique(A))*length(unique(B))-1-e[1,1]-e[2,1]-e[3,1])
    e[4,2]<-se2
    e[4,3]<-e[4,2]/e[4,1]
    e[3,4]<-e[3,3]/e[4,3]

    f<-as.numeric(e[1,4])
    l<-as.numeric(e[2,4])
    p<-as.numeric(e[3,4])
    g<-as.numeric(e[1,1])
    j<-as.numeric(e[2,1])
    h<-as.numeric(e[3,1])
    o<-as.numeric(e[4,1])

    e[1,5]<-1-pf(f,g,h)
    e[2,5]<-1-pf(l,j,h)
    e[3,5]<-1-pf(p,h,o)
    n<-NULL
    n[1]<- "A"
    n[2]<- "B"
    n[3]<- "E1(A*B)"
    n[4]<- "E2"
    rownames(e)<-n

    if(e[3,5]<0.1){
      print(e)
      cat("\n")
    }else{
      za<-anova(lm(ay~A+B))
      az<-NULL
      az<-za[c(1,2,3),]
      az[1,1]<-length(unique(A))-1
      az[1,2]<-round(sa,3)
      az[1,3]<-az[1,2]/az[1,1]
      az[2,2]<-round(sb,3)
      az[2,3]<-az[2,2]/az[2,1]
      az[3,1]<-(r*length(unique(A))*length(unique(B))-1-e[1,1]-e[2,1])
      az[3,2]<-se22
      az[3,3]<-az[3,2]/az[3,1]
      az[1,4]<-az[1,3]/az[3,3]
      az[2,4]<-az[2,3]/az[3,3]

      f1<-as.numeric(az[1,4])
      l1<-as.numeric(az[2,4])
      g1<-as.numeric(az[1,1])
      j1<-as.numeric(az[2,1])
      h1<-as.numeric(az[3,1])
      az[1,5]<-1-pf(f1,g1,h1)
      az[2,5]<-1-pf(l1,j1,h1)
      print(az)
    }
  }

}
