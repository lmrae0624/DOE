#' interaction.effect
#'
#' interaction plot
#'
#' @param data,fac1,fac2,y
#'
#' @return interaction plot
#'
#' @examples
#' A<-c(1,1,2,2,3,3)
#' A<-as.factor(c(rep(A,6)))
#' B<-as.factor(c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)))
#' y<-c(305,302,322,325,320,322,335,337,350,348,342,344,366,364,326,324,338,336,372,374,330,330,348,348,376,373,327,330,350,350,348,350,310,308,330,328)
#' z<-data.frame(A,B,y)
#'
#' @export
#'
#' @importFrom
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 geom_line
#' ggplot2 xlab
#' ggplot2 ylab
#' ggplot2 labs
#' ggplot2 ggtitle
#' ggplot2 theme
#' ggplot2 element_text
#'

interaction.effect<-function(data, fac1, fac2, y){

  fac1.lv<-levels(factor(fac1))
  fac2.lv<-levels(factor(fac2))

  intermean<-NULL
  inter.data<-NULL

  for(i in fac1.lv){
    for(j in fac2.lv){
      intermean<-as.numeric(mean(y[fac1==i & fac2==j]))

      inter.data<-rbind(inter.data, data.frame(i, j, intermean))
    }
  }
  names(inter.data)[1]<-c(paste(deparse(substitute(fac1))))
  names(inter.data)[2]<-c(paste(deparse(substitute(fac2))))

  ggplot(inter.data, aes(x=as.numeric(unlist(inter.data[1])), y=intermean,
                         group=as.character(unlist(inter.data[2])),
                         colour=as.character(unlist(inter.data[2])),
                         shape=as.character(unlist(inter.data[2])))) +
    geom_point(size=2) + geom_line(size=0.7) + xlab(names(inter.data[1])) + ylab("Mean") +
    labs(colour=names(inter.data[2]), shape=names(inter.data[2]))+
    ggtitle("Interaction Plot")+
    theme(plot.title = element_text(hjust = 0.5))
}
