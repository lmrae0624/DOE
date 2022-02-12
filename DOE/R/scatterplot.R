#' scatter.plot
#'
#' scatter plot
#'
#' @param data,fac1,fac2,y
#'
#' @return scatter plots
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
#' b<-as.factor(rep(c(1,2,3),4))
#' y<-c(97.6,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 xlab
#' ggplot2 ylab
#' ggplot2 labs
#' ggplot2 ggtitle
#' ggplot2 element_text
#'
scatter.plot<-function(data, fac1, fac2=NULL, y){


  if(is.null(fac2)){
    dat<-data.frame(fac1,y)
    names(dat)[1]<-c(paste(deparse(substitute(fac1))))

    ggplot(dat, aes(x=as.numeric(unlist(dat[1])), y=y)) +
      geom_point(aes(color=as.character(unlist(dat[1]))), size=2) +
      xlab(names(dat[1])) + ylab("Y") +
      labs(colour=names(dat[1]))+
      ggtitle("Scatter plot")+
      theme(plot.title = element_text(hjust = 0.5))

  }else{
    dat<-data.frame(fac1,fac2,y)
    names(dat)[1]<-c(paste(deparse(substitute(fac1))))
    names(dat)[2]<-c(paste(deparse(substitute(fac2))))

    ggplot(dat, aes(x=as.numeric(unlist(dat[1])), y=y)) +
      geom_point(aes(color=as.character(unlist(dat[2]))), size=2) +
      xlab(names(dat[1])) + ylab("Y") +
      labs(colour=names(dat[2]))+
      ggtitle("Scatter plot")+
      theme(plot.title = element_text(hjust = 0.5))
  }
}
