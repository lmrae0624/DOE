#' main.effect
#'
#' main effect plot
#'
#' @param data,fac,y
#'
#' @return main effect plots
#'
#' @examples
#' y <- c(8.44,8.36,8.28,8.59,8.91,8.6,9.34,9.41,9.69,8.92,8.92,8.74)
#' n <- rep(3, 4)
#' level <- as.factor(rep(1:4, n))
#' z<-data.frame(level,y)
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
#' ggplot2 ggtitle
#' ggplot2 theme
#' ggplot2 element_text

main.effect<-function(data, fac, y){

  lv<-as.numeric(levels(factor(fac)))
  data.mean<-NULL

  for(i in lv){
    data.mean[i]<-mean(y[fac==i])
  }

  m.data<-data.frame(lv, data.mean)
  names(m.data)[1]<-c(paste(deparse(substitute(fac))))

  ggplot(m.data, aes(x=as.numeric(unlist(m.data[1])), y=data.mean)) +
    geom_point(colour="skyblue3") + geom_line(colour="skyblue3",size=1) +
    xlab(names(m.data[1])) + ylab("Mean")+
    ggtitle("Main Effect Plot")+
    theme(plot.title = element_text(hjust = 0.5))
}
