#' error.var
#'
#' error variance estimation of one way anova
#'
#' @param formula,data,alpha
#'
#' @return error variance
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
#' stats anova
#' stats lm

error.var<-function(formula,data,alpha){
  lower<-NULL
  upper<-NULL

  b<-anova(lm(formula))
  upper<-round(b["Residuals","Sum Sq"]/qchisq(p=alpha/2, df=b["Residuals","Df"]),4)
  lower<-round(b["Residuals","Sum Sq"]/qchisq(p=1-alpha/2, df=b["Residuals","Df"], lower.tail = TRUE),4)

  print(paste("(",lower," , ",upper,")"))
}
