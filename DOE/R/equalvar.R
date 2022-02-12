#'
#' equalvar
#' equal variance
#'
#' @param formula
#'
#' @return bartlett's k-squared, df, p-value
#'
#' @examples
#' y <- c(8.44,8.36,8.28,8.59,8.91,8.6,9.34,9.41,9.69,8.92,8.92,8.74)
#' n <- rep(3, 4)
#' level <- as.factor(rep(1:4, n))
#' z<-data.frame(level,y)
#'
#' @export
equal.var<-function(formula){
  bartlett.test(formula)
}
