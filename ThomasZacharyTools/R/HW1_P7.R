#' Hw1 Problem 7
#' 
#' @param x data.frame
#' @param func function
#' @param interval object
#' 
#' @return scalar
#' @export
#' @examples 
#' x1 <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
#' x2 <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p7c.txt"))
#' x3 <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p7b.txt"))
#' func1 <- function(theta,x) dgamma(x, shape = theta, log = TRUE)
#' func2 <- function(theta,x) dcauchy(x, location = theta, log = TRUE)
#' func3 <- function(theta,x) dbinom(x, 20, prob = 1/(1+exp(-theta)), log = TRUE)
#' result_gamma <- func4(x1, func1, c(0,3))
#' result_cauchy <- func4(x2, func2, c(0,100))
#' result_dbinom <- func4(x3, func3, c(-10,10))  
#'
func4 <- function(x, func, interval)
{
  f4<-function(theta,x) 
  {sum(func(theta,x))}
  
  oout <- optimize(f4, maximum = TRUE, interval, x=x)
  return(oout$maximum)
}
         
   