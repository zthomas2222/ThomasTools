#' Calculate Mean, Variane, SD
#'
#' Computes the mean, variance and sd of a vector
#' 
#' Quiz 1 Problem 1
#' 
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func5(rnorm(10))
func5 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}


#' Calculate Mean, Variane, SD (again)
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#' 
#' Quiz 1 Problem 2
#' 
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func6(rnorm(10))
func6 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}


#' MLE of gamma distribution
#'
#' Computes the liklihood of a gamma distribution
#'
#' Quiz 1 Problem 3
#' 
#' @param x vector
#'
#' @return scalar
#' @export
#' @examples
#' func7(rnorm(10))
func7 <- function(x){
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
}


#' Weighted mean, var, sd
#'
#' Computes the weighted mean, var, sd
#'
#' HW 1 Problem 4
#' 
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' data(d)
#' func8(d)
func8 <- function(d){
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Highlevel check function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' HW1 Problem 6
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' func9(NA)
func9 <- function(x){
  
  tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
  tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
  tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
  tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
  tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
  
}

#' Matrix Multiplication
#' 
#' Takes a matrix A and a vector x and calculates xT*A^-1*x
#' 
#' Quiz 2 Problem 1
#' @param A matrix
#' @param x vector
#' @return matrix
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' func10(a,x)
func10 <- function(A,x)
{
  stopifnot(nrow(A) == ncol(A))
  stopifnot(length(x) == nrow(A))
  stopifnot(is.finite(A))
  stopifnot(is.finite(x))
  X <- cbind(x)
  xt <- t(X)
  Ai <- solve(A)
  m <- xt%*%Ai%*%X
  return(m)
}


#' Matrix Multiplication (again)
#' 
#' Takes a matrix A and a vector x and calculates xT*A^-1*x
#' 
#' Quiz 2 Problem 2
#' 
#' @param A matrix
#' @param x vector
#' @return matrix
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' a %func11% x
"%func11%" <- function(A,x)
{
  stopifnot(nrow(A) == ncol(A))
  stopifnot(length(x) == nrow(A))
  stopifnot(is.finite(A))
  stopifnot(is.finite(x))
  X <- cbind(x)
  xt <- t(X)
  Ai <- solve(A)
  return(xt%*%Ai%*%X)
}

#' Matrix Standardization
#' 
#' Takes a matrix and standardizes its columns
#' 
#' Quiz 2 Problem 3
#' 
#' @param a matrix
#' @return matrix
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' func12(a)
func12 <- function(a)
{
  stopifnot(nrow(a) > 1)
  stopifnot(is.finite(a))
  s <- matrix(nr = nrow(a), nc = ncol(a))
  for(i in 1:ncol(a))
  {
    x <- a[,i]
    st <- (x-mean(x))/sd(x)
    s[,i] <- st
  }
  return(s)
}


#' Matrix Standardization (again)
#' 
#' Takes a matrix and standardizes its columns
#' 
#' HW 2 Problem 4
#' 
#' @param a matrix
#' @return matrix
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' func13(a)
func13 <- function(a)
{
  stopifnot(nrow(a) > 1)
  stopifnot(is.finite(a))
  s <- matrix(nrow = nrow(a), ncol = ncol(a))
  m <- apply(a, 2, mean)
  m <- cbind(m)
  m <- t(m)
  st <- apply(a, 2, sd)
  st <- cbind(st)
  st <- t(st)
  s <- apply(a, 1, function(x) ((x-m)/st))
  s <- t(s)
  return(s)
}


#' Handmade Apply Function
#' 
#' Functions the same as the apply() function
#' 
#' HW2 Problem 5
#' 
#' @param X array
#' @param MARGIN object
#' @param FUN function
#' @return array
#' @export
#' @examples 
#' fred <- matrix(1:6, ncol = 2)
#' myapply(fred,1,"mean")
#' myapply(fred, 2, "mean")
#' myapply(fred, 1, "max")
#' myapply(fred, 2, "max")
#' myapply(fred, 1, function(x) quantile(x,0.75))
#' myapply(fred, 2, function(x) quantile(x,0.75))
#' myapply(fred,1,"quantile",probs=.75)
#' myapply(fred,2,"quantile",probs=.75)
#' myapply(fred,1,"quantile",probs=(1:3)/4)
#' myapply(fred,2,"quantile",probs=(1:3)/4)
myapply <- function(X, MARGIN, FUN, ...)
{
  if(length(dim(X))!=2)
  {
    stop("matrix is not 2d")
  } 
  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not in 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }
  }else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}

