context("Homework functions")

test_that("func5 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  func5(x)
  expect_identical(func5(x), x_list)
})

test_that("func6 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func6(x), x_list)
  save<-try(func6(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func8 computes mean, var, sd",
{
  x <- 1:4
  p<- c(.1,.2,.3,.4)
  r <- data.frame(x,p)
  var1 <- function(d){sum(((d$x-weighted.mean(d$x,d$p))^2)*d$p)}
  x_list <- list(mean=weighted.mean(x,p), var=var1(r), sd=sqrt(var1(r)))
  func8(r)
  expect_identical(func8(r), x_list)
})


test_that("func10 calculates xT*A^-1*X",
{
  A <- matrix(c(1:8,10),nro=3, ncol=3)
  x <- cbind(1:3)
  xt <- t(x)
  Ai <- solve(A)
  m <- xt%*%Ai%*%x
  func10(A,x)
  expect_identical(func10(A,x), m)
})

test_that("func11 calculates xT*A^-1*X",
{
  A <- matrix(c(1:8,10),nrow = 3, ncol=3)
  x <- cbind(1:3)
  xt <- t(x)
  Ai <- solve(A)
  expect_identical(A %func11% x, xt%*%Ai%*%x)
})

test_that("func12 standardizes a matrix",
{
  a <- matrix(1:9, nrow = 3, ncol = 3)
  stand <- matrix(nr = nrow(a), nc = ncol(a))
  for(i in 1:ncol(a))
  {
    x <- a[,i]
    st <- (x-mean(x))/sd(x)
    stand[,i] <- st
  }
  expect_identical(func12(a), stand)
})

test_that("func13 standardizes a matrix",
{
  a <- matrix(1:9, nrow = 3, ncol = 3)
  s <- matrix(nr = nrow(a), nc = ncol(a))
  m <- apply(a, 2, mean)
  m <- cbind(m)
  m <- t(m)
  st <- apply(a, 2, sd)
  st <- cbind(st)
  st <- t(st)
  s <- apply(a, 1, function(x) ((x-m)/st))
  s <- t(s)
  expect_identical(func13(a), s)
})

test_that("myapply works the same as apply",
{
  x <- matrix(1:9, ncol = 3)
  x1 <- apply(x,2,"mean")
  expect_identical(myapply(x,2,"mean"), x1)
})





