#' Wrapper function for ggplot2 for data d
#'
#' 
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(farmData)
#' plotmydata(farmData)
plotmydata<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=age1, y=educ1)+ggplot2::geom_point()
}

