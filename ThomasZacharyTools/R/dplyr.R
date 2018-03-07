#' wrapper function for dplyr for data farmData
#' 
#' @param x data.frame
#' @return data.frame
#' @export
#'
#' @examples
#' data(farmData)
#' dplyr(farmData)

dplyr<- function(x)
  {
  library(magrittr)
  x %>% dplyr::select(gender1) 
  }

