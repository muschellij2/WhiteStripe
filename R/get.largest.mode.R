#' @title Grab largest peak
#'
#' @description This function grabs the largest peak of the histogram
#' @param x values of midpoints from \code{\link{hist}}
#' @param y values of counts from \code{\link{hist}}
#' @param ... arguments to be passed to \code{\link{smoooth.hist}}
#' @export
#' @return Value of x that is the largest peak
#' @examples \dontrun{
#'
#'}
get.largest.mode <- function(x, y, 
  ...) {
  
  #estimate derivative
  
  system.time({
    smooth1 = smooth.hist(x, y, ...)
  })
  dy = get.deriv.smooth.hist(
    x,
    coefs=smooth1$coefs,
    knots=smooth1$knots,
    deg=smooth1$deg,
    deriv.deg=1)
  which.zero.crossing = which(
    (dy[1:(length(x)-1)]>0) > (dy[2:(length(x))]>0)
    )
  largest.peak = x[which.zero.crossing[which.max(y[which.zero.crossing])]]
  return(largest.peak)	
}

#' @title Get Last Peak
#'
#' @description This function grabs the last peak or shoulder.
#' @param x values of midpoints from \code{\link{hist}}
#' @param y values of counts from \code{\link{hist}}
#' @param rare.prop Proportion used to remove rare intensity tail
#' @param ... arguments to be passed to \code{\link{smoooth.hist}}
#' @export
#' @return Value of x that is the last peak
#' @examples \dontrun{
#'
#'}
get.last.mode = function(x,y, 
  rare.prop=1/5, ...) {
  
  
  #Remove rare intensity tail
  which.rare <- y < (rare.prop*max(y))
  y = y[!which.rare]
  x = x[!which.rare]
  
  #estimate derivative
  system.time({
    smooth1 = smooth.hist(x, y, ...)
  })
  dy<-get.deriv.smooth.hist(x, 
    coefs=smooth1$coefs,
    knots=smooth1$knots,
    deg=smooth1$deg,
    deriv.deg=1)
  which.zero.crossing<-which(
    (dy[1:(length(x)-1)]>0) > (dy[2:(length(x))]>0)
  )
  last.peak<-max(x[which.zero.crossing])
  #if (last.peak<median(t1.voi[t1.voi>mean(t1.voi)])) {
  # biggest.shoulder<-(x[x>median(t1.voi[t1.voi>mean(t1.voi)])])[which.min(abs(dy[x>median(t1.voi[t1.voi>mean(t1.voi)])]))]
  # return(biggest.shoulder)
  #} else {
  # return(last.peak)
  #}
  return(last.peak) 
}

