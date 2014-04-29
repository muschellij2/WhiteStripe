#' @title Make Image VOI
#'
#' @description Creates a VOI of Image for the specified slices
#' @param img Image (T1 usually or T2).  Array or object of class nifti 
#' @param slices Slices to take for the image voi
#' @param na.rm Remove NAs from mean.  This is for double checking
#' @export
#' @return VOI of image.
#' @examples \dontrun{
#'
#'}
make_img_voi = function(img, slices = 80:120, na.rm = TRUE){
  img.voi = img[,,slices]  
  mn = mean(img, na.rm=na.rm)
  img.voi = img.voi[ img.voi > mn]
  return(img.voi)
}


#' @title Performs White Stripe of  Images
#'
#' @description Returns the mean/sd of the whitestripe and indices
#' for them on the image 
#' @param img Image (T1 usually or T2).  Array or object of class nifti 
#' @param breaks Number of breaks passed to \code{\link{hist}}
#' @param whitestripe.width Radius of the white stripe
#' @param arr.ind Whether indices should be array notation or not, 
#' passed to \code{\link{which}}
#' @param ... Arguments to be passed to \code{\link{get.last.mode}}
#' @export
#' @keywords whitestripe, intensity normalization
#' @return List of indices of whitestripe, last mode of histogram,
#' mean of whitestripe, standard deviation of whitestripe
#' @examples \dontrun{
#'
#'}
whitestripe = function(img, breaks=2000, 
                       whitestripe.width = 0.05, arr.ind= FALSE, ...){
  length.img = prod(dim(img))
  img.voi = make_img_voi(img)
  img.hist = hist(img.voi, 
                  breaks=breaks, 
                  plot=FALSE)
  y.in = img.hist$counts
  x.in = img.hist$mids
  x.in = x.in[!is.na(y.in)];
  y.in = y.in[!is.na(y.in)]
  
  img.last.mode = get.last.mode(x.in, y.in, ...)
  img.last.mode.q = mean(img.voi < img.last.mode)
  whitestripe = quantile(img.voi,
                         probs=c(
                           max(img.last.mode.q - whitestripe.width,0),
                           min(img.last.mode.q + whitestripe.width,1)
                         )
  )
  whitestripe.ind = which(
    ( img > whitestripe[1]) & (img < whitestripe[2]) ,
    arr.ind=arr.ind
  )
  err = FALSE
  if (length(whitestripe.ind)==0) {
    warning("Length of White Stripe is 0, doing whole brain")
    whitestripe.ind = 1:length.img 
    err = TRUE
  }
  #ERROR IN WHITE STRIPE MEANS DO WHOLE-IMAGE NORMALIZATION
  
  mu.whitestripe = img.last.mode
  sig.whitestripe = sd(img[whitestripe.ind])
  #   
  #     img.whitestripe.norm = (img-mu.whitestripe)/sig.whitestripe
  #       
  return( list(
    whitestripe.ind = whitestripe.ind, 
    img.last.mode = img.last.mode, 
    mu.whitestripe = mu.whitestripe,
    sig.whitestripe = sig.whitestripe,
    err = err  ))
}


#' @title Normalize Image using white stripe
#'
#' @description Taking the indices from white stripe to normalize the 
#' intensity values of the brain
#' @param img Array or object of class nifti
#' @param indices Indices of white stripe from \code{\link{whitestripe}}
#' @param ... arguments to be passed to \code{\link{mean}} and \code{\link{sd}}
#' @export
#' @return Object of same class as img, but normalized
#' @examples \dontrun{
#'
#'}
whitestripe_norm = function(img, indices, ...){
  mu = mean(img[indices], ...)
  sig = sd(img[indices], ...)
  img = (img-mu)/sig
  return(img)
}