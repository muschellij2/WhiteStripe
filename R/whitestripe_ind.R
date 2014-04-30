#' @title Make Image VOI
#'
#' @description Creates a VOI of Image for the specified slices
#' @param img Image (T1 usually or T2).  Array or object of class nifti 
#' @param slices Slices to take for the image voi
#' @param na.rm Remove NAs from mean.  This is for double checking
#' @export
#' @return VOI of image.
make_img_voi = function(img, slices = 80:120, na.rm = TRUE){
  img.voi = img[,,slices]  
  mn = mean(img, na.rm=na.rm)
  img.voi = img.voi[ img.voi > mn]
  return(img.voi)
}


#' @title Performs White Stripe of T1 or T2 Images
#'
#' @description Returns the mean/sd of the whitestripe and indices
#' for them on the image 
#' @param img Image (T1 or T2).  Array or object of class nifti 
#' @param type T1 or T2 image whitestripe
#' @param breaks Number of breaks passed to \code{\link{hist}}
#' @param whitestripe.width Radius of the white stripe
#' @param arr.ind Whether indices should be array notation or not, 
#' passed to \code{\link{which}}
#' @param verbose Print diagnostic information
#' @param ... Arguments to be passed to \code{\link{get.last.mode}}
#' @export
#' @return List of indices of whitestripe, last mode of histogram,
#' array/nifti of 0/1 corresponding to the mask,
#' mean of whitestripe, standard deviation of whitestripe
#' @examples 
#' \dontrun{
#' t1 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe"))
#' t1.ind = whitestripe(t1, "T1")
#' t1.mask = whitestripe_ind_to_mask(t1, t1.ind$whitestripe.ind)
#' t1.mask[t1.mask == 0] = NA
#' orthographic(t1, t1.mask, col.y="red") 
#' t2 = readNIfTI(system.file("T2Strip.nii.gz", package="WhiteStripe"))
#' t2.ind = whitestripe(t2, "T2") 
#' t2.mask = whitestripe_ind_to_mask(t2, t2.ind$whitestripe.ind)
#' t2.mask[t2.mask == 0] = NA
#' orthographic(t2, t2.mask, col.y="red")  
#'}
whitestripe = function(img, type=c("T1", "T2"), breaks=2000, 
                       whitestripe.width = 0.05, 
                       arr.ind= FALSE, verbose = TRUE, ...){
  length.img = prod(dim(img))
  if (verbose){
    cat(paste0("Making ", type, " Image VOI\n"))
  }
  img.voi = make_img_voi(img)
  if (verbose){
    cat(paste0("Making ", type, " Histogram\n"))
  }
  img.hist = hist(img.voi, 
                  breaks=breaks, 
                  plot=FALSE)
  y.in = img.hist$counts
  x.in = img.hist$mids
  x.in = x.in[!is.na(y.in)];
  y.in = y.in[!is.na(y.in)]
  
  stopifnot(length(type) == 1)
  type = match.arg(type)
  if (verbose){
    cat(paste0("Getting ", type, " Modes\n"))
  }  
  if (type == "T1") {
    img.mode = get.last.mode(x.in, y.in, ...)
  }
  if (type == "T2"){
    img.mode = get.largest.mode(x.in, y.in, ...) 
  }
  img.mode.q = mean(img.voi < img.mode)
  if (verbose){
    cat("Quantile VOI\n")
  }    
  whitestripe = quantile(img.voi,
                         probs=c(
                           max(img.mode.q - whitestripe.width,0),
                           min(img.mode.q + whitestripe.width,1)
                         )
  )
  whitestripe.ind = which(
    ( img > whitestripe[1]) & (img < whitestripe[2]) ,
    arr.ind=arr.ind
  )
  err = FALSE
  if (length(whitestripe.ind)==0) {
    warning(paste0("Length of White Stripe is 0 for ", type, ", doing whole brain"))
    whitestripe.ind = 1:length.img 
    err = TRUE
  }
  #ERROR IN WHITE STRIPE MEANS DO WHOLE-IMAGE NORMALIZATION
  
  mu.whitestripe = img.mode
  sig.whitestripe = sd(img[whitestripe.ind])
  #   
  #     img.whitestripe.norm = (img-mu.whitestripe)/sig.whitestripe
  #      
  mask.img = img
  mask.img[!is.na(mask.img) | is.na(mask.img)] = 0
  mask.img[whitestripe.ind] = 1  
  if (inherits(img, "nifti")){
    mask.img = cal_img(mask.img)
    mask.img = zero_trans(mask.img)
  }  
  
  return( list(
    whitestripe.ind = whitestripe.ind, 
    img.mode = img.mode, 
    mask.img = mask.img,
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
whitestripe_norm = function(img, indices, ...){
  mu = mean(img[indices], ...)
  sig = sd(img[indices], ...)
  img = (img-mu)/sig
  if (inherits(img, "nifti")){
    img = cal_img(img)
    img = zero_trans(img)
  }
  return(img)
}

#' @title Hybrid WhiteStripe
#'
#' @description Uses t1 and t2 WhiteStripe to get an intersection of 
#' the two masks for a hybrid approach
#' @param t1 T1 image, array or class nifti
#' @param t2 T2 image, array or class nifti
#' @param ... arguments passed to \code{\link{whitestripe}}
#' @export
#' @keywords hybrid, whitestripe
#' @seealso whitestripe
#' @return List of indices of overlap mask
#' @alias hybrid
#' @examples 
#' \dontrun{
#' t1 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe"))
#' t2 = readNIfTI(system.file("T2Strip.nii.gz", package="WhiteStripe"))
#' ind = whitestripe_hybrid(t1, t2)
#'}
whitestripe_hybrid = function(t1, t2, ...){
  t1.ws = whitestripe(t1, type="T1", ...)
  t2.ws = whitestripe(t2, type="T2", ...)
  whitestripe.ind = intersect(t1.ws$whitestripe.ind, 
    t2.ws$whitestripe.ind)
  return(list(
    whitestripe.ind= whitestripe.ind
  ))
}

#' @title WhiteStripe Indices to Mask
#'
#' @description Uses WhiteStripe indices to create image mask
#' @param img Array or class nifti that is corresponds to dimensions of the images the 
#' indices were generated from
#' @param indices indices from \code{\link{whitestripe}}
#' @param writeimg logical to write image or not
#' @param ... arguments to passed to \code{\link{writeNIfTI}} for writing image
#' @export
#' @keywords hybrid, whitestripe
#' @seealso whitestripe, whitestripe_hybrid
#' @return Class of array or nifti depending on img input
#' @alias whitemask
#' @examples 
#' \dontrun{
#' t1 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe"))
#' t2 = readNIfTI(system.file("T2Strip.nii.gz", package="WhiteStripe"))
#' ind = whitestripe_hybrid(t1, t2)
#' mask = whitestripe_ind_to_mask(t1, ind$whitestripe.ind)
#' orthographic(mask)
#' } 
whitestripe_ind_to_mask = function(img, indices, writeimg=FALSE, ...){
  img[!is.na(img) | is.na(img)] = FALSE
  img[indices] = TRUE
  if (inherits(img, "nifti")){
    img = cal_img(img)
    img = zero_trans(img)
    if (writeimg){
      writeNIfTI(nim=img, ...)
    }
  } 
  
  return(img)
}



#' @title Set Max/Min for nifti object
#' @return object of type nifti
#' @param img nifti object
#' @description Rescales image @cal_max and @cal_min to be the max and min,
#' removing NA's, of the image
#' @name cal_img
#' @export
cal_img = function(img){
  cmax = max(img, na.rm=TRUE) 
  cmax = ifelse(is.finite(cmax), cmax, 0)
  cmin = min(img, na.rm=TRUE) 
  cmin = ifelse(is.finite(cmin), cmin, 0)  
  img@cal_max = cmax
  img@cal_min = cmin
  img
}

#' @title Change intercept to 0 and slope to 1
#' @return object of type nifti
#' @param img nifti object (or character of filename)
#' @description Forces image @scl_slope to 1 nad and @scl_inter to be 0
#' @name zero_trans
#' @import oro.nifti
#' @export
zero_trans = function(img){
  img = check_nifti(img)
  img@scl_slope = 1
  img@scl_inter = 0
  return(img)
}


#' @title Check if nifti image or read in
#' @import oro.nifti
#' @description Simple check to see if input is character or of class nifti
#' @return nifti object
#' @seealso \link{readNIfTI}
#' @param x character path of image or 
#' an object of class nifti
#' @param reorient (logical) passed to \code{\link{readNIfTI}} if the image
#' is to be re-oriented
#' @import oro.nifti
#' @export
check_nifti = function(x, reorient=FALSE){
  if (inherits(x, "character")) {
    img = readNIfTI(x, reorient=reorient)
  } else {
    if (inherits(x, "nifti")){
      img = x
    } else {
      stop("x has unknown class - not char or nifti")
    }
  }
  return(img)
}