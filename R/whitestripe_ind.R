#' @title Make Image VOI
#'
#' @description Creates a VOI of Image for the specified slices
#' @param img Image (T1 usually or T2).  Array or object of class nifti
#' @param slices Slices to take for the image voi
#' @param na.rm Remove NAs from mean.  This is for double checking
#' @param ... Arguments passed from other methods (not used)
#' @import methods
#' @export
#' @return VOI of image.
make_img_voi = function(img, slices = 80:120, na.rm = TRUE, ...){
  if (inherits(img, "img_voi")) {
    return(img)
  }
  img = as(img, "array")
  d3 = dim(img)[3]
  d3 = seq(d3)
  if (!all(slices %in% d3)) {
    stop(paste0("Cannot subset image slices, please",
                " check slices and image dimensions!"))
  }
  img.voi = img[,,slices]
  mn = mean(img, na.rm = na.rm)
  img.voi = img.voi[ img.voi > mn]
  if (na.rm) {
    img.voi = img.voi[!is.na(img.voi)]
  }
  class(img.voi) = "img_voi"
  attr(img.voi, "slices") = slices
  return(img.voi)
}


#' @title Performs White Stripe of T1 or T2 Images
#'
#' @description Returns the mean/sd of the whitestripe and indices
#' for them on the image
#' @param img Image (T1, T2, FA, or MD).  Array or object of class nifti
#' @param type T1, T2, FA, or MD image whitestripe
#' @param breaks Number of breaks passed to \code{\link{hist}}
#' @param whitestripe.width Radius of the white stripe
#' @param whitestripe.width.l Lower Radius of the white stripe
#' @param whitestripe.width.u Upper Radius of the white stripe
#' @param arr.ind Whether indices should be array notation or not,
#' passed to \code{\link{which}}
#' @param verbose Print diagnostic information
#' @param stripped Applying to skull-stripped image. NOTE: This does NOT do a
#' subset of slices, as \code{\link{make_img_voi}}.
#' @param slices slices to use for \code{\link{make_img_voi}} if only a subset to
#' estimate the distribution.
#' @param ... Arguments to be passed to \code{\link{get.last.mode}}
#' @export
#' @return List of indices of whitestripe, last mode of histogram,
#' array/nifti of 0/1 corresponding to the mask,
#' mean of whitestripe, standard deviation of whitestripe
#' @details This function takes in an image and computes a window of the
#' distribution called the white stripe.  If you wish to pass in values you have
#' subset, such as single from a skull-stripped image, you can pass in \code{img}
#' and set the class to \code{img_voi} (\code{class(img) = "img_voi"}) and this
#' will not rerun \code{\link{make_img_voi}}.
#' @examples
#' \dontrun{
#' library(WhiteStripe)
#' lib.loc = tempdir()
#' if (WhiteStripe::download_img_data(lib.loc = lib.loc)){
#' library(oro.nifti)
#' set.seed(1)
#' t1 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe",
#' lib.loc = lib.loc))
#' t1.ind = whitestripe(t1, "T1")
#' set.seed(2)
#' t1_2 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe",
#' lib.loc = lib.loc))
#' t1_2.ind = whitestripe(t1_2, "T1") 
#' t1.mask = whitestripe_ind_to_mask(t1, t1.ind$whitestripe.ind)
#' t1.mask[t1.mask == 0] = NA
#' orthographic(t1, t1.mask, col.y="red")
#' t2 = readNIfTI(system.file("T2Strip.nii.gz", package="WhiteStripe",
#' lib.loc = lib.loc))
#' t2.ind = whitestripe(t2, "T2")
#' t2.mask = whitestripe_ind_to_mask(t2, t2.ind$whitestripe.ind)
#' t2.mask[t2.mask == 0] = NA
#' orthographic(t2, t2.mask, col.y="red")
#' }
#'}
#' @importFrom stats sd quantile
#' @importFrom utils download.file
#' @importFrom graphics hist
whitestripe <- function(
  img,
  type = c("T1", "T2", "FA", "MD", "first", "last", "largest"),
  breaks = 2000, whitestripe.width = 0.05,
  whitestripe.width.l = whitestripe.width,
  whitestripe.width.u = whitestripe.width,
  arr.ind = FALSE, verbose = TRUE,
  stripped = FALSE, slices = NULL, ...)  {
  
  if (is.character(img)) {
    img = neurobase::check_nifti(img)
  }
  if (verbose) {
    message(paste0("Making Image VOI\n"))
  }
  if (stripped) {
    img.voi <- img[img > 0]
  } else {
    is_voi = inherits(img, "img_voi")
    if (is.null(slices) & !is_voi) {
      message(
        paste0("Using all slices of the image as slices not defined!", 
               " Use stripped = TRUE if using skull-stripped images.")
      )        
      d3 = dim(as(img, "array"))[3]
      slices = seq(d3)        
    }
    img.voi <- make_img_voi(img, slices = slices, ...)
  }
  if (verbose) {
    message(paste0("Making ", type, " Histogram\n"))
  }
  img.hist = hist(img.voi, breaks = breaks, plot = FALSE)
  y.in = img.hist$counts
  x.in = img.hist$mids
  x.in = x.in[!is.na(y.in)]
  y.in = y.in[!is.na(y.in)]
  if (min(abs(img.voi)) <= 1 && stripped) {
    warning(
      paste0("Stripped data has very small, but > 0 values, 
                   probably rounding needed, such as ",
             "img[abs(img) <= 1] = 0")
    )
  }
  type = match.arg(type)
  stopifnot(length(type) == 1)
  if (verbose) {
    cat(paste0("Getting ", type, " Modes\n"))
  }
  if (type %in% c("T1", "FA", "last")) {
    img.mode = get.last.mode(x.in, y.in, verbose = verbose,
                             ...)
  }
  if (type %in% c("T2", "largest")) {
    img.mode = get.largest.mode(x.in, y.in, verbose = verbose,
                                ...)
  }
  if (type %in% c("MD", "first")) {
    img.mode = get.first.mode(x.in, y.in, verbose = verbose,
                              ...)
  }    
  img.mode.q = mean(img.voi < img.mode)
  if (verbose) {
    cat(paste0("Quantile ", type, " VOI\n"))
  }
  whitestripe = quantile(img.voi, probs = c(max(img.mode.q -
                                                  whitestripe.width.l, 0), min(img.mode.q + whitestripe.width.u,
                                                                               1)), na.rm = TRUE)
  whitestripe.ind = which((img > whitestripe[1]) & (img < whitestripe[2]),
                          arr.ind = arr.ind)
  err = FALSE
  if (length(whitestripe.ind) == 0) {
    warning(paste0("Length of White Stripe is 0 for ", type,
                   ", using whole brain normalization"))
    whitestripe.ind = which(img > mean(img))
    err = TRUE
  }
  mu.whitestripe = img.mode
  sig.whitestripe = sd(img[whitestripe.ind])
  mask.img = img
  mask.img[!is.na(mask.img) | is.na(mask.img)] = 0
  mask.img[whitestripe.ind] = 1
  if (inherits(img, "nifti")) {
    mask.img = cal_img(mask.img)
    mask.img = zero_trans(mask.img)
  }
  return(list(whitestripe.ind = whitestripe.ind, img.mode = img.mode,
              mask.img = mask.img, mu.whitestripe = mu.whitestripe,
              sig.whitestripe = sig.whitestripe, img.mode.q = img.mode.q,
              whitestripe = whitestripe, whitestripe.width = whitestripe.width,
              whitestripe.width.l = whitestripe.width.l, whitestripe.width.u = whitestripe.width.u,
              err = err))
}



#' @title Normalize Image using white stripe
#'
#' @description Taking the indices from white stripe to normalize the
#' intensity values of the brain
#' @param img Array or object of class nifti
#' @param indices Indices of white stripe from \code{\link{whitestripe}}.
#' Can also be a mask (indices where mask > 0 are used.)
#' @param ... arguments to be passed to \code{\link{mean}} and \code{\link{sd}}
#' @export
#' @return Object of same class as \code{img}, but normalized
whitestripe_norm = function(img, indices, ...){
  if (inherits(indices, "nifti")){
    indices = which(indices > 0)
  }
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
#' @keywords hybrid whitestripe
#' @seealso whitestripe
#' @return List of indices of overlap mask, and overlap of class array or nifti
#' @aliases hybrid
#' @examples
#' \dontrun{
#' lib.loc = tempdir()
#' if (download_img_data(lib.loc = lib.loc)){
#' t1 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe", 
#' lib.loc = lib.loc))
#' t2 = readNIfTI(system.file("T2Strip.nii.gz", package="WhiteStripe",
#' lib.loc = lib.loc))
#' ind = whitestripe_hybrid(t1, t2)
#' }
#'}
whitestripe_hybrid = function(t1, t2, ...){
  t1.ws = whitestripe(t1, type="T1", ...)
  t2.ws = whitestripe(t2, type="T2", ...)
  whitestripe.ind = intersect(t1.ws$whitestripe.ind,
                              t2.ws$whitestripe.ind)
  mask.img = t1
  mask.img[!is.na(mask.img) | is.na(mask.img)] = 0
  mask.img[whitestripe.ind] = 1
  if (inherits(t1, "nifti")){
    mask.img = cal_img(mask.img)
    mask.img = zero_trans(mask.img)
  }
  return(list(
    whitestripe.ind= whitestripe.ind,
    mask.img = mask.img
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
#' @keywords hybrid whitestripe
#' @seealso whitestripe, whitestripe_hybrid
#' @return Class of array or nifti depending on \code{img} input
#' @aliases whitemask
#' @importFrom oro.nifti writeNIfTI
#' @importFrom oro.nifti cal_img
#' @importFrom oro.nifti zero_trans
#' @examples
#' \donttest{
#' lib.loc = tempdir()
#' if (download_img_data(lib.loc = lib.loc)){
#'   t1 = oro.nifti::readNIfTI(system.file("T1Strip.nii.gz",
#'                                         package="WhiteStripe",
#'                                         lib.loc = lib.loc))
#'   t2 = oro.nifti::readNIfTI(system.file("T2Strip.nii.gz",
#'                                         package="WhiteStripe",
#'                                         lib.loc = lib.loc))
#'   ind = whitestripe_hybrid(t1, t2)
#'   mask = whitestripe_ind_to_mask(t1, ind$whitestripe.ind)
#'   oro.nifti::orthographic(mask)
#' }
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


