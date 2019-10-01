#' Histogram of VOI of T1 template image
#' @format A volume of interest histogram from a T1 image for smoothing
#' @keywords datasets
#' @examples
#' \dontrun{
#' lib.loc = tempdir() 
#' if (download_img_data(lib.loc = lib.loc)){
#' t1 = readNIfTI(system.file("T1Strip.nii.gz", package="WhiteStripe",
#' lib.loc = lib.loc))
#' t1.voi = make_img_voi(t1)
#' any(is.na(t1.voi))
#' # FALSE
#' t1.voi.hist = hist(t1.voi, 
#' breaks=2000, 
#' plot=FALSE) 
#' #save(t1.voi.hist, file="data/t1.voi.hist.rda", compress = TRUE,
#' # compression_level=9)
#' }
#' } 
"t1.voi.hist"


#' Histogram of VOI of T2 template image
#' @format A histogram volume of interest from a T2 image for smoothing
#' @keywords datasets
#' @examples
#' \dontrun{
#' lib.loc = tempdir() 
#' if (download_img_data(lib.loc = lib.loc)){
#' t2 = readNIfTI(system.file("T2Strip.nii.gz", package="WhiteStripe",
#' lib.loc = lib.loc))
#' t2.voi = make_img_voi(t2)
#' any(is.na(t2.voi))
#' # FALSE 
#' t2.voi.hist = hist(t2.voi, 
#' breaks=2000, 
#' plot=FALSE)  
#' #save(t2.voi.hist, file="data/t2.voi.hist.rda", compress = TRUE,
#' # compression_level=9) 
#' }
#' } 
"t2.voi.hist"


#' Smoothed histogram of image
#' @format A GAM from \code{mgcv} for x and y from histograms
#' @keywords datasets
#' @examples
#' \dontrun{ 
#' data(t2.voi.hist)
#' y = t2.voi.hist$counts
#' x = t2.voi.hist$mids
#' x = x[!is.na(y)];
#' y = y[!is.na(y)]
#' # 70 used for speed of example
#' s.hist = smooth_hist(x, y, k=70)
#' }
"s.hist"


#' Midpoints from VOI histogram
#' @format x values from histogram for VOI
#' @keywords datasets
"xvals"

