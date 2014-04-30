\name{whitestripe_hybrid}
\alias{whitestripe_hybrid}
\title{Hybrid WhiteStripe}
\usage{
whitestripe_hybrid(t1, t2, ...)
}
\arguments{
  \item{t1}{T1 image, array or class nifti}

  \item{t2}{T2 image, array or class nifti}

  \item{...}{arguments passed to \code{\link{whitestripe}}}
}
\value{
List of indices of overlap mask
}
\description{
Uses t1 and t2 WhiteStripe to get an intersection of the
two masks for a hybrid approach
}
\examples{
t1 = readNIfTI("~/Dropbox/Packages/WhiteStripe/VolumetricT1Strip.nii.gz")
t2 = readNIfTI("~/Dropbox/Packages/WhiteStripe/T2Strip.nii.gz")
ind = whitestripe_hybrid(t1, t2)
\dontrun{

}
}
\seealso{
whitestripe
}
\keyword{hybrid,}
\keyword{whitestripe}

