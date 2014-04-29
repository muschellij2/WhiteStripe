\name{whitestripe}
\alias{whitestripe}
\title{Performs White Stripe of  Images}
\usage{
whitestripe(img, breaks = 2000, whitestripe.width = 0.05, arr.ind = FALSE,
  ...)
}
\arguments{
  \item{img}{Image (T1 usually or T2).  Array or object of
  class nifti}

  \item{breaks}{Number of breaks passed to
  \code{\link{hist}}}

  \item{whitestripe.width}{Radius of the white stripe}

  \item{arr.ind}{Whether indices should be array notation
  or not, passed to \code{\link{which}}}

  \item{...}{Arguments to be passed to
  \code{\link{get.last.mode}}}
}
\value{
List of indices of whitestripe, last mode of histogram,
mean of whitestripe, standard deviation of whitestripe
}
\description{
Returns the mean/sd of the whitestripe and indices for them
on the image
}
\examples{
\dontrun{

}
}

