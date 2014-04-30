\name{whitestripe_norm}
\alias{whitestripe_norm}
\title{Normalize Image using white stripe}
\usage{
whitestripe_norm(img, indices, ...)
}
\arguments{
  \item{img}{Array or object of class nifti}

  \item{indices}{Indices of white stripe from
  \code{\link{whitestripe}}}

  \item{...}{arguments to be passed to \code{\link{mean}}
  and \code{\link{sd}}}
}
\value{
Object of same class as img, but normalized
}
\description{
Taking the indices from white stripe to normalize the
intensity values of the brain
}
