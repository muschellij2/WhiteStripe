\name{get.last.mode}
\alias{get.last.mode}
\title{Get Last Peak}
\usage{
get.last.mode(x, y, rare.prop = 1/5, ...)
}
\arguments{
  \item{x}{values of midpoints from \code{\link{hist}}}

  \item{y}{values of counts from \code{\link{hist}}}

  \item{rare.prop}{Proportion used to remove rare intensity
  tail}

  \item{...}{arguments to be passed to
  \code{\link{smoooth.hist}}}
}
\value{
Value of x that is the last peak
}
\description{
This function grabs the last peak or shoulder.
}
\examples{
\dontrun{

}
}

