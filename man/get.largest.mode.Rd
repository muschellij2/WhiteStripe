\name{get.largest.mode}
\alias{get.largest.mode}
\title{Grab largest peak}
\usage{
get.largest.mode(x, y, verbose = TRUE, ...)
}
\arguments{
  \item{x}{values of midpoints from \code{\link{hist}}}

  \item{y}{values of counts from \code{\link{hist}}}

  \item{verbose}{print diagnostic output}

  \item{...}{arguments to be passed to
  \code{\link{smooth_hist}}}
}
\value{
Value of x that is the largest peak
}
\description{
This function grabs the largest peak of the histogram
}
\examples{
data(t2.voi)
system.time({
img.hist = hist(t2.voi,
breaks=2000,
plot=FALSE)
y = img.hist$counts
x = img.hist$mids
x = x[!is.na(y)];
y = y[!is.na(y)]
# 70 used for speed of example
nawm_peak = get.largest.mode(x, y, k=70)
hist(t2.voi, breaks=2000, border="red")
abline(v=nawm_peak)
})
}

