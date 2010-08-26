\name{emg-package}
\alias{emg-package}
\alias{emg}
\docType{package}
\title{Exponentially Modified Gaussian (EMG) Distribution}
\description{
  Provide basic functions for a mixture of gaussian and exponential distributions.
}
\details{
\tabular{ll}{
Package: \tab emg\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2010-08-26\cr
License: \tab GPL 2.0\cr
LazyLoad: \tab yes\cr
}
  Provides basic distribution functions for the EMG model, pemg, demg, qemg and remg. As well as an MLE estimation
  routine emg.mle.
}
\author{
Shawn Garbett

Maintainer: Shawn Garbett <Shawn.Garbett@Vanderbilt.edu>
}
\references{
  Golubev. Exponentially modified Gaussian (EMG) relevance to distributions related to cell proliferation and differentiation. J Theor Biol. 2010 Jan 21;262(2):257-66. Epub 2009 Oct 13.
}
\keyword{ package }
\seealso{
\code{\link[emg]{EMG}}
\code{\link[emg]{emg.mle}}
\code{\link[stats]{Normal}}
\code{\link[stats]{Exponential}}
}
\examples{
  y <- remg(200)
  hist(y, freq=FALSE, ylim=c(0, 0.35), breaks=20)
  x <- 1:100/100 * 8 - 3
  lines(x, demg(x))
  m <- emg.mle(y)
  sqrt(diag(m@vcov)) # Show stderr in estimate
  ks.test(y, "pemg", 0, 1, 1)
}
