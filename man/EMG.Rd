% Copyright 2010 Shawn Garbett
% Distributed under GPL 2 or later

\name{EMG}
\alias{EMG}
\alias{demg}
\alias{pemg}
\alias{qemg}
\alias{remg}

\title{The Exponential Modified Gaussian (EMG) Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the EMG distribution with three parameters, mu, sigma, lambda.
  The distribution is a mixture of an exponential and gaussian (normal) distribution.
}
\usage{
demg(x, mu = 0, sigma = 1, lambda = 1)
pemg(q, mu = 0, sigma = 1, lambda = 1)
qemg(p, mu = 0, sigma = 1, lambda = 1)
remg(n, mu = 0, sigma = 1, lambda = 1)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{mu}{mu value}
  \item{sigma}{sigma value}
  \item{lambda}{lambda value}
}
\value{
  \code{demg} gives the density,
  \code{pemg} gives the distribution function,
  \code{qemg} gives the quantile function, and
  \code{remg} generates random deviates.
}
\details{
  If \code{mean} or \code{sd} are not specified they assume the default
  values of \code{0} and \code{1}, respectively.

  The EMG distribution has density
  \deqn{
    f(x) =
    \frac{1}{2} \lambda e^{\frac{\lambda}{2} (2 \mu + \lambda \sigma^2 - 2 x)}
    erfc((\mu + \lambda \sigma \sigma - x) / (\sqrt{2} \sigma))
  }{
    f(x) = 1/2 \lambda e^(\lambda / 2 * (2 \mu + \lambda * \sigma^2 - 2 x))
    erfc((\mu + \lambda * \sigma ^2 - x) / (sqrt(2) * \sigma))
  }
  where \eqn{\mu}{mu} is the mean of the normal distribution,
  \eqn{\sigma}{sigma} the standard deviation of the normal and 
  \eqn{\lambda}{lambda} rate of the exponential.
}
\seealso{
  \code{\link[stats]{runif}} and \code{\link[base]{.Random.seed}} about random number
  generation, \code{\link[stats]{dnorm}} for the normal distribution, and
  \code{\link[stats]{dexp}} for the exponential distribution.
}
\references{
   Golubev. Exponentially modified Gaussian (EMG) relevance to distributions related to cell proliferation and differentiation. J Theor Biol. 2010 Jan 21;262(2):257-66. Epub 2009 Oct 13.
}
\examples{
  plot(demg, -2, 5)
}
\keyword{distribution}