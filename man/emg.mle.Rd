\name{emg.mle}
\alias{emg.mle}
\title{Maximum Likelihood estimate of parameters}
\description{
  Compute the maximum likelihood model for the parameters given a set of observations. Returns a model with
  estimates for mu, sigma, and lambda.
}
\usage{
  emg.mle(x, lower=list(mu=-Inf, sigma=1e-6, lambda=1e-6))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vector of observations to estimate parameters for. }
  \item{lower}{list of lower bounds for parameters. }
}
\details{
  
}
\value{
   An object of class \code{\link[stats4]{mle-class}}.
}
\author{ Shawn Garbett }

\seealso{
\code{\link[emg]{EMG}}
\code{\link[emg]{emg.nllik}}
}
\examples{
  y <- remg(200)
  emg.mle(y)
}