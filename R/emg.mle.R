emg.mle <- function(x, lower=list(mu=-Inf, sigma=1e-6, lambda=1e-6))
{
   mle(function(mu, sigma, lambda){emg.nllik(x, mu, sigma, lambda)},
             method='L-BFGS-B',
             lower=lower,
             start=list(mu=median(x), sigma=sqrt(sd(x)), lambda=1/mean(x)))
}

