pemg <- function(q, mu=0, sigma=1, lambda=1)
{
  result <- rep(NaN, length(q))
  for(i in 1:length(q))
  {
    result[i] <- integrate(function(y){demg(y, mu, sigma, lambda)}, -Inf, q[i])$value
  }
  return(result)
}

