# This analytical form was contributed by Mark Kozdoba
# N(mu, sigma^2) + Exp(lambda)
pemg <- function(q, mu=0, sigma=1, lambda=1)
{
  u <- (q - mu)*lambda
  sigma1 <- lambda*sigma

  pnorm(u, 0, sigma) - exp(-u + (sigma1*sigma1)/2 + log(pnorm(u, sigma1*sigma1, sigma1)))
}