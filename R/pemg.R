# This analytical form was contributed by Mark Kozdoba
# N(mu,sigma^2) + Exp(lambda)
pemg <- function (q, mu = 0, sigma = 1, lambda = 1)
{
    #renormalize
    u <- (q - mu) * lambda
    sigma1 <- lambda * sigma

    #compute for the case where mu=0, sigma = sigma1, lambda = 1
    pnorm(u, 0, sigma1) - exp(-u + (sigma1 * sigma1)/2 + log(pnorm(u,
        sigma1 * sigma1, sigma1)))
}