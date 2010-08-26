demg <- function(x, mu=0, sigma=1, lambda=1)
{
  if(length(mu)     > 1) {stop("Single value for mu required")    }
  if(length(sigma)  > 1) {stop("Single value for sigma required") }
  if(length(lambda) > 1) {stop("Single value for lambda required")}
  if(sigma <= 0.0)       {stop("Sigma must be greater than zero") }
  if(lambda <= 0.0)      {stop("Lambda must be greater than zero")}

  erfc <- 2 * pnorm((mu+lambda*sigma*sigma-x)/ (sqrt(2)*sigma) * sqrt(2), lower = FALSE)
  result <- 0.5 * lambda * exp(lambda/2 * (2*mu+lambda*sigma*sigma-2*x)) * Re(erfc)
  result[is.nan(result)] <- 0
  result
}

