emg.nllik <- function(x, mu, sigma, lambda)
{
  #print(paste('nllemg',length(x), mu, sigma, lambda, '=>', demg(x, mu, sigma, lambda)))
  sum(-log(demg(x, mu, sigma, lambda)))
}
