
# Function to conduct sampling
conduct_sampling <- function(prob, nsample){
  x <- runif(n=nsample, min=0, max=1)
  nover <- sum(x <= prob)
  pover <- nover / nsample
  return(pover)
}
