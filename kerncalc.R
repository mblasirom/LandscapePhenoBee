# Exponential kernel computation, from Häussler et al. 2017

kerncalc <- function(gamma, cell.size, maxSize = Inf, decaycut=0.99)
{
  radius = min(maxSize, round(qexp(decaycut,cell.size / gamma)))
  mat_nrow = (radius * 2) + 1
  mat_ncol = (radius * 2) + 1
  ij <- cbind(1:mat_nrow, rep(1:mat_ncol, each= mat_nrow))
  reachable <- sqrt((ij[,1] - (radius + 1)) ^ 2 + (ij[,2] - (radius + 1)) ^ 2) <= radius
  decay <- exp(- (cell.size/gamma) * (sqrt((ij[,1]  - (radius + 1)) ^ 2 + (ij[,2]  - (radius + 1)) ^ 2)))
  decay <- decay * reachable
  decay = decay / sum(decay)
  decay <- matrix(decay, mat_nrow, mat_ncol)
  return(list(decay=decay,decaycut=decaycut))
}

