## =============================================================================
## Example 1: simple standard problem
## solve the BVP ODE:
## d2y/dt^2=-3py/(p+t^2)^2
## y(t= -0.1)=-0.1/sqrt(p+0.01)
## y(t= 0.1)= 0.1/sqrt(p+0.01)
## where p = 1e-5
##
## analytical solution y(t) = t/sqrt(p + t^2).
##
## The problem is rewritten as a system of 2 ODEs:
## dy=y2
## dy2=-3p*y/(p+t^2)^2
## =============================================================================

library(bvpSolve)

fun <- function(t, y, pars) {
   dy1 <- y[2]
   dy2 <- - 3 * p * y[1] / (p+t*t)^2
   return(list(c(dy1,dy2))) }

p <- 1e-5

init <- c(-0.1 / sqrt(p+0.01), NA)
end <- c( 0.1 / sqrt(p+0.01), NA)

sol <- bvpcol(yini = init, yend = end,x = seq(-0.1, 0.1, by = 0.001), func = fun)
print (sol)
