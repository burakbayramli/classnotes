xf <- c(4,5,4,1,0,4,3,4,0,6,3,3,4,0,2,6,3,3,5,4,5,3,1,4,4,1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,2,1,1,1,1,3,0,0,1,0,1,1,0,0,3,1,0,3,2,2,0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,0,1,1,0,2,3,3,1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1)

udist <- function(n) rep(1/n, n) 

#natural to working
n2wp <- function(p) {
	m <- length(p)
	log(p[2:m]/(1 - sum(p[2:m])))
}

#working to natural
w2np <- function(lp) {
	rv <- exp(lp)/(1 + sum(exp(lp)))	
	c(1 - sum(rv), rv)
}

#optimisation function
of <- function(pv, m, x) {
	#convert working parameters to natural paramters
	pr <- exp(pv[1:m])
	probs <- w2np(pv[(m+1):(2*m - 1)])
	#calculate -ve log likelihood
	-sum(log(outer(x, pr, dpois) %*% probs))
}

#initial estimates and probabilities for 2, 3 and 4 distributions
#the lambda values I just guess, and use an uniform distribution
#for the initial mixing distribution.
pv <- c(log(c(1, 2)), n2wp(udist(2)))

#number of distributions to fit
m <- 2

#fit using nlm
fv <-nlm(of, pv, m, xf, print.level=0) 
rv <- fv$est

#lambda estimates
exp(rv[1:m])
#mixing distribution
w2np(rv[(m+1):(2*m-1)])
