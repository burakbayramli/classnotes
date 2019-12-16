import numpy as np

def loginnerprodexp(t,a):
    eps=1e-15
    t[t>0.] = 1
    tmp = np.dot(t,np.exp(a)) + eps
    b=np.log(tmp)
    return b

def logsumexp(a):
    return np.log(np.sum(np.exp(a), axis=0))

def do_EMmixtureBernoulli(Y,K,iter,tol):
    N,D=Y.shape
    OMY=1+(-1*Y) #  "One minus Y", (1-Y)
    tmp=np.random.rand(N,K)
    tmp2=np.sum(tmp,axis=1).reshape((N,1))
    tmp3=np.tile(tmp2,(1,K))
    lR=np.log(np.divide(tmp, tmp3))
    L = []
    for i in range(iter):
        # lPi log Mixture params Kx1
	lPi=np.tile(-1 * np.log(N),(K,1))+logsumexp(lR).T.reshape((K,1))
        const=np.tile(logsumexp(lR).T.reshape((K,1)),(1,D))
        # lP log Bernoulli params KxD
        lP=loginnerprodexp(Y.T,lR).T - const
        # lOMP log(1-P), also KxD
        lOMP=loginnerprodexp(OMY.T,lR).T-const
	
	# *** E-step        
	lR=np.tile(lPi.T,(N,1))+np.dot(Y,lP.T) + np.dot(OMY,lOMP.T) # + const
        Z=logsumexp(lR.T)

        lR=lR-np.tile(Z.T.reshape((N,1)),(1,K))
        L.append(np.sum(Z))
        if (i>1):
            if np.abs(L[i]-L[i-1]) < tol: break
                        
    iters = i
    return lR,lPi,lP,L,iters


def EMmixtureBernoulli(Y,K,iter,tol,attempts):
    Lbest = -np.inf
    eps=1e-15
    # EM'i farkli noktalardan birkac kere (attempts kadar) baslat
    # En iyi sonucun sonuclarini elde tut
    for attempt in range(attempts):
        lRtmp,lPitmp,lPtmp,L,iters = do_EMmixtureBernoulli(Y,K,iter,eps)
        if L[iters]>Lbest:
            lR=lRtmp
            lPi=lPitmp
            lP=lPtmp
            Lbest=L[iters]
            itersbest=iters
    aic = -2*Lbest + 2*lP.shape[0]*lP.shape[1]
    return lR, lPi, lP, Lbest, aic
