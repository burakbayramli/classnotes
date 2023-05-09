import numpy as np

def kalman_filter(x,y):

    delta=0.0001
    Ve=0.001

    yhat = np.ones(len(y))*np.nan
    e = np.ones(len(y))*np.nan
    Q = np.ones(len(y))*np.nan
    R = np.zeros((2,2))
    P = np.zeros((2,2))

    beta = np.matrix(np.zeros((2,len(y)))*np.nan)

    Vw=delta/(1-delta)*np.eye(2)

    beta[:, 0]=0.

    for t in range(len(y)):
        if (t > 0):
            beta[:, t]=beta[:, t-1]
            R=P+Vw

        yhat[t]=np.dot(x[t, :],beta[:, t])

        xt = np.matrix(x[t, :])
        Q[t] = np.dot(np.dot(xt,R),xt.T) + Ve

        e[t]=y[t]-yhat[t]

        K=np.dot(R,np.matrix(x[t, :]).T) / Q[t]

        beta[:, t]=beta[:, t]+np.dot(K,np.matrix(e[t]))

        P=R-np.dot(np.dot(K,xt),R)

    return beta, e, Q
