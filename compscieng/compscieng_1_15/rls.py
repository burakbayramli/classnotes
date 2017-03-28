import numpy as np

def rlse_online(aT_k1,b_k1,x,P): 
    K = np.dot(P,aT_k1.T)/(np.dot(np.dot(aT_k1,P),aT_k1.T)+1)
    x = x +K*(b_k1-np.dot(aT_k1,x))
    P = P-np.dot(K,np.dot(aT_k1,P))
    return x,K,P

