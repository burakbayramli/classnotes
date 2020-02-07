import numpy as np, pandas as pd
        
def predict(w, x):
    return np.dot(w.reshape((len(x),1)).T,x)

def train_sgd(data, labels, lam, iter, batch_size):
    m,n = data.shape; w = np.zeros(n)
    idx = range(m)
    eta = 0.0001
    for t in range(1, iter):
        w_delta = np.zeros(n)
        np.random.shuffle(idx)
        for j in range(batch_size):
            i = idx[j]
            p = predict(w, data[i,:])
            if labels[i]*p < 1.:
                w_delta += labels[i]*data[i,:]
        w = (1.0 - eta*lam)*w + (eta/batch_size)*w_delta
    return w
