import numpy as np

def prepare_data():
    a=.1
    b=.8
    Ttotal=200
    xs = np.zeros((1,Ttotal))[0]
    xs[0] = np.random.randint(2)
    y = np.zeros((1,Ttotal))[0]
    for t in range(1,Ttotal):
        if np.random.rand() < a:
            xs[t] = 1-xs[t-1]
        else:
            xs[t] = xs[t-1]
        if xs[t]:
            if np.random.rand() < b:
                y[t] = 1
            else:
                y[t] = 0
        else:
            if np.random.rand() < 1-b:
                y[t] = 1
            else:
                y[t] = 0
    return y, Ttotal, a, b, xs
