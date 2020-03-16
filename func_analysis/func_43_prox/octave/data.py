import scipy.io as sio
import pandas as pd
import numpy as np

diabetes = pd.read_csv("~/Documents/classnotes/stat/stat_120_regular/diabetes.csv",sep=';')
y = np.array(diabetes['response'].astype(float))
A = np.array(diabetes.drop("response",axis=1))
sio.savemat("A.mat", {"A": A})
sio.savemat("y.mat", {"y": y})

from sklearn.linear_model import Lasso
res = Lasso(alpha = 0.1).fit(A, y)
print (res.coef_)



