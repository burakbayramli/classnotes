from scipy.io import mmread, mmwrite
import numpy as np, time, sys, os
import funk2

user_feature_matrix = np.loadtxt("/tmp/user_feature_matrix2.dat")
movie_feature_matrix = np.loadtxt("/tmp/movie_feature_matrix2.dat")
A = mmread('%s/Downloads/A_m100k_test.mtx'  % os.environ['HOME']).tocoo()
AA = A.tocsc()

sqs = []
for i in range(len(A.data)):
    user_id = A.row[i]
    movie_id = A.col[i]
    real = AA[user_id, movie_id]
    pred = funk2.predict_rating(user_id, movie_id, user_feature_matrix, movie_feature_matrix)
    print pred, real
    tmp = (real-pred)**2
    sqs.append(tmp)

mse = np.mean(np.array(sqs))
print 'rmse', np.sqrt(mse)
