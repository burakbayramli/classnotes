# Requires Movielens 100k data 
from scipy.io import mmread, mmwrite
import numpy as np, time, sys
from numba import jit
import os

def create_user_feature_matrix(review_matrix, NUM_FEATURES):
    num_users = review_matrix.shape[0]
    user_feature_matrix = 1./NUM_FEATURES * \
        np.random.randn(NUM_FEATURES, num_users).astype(np.float32)
    return user_feature_matrix

def create_movie_feature_matrix(review_matrix, NUM_FEATURES):
    num_movies = review_matrix.shape[1]
    movie_feature_matrix = 1./NUM_FEATURES * \
        np.random.randn(NUM_FEATURES, num_movies).astype(np.float32)
    return movie_feature_matrix

@jit(nopython=True)
def predict_rating(user_id, movie_id, user_feature_matrix, movie_feature_matrix):
    rating = 1.
    for f in range(user_feature_matrix.shape[0]):
        rating += \
            user_feature_matrix[f, user_id] * \
            movie_feature_matrix[f, movie_id]
    if rating > 5: rating = 5
    elif rating < 1: rating = 1
    return rating

@jit(nopython=True)
def sgd_inner(feature, A_row, A_col, A_data,
              user_feature_matrix, movie_feature_matrix,
              NUM_FEATURES):
    K = 0.015
    LEARNING_RATE = 0.001
    squared_error = 0
    for k in range(len(A_data)):
        user_id = A_row[k]
        movie_id = A_col[k]
        rating = A_data[k]
        p = predict_rating(user_id,
                           movie_id,
                           user_feature_matrix,
                           movie_feature_matrix)
        err = rating - p            
        squared_error += err ** 2
        user_feature_value = user_feature_matrix[feature, user_id]
        movie_feature_value = movie_feature_matrix[feature, movie_id]
        user_feature_matrix[feature, user_id] += \
            LEARNING_RATE * (err * movie_feature_value - K * user_feature_value)
        movie_feature_matrix[feature, movie_id] += \
            LEARNING_RATE * (err * user_feature_value - K * movie_feature_value)

    return squared_error

def calculate_features(A_row, A_col, A_data,
                       user_feature_matrix, movie_feature_matrix,
                       NUM_FEATURES):
    MIN_IMPROVEMENT = 0.0001
    MIN_ITERATIONS = 200
    rmse = 0
    last_rmse = 0
    print len(A_data)
    num_ratings = len(A_data)
    for feature in xrange(NUM_FEATURES):
        iter = 0
        while (iter < MIN_ITERATIONS) or  (rmse < last_rmse - MIN_IMPROVEMENT):
            last_rmse = rmse
            squared_error = sgd_inner(feature, A_row, A_col, A_data,
                                      user_feature_matrix, movie_feature_matrix,
                                      NUM_FEATURES)
            rmse = (squared_error / num_ratings)
            iter += 1
        print ('Squared error = %f' % squared_error)
        print ('RMSE = %f' % rmse)
        print ('Feature = %d' % feature)
    return last_rmse

def main():
    LAMBDA = 0.02
    NUM_FEATURES = 30

    A = mmread('%s/Downloads/A_m100k_train' % os.environ['HOME'])

    user_feature_matrix = create_user_feature_matrix(A, NUM_FEATURES)
    movie_feature_matrix = create_movie_feature_matrix(A, NUM_FEATURES)

    A = A.tocoo()

    rmse = calculate_features(A.row, A.col, A.data,
                              user_feature_matrix, movie_feature_matrix,
                              NUM_FEATURES )
    print 'rmse', rmse

    np.savetxt("/tmp/user_feature_matrix2.dat", user_feature_matrix)
    np.savetxt("/tmp/movie_feature_matrix2.dat", movie_feature_matrix)

if __name__ == "__main__": 
    main()
