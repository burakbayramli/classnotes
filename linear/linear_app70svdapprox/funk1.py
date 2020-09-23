# Requires Movielens 100k data 
from scipy.io import mmread, mmwrite
import numpy as np, time, sys
from numba import jit
import os

def get_user_ratings(user_id, review_matrix):
    user_reviews = review_matrix[user_id]
    user_reviews = user_reviews.toarray().ravel()
    user_rated_movies, = np.where(user_reviews > 0)
    user_ratings = user_reviews[user_rated_movies]
    return user_ratings

def get_movie_ratings(movie_id, review_matrix):
    movie_reviews = review_matrix[:, movie_id]
    movie_reviews = movie_reviews.toarray().ravel()
    movie_rated_users, = np.where(movie_reviews > 0)
    movie_ratings = movie_reviews[movie_rated_users]
    return movie_ratings

def calculate_global_average(review_matrix):
    all_reviews = review_matrix.toarray().ravel()
    average = np.average(all_reviews, weights=(all_reviews > 0))
    return average

def compute_averages(A, user_pseudo_average_ratings, movie_pseudo_average_ratings, global_average):
    AA = A.tocsr()
    for user in range(len(user_pseudo_average_ratings)):
        user_pseudo_average_ratings[user] = \
            calculate_pseudo_average_user_rating(user, AA, global_average)

    for movie in range(len(movie_pseudo_average_ratings)):
        movie_pseudo_average_ratings[movie] = \
            calculate_pseudo_average_movie_rating(movie, AA, global_average)

def create_user_feature_matrix(review_matrix, NUM_FEATURES, FEATURE_INIT_VALUE):
    num_users = review_matrix.shape[0]
    user_feature_matrix = 1./NUM_FEATURES * np.random.randn(NUM_FEATURES, num_users).astype(np.float32)
    return user_feature_matrix

def create_movie_feature_matrix(review_matrix, NUM_FEATURES, FEATURE_INIT_VALUE):
    num_movies = review_matrix.shape[1]
    movie_feature_matrix = 1./NUM_FEATURES * np.random.randn(NUM_FEATURES, num_movies).astype(np.float32)
    return movie_feature_matrix

def calculate_pseudo_average_movie_rating(movie_id, review_matrix, global_average):
    movie_ratings = get_movie_ratings(movie_id, review_matrix)
    k = 25
    return (global_average * k + np.sum(movie_ratings)) / (k + np.size(movie_ratings))

def calculate_pseudo_average_user_rating(user_id, review_matrix, global_average):
    user_ratings = get_user_ratings(user_id, review_matrix)
    k = 25
    return (global_average * k + np.sum(user_ratings)) / (k + np.size(user_ratings))

@jit(nopython=True)
def predict_rating(user_id, movie_id, user_feature_matrix, movie_feature_matrix):
    rating = 1.
    for f in range(user_feature_matrix.shape[0]):
        rating += user_feature_matrix[f, user_id] * movie_feature_matrix[f, movie_id]        
    if rating > 5: rating = 5
    elif rating < 1: rating = 1
    return rating

@jit(nopython=True)
def sgd_inner(feature, A_row, A_col, A_data, user_feature_matrix, movie_feature_matrix, global_average, user_pseudo_average_ratings, movie_pseudo_average_ratings,NUM_FEATURES):
    K = 0.015
    LEARNING_RATE = 0.001
    squared_error = 0
    for k in range(len(A_data)):
        user_id = A_row[k]
        movie_id = A_col[k]
        rating = A_data[k]
        p = predict_rating(user_id, movie_id, user_feature_matrix, movie_feature_matrix)
        u_diff = user_pseudo_average_ratings[user_id] - global_average
        m_diff = movie_pseudo_average_ratings[movie_id] - global_average
        err = rating - global_average - u_diff - m_diff - p
            
        squared_error += err ** 2

        user_feature_value = user_feature_matrix[feature, user_id]
        movie_feature_value = movie_feature_matrix[feature, movie_id]
        user_feature_matrix[feature, user_id] += \
            LEARNING_RATE * (err * movie_feature_value - K * user_feature_value)
        movie_feature_matrix[feature, movie_id] += \
            LEARNING_RATE * (err * user_feature_value - K * movie_feature_value)

    return squared_error

def calculate_features(A_row, A_col, A_data, user_feature_matrix, movie_feature_matrix, global_average, user_pseudo_average_ratings, movie_pseudo_average_ratings,NUM_FEATURES):
    MIN_IMPROVEMENT = 0.0001
    MIN_ITERATIONS = 100
    rmse = 0
    last_rmse = 0
    for feature in xrange(NUM_FEATURES):
        iter = 0
        while (iter < MIN_ITERATIONS) or  (rmse < last_rmse - MIN_IMPROVEMENT):
            last_rmse = rmse
            squared_error = sgd_inner(feature, A_row, A_col, A_data, user_feature_matrix, movie_feature_matrix, global_average, user_pseudo_average_ratings, movie_pseudo_average_ratings,NUM_FEATURES)
            rmse = np.sqrt(squared_error / len(A_data))
            iter += 1
        print ('RMSE = %f' % rmse)
        print ('Feature = %d' % feature)
    return last_rmse

if __name__ == "__main__": 
 
    LAMBDA = 0.02
    FEATURE_INIT_VALUE = 0.1
    NUM_FEATURES = 20

    A = mmread('%s/Downloads/A_m100k_train' % os.environ['HOME'])

    user_feature_matrix = create_user_feature_matrix(A, NUM_FEATURES, FEATURE_INIT_VALUE)
    movie_feature_matrix = create_movie_feature_matrix(A, NUM_FEATURES, FEATURE_INIT_VALUE)
    global_average = calculate_global_average(A)

    users, movies = A.nonzero()
    A = A.tocoo()

    user_pseudo_average_ratings = np.array([np.nan for x in range(np.max(users)+1) ])
    movie_pseudo_average_ratings = np.array([np.nan for x in range(np.max(movies)+1) ])

    compute_averages(A, user_pseudo_average_ratings, movie_pseudo_average_ratings, global_average)
    rmse = calculate_features(A.row, A.col, A.data, user_feature_matrix, movie_feature_matrix, global_average, user_pseudo_average_ratings, movie_pseudo_average_ratings, NUM_FEATURES )
    print 'rmse', rmse

    np.savetxt("/tmp/user_pseudo_average_ratings1.dat", user_pseudo_average_ratings)
    np.savetxt("/tmp/movie_pseudo_average_ratings1.dat", movie_pseudo_average_ratings)
    np.savetxt("/tmp/user_feature_matrix1.dat", user_feature_matrix)
    np.savetxt("/tmp/movie_feature_matrix1.dat", movie_feature_matrix)
    with open("/tmp/global_average.dat", 'w') as f: f.write(str(global_average))
