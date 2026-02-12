import numpy as np, sys, csv, json, os
import pandas as pd

csv.field_size_limit(sys.maxsize)

K = 25
N_ITERS = 100
BURN_IN = 5
THIN = 2

lambda_U = 5.0
lambda_V = 5.0
lambda_b = 2.0
lambda_c = 2.0
sigma2 = 1

d = "/opt/Downloads/ml-latest-small"
USER_MOVIE_FILE = d + "/user_movie.txt"
MOVIE_USER_FILE = d + "/movie_user.txt"

n_users, n_movies, global_mu = 611, 9742, 3.5

rng = np.random.default_rng(42)

U = 0.1 * rng.standard_normal((n_users, K))
V = 0.1 * rng.standard_normal((n_movies, K))

U_mean = np.zeros_like(U)
V_mean = np.zeros_like(V)
n_kept = 0

eyeK = np.eye(K)

for it in range(1, N_ITERS + 1):
    print ('iteration', it)

    with open(USER_MOVIE_FILE, newline="") as csvfile:
        rd = csv.reader(csvfile, delimiter="|")
        for row in rd:
            i = int(row[0])
            ratings = json.loads(row[1])
            tsum = np.zeros((K,K))
            for j,rating in ratings.items(): tsum += np.dot(V[int(j)].reshape(K,1), \
                                                            V[int(j)].reshape(1,K) )
            
            invsigma = lambda_U*eyeK + (1.0 / sigma2) * tsum
            sigma = np.linalg.inv(invsigma)
            mutmp = np.array([ V[int(j)]*(rating-global_mu) for j,rating in ratings.items()]).sum(axis=0)
            mu = np.dot(sigma, 1.0/sigma2 * mutmp)
            U[i, :] = rng.multivariate_normal(mu, sigma)

            
    with open(MOVIE_USER_FILE, newline="") as csvfile:
        rd = csv.reader(csvfile, delimiter="|")
        for row in rd:
            j = int(row[0])
            ratings = json.loads(row[1])
            tsum = np.zeros((K,K))            
            for i,rating in ratings.items(): tsum += np.dot(U[int(i)].reshape(K,1), \
                                                            U[int(i)].reshape(1,K) )
            
            invsigma = lambda_V*eyeK + (1.0 / sigma2) * tsum
            sigma = np.linalg.inv(invsigma)
            mutmp = np.array([ U[int(i)]*(rating-global_mu) for i,rating in ratings.items()]).sum(axis=0)
            mu = np.dot(sigma, 1.0/sigma2 * mutmp)
            V[j, :] = rng.multivariate_normal(mu, sigma)

            
    if it >= BURN_IN and it % THIN == 0:
        n_kept += 1
        alpha = 1.0 / n_kept
        U_mean += alpha * (U - U_mean)
        V_mean += alpha * (V - V_mean)        

np.savez(
    d + "/bpmf_posterior.npz",
    U=U_mean,
    V=V_mean,
    mu=mu,
    user_ids=np.arange(n_users),
    movie_ids=np.arange(n_movies),
)

print("Saved bpmf_posterior.npz")
