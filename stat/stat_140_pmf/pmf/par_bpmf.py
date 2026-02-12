import os, numpy as np, json, util
from multiprocessing import Process
import glob, shutil

# Configuration
# Set this to your dataset
DATASET = "large"  # "small" or "large"

if DATASET == "small":
    n_users, n_movies, global_mu = 610, 9742, 3.5019
    d = "/opt/Downloads/ml-latest-small"
else:
    #n_users, n_movies, global_mu = 200948, 87584, 3.5
    n_users, n_movies, global_mu = 200948, 40404, 3.5
    d = "/opt/Downloads/ml-32m"

o = "/dev/shm"
USER_MOVIE_FILE = d + "/user_movie.txt"
MOVIE_USER_FILE = d + "/movie_user.txt"

N_WORKERS = 6
N_ITER = 250
K = 25
BURN_IN = 10
THIN = 3

rng = np.random.default_rng()

lambda_U, lambda_V = 5.0, 5.0
lambda_b, lambda_c = 2.0, 2.0
sigma2 = 1.0                 

eyeK = np.eye(K)

def prep_matrices():
    U = 0.1 * rng.standard_normal((n_users, K))
    V = 0.1 * rng.standard_normal((n_movies, K))
    
    print ('\nInitializing U,V,b,c with shapes:', U.shape, V.shape)
    np.savetxt(o + "/U.csv", U, delimiter=';',fmt='%1.6f')
    np.savetxt(o + "/V.csv", V, delimiter=';',fmt='%1.6f')

def combine_matrices(prefix):
    files = sorted(glob.glob(o + "/" + prefix + '-[0-9]*.csv'))
    with open(o + "/" + prefix + '.csv', 'wb') as destination:
        for filename in files:
            with open(filename, 'rb') as source:
                shutil.copyfileobj(source, destination)    
    
class GibbsUserJob:
    def __init__(self, ci):
        self.rng = np.random.default_rng()
        # Load current global state
        self.U = np.loadtxt(o + "/U.csv", delimiter=';')
        self.V = np.loadtxt(o + "/V.csv", delimiter=';')
        self.ci = ci
        # Tracking bounds
        self.min_i = float('inf')
        self.max_i = -1

    def exec(self, line):
        if not line.strip(): return
        row = line.split('|')
        i = int(row[0])
        
        # Track range
        if i < self.min_i: self.min_i = i
        if i > self.max_i: self.max_i = i

        ratings = json.loads(row[1])
        tsum = np.zeros((K,K))
        for j,rating in ratings.items(): tsum += np.dot(self.V[int(j)].reshape(K,1), \
                                                        self.V[int(j)].reshape(1,K) )
        
        invsigma = lambda_U*eyeK + (1.0 / sigma2) * tsum
        sigma = np.linalg.inv(invsigma)
        mutmp = np.array([ self.V[int(j)]*(rating-global_mu) for j,rating in ratings.items()]).sum(axis=0)
        mu = np.dot(sigma, 1.0/sigma2 * mutmp)
        self.U[i, :] = rng.multivariate_normal(mu, sigma)

        
    def post(self):
        if self.max_i == -1: return # No data processed
        
        # Save specific slices
        np.savetxt(f"{o}/U-{self.ci}.csv", self.U[self.min_i : self.max_i + 1], delimiter=';', fmt='%1.6f')

class GibbsMovieJob:
    def __init__(self, ci):
        self.rng = np.random.default_rng()
        self.U = np.loadtxt(o + "/U.csv", delimiter=';')
        self.V = np.loadtxt(o + "/V.csv", delimiter=';')
        self.ci = ci
        self.min_j = float('inf')
        self.max_j = -1

    def exec(self, line):
        if not line.strip(): return
        row = line.split('|')
        j = int(row[0])
        
        if j < self.min_j: self.min_j = j
        if j > self.max_j: self.max_j = j

        ratings = json.loads(row[1])
        tsum = np.zeros((K,K))            
        for i,rating in ratings.items(): tsum += np.dot(self.U[int(i)].reshape(K,1), \
                                                        self.U[int(i)].reshape(1,K) )
        
        invsigma = lambda_V*eyeK + (1.0 / sigma2) * tsum
        sigma = np.linalg.inv(invsigma)
        mutmp = np.array([ self.U[int(i)]*(rating-global_mu) for i,rating in ratings.items()]).sum(axis=0)
        mu = np.dot(sigma, 1.0/sigma2 * mutmp)
        self.V[j, :] = rng.multivariate_normal(mu, sigma)
        
        
    def post(self):
        if self.max_j == -1: return        
        np.savetxt(f"{o}/V-{self.ci}.csv", self.V[self.min_j : self.max_j + 1], delimiter=';', fmt='%1.6f')

# Main execution loop
prep_matrices()

U = np.loadtxt(o + "/U.csv", delimiter=';')
V = np.loadtxt(o + "/V.csv", delimiter=';')

U_mean = np.zeros_like(U)
V_mean = np.zeros_like(V)

n_kept = 0

print("\nStarting Gibbs sampling...\n")
    
for iteration in range(N_ITER): 
    print(f'----Iteration {iteration}', end='')    
    
    ps = [Process(target=util.process,args=(USER_MOVIE_FILE, N_WORKERS, GibbsUserJob(ci=i))) for i in range(N_WORKERS)]
    for p in ps: p.start()
    for p in ps: p.join()
        
    combine_matrices("U")

    ps = [Process(target=util.process,args=(MOVIE_USER_FILE, N_WORKERS, GibbsMovieJob(ci=i))) for i in range(N_WORKERS)]
    for p in ps: p.start()
    for p in ps: p.join()
                
    combine_matrices("V")

    U = np.loadtxt(o + "/U.csv", delimiter=';')
    V = np.loadtxt(o + "/V.csv", delimiter=';')

    if iteration >= BURN_IN and (iteration - BURN_IN) % THIN == 0:
        n_kept += 1
        alpha = 1.0 / n_kept
        U_mean += alpha * (U - U_mean)
        V_mean += alpha * (V - V_mean)
        print(f" [sample {n_kept} kept]", end='')

    if iteration % 50 == 0:
        np.savez(
            o + "/bpmf_posterior.npz",
            U=U_mean,
            V=V_mean,
            mu=global_mu,
            n_kept=n_kept,
        )
                
    print()  # Just newline
    
np.savez(
    o + "/bpmf_posterior.npz",
    U=U_mean,
    V=V_mean,
    mu=global_mu,
    n_kept=n_kept,
)

