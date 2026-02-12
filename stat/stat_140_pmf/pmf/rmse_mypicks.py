import numpy as np
import pandas as pd
import os

# 1 0.8429
# 2 0.8427
# 3 0.8435
# 4 0.8427
# 5 0.8447

# --- Configuration ---
#posterior_file = "bpmf_posterior.npz"
posterior_file = "par/3/bpmf_posterior.npz"
#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"
lambda_U = 5.0 
global_mu = 3.5 

# 1. Load the trained model parameters
data = np.load(posterior_file)
V = data['V'] # Movie matrix (n_movies x K)
K = V.shape[1]

# 2. Load personal ratings and mappings
picks = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movpicks.csv',index_col=0).to_dict('index')
mov_df = pd.read_csv(d + "/movies3.csv",index_col="title")
mov_title_to_id = mov_df['movieId'].to_dict()
mov_id_title = pd.read_csv(d + "/movies3.csv",index_col="movieId")['title'].to_dict()

my_ratings = dict((mov_title_to_id[m], picks[m]['rating']) for m in picks if m in mov_title_to_id)

# 3. Infer user latent vector (u_self) using Ridge Regression
rated_vectors = []
adjusted_ratings = []
actual_rating_values = []
for m_id, rating in my_ratings.items():
    if m_id < V.shape[0]: 
        rated_vectors.append(V[m_id])
        # Center ratings around global mean for inference
        adjusted_ratings.append(rating - global_mu)
        actual_rating_values.append(rating)
        
V_rated = np.array(rated_vectors) # Matrix of rated movie vectors (N_rated x K)
y = np.array(adjusted_ratings)     # Vector of centered ratings (N_rated)

# Solving the Ridge Regression formula:
# u_self = (V_rated^T * V_rated + lambda_U * I)^-1 * V_rated^T * y
A = np.dot(V_rated.T, V_rated) + lambda_U * np.eye(K)
b = np.dot(V_rated.T, y)
u_self = np.linalg.solve(A, b)

# 4. Calculate Test RMSE on Training Ratings
# Formula: predicted = mu + u_self dot V_j

predicted_ratings = []
for m_id in my_ratings.keys():
    if m_id < V.shape[0]:
        pred = global_mu + np.dot(u_self, V[m_id])
        predicted_ratings.append(pred)
        #print (m_id, mov_id_title[m_id], pred, my_ratings[m_id])
    
actual_ratings = np.array(actual_rating_values)
predicted_ratings = np.array(predicted_ratings)

mse = np.mean((actual_ratings - predicted_ratings)**2)
rmse = np.sqrt(mse)

print(f"Personalized Rating RMSE: {rmse:.4f}")
