import numpy as np, re, pandas as pd
import os

# --- Configuration ---
top_n = 1000
# Should match lambda_U used in training
lambda_U = 5.0 
global_mu = 3.5
#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"
posterior_file = d + "/bpmf_posterior.npz"

# 1. Load the trained model parameters
print(f"Loading model from {posterior_file}...")
data = np.load(posterior_file)
U = data['U'] # User matrix (n_users x K)
V = data['V'] # Movie matrix (n_movies x K)
K = V.shape[1]

print("Loading personal ratings...")
picks = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movpicks.csv',index_col=0).to_dict('index')
mov_df = pd.read_csv(d + "/movies3.csv",index_col="title")
mov_title_to_id = mov_df['movieId'].to_dict()
mov_id_to_title = pd.read_csv(d + "/movies3.csv",index_col="movieId")['title'].to_dict()

my_ratings = dict((mov_title_to_id[m], picks[m]['rating']) for m in picks if m in mov_title_to_id)
rated_movie_ids = set(my_ratings.keys())

print("Inferring user latent vector using actual ratings...")
rated_vectors = []
adjusted_ratings = []
for m_id, rating in my_ratings.items():
    if m_id < V.shape[0]: 
        rated_vectors.append(V[m_id])
        # Center ratings around global mean
        adjusted_ratings.append(rating - global_mu)
            
if not rated_vectors:
    print("No ratings found to infer user vector. Cannot recommend.")
    exit()

V_rated = np.array(rated_vectors) # Matrix of rated movie vectors (N_rated x K)
y = np.array(adjusted_ratings)     # Vector of centered ratings (N_rated)

# Solving the Ridge Regression formula:
# u_self = (V_rated^T * V_rated + lambda_U * I)^-1 * V_rated^T * y
A = np.dot(V_rated.T, V_rated) + lambda_U * np.eye(K)
b = np.dot(V_rated.T, y)
u_self = np.linalg.solve(A, b)

# 5. Predict ratings for all movies
print("Predicting ratings for unrated movies...")
all_movie_ids = np.arange(V.shape[0])
predictions = []

for m_id in all_movie_ids:
    if m_id not in rated_movie_ids:
        # Formula: mu + u_self dot V_j
        pred_rating = global_mu + np.dot(u_self, V[m_id])
        predictions.append((m_id, pred_rating))

predictions.sort(key=lambda x: x[1], reverse=True)

print(f"\nTop {top_n} Recommendations for You (Released > 2010):")
print("-" * 40)
count = 0
for i in range(len(predictions)):
    if count >= top_n:
        break        
    m_id, score = predictions[i]
    title = mov_id_to_title.get(m_id, "Unknown Title")
    
    # Your filtering logic
    ys = re.findall('\\((\\d\\d\\d\\d)\\)', title)  
    if len(ys) > 0 and int(ys[0]) > 2010: 
        print(f"{count+1}. {title} (Predicted Rating: {score:.2f})")
        count += 1
