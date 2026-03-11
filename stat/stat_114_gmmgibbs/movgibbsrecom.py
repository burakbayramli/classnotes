import pandas as pd, numpy as np, os, sys
import re, pickle, csv, json, random
from scipy.special import gammaln
from collections import defaultdict

csv.field_size_limit(sys.maxsize)

#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"
MODEL_FILE = d + "/gmm_model.pkl"
USER_MOVIE_FILE = d + "/user_movie.txt"
picks = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movpicks.csv',index_col=0).to_dict('index')
skips = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movskips.csv',index_col=0).to_dict('index')
genre = pd.read_csv(d + "/movies3.csv",index_col="movieId")['genres'].to_dict()
mov = pd.read_csv(d + "/movies3.csv",index_col="title")['movieId'].to_dict()
mov_id_title = pd.read_csv(d + "/movies3.csv",index_col="movieId")['title'].to_dict()

def mvn_logpdf(x, mu, Sigma):
    D    = len(x)
    diff = x - mu
    sign, logdet = np.linalg.slogdet(Sigma)
    if sign <= 0:
        return -np.inf
    maha = diff @ np.linalg.solve(Sigma, diff)
    return -0.5 * (D * np.log(2 * np.pi) + logdet + maha)

def assign_cluster(my_ratings: dict, model_path: str = MODEL_FILE):
    with open(model_path, "rb") as f:
        m = pickle.load(f)

    R_norm         = m["R_norm"]          # (N, M)
    X              = m["X"]               # (N, 2)
    z_map          = m["z_map"]           # (N,)
    users          = m["users"]           # list len N
    movie_idx      = m["movie_idx"]       # {movie_id -> col}
    reducer        = m["reducer"]         # fitted UMAP
    cluster_params = m["cluster_params"]  # list of K dicts
    K              = m["K"]

    N, M = R_norm.shape

    r_new = np.zeros(M, dtype=np.float32)
    unknown_movies = []
    for mid, rating in my_ratings.items():
        if mid in movie_idx:
            r_new[movie_idx[mid]] = rating
        else:
            unknown_movies.append(mid)

    if unknown_movies:
        print(f"  ⚠  Movies not in training vocabulary (ignored): {unknown_movies}")

    rated_count = (r_new != 0).sum()
    if rated_count == 0:
        raise ValueError("None of your movie ids appear in the training data.")
    print(f"  {rated_count} of your {len(my_ratings)} rated movies found in vocabulary.")

    # Normalise
    norm = np.linalg.norm(r_new)
    if norm == 0:
        raise ValueError("Rating vector is all zeros after vocabulary filtering.")
    r_new_norm = r_new / norm             # (M,)

    sim   = R_norm @ r_new_norm           # (N,)  cosine similarities
    sim   = np.clip(sim, -1.0, 1.0)
    d_new = np.arccos(sim) / np.pi        # (N,)  angular distances ∈ [0,1]

    nn_k    = 10
    nn_idx  = np.argsort(d_new)[:nn_k]
    weights = 1.0 / (d_new[nn_idx] + 1e-9)
    weights /= weights.sum()
    x_new_circle = (weights[:, None] * X[nn_idx]).sum(axis=0)
    print(f"  Embedded position: ({x_new_circle[0]:.4f}, {x_new_circle[1]:.4f})")

    log_scores = np.zeros(K)
    for k in range(K):
        cp = cluster_params[k]
        log_scores[k] = mvn_logpdf(x_new_circle, cp["mu"], cp["Sigma"])

    log_scores -= log_scores.max()
    probs = np.exp(log_scores)
    probs /= probs.sum()

    best_k = int(np.argmax(probs))

    mask_k   = z_map == best_k
    users_k  = np.array(users)[mask_k]
    dists_k  = d_new[mask_k]
    nn_order = np.argsort(dists_k)[:5]

    print("\n" + "═" * 52)
    print(f"  Assigned cluster : {best_k}")
    print(f"  Cluster size     : {mask_k.sum()} users")
    print()
    print("  Cluster probabilities:")
    for k in range(K):
        bar = "█" * int(probs[k] * 30)
        print(f"    C{k}  {probs[k]:.3f}  {bar}")
    print()
    print("  5 nearest neighbours in your cluster:")
    for rank, idx in enumerate(nn_order, 1):
        uid  = users_k[idx]
        dist = dists_k[idx]
        print(f"    {rank}. user {uid:5d}  angular-dist={dist:.4f}")
    print("═" * 52)

    cluster3_users = [users[i] for i in range(len(users)) if z_map[i] == best_k]

    recoms = []
    total_top_d = defaultdict(int)
    
    with open(USER_MOVIE_FILE) as csvfile:   
        rd = csv.reader(csvfile,delimiter='|')
        for i,row in enumerate(rd):
            jrow = json.loads(row[1])
            # if the user exists in the closest users list
            if int(row[0]) in cluster3_users:
                # get this person's movie ratings
                for movid,rating in jrow.items():
                    if int(movid) not in mov_id_title: continue 
                    fres = re.findall('\((\d\d\d\d)\)', mov_id_title[int(movid)])
                    if rating == 5 and \
                       mov_id_title[int(movid)] not in picks and \
                       mov_id_title[int(movid)] not in skips and \
                       'Animation' not in genre[int(movid)] and \
                       'Documentary' not in genre[int(movid)] and \
                       len(fres)>0 and int(fres[0]) > 2010: \
                       total_top_d[mov_id_title[int(movid)]] += 1

    print ("Done collecting.. Writing.. ")
    df = pd.DataFrame(list(total_top_d.items()), columns=['Item', 'Count'])
    df = df.sort_values(by='Count', ascending=False)
    df = df.head(2000)
    df.to_csv("/opt/Downloads/movierecom5.csv",index=None)
                    
def recommend():
    MY_RATINGS = dict((mov[p],float(picks[p]['rating'])) for p in picks if p in mov)
    assign_cluster(MY_RATINGS)
    
if __name__ == "__main__": 
    recommend()
