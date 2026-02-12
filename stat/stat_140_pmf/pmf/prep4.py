import numpy as np, re, pandas as pd
import os

# --- Configuration ---
#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"

# 3. Load personal ratings, mappings, and popularity data
mov_df = pd.read_csv(d + "/movies3.csv",index_col="title")
mov_title_to_id = mov_df['movieId'].to_dict()
mov_id_to_title = pd.read_csv(d + "/movies3.csv",index_col="movieId")['title'].to_dict()

# --- NEW: Calculate Popularity (Average Rating) ---
# Assuming ratings.csv exists in the same directory d
ratings_df = pd.read_csv(d + "/ratings3.csv")
# Calculate mean rating per movie and map to movieId index
movie_averages = ratings_df.groupby('movieId')['rating'].mean()
movie_averages.to_csv(d + "/ratings_avg3.csv")
