import pandas as pd

#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"

def preprocess_movielens(rating_file, movie_file, threshold=100):
    """
    Filters Movielens data to keep only movies with more than 'threshold' ratings.
    """
    print("Reading ratings data...")
    # Load ratings to count frequency
    ratings_df = pd.read_csv(rating_file)

    # 1. First Pass: Count ratings per movie
    print("Counting ratings per movie...")
    # Group by movie ID and count ratings
    movie_counts = ratings_df['movieId'].value_counts()
    
    # Create a dictionary mapping movieId to count
    counts_dict = movie_counts.to_dict()

    # 2. Filtering: Identify movies above the threshold
    popular_movies = movie_counts[movie_counts > threshold].index
    print(f"Keeping {len(popular_movies)} movies with > {threshold} ratings.")

    # 3. Second Pass: Create filtered files
    print("Filtering ratings and creating ratings2.csv...")
    # Filter ratings dataframe
    filtered_ratings = ratings_df[ratings_df['movieId'].isin(popular_movies)]
    filtered_ratings.to_csv(d + '/ratings2.csv', index=False)

    print("Filtering movies and creating movies2.csv...")
    # Load and filter movie metadata
    movies_df = pd.read_csv(movie_file)
    filtered_movies = movies_df[movies_df['movieId'].isin(popular_movies)]
    filtered_movies.to_csv(d + '/movies2.csv', index=False)

    print("Done.")

# --- Execution ---
# Replace these paths with your actual file locations
RATING_CSV = d + '/ratings.csv'
MOVIE_CSV = d + '/movies.csv'
THRESHOLD = 10

preprocess_movielens(RATING_CSV, MOVIE_CSV, THRESHOLD)
