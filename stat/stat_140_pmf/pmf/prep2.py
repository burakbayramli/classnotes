import csv

#d = "/opt/Downloads/ml-latest-small"
d = "/opt/Downloads/ml-32m"

def remap_movielens_ids(movies_input, ratings_input):
    # Dictionaries to store {old_id: new_id}
    movie_id_map = {}
    user_id_map = {} # Added for users
    
    # 1. Process movies.csv and create the map
    print("Processing movies and creating ID map...")
    with open(movies_input, mode='r', encoding='utf-8') as f_in, \
         open(d + '/movies3.csv', mode='w', newline='', encoding='utf-8') as f_out:
        
        reader = csv.DictReader(f_in)
        writer = csv.DictWriter(f_out, fieldnames=reader.fieldnames)
        writer.writeheader()
        
        for i, row in enumerate(reader):
            old_id = row['movieId']
            new_id = str(i) # Start from 0, 1, 2...
            
            movie_id_map[old_id] = new_id
            row['movieId'] = new_id
            writer.writerow(row)

    # 2. Process ratings.csv using both maps
    print("Applying changes to ratings...")
    with open(ratings_input, mode='r', encoding='utf-8') as f_in, \
         open(d + '/ratings3.csv', mode='w', newline='', encoding='utf-8') as f_out:
        
        reader = csv.DictReader(f_in)
        writer = csv.DictWriter(f_out, fieldnames=reader.fieldnames)
        writer.writeheader()
        
        for row in reader:
            old_movie_id = row['movieId']
            old_user_id = row['userId']
            
            # Remap Movie ID
            if old_movie_id in movie_id_map:
                row['movieId'] = movie_id_map[old_movie_id]
                
                # Remap User ID: Map 1-based to 0-based
                # Since there are no gaps, we can use the existing ID or a counter
                if old_user_id not in user_id_map:
                    user_id_map[old_user_id] = str(len(user_id_map))
                
                row['userId'] = user_id_map[old_user_id]
                writer.writerow(row)
            else:
                continue

    print("Success! Generated 'movies3.csv' and 'ratings3.csv' with remapped IDs.")
    
if __name__ == "__main__":
    remap_movielens_ids(d + '/movies2.csv', d + '/ratings2.csv')
