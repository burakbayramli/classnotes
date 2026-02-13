import csv
import json
from collections import defaultdict

#dir = "/opt/Downloads/ml-32m"
dir = "/opt/Downloads/ml-latest-small"
input_file = dir + '/ratings3.csv'

test_set = defaultdict(dict)

def user_movie():
    output_file = dir + '/user_movie.txt'
    user_ratings = defaultdict(dict)
    barx = 3    
    with open(input_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        line_count = 0
        for row in reader:
            user_id = row['userId']
            movie_id = row['movieId']
            rating = float(row['rating'])
            barx = barx + (rating - barx) / (line_count+1)
            user_ratings[user_id][movie_id] = rating
            line_count += 1
            if line_count % 1000000 == 0:
                print(f"Processed {line_count:,} ratings...")

    print(f"Total ratings processed: {line_count:,}")
    print(f"Total users: {len(user_ratings):,}")
    print("Writing output file...")
    with open(output_file, 'w', encoding='utf-8') as f:
        for user_id in sorted(user_ratings.keys(), key=int):
            ratings_json = json.dumps(user_ratings[user_id])
            f.write(f"{user_id}|{ratings_json}\n")            
            
    print(f"Output written to: {output_file}")
    print ("Global mean", barx)
    print("Done!")

def movie_user():

    output_file = dir + '/movie_user.txt'
    movie_ratings = defaultdict(dict)
    with open(input_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        line_count = 0
        for row in reader:
            user_id = row['userId']
            movie_id = row['movieId']
            rating = float(row['rating'])
            movie_ratings[movie_id][user_id] = rating
            line_count += 1
            if line_count % 1000000 == 0:
                print(f"Processed {line_count:,} ratings...")

    print(f"Total ratings processed: {line_count:,}")
    print(f"Total movies: {len(movie_ratings):,}")
    print("Writing output file...")
    with open(output_file, 'w', encoding='utf-8') as f:
        for movie_id in sorted(movie_ratings.keys(), key=int):
            ratings_json = json.dumps(movie_ratings[movie_id])
            f.write(f"{movie_id}|{ratings_json}\n")

    print(f"Output written to: {output_file}")
    print("Done!")    

if __name__ == "__main__":
    
    user_movie()
    movie_user()
