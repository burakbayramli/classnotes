'''
For each Netflix file (streamed in through stdin), it will
convert the file from

movieId:
user,rating,date

into
movieId \tab   user1:rating, user2:rating, ...
'''

import re, sys

movie_id = None
ratings = []
for line in sys.stdin.readlines():
    line = line.rstrip()
    m = re.search(r'(\d+)\:',line)
    if m:
        if movie_id: print str(movie_id) + "\t" + ";".join(ratings)
        movie_id = int(m.group(1))
        ratings = []
    else: 
        tokens = line.split(",")
        ratings.append(tokens[0] + ":" + tokens[1])
    
print str(movie_id) + "\t" + ";".join(ratings)
