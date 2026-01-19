#
# Recommend movies based on Grouplens ratings file
#
# https://grouplens.org/datasets/movielens/latest/
#
# Download the full file, and unzip in a known location set in d
#
from scipy.sparse import csr_matrix
import scipy.sparse.linalg, json
import pandas as pd, numpy as np
import os, sys, re, csv

csv.field_size_limit(sys.maxsize)

d = "/opt/Downloads/ml-32m"

def sim_recommend():
    fin = d + "/user_movie.txt"
    picks = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movpicks.csv',index_col=0).to_dict('index')
    skips = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movskips.csv',index_col=0).to_dict('index')
    mov = pd.read_csv(d + "/movies.csv",index_col="title")['movieId'].to_dict()
    genre = pd.read_csv(d + "/movies.csv",index_col="movieId")['genres'].to_dict()
    mov_id_title = pd.read_csv(d + "/movies.csv",index_col="movieId")['title'].to_dict()
    picks_json = dict((mov[p],float(picks[p]['rating'])) for p in picks if p in mov)
    picks_norm = np.sqrt(sum(v**2 for v in picks_json.values()))
    res = []
    with open(fin) as csvfile:   
        rd = csv.reader(csvfile,delimiter='|')
        for i,row in enumerate(rd):
            jrow = json.loads(row[1])
            jrow_norm = np.sqrt(sum(v**2 for v in jrow.values()))
            dp = sum(jrow[key]*picks_json.get(int(key), 0) for key in jrow)
            dp = dp / ((picks_norm*jrow_norm)+1e-10)
            res.append([row[0],dp])
            if i % 1e4 == 0: print (i,dp)
          

    df = pd.DataFrame(res).set_index(0)
    df = df.sort_values(by=1,ascending=False).head(400)
    df = df.to_dict()[1] # the final list of close users

    recoms = []
    with open(fin) as csvfile:   
        rd = csv.reader(csvfile,delimiter='|')
        for i,row in enumerate(rd):
            jrow = json.loads(row[1])
            # if the user exists in the closest users list
            if str(row[0]) in df:
                # get this person's movie ratings
                for movid,rating in jrow.items():
                    if int(movid) not in mov_id_title: continue 
                    fres = re.findall('\((\d\d\d\d)\)', mov_id_title[int(movid)])
                    if rating >= 4 and \
                       mov_id_title[int(movid)] not in picks and \
                       mov_id_title[int(movid)] not in skips and \
                       'Animation' not in genre[int(movid)] and \
                       'Documentary' not in genre[int(movid)] and \
                       len(fres)>0 and int(fres[0]) > 2010: \
                       # add the picks of this user multiplied by his closeness
                       recoms.append([mov_id_title[int(movid)],rating*df[row[0]]])

    df = pd.DataFrame(recoms)
    df = df.sort_values(1,ascending=False)
    df = df.drop_duplicates(0)
    df.to_csv("/opt/Downloads/movierecom3.csv",index=None,header=False)
        
if __name__ == "__main__":      
    sim_recommend()
                
