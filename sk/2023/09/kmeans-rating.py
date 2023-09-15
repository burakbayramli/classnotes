import os, numpy as np, util, json, pandas as pd
import numpy.linalg as lin
import json, csv

d = "."
def preproc():
    fin = d + "/ratings.csv"
    fout = d + "/ratings-json.csv"
    curruser = 0
    row_dict = {}
    fout = open(fout, "w")
    with open(fin) as csvfile:   
        rd = csv.reader(csvfile,delimiter=',')
        headers = {k: v for v, k in enumerate(next(rd))}
        for row in rd:
            if row[headers['userId']] != curruser:
                fout.write(str(curruser) + "|")
                fout.write(json.dumps(row_dict))
                fout.write("\n")
                fout.flush()
                curruser = row[headers['userId']]
                row_dict = {}       
            row_dict[int(row[headers['movieId']])] = float(row[headers['rating']])
    fout.close()

K = 5
M = 9742
U = 610
dir = ".."

fin1  = "../ratings-json.csv"

class KMeans1Job:
    def __init__(self,ci,iter_no):
        self.ci = ci
        self.iter_no = iter_no
        self.movie_id_int = json.loads(open("data/movie_id_int.json").read())
        self.cluster_ass = np.load("data/cluster-assignments-%d.npz" % int(self.iter_no-1))['arr_0']
        self.m = np.zeros((K,M),dtype=np.float)
        
    def exec(self,line):
        id,jsval = line.split("|")
        ratings = json.loads(jsval)
        my_k = int(self.cluster_ass[int(id)-1])
        for mov,rating in ratings.items():
            self.m[my_k, self.movie_id_int[mov]] = self.m[my_k, self.movie_id_int[mov]] +  (rating - self.m[my_k, self.movie_id_int[mov]]) / int(id)
                
    def post(self):
        np.savez('data/means-%d' % self.iter_no,self.m)
        #df = pd.DataFrame(self.m)
        #df.T.to_csv('data/out-m.csv')

class KMeans2Job:
    def __init__(self,ci,iter_no):
        self.ci = ci
        self.iter_no = iter_no
        self.means = np.load("data/means-%d.npz" % int(self.iter_no))['arr_0']
        self.movie_id_int = json.loads(open("data/movie_id_int.json").read())
        self.cluster_ass = np.zeros((U,))

        
    def exec(self,line):
        id,jsval = line.split("|")
        ratings = json.loads(jsval)
        vec = np.zeros((1,M))
        for mov,rating in ratings.items():
            vec[0,self.movie_id_int[str(mov)]] = rating
        nearest = np.argmin(lin.norm(vec - self.means,axis=1))
        self.cluster_ass[int(id)-1] = nearest
        
    def post(self):
        np.savez('data/cluster-assignments-%d' % self.iter_no,self.cluster_ass)
        
        
def prepare():

    df = pd.read_csv(dir + "/movies.csv")
    d = df.reset_index().set_index('movieId')['index'].to_dict()
    fout = open("data/movie_id_int.json","w")
    fout.write(json.dumps(d))
    fout.close()

    df = pd.read_csv(dir + "/movies.csv")
    d = df.reset_index().set_index('title')['index'].to_dict()
    fout = open("data/movie_title_int.json","w")
    fout.write(json.dumps(d))
    fout.close()
    
    cluster_ass = np.random.randint(K,size=U)
    np.savez('data/cluster-assignments-0',cluster_ass)

    
prepare()
#util.process(file_name='../ratings-json.csv', N=1, hookobj = KMeans1Job(0,1))
#util.process(file_name='../ratings-json.csv', N=1, hookobj = KMeans2Job(0,1))

for i in range(1,10):
    util.process(file_name=fin1, N=1, hookobj = KMeans1Job(0,i))
    util.process(file_name=fin1, N=1, hookobj = KMeans2Job(0,i))
