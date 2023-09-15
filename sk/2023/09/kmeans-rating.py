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

import os, numpy as np, util, json, pandas as pd
import numpy.linalg as lin, glob

K = 5
M = 9742
U = 610
dir = ".."

fin1  = "../ratings-json.csv"

np.random.seed(0)

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
            if self.m[my_k, self.movie_id_int[mov]] == 0.0:
                self.m[my_k, self.movie_id_int[mov]] = rating
            else: self.m[my_k, self.movie_id_int[mov]] = self.m[my_k, self.movie_id_int[mov]] +  (rating - self.m[my_k, self.movie_id_int[mov]]) / int(id)
                
    def post(self):
        np.savez('data/means-%d-%d' % (self.iter_no,self.ci),self.m)

class KMeans2Job:
    def __init__(self,ci,iter_no):
        self.ci = ci
        self.iter_no = iter_no
        self.means = np.load("data/means-%d.npz" % int(self.iter_no))['arr_0']
        self.movie_id_int = json.loads(open("data/movie_id_int.json").read())
        self.cluster_ass = {}

        
    def exec(self,line):
        id,jsval = line.split("|")
        ratings = json.loads(jsval)
        vec = np.zeros((1,M))
        for mov,rating in ratings.items():
            vec[0,self.movie_id_int[str(mov)]] = rating
        nearest = np.argmin(lin.norm(vec - self.means,axis=1))
        self.cluster_ass[int(id)-1] = int(nearest)
        
    def post(self):
        #np.savez('data/cluster-assignments-%d' % self.iter_no,self.cluster_ass)
        fout = open("data/cluster-assignments-%d-%d.json" % (self.iter_no,self.ci),"w")
        fout.write(json.dumps(self.cluster_ass))
        fout.close()
        
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

N = 2 # processes
    
def combine_means(iter_no):
    means = np.zeros((K,M))
    for f in glob.glob("data/means-%d-*.npz" % iter_no):
        print (f)
        means += np.load(f)['arr_0']
    means = means / N
    np.savez('data/means-%d' % (iter_no),means)
    
def combine_cluster_ass(iter_no):
    cluster_ass = np.zeros((U,))
    for f in glob.glob("data/cluster-assignments-%d-*.json" % iter_no):
        print (f)
        d = json.loads(open(f).read())        
        for id,k in d.items():
            cluster_ass[int(id)] = k
        
    np.savez('data/cluster-assignments-%d' % (iter_no),cluster_ass)
    
    
        
prepare()

#util.process(file_name='../ratings-json.csv', N=N, hookobj = KMeans1Job(0,1))
#util.process(file_name='../ratings-json.csv', N=N, hookobj = KMeans1Job(1,1))
#combine_means(1)
#util.process(file_name=fin1, N=2, hookobj = KMeans2Job(0,iter_no=1))
#util.process(file_name=fin1, N=2, hookobj = KMeans2Job(1,iter_no=1))
#combine_cluster_ass(1)

for iter_no in range(1,10):
    print ('iteration=================================', iter_no)
    print ('means')
    util.process(file_name=fin1, N=2, hookobj = KMeans1Job(0,iter_no))
    util.process(file_name=fin1, N=2, hookobj = KMeans1Job(1,iter_no))
    combine_means(iter_no)
    print ('cluster assignmets')
    util.process(file_name=fin1, N=2, hookobj = KMeans2Job(0,iter_no))
    util.process(file_name=fin1, N=2, hookobj = KMeans2Job(1,iter_no))
    combine_cluster_ass(iter_no)
