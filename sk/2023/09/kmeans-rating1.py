import os, numpy as np, util, json, pandas as pd
import numpy.linalg as lin

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
        self.s = np.zeros((K,M))
        self.N = np.zeros((K,M))
                
    def exec(self,line):
        id,jsval = line.split("|")
        ratings = json.loads(jsval)
        my_k = int(self.cluster_ass[int(id)-1])
        for mov,rating in ratings.items():
            self.s[my_k, self.movie_id_int[mov]] += rating
            self.N[my_k, self.movie_id_int[mov]] += 1.0
                
    def post(self):
        means = np.divide(self.s, self.N, out=np.zeros_like(self.s), where=self.N>0)
        np.savez('data/means-%d' % self.iter_no, means)

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

for iter_no in range(1,10):
    print ('iteration=================================', iter_no)
    print ('means')
    util.process(file_name=fin1, N=1, hookobj = KMeans1Job(0,iter_no))
    print ('cluster assignmets')
    util.process(file_name=fin1, N=1, hookobj = KMeans2Job(0,iter_no))
