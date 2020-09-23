import numpy as np
from memo import *

def rec_dag_sp2(W, s, t, debug=True): 
    parent = {}
    @memo                                      
    def d(u):                                  
        if u == t: return 0                    
        distances = [W[u][v]+d(v) for v in W[u]]
        min_dist = min(distances)
        parent[u] = list(W[u])[np.argmin(distances)]
        if debug: print 'Geri donus,',u,'uzerindeyiz, mesafe=',min_dist
        return min_dist
    return d(s), parent

