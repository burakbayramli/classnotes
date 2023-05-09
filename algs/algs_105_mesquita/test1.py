import itertools, os
import pandas as pd, sys
import numpy as np, matplotlib.pylab as plt

Q = 1.0 ; T = 1.0

class Game:

    def __init__(self,df):
        self.df = df.copy()
        self.df_orig = self.df.copy()
        # dictionaries of df variables - used for speedy access
        self.df_capability = df.Capability.to_dict()    
        self.df_position = df.Position.to_dict()    
        self.df_salience = df.Salience.to_dict()    
        self.max_pos = df.Position.max()
        self.min_pos = df.Position.min()

    def weighted_median(self):
        df = self.df.copy()
        df['w'] = df.Capability*df.Salience
        df = df.sort_index(by='Position',ascending=True)
        df['w'] = df['w'] / df['w'].sum()
        df['w'] = df['w'].cumsum()       
        return float(df[df['w']>=0.5].head(1).Position)

    def mean(self):
        return (self.df.Capability*self.df.Position*self.df.Salience).sum() / \
               (self.df.Capability*self.df.Salience).sum()

    def Usi_i(self,i,j,ri=1.):
        tmp1 = self.df_position[i]-self.df_position[j]
        tmp2 = self.max_pos-self.min_pos
        return 2. - 4.0 * ( (0.5-0.5*np.abs(float(tmp1)/tmp2) )**ri)

    def Ufi_i(self,i,j,ri=1.):
        tmp1 = self.df_position[i]-self.df_position[j]
        tmp2 = self.df.Position.max()-self.df.Position.min()
        return 2. - 4.0 * ( (0.5+0.5*np.abs(float(tmp1)/tmp2) )**ri )

    def Usq_i(self,i,ri=1.):
        return 2.-(4.*(0.5**ri))

    def Ui_ij(self,i,j):
        tmp1 = self.df_position[i] - self.df_position[j]
        tmp2 = self.max_pos-self.min_pos
        return 1. - 2.*np.abs(float(tmp1) / tmp2) 

    def v(self,i,j,k):
        return self.df_capability[i]*self.df_salience[i]*(self.Ui_ij(i,j)-self.Ui_ij(i,k)) 

    def Pi(self,i):
        l = np.array([[i,j,k] for (j,k) in itertools.combinations(range(len(self.df)), 2 ) if i!=j and i!=k])
        U_filter = np.array(map(lambda (i,j,k): self.Ui_ij(j,i)>self.Ui_ij(i,k), l))
        lpos = l[U_filter]
        tmp1 = np.sum(map(lambda (i,j,k): self.v(j,i,k), lpos))
        tmp2 = np.sum(map(lambda (i,j,k): np.abs(self.v(j,i,k)), l))
        return float(tmp1)/tmp2

    def Ubi_i(self,i,j,ri=1):
        tmp1 = np.abs(self.df_position[i] - self.weighted_median()) + \
               np.abs(self.df_position[i] - self.df_position[j])
        tmp2 = np.abs(self.max_pos-self.min_pos)
        return 2. - (4. * (0.5 - (0.25 * float(tmp1) / tmp2))**ri)

    def Uwi_i(self,i,j,ri=1):
        tmp1 = np.abs(self.df_position[i] - self.weighted_median()) + \
               np.abs(self.df_position[i] - self.df_position[j])
        tmp2 = np.abs(self.max_pos-self.min_pos)
        return 2. - (4. * (0.5 + (0.25 * float(tmp1) / tmp2))**ri)
        
    def EU_i(self,i,j,r=1):
        term1 = self.df_salience[j]*self.Pi(i)*self.Usi_i(i,j,r)
        term2 = self.df_salience[j]*(1.-self.Pi(i))*self.Ufi_i(i,j,r)
        term3 = (1-self.df_salience[j])*self.Usi_i(i,j,r)
        term4 = Q*self.Usq_i(i,r)
        term5 = (1.-Q)*( T*self.Ubi_i(i,j,r) + (1.-T)*self.Uwi_i(i,j,r) )
        return (term1+term2+term3)-(term4+term5)

    def EU_j(self,i,j,r=1):
        return self.EU_i(j,i,r)
        
    def Ri(self,i):
        # get all j's expect i
        l = [x for x in range(len(self.df)) if x!= i]
        tmp = np.array(map(lambda x: self.EU_j(i,x), l))
        numterm1 = 2*np.sum(tmp)
        numterm2 = (len(self.df)-1)*np.max(tmp)
        numterm3 = (len(self.df)-1)*np.min(tmp)
        return float(numterm1-numterm2-numterm3) / (numterm2-numterm3)

    def ri(self,i):
        Ri_tmp = self.Ri(i)
        return (1-Ri_tmp/3.) / (1+Ri_tmp/3.)

    def do_round(self):
        df_new = self.df.copy()        
        # reinit
        self.df_capability = self.df.Capability.to_dict()    
        self.df_position = self.df.Position.to_dict()    
        self.df_salience = self.df.Salience.to_dict()    
        self.max_pos = self.df.Position.max()
        self.min_pos = self.df.Position.min()

        self.df_orig_position = self.df_orig.Position.to_dict()    
        
        offers = [list() for i in range(len(self.df))]
        ris = [self.ri(i) for i in range(len(self.df))]
        for (i,j) in itertools.combinations(range(len(self.df)), 2 ):
            if i==j: continue
            eui = self.EU_i(i,j,r=ris[i])
            euj = self.EU_j(i,j,r=ris[j])            
            if eui > 0 and euj > 0  and np.abs(eui) > np.abs(euj):
                # conflict - actor i has upper hand
                j_moves = self.df_position[i]-self.df_orig_position[j]
                print i,j,eui,euj,'conflict', i, 'wins', j, 'moves',j_moves
                offers[j].append(j_moves)
            elif eui > 0 and euj > 0  and np.abs(eui) < np.abs(euj):
                # conflict - actor j has upper hand
                i_moves = self.df_position[j]-self.df_orig_position[i]
                print i,j,eui,euj,'conflict', j, 'wins', i, 'moves',i_moves
                offers[i].append(i_moves)
            elif eui > 0 and euj < 0 and np.abs(eui) > np.abs(euj):
                # compromise - actor i has the upper hand
                print i,j,eui,euj,'compromise', i, 'upper hand'
                xhat = (self.df_position[i]-self.df_orig_position[j]) * np.abs(euj/eui)
                offers[j].append(xhat)
            elif eui < 0 and euj > 0 and np.abs(eui) < np.abs(euj):
                # compromise - actor j has the upper hand
                print i,j,eui,euj,'compromise', j, 'upper hand'
                xhat = (self.df_position[j]-self.df_orig_position[i]) * np.abs(eui/euj)
                offers[i].append(xhat)
            elif eui > 0 and euj < 0 and np.abs(eui) < np.abs(euj):
                # capitulation - actor i has upper hand
                j_moves = self.df_position[i]-self.df_orig_position[j]
                print i,j,eui,euj,'capitulate', i, 'wins', j, 'moves',j_moves
                offers[j].append(j_moves)
            elif eui < 0 and euj > 0 and np.abs(eui) > np.abs(euj):
                # capitulation - actor j has upper hand
                i_moves = self.df_position[j]-self.df_orig_position[i]
                print i,j,eui,euj,'capitulate', j, 'wins', i, 'moves',i_moves
                offers[i].append(i_moves)
            else:
                print i,j,eui,euj,'nothing'

        # choose offer requiring minimum movement, then
        # update positions
        print offers
        #exit()
        df_new['offer'] = map(lambda x: 0 if len(x)==0 else x[np.argmin(np.abs(x))],offers)
        df_new.loc[:,'Position'] = df_new.Position + df_new.offer

        # in case max/min is exceeded
        df_new.loc[df_new['Position']>self.max_pos,'Position'] = self.max_pos
        df_new.loc[df_new['Position']<self.min_pos,'Position'] = self.min_pos
        self.df = df_new
        print self.df

if __name__ == "__main__": 
         
    if len(sys.argv) < 3:
        print "\nUsage: run.py [CSV] [ROUNDS]"
        exit()

    df = pd.read_csv(sys.argv[1]); print df
    df.Position = df.Position.astype(float)
    df.Capability = df.Capability.astype(float)
    df.Salience = df.Salience/100.

    game = Game(df)
    print 'weighted_median', game.weighted_median(), 'mean', game.mean()

    results = pd.DataFrame(index=df.index)
    for i in range(int(sys.argv[2])):
        results[i] = game.df.Position
        df = game.do_round(); print df
        print 'weighted_median', game.weighted_median(), 'mean', game.mean()

    results =  results.T
    results.columns = game.df.Actor
    print results
    results.plot()
    fout = '%s/out-%s.png' % (os.environ['TEMP'],sys.argv[1].replace(".csv","-csv"))
    plt.savefig(fout)

        
