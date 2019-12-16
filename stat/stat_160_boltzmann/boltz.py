import numpy as np

class Boltzmann:

    def __init__(self,n_iter=100,eta=0.1,sample_size=100,init_sample_size=10):
        self.n_iter = n_iter
        self.eta = eta
        self.sample_size = sample_size
        self.init_sample_size = init_sample_size
    
    def sigmoid(self, u):
        return 1./(1.+np.exp(-u));

    def draw(self, Sin,T):
        """
        Bir Gibbs gecisi yaparak dagilimdan bir orneklem al
        """
        D=Sin.shape[0]
        S=Sin.copy()
        rand = np.random.rand(D,1)
        for i in xrange(D):
            h=np.dot(T[i,:],S)
            S[i]=rand[i]<self.sigmoid(h);
        return S

    def sample(self, T):
        N=T.shape[0]        
        # sigmoid(0) her zaman 0.5 olacak
        s=np.random.rand(N)<self.sigmoid(0)
        # alttaki dongu atlama / gozonune alinmayacak degerler icin        
        for k in xrange(self.init_sample_size):
            s=self.draw(s,T)
        S=np.zeros((N,self.sample_size))
        S[:,0]=s
        # simdi degerleri toplamaya basla
        for i in xrange(1,self.sample_size):
            S[:,i]=self.draw(S[:,i-1],T)
        return S.T

    def normc(self, X):
        """
        normalizasyon sabitini dondur
        """
        def f(x): return np.exp(0.5 * np.dot(np.dot(x,self.W), x))
        S = 2*self.sample(self.W)-1
        # sozluk icinde anahtar tek x degeri boylece bir
        # olasilik degeri sadece bir kere toplanir
        res = dict((tuple(s),f(s)) for s in S)
        return np.sum(res.values())
    
    def fit(self, X):
        W=np.zeros((X.shape[1],X.shape[1]))
        W_data=np.dot(X.T,X)/X.shape[1];
        for i in range(self.n_iter):
            if i % 10 == 0: print 'Iteration', i
            S = self.sample(W)
            S = (S*2)-1
            W_guess=np.dot(S.T,S)/S.shape[1];
            W += self.eta * (W_data - W_guess)
            np.fill_diagonal(W, 0)
        self.W = W
        self.C = self.normc(X)

    def predict_proba(self, X):
        return np.diag(np.exp(0.5 * np.dot(np.dot(X, self.W), X.T))) / self.C
