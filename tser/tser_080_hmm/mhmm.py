## Taken from pyhmmtb project who, in turn, claims to have
## converted Kevin Murphy's mix Gauss HMM code. 

import numpy as np, string
from numpy.dual import cholesky
from numpy.linalg.linalg import LinAlgError

EPS = 2.2204e-16

def sqdist(p, q, A=None):
    '''
    % SQDIST      Squared Euclidean or Mahalanobis distance.
    % SQDIST(p,q)   returns m(i,j) = (p(:,i) - q(:,j))'*(p(:,i) - q(:,j)).
    % SQDIST(p,q,A) returns m(i,j) = (p(:,i) - q(:,j))'*A*(p(:,i) - q(:,j)).
    
    %  From Tom Minka's lightspeed toolbox
    '''
    
    d, pn = p.shape;
    d, qn = q.shape;
    
    if A==None:
        pmag = np.sum(np.multiply(p, p), 0)
        qmag = np.sum(np.multiply(q, q), 0)
        m = np.tile(qmag, (pn, 1)) + np.tile(pmag.T, (1, qn)) - 2 * np.dot(p.T,q)
        #m = ones(pn,1)*qmag + pmag'*ones(1,qn) - 2*p'*q;
    else:
        Ap = np.dot(A,p)
        Aq = np.dot(A,q)
        pmag = np.sum(np.multiply(p, Ap), 0)
        qmag = np.sum(np.multiply(q, Aq), 0)
        m = np.tile(qmag, (pn, 1)) + np.tile(pmag.T, (1, qn)) - 2 * np.dot(p.T,Aq)
    return m


def normalise(A, dim=None):
    '''
    % NORMALISE Make the entries of a (multidimensional) array sum to 1
    % [M, c] = normalise(A)
    % c is the normalizing constant
    %
    % [M, c] = normalise(A, dim)
    % If dim is specified, we normalise the specified dimension only,
    % otherwise we normalise the whole array.
    '''
    
    if dim==None:
        z = np.sum(A[:])
        # Set any zeros to one before dividing
        # This is valid, since c=0 => all i. A(i)=0 => the answer should be 0/1=0
        s = z + (z==0)
        M = A / (1.*s)
    elif dim==0: # normalize each column
        z = np.sum(A, axis=0)
        s = z + (z==0)
        #M = A ./ (d'*ones(1,size(A,1)))';
        M = np.multiply(A, np.tile(1.*s, A.shape[0], 1))
    else:
        # Keith Battocchi - v. slow because of repmat
        z=np.sum(A,dim)
        s = z + (z==0)
        L=A.shape[dim]
        d=A.ndim
        v=np.ones((d,))
        v[dim]=L;
        #c=repmat(s,v);
        c=np.tile(1.*s,v.T)
        M=np.divide(A, c)

    return M, z

def em_converged(loglik, previous_loglik, threshold=1e-4, check_increased=True):
    '''
    % EM_CONVERGED Has EM converged?
    % [converged, decrease] = em_converged(loglik, previous_loglik, threshold)
    %
    % We have converged if the slope of the log-likelihood function falls below 'threshold', 
    % i.e., |f(t) - f(t-1)| / avg < threshold,
    % where avg = (|f(t)| + |f(t-1)|)/2 and f(t) is log lik at iteration t.
    % 'threshold' defaults to 1e-4.
    %
    % This stopping criterion is from Numerical Recipes in C p423
    %
    % If we are doing MAP estimation (using priors), the likelihood can decrase,
    % even though the mode of the posterior is increasing.
    '''

    converged = False
    decrease = True
    
    if check_increased:
        if loglik - previous_loglik < -1e-3: # allow for a little imprecision
            print '******likelihood decreased from %6.4f to %6.4f!\n' % (previous_loglik, loglik)
            decrease = True
            converged = False
            return converged, decrease
            
    delta_loglik = np.abs(loglik - previous_loglik)
    if delta_loglik == np.inf:
        converged = False
    else:
        avg_loglik = (np.abs(loglik) + np.abs(previous_loglik) + EPS)/2.;
        if (delta_loglik / avg_loglik) < threshold:
            converged = True

    return converged, decrease


'''
Created on 13.06.2013

@author: christian
'''

import numpy as np
from scipy.constants.constants import pi
from numpy.dual import det, inv, eig
import logging

EPS = 2.2204e-16

def gaussian_prob(x, m, C, use_log=False):
    '''
    % GAUSSIAN_PROB Evaluate a multivariate Gaussian density.
    % p = gaussian_prob(X, m, C)
    % p(i) = N(X(:,i), m, C) where C = covariance matrix and each COLUMN of x is a datavector
    
    % p = gaussian_prob(X, m, C, 1) returns log N(X(:,i), m, C) (to prevents underflow).
    %
    % If X has size dxN, then p has size Nx1, where N = number of examples
    '''

    m = np.asmatrix(m)
    if m.shape[0]==1:
        m = m.T
    
    d, N = x.shape

    M = np.tile(m, (1,N)) # replicate the mean across columns
    denom = (2*pi)**(0.5*d)*np.sqrt(np.abs(det(C)))
    mahal = np.sum(np.multiply(np.dot((x-M).T, inv(C)),(x-M).T),1) # Chris Bregler's trick
    if np.any(mahal<0):
        logging.warning('mahal < 0 => C is not psd')
    if use_log:
        p = -0.5*mahal - np.log(denom);
    else:
        p = np.exp(-0.5*mahal) / (denom+EPS);
    
    return np.asarray(p)[:,0]

def sample_discrete(prob, r=1, c=None):
    '''
    % SAMPLE_DISCRETE Like the built in 'rand', except we draw from a non-uniform discrete distrib.
    % M = sample_discrete(prob, r, c)
    %
    % Example: sample_discrete([0.8 0.2], 1, 10) generates a row vector of 10 random integers from {1,2},
    % where the prob. of being 1 is 0.8 and the prob of being 2 is 0.2.
    '''
    
    assert prob.ndim == 1
    
    n = len(prob)
    
    if c==None:
        c = r
    
    R = np.random.rand(r, c)
    M = np.zeros((r, c), dtype=int)
    cumprob = np.cumsum(prob[:])
    
    if n < r*c:
        for i in range(0, n):
            M = M + (R > cumprob[i])
    else:
        # loop over the smaller index - can be much faster if length(prob) >> r*c
        cumprob2 = cumprob[0:-1]
        for i in range(0,r):
            for j in range(0,c):
                M[i,j] = np.sum(R[i,j] > cumprob2)
    
    # Slower, even though vectorized
    #cumprob = reshape(cumsum([0 prob(1:end-1)]), [1 1 n]);
    #M = sum(R(:,:,ones(n,1)) > cumprob(ones(r,1),ones(c,1),:), 3);
    
    # convert using a binning algorithm
    #M=bindex(R,cumprob);
    
    return M
    
def gaussian_sample(mu, covar, nsamp):
    '''
    %GSAMP    Sample from a Gaussian distribution.
    %
    %    Description
    %
    %    X = GSAMP(MU, COVAR, NSAMP) generates a sample of size NSAMP from a
    %    D-dimensional Gaussian distribution. The Gaussian density has mean
    %    vector MU and covariance matrix COVAR, and the matrix X has NSAMP
    %    rows in which each row represents a D-dimensional sample vector.
    %
    %    See also
    %    GAUSS, DEMGAUSS
    %
    
    %    Copyright (c) Ian T Nabney (1996-2001)
    '''
    
    d = covar.shape[0]
    
    mu = np.reshape(mu, (1, d)) # Ensure that mu is a row vector
    
    eigval, evec = eig(covar)
    
    eigval = np.diag(eigval)
    
    coeffs = np.dot(np.random.randn(nsamp, d), np.sqrt(eigval))
    
    x = np.dot(np.ones((nsamp, 1)),mu) + np.dot(coeffs, evec.T)
    
    return x


'''
Created on 12.06.2013

@author: christian
'''

import numpy as np

def mc_sample(prior, trans, len, numex=1):
    '''
    % SAMPLE_MC Generate random sequences from a Markov chain.
    % STATE = SAMPLE_MC(PRIOR, TRANS, LEN) generates a sequence of length LEN.
    %
    % STATE = SAMPLE_MC(PRIOR, TRANS, LEN, N) generates N rows each of length LEN.
    '''
    
    trans = np.asarray(trans)
    
    S = np.zeros((numex,len), dtype=int);
    for i in range(0,numex):
        S[i, 0] = sample_discrete(prior)
        for t in range(1, len):
            S[i, t] = sample_discrete(trans[S[i,t-1],:])
    return S

def mk_leftright_transmat(Q, p):
    '''
    % MK_LEFTRIGHT_TRANSMAT Q = num states, p = prob on (i,i), 1-p on (i,i+1)
    % function transmat = mk_leftright_transmat(Q, p)
    '''
    
    transmat = p*np.diag(np.ones((Q,))) + (1-p)*np.diag(np.ones((Q-1,)),1)
    transmat[Q-1,Q-1]=1
    
    return transmat

'''
Created on 13.06.2013

@author: christian
'''

def isposdef(a):
    '''
    % ISPOSDEF   Test for positive definite matrix.
    %    ISPOSDEF(A) returns 1 if A is positive definite, 0 otherwise.
    %    Using chol is much more efficient than computing eigenvectors.
    
    %  From Tom Minka's lightspeed toolbox
    '''
    
    try:
        cholesky(a)
        return True
    except LinAlgError:
        return False
    
import numpy as np
from scipy.constants.constants import pi
from numpy.dual import inv
import logging
from sklearn.mixture.gmm import GMM

def mixgauss_prob(data, mu, Sigma, mixmat = None, unit_norm = False):
    '''
    % EVAL_PDF_COND_MOG Evaluate the pdf of a conditional mixture of Gaussians
    % function [B, B2] = eval_pdf_cond_mog(data, mu, Sigma, mixmat, unit_norm)
    %
    % Notation: Y is observation, M is mixture component, and both may be conditioned on Q.
    % If Q does not exist, ignore references to Q=j below.
    % Alternatively, you may ignore M if this is a conditional Gaussian.
    %
    % INPUTS:
    % data(:,t) = t'th observation vector 
    %
    % mu(:,k) = E[Y(t) | M(t)=k] 
    % or mu(:,j,k) = E[Y(t) | Q(t)=j, M(t)=k]
    %
    % Sigma(:,:,j,k) = Cov[Y(t) | Q(t)=j, M(t)=k]
    % or there are various faster, special cases:
    %   Sigma() - scalar, spherical covariance independent of M,Q.
    %   Sigma(:,:) diag or full, tied params independent of M,Q. 
    %   Sigma(:,:,j) tied params independent of M. 
    %
    % mixmat(k) = Pr(M(t)=k) = prior
    % or mixmat(j,k) = Pr(M(t)=k | Q(t)=j) 
    % Not needed if M is not defined.
    %
    % unit_norm - optional; if 1, means data(:,i) AND mu(:,i) each have unit norm (slightly faster)
    %
    % OUTPUT:
    % B(t) = Pr(y(t)) 
    % or
    % B(i,t) = Pr(y(t) | Q(t)=i) 
    % B2(i,k,t) = Pr(y(t) | Q(t)=i, M(t)=k) 
    %
    % If the number of mixture components differs depending on Q, just set the trailing
    % entries of mixmat to 0, e.g., 2 components if Q=1, 3 components if Q=2,
    % then set mixmat(1,3)=0. In this case, B2(1,3,:)=1.0.
    '''

    if mu.ndim == 1:
        d = len(mu)
        Q = 1
        M = 1
    elif mu.ndim == 2:
        d, Q = mu.shape
        M = 1
    else:
        d, Q, M = mu.shape;
    
    d, T = data.shape
    
    if mixmat == None:
        mixmat = np.asmatrix(np.ones((Q,1)))
    
    # B2 = zeros(Q,M,T); % ATB: not needed allways
    # B = zeros(Q,T);
    
    if np.isscalar(Sigma):
        mu = np.reshape(mu, (d, Q * M))
        if unit_norm:  # (p-q)'(p-q) = p'p + q'q - 2p'q = n+m -2p'q since p(:,i)'p(:,i)=1
            # avoid an expensive repmat
            print('unit norm')
            # tic; D = 2 -2*(data'*mu)'; toc 
            D = 2 - 2 * np.dot(mu.T * data)
            # tic; D2 = sqdist(data, mu)'; toc
            D2 = sqdist(data, mu).T
            assert(approxeq(D,D2)) 
        else:
            D = sqdist(data, mu).T
        del mu 
        del data  # ATB: clear big old data
        # D(qm,t) = sq dist between data(:,t) and mu(:,qm)
        logB2 = -(d / 2.) * np.log(2 * pi * Sigma) - (1 / (2. * Sigma)) * D  # det(sigma*I) = sigma^d
        B2 = np.reshape(np.asarray(np.exp(logB2)), (Q, M, T))
        del logB2  # ATB: clear big old data
      
    elif Sigma.ndim == 2:  # tied full
        mu = np.reshape(mu, (d, Q * M))
        D = sqdist(data, mu, inv(Sigma)).T
        # D(qm,t) = sq dist between data(:,t) and mu(:,qm)
        logB2 = -(d/2)*np.log(2*pi) - 0.5*logdet(Sigma) - 0.5*D;
        #denom = sqrt(det(2*pi*Sigma));
        #numer = exp(-0.5 * D);
        #B2 = numer/denom;
        B2 = np.reshape(np.asarray(np.exp(logB2)), (Q, M, T))
      
    elif Sigma.ndim==3: # tied across M
        B2 = np.zeros((Q,M,T))
        for j in range(0,Q):
            # D(m,t) = sq dist between data(:,t) and mu(:,j,m)
            if isposdef(Sigma[:,:,j]):
                D = sqdist(data, np.transpose(mu[:,j,:], (0, 2, 1)), inv(Sigma[:,:,j])).T
                logB2 = -(d / 2) * np.log(2 * pi) - 0.5 * logdet(Sigma[:, :, j]) - 0.5 * D;
                B2[j, :, :] = np.exp(logB2);
            else:
                logging.error('mixgauss_prob: Sigma(:,:,q=%d) not psd\n' % j)
      
    else:  # general case
        B2 = np.zeros((Q, M, T))
        for j in range(0, Q):
            for k in range(0, M):
                # if mixmat(j,k) > 0
                B2[j, k, :] = gaussian_prob(data, mu[:, j, k], Sigma[:, :, j, k]);
    
    # B(j,t) = sum_k B2(j,k,t) * Pr(M(t)=k | Q(t)=j) 
    
    # The repmat is actually slower than the for-loop, because it uses too much memory
    # (this is true even for small T).
    
    # B = squeeze(sum(B2 .* repmat(mixmat, [1 1 T]), 2));
    # B = reshape(B, [Q T]); % undo effect of squeeze in case Q = 1
      
    B = np.zeros((Q, T))
    if Q < T:
        for q in range(0, Q):
            # B(q,:) = mixmat(q,:) * squeeze(B2(q,:,:)); % squeeze chnages order if M=1
            B[q, :] = np.dot(mixmat[q, :], B2[q, :, :])  # vector * matrix sums over m #TODO: had to change this. Is this correct?
    else:
        for t in range(0, T):
            B[:, t] = np.sum(np.asarray(np.multiply(mixmat, B2[:, :, t])), 1)  # sum over m
    # t=toc;fprintf('%5.3f\n', t)
    
    # tic
    # A = squeeze(sum(B2 .* repmat(mixmat, [1 1 T]), 2));
    # t=toc;fprintf('%5.3f\n', t)
    # assert(approxeq(A,B)) % may be false because of round off error

    return B, B2

def mixgauss_Mstep(w, Y, YY, YTY, **kwargs):
    '''
    % MSTEP_COND_GAUSS Compute MLEs for mixture of Gaussians given expected sufficient statistics
    % function [mu, Sigma] = Mstep_cond_gauss(w, Y, YY, YTY, varargin)
    %
    % We assume P(Y|Q=i) = N(Y; mu_i, Sigma_i)
    % and w(i,t) = p(Q(t)=i|y(t)) = posterior responsibility
    % See www.ai.mit.edu/~murphyk/Papers/learncg.pdf.
    %
    % INPUTS:
    % w(i) = sum_t w(i,t) = responsibilities for each mixture component
    %  If there is only one mixture component (i.e., Q does not exist),
    %  then w(i) = N = nsamples,  and 
    %  all references to i can be replaced by 1.
    % YY(:,:,i) = sum_t w(i,t) y(:,t) y(:,t)' = weighted outer product
    % Y(:,i) = sum_t w(i,t) y(:,t) = weighted observations
    % YTY(i) = sum_t w(i,t) y(:,t)' y(:,t) = weighted inner product
    %   You only need to pass in YTY if Sigma is to be estimated as spherical.
    %
    % Optional parameters may be passed as 'param_name', param_value pairs.
    % Parameter names are shown below; default values in [] - if none, argument is mandatory.
    %
    % 'cov_type' - 'full', 'diag' or 'spherical' ['full']
    % 'tied_cov' - 1 (Sigma) or 0 (Sigma_i) [0]
    % 'clamped_cov' - pass in clamped value, or [] if unclamped [ [] ]
    % 'clamped_mean' - pass in clamped value, or [] if unclamped [ [] ]
    % 'cov_prior' - Lambda_i, added to YY(:,:,i) [0.01*eye(d,d,Q)]
    %
    % If covariance is tied, Sigma has size d*d.
    % But diagonal and spherical covariances are represented in full size.
    '''

    cov_type = kwargs.pop('cov_type', 'full')
    tied_cov = kwargs.pop('tied_cov', 0)
    clamped_cov = kwargs.pop('clamped_cov', [])
    clamped_mean = kwargs.pop('clamped_mean', None)
    cov_prior = kwargs.pop('cov_prior', None)
    
    Y = np.asmatrix(Y)
    
    Ysz, Q = Y.shape
    N = np.sum(w, 0)
    if cov_prior==None:
        # cov_prior = zeros(Ysz, Ysz, Q);
        # for q=1:Q
        #  cov_prior(:,:,q) = 0.01*cov(Y(:,q)');
        # end
        cov_prior = np.transpose(np.tile(0.01 * np.eye(Ysz), (Q, 1, 1)), (1,2,0))
    # YY = reshape(YY, [Ysz Ysz Q]) + cov_prior; % regularize the scatter matrix
    YY = np.reshape(YY, (Ysz, Ysz, Q))
    
    # Set any zero weights to one before dividing
    # This is valid because w(i)=0 => Y(:,i)=0, etc
    w = w + (w == 0);
                
    if clamped_mean!=None:
        mu = np.asmatrix(clamped_mean)
    else:
        # eqn 6
        # mu = Y ./ repmat(w(:)', [Ysz 1]);% Y may have a funny size
        mu = np.asmatrix(np.zeros((Ysz, Q)))
        for i in range(0, Q):
            mu[:, i] = Y[:, i] / w[i]
    
    if not len(clamped_cov) == 0:
        Sigma = clamped_cov
        return mu, Sigma
    
    if not tied_cov:
        Sigma = np.zeros((Ysz, Ysz, Q))
        for i in range(0, Q):
            if cov_type[0] == 's':
                # eqn 17
                s2 = (1 / Ysz) * ((YTY[i] / w[i]) - np.dot(mu[:, i].T, mu[:, i]))
                Sigma[:, :, i] = s2 * np.eye(Ysz)
            else:
                # eqn 12
                SS = YY[:, :, i] / w[i] - np.dot(mu[:, i], mu[:, i].T)
                if cov_type[0] == 'd':
                    SS = np.diag(np.diag(SS))
                Sigma[:, :, i] = SS
    else:  # tied cov
        if cov_type[0] == 's':
            # eqn 19
            s2 = (1 / (N * Ysz)) * (np.sum(YTY, 1) + np.sum(np.multiply(np.diag(np.dot(mu.T, mu)), w)))
            Sigma = s2 * np.eye(Ysz)
        else:
            SS = np.zeros((Ysz, Ysz))
            # eqn 15
            for i in range(1, Q):  # probably could vectorize this...
                SS = SS + YY[:, :, i] / N - np.dot(mu[:, i], mu[:, i].T)
            if cov_type[0] == 'd':
                Sigma = np.diag(np.diag(SS))
            else:
                Sigma = SS
    
    if tied_cov:
        Sigma = np.tile(Sigma, (1, 1, Q))
    Sigma = Sigma + cov_prior

    return np.asarray(mu), Sigma

def mixgauss_init(M, data, cov_type, method='kmeans'):
    '''
    % MIXGAUSS_INIT Initial parameter estimates for a mixture of Gaussians
    % function [mu, Sigma, weights] = mixgauss_init(M, data, cov_type. method)
    %
    % INPUTS:
    % data(:,t) is the t'th example
    % M = num. mixture components
    % cov_type = 'full', 'diag' or 'spherical'
    % method = 'rnd' (choose centers randomly from data) or 'kmeans' (needs netlab)
    %
    % OUTPUTS:
    % mu(:,k) 
    % Sigma(:,:,k) 
    % weights(k)
    '''
    
    if isinstance(data, list):
        data = np.hstack(data)
    elif data.ndim==3:
        O, T, N = data.shape
        data = np.reshape(np.transpose(data, (0, 2, 1)), (O, T*N))
    d, T = data.shape
    
    if method=='rnd':
        C = np.atleast_2d(np.cov(data))
        Sigma = np.transpose(np.tile(np.diag(np.diag(C))*0.5, (M, 1, 1)), (2, 1, 0))
        # Initialize each mean to a random data point
        indices = np.arange(T)
        np.random.shuffle(indices)
        mu = data[:,indices[0:M]]
        weights, _ = normalise(np.ones((M,1)))
    elif method=='kmeans':
        
        gmm = GMM(n_components=M, covariance_type=cov_type,
                  thresh=1e-2, min_covar=1e-3,
                  n_iter=5, n_init=1, params='wmc', init_params='wmc')
        
        gmm.fit(data.T)
        
        mu = gmm.means_.T
        weights = np.asmatrix(gmm.weights_).T
        covars = gmm.covars_
        print covars.shape
        
        Sigma = np.zeros((d,d,M))
        for m in range(M):
            if cov_type=='diag':
                Sigma[:,:,m] = np.diag(covars[m,:])
            elif cov_type=='full':
                Sigma[:,:,m] = covars[:,:,m]
            elif cov_type=='spherical':
                Sigma[:,:,m] = covars[m] * np.eye(d)
    
    return mu, Sigma, weights
    
def mhmm_sample(T, numex, initial_prob, transmat, mu, Sigma, mixmat=None):
    '''
    % SAMPLE_MHMM Generate random sequences from an HMM with (mixtures of) Gaussian output.
    % [obs, hidden] = sample_mhmm(T, numex, initial_prob, transmat, mu, Sigma, mixmat)
    %
    % INPUTS:
    % T - length of each sequence
    % numex - num. sequences
    % init_state_prob(i) = Pr(Q(1) = i)
    % transmat(i,j) = Pr(Q(t+1)=j | Q(t)=i)
    % mu(:,j,k) = mean of Y(t) given Q(t)=j, M(t)=k
    % Sigma(:,:,j,k) = cov. of Y(t) given Q(t)=j, M(t)=k
    % mixmat(j,k) = Pr(M(t)=k | Q(t)=j) : set to ones(Q,1) or omit if single mixture
    %
    % OUTPUT:
    % obs(:,t,l) = observation vector at time t for sequence l
    % hidden(t,l) = the hidden state at time t for sequence l
    '''
    
    assert initial_prob.ndim == 1
    
    Q = len(initial_prob);
    if mixmat==None:
        mixmat = np.ones((Q,1))
    O = mu.shape[0]
    hidden = np.zeros((T, numex))
    obs = np.zeros((O, T, numex))
    
    hidden = mc_sample(initial_prob, transmat, T, numex).T
    for i in range(0,numex):
        for t in range(0,T):
            q = hidden[t,i]
            m = np.asscalar(sample_discrete(mixmat[q,:], 1, 1))
            obs[:,t,i] = gaussian_sample(mu[:,q,m], Sigma[:,:,q,m], 1)
    
    return obs, hidden

def mhmm_em(data, prior, transmat, mu, Sigma, mixmat=None, **kwargs):
    '''
    % LEARN_MHMM Compute the ML parameters of an HMM with (mixtures of) Gaussians output using EM.
    % [ll_trace, prior, transmat, mu, sigma, mixmat] = learn_mhmm(data, ...
    %   prior0, transmat0, mu0, sigma0, mixmat0, ...) 
    %
    % Notation: Q(t) = hidden state, Y(t) = observation, M(t) = mixture variable
    %
    % INPUTS:
    % data{ex}(:,t) or data(:,t,ex) if all sequences have the same length
    % prior(i) = Pr(Q(1) = i), 
    % transmat(i,j) = Pr(Q(t+1)=j | Q(t)=i)
    % mu(:,j,k) = E[Y(t) | Q(t)=j, M(t)=k ]
    % Sigma(:,:,j,k) = Cov[Y(t) | Q(t)=j, M(t)=k]
    % mixmat(j,k) = Pr(M(t)=k | Q(t)=j) : set to [] or ones(Q,1) if only one mixture component
    %
    % Optional parameters may be passed as 'param_name', param_value pairs.
    % Parameter names are shown below; default values in [] - if none, argument is mandatory.
    %
    % 'max_iter' - max number of EM iterations [10]
    % 'thresh' - convergence threshold [1e-4]
    % 'verbose' - if 1, print out loglik at every iteration [1]
    % 'cov_type' - 'full', 'diag' or 'spherical' ['full']
    %
    % To clamp some of the parameters, so learning does not change them:
    % 'adj_prior' - if 0, do not change prior [1]
    % 'adj_trans' - if 0, do not change transmat [1]
    % 'adj_mix' - if 0, do not change mixmat [1]
    % 'adj_mu' - if 0, do not change mu [1]
    % 'adj_Sigma' - if 0, do not change Sigma [1]
    %
    % If the number of mixture components differs depending on Q, just set  the trailing
    % entries of mixmat to 0, e.g., 2 components if Q=1, 3 components if Q=2,
    % then set mixmat(1,3)=0. In this case, B2(1,3,:)=1.0.
    '''
    
    max_iter = kwargs.pop('max_iter', 10)
    thresh = kwargs.pop('thresh', 1e-4)
    verbose = kwargs.pop('verbose', True)
    cov_type = kwargs.pop('cov_type', 'full')
    adj_prior = kwargs.pop('adj_prior', True)
    adj_trans = kwargs.pop('adj_trans', True)
    adj_mix = kwargs.pop('adj_mix', True)
    adj_mu = kwargs.pop('adj_mu', True)
    adj_Sigma = kwargs.pop('adj_Sigma', True)
      
    previous_loglik = -np.Inf
    loglik = 0
    converged = False
    num_iter = 1
    LL = []

    if not isinstance(data, list):
        data = [data[:,:,i] for i in range(data.shape[2])]
    numex = len(data)
    
    O = data[0].shape[0]
    Q = len(prior)
    if mixmat==None:
        mixmat = np.ones((Q,1))
    M = mixmat.shape[1]
    if M == 1:
        adj_mix = False
    
    while (num_iter <= max_iter) and not converged:
        # E step
        loglik, exp_num_trans, exp_num_visits1, postmix, m, ip, op = ess_mhmm(prior, transmat, mixmat, mu, Sigma, data)
      
        # M step
        if adj_prior:
            prior, _ = normalise(exp_num_visits1)
        if adj_trans:
            transmat, _ = mk_stochastic(exp_num_trans)
        if adj_mix:
            mixmat, _ = mk_stochastic(postmix)
        if adj_mu or adj_Sigma:
            postmixx = np.reshape(np.transpose(postmix), (M*Q,))
            mm = np.reshape(np.transpose(m,(0,2,1)), (O, M*Q))
            opp = np.reshape(np.transpose(op, (0,1,3,2)), (O*O, M*Q))
            ipp = np.reshape(np.transpose(ip), (M*Q,))
            mu2, Sigma2 = mixgauss_Mstep(postmixx, mm, opp, ipp, cov_type=cov_type)
            if adj_mu:
                mu = np.transpose(np.reshape(mu2, (O, M, Q)), (0,2,1))
            if adj_Sigma:
                Sigma = np.transpose(np.reshape(Sigma2, (O, O, M, Q)), (0, 1, 3, 2))
      
        if verbose:
            print 'iteration %d, loglik = %f' % (num_iter, loglik)
        num_iter =  num_iter + 1
        converged, _ = em_converged(loglik, previous_loglik, thresh)
        previous_loglik = loglik;
        LL.append(loglik);
        
    return LL, prior, transmat, mu, Sigma, mixmat

def ess_mhmm(prior, transmat, mixmat, mu, Sigma, data):
    '''
    % ESS_MHMM Compute the Expected Sufficient Statistics for a MOG Hidden Markov Model.
    %
    % Outputs:
    % exp_num_trans(i,j)   = sum_l sum_{t=2}^T Pr(Q(t-1) = i, Q(t) = j| Obs(l))
    % exp_num_visits1(i)   = sum_l Pr(Q(1)=i | Obs(l))
    %
    % Let w(i,k,t,l) = P(Q(t)=i, M(t)=k | Obs(l))
    % where Obs(l) = Obs(:,:,l) = O_1 .. O_T for sequence l
    % Then 
    % postmix(i,k) = sum_l sum_t w(i,k,t,l) (posterior mixing weights/ responsibilities)
    % m(:,i,k)   = sum_l sum_t w(i,k,t,l) * Obs(:,t,l)
    % ip(i,k) = sum_l sum_t w(i,k,t,l) * Obs(:,t,l)' * Obs(:,t,l)
    % op(:,:,i,k) = sum_l sum_t w(i,k,t,l) * Obs(:,t,l) * Obs(:,t,l)'
    '''

    verbose = False

    # [O T numex] = size(data);
    numex = len(data)
    O = data[0].shape[0]
    Q = len(prior)
    M = mixmat.shape[1]
    exp_num_trans = np.zeros((Q, Q));
    exp_num_visits1 = np.zeros((Q, 1));
    postmix = np.zeros((Q, M));
    m = np.zeros((O, Q, M));
    op = np.zeros((O, O, Q, M));
    ip = np.zeros((Q, M));

    mix = M > 1

    loglik = 0
    if verbose:
        print 'forwards-backwards example # '
    for ex in range(0, numex):
        if verbose:
            print '%d ' % ex
        # obs = data(:,:,ex);
        obs = data[ex]
        T = obs.shape[1]
        if mix:
            B, B2 = mixgauss_prob(obs, mu, Sigma, mixmat)
            alpha, beta, gamma, current_loglik, xi_summed, gamma2 = fwdback(prior, transmat, B, obslik2=B2, mixmat=mixmat, compute_xi=True, compute_gamma2=True)
        else:
            B, B2 = mixgauss_prob(obs, mu, Sigma)
            alpha, beta, gamma, current_loglik, xi_summed, _ = fwdback(prior, transmat, B)
        loglik = loglik + current_loglik
        if verbose:
            print 'll at ex %d = %f\n' % (ex, loglik)
        
        exp_num_trans = exp_num_trans + xi_summed  # sum(xi,2)
        exp_num_visits1 = exp_num_visits1 + gamma[:, 0]
        
        if mix:
            postmix = postmix + np.sum(gamma2, 2)
        else:
            postmix = postmix + np.sum(gamma, 1) 
            gamma2 = np.reshape(gamma, (Q, 1, T))  # gamma2(i,m,t) = gamma(i,t)
        for i in range(0, Q):
            for k in range(0, M):
                w = np.reshape(gamma2[i, k, :], (1, T))  # w(t) = w(i,k,t,l)
                wobs = np.multiply(obs,w)  # np.repmat(w, [O 1]) # wobs(:,t) = w(t) * obs(:,t)
                m[:, i, k] = m[:, i, k] + np.sum(wobs, 1)  # m(:) = sum_t w(t) obs(:,t)
                op[:, :, i, k] = op[:, :, i, k] + np.dot(wobs, obs.T)  # op(:,:) = sum_t w(t) * obs(:,t) * obs(:,t)'
                ip[i, k] = ip[i, k] + np.sum(np.sum(np.multiply(wobs, obs), 1))  # ip = sum_t w(t) * obs(:,t)' * obs(:,t)
    if verbose:
        print

    return loglik, exp_num_trans, np.asarray(exp_num_visits1)[:,0], postmix, m, ip, op

def mk_stochastic(T):
    '''
    % MK_STOCHASTIC Ensure the argument is a stochastic matrix, i.e., the sum over the last dimension is 1.
    % [T,Z] = mk_stochastic(T)
    %
    % If T is a vector, it will sum to 1.
    % If T is a matrix, each row will sum to 1.
    % If T is a 3D array, then sum_k T(i,j,k) = 1 for all i,j.
    
    % Set zeros to 1 before dividing
    % This is valid since S(j) = 0 iff T(i,j) = 0 for all j
    '''
    
    T = np.asfarray(T)

    if T.ndim==1 or (T.ndim==2 and (T.shape[0]==1 or T.shape[1]==1)): # isvector
        T,Z = normalise(T)
    elif T.ndim==2: # matrix
        T = np.asmatrix(T)
        Z = np.sum(T,1) 
        S = Z + (Z==0)
        norm = np.tile(S, (1, T.shape[1]))
        T = np.divide(T, norm)
    else: # multi-dimensional array
        ns = T.shape
        T = np.asmatrix(np.reshape(T, (np.prod(ns[0:-1]), ns[-1])))
        Z = np.sum(T,1)
        S = Z + (Z==0)
        norm = np.tile(S, (1, ns[-1]))
        T = np.divide(T, norm)
        T = np.reshape(np.asarray(T), ns)

    return T,Z

def fwdback(init_state_distrib, transmat, obslik, **kwargs):
    '''
    % FWDBACK Compute the posterior probs. in an HMM using the forwards backwards algo.
    %
    % [alpha, beta, gamma, loglik, xi, gamma2] = fwdback(init_state_distrib, transmat, obslik, ...)
    %
    % Notation:
    % Y(t) = observation, Q(t) = hidden state, M(t) = mixture variable (for MOG outputs)
    % A(t) = discrete input (action) (for POMDP models)
    %
    % INPUT:
    % init_state_distrib(i) = Pr(Q(1) = i)
    % transmat(i,j) = Pr(Q(t) = j | Q(t-1)=i)
    %  or transmat{a}(i,j) = Pr(Q(t) = j | Q(t-1)=i, A(t-1)=a) if there are discrete inputs
    % obslik(i,t) = Pr(Y(t)| Q(t)=i)
    %   (Compute obslik using eval_pdf_xxx on your data sequence first.)
    %
    % Optional parameters may be passed as 'param_name', param_value pairs.
    % Parameter names are shown below; default values in [] - if none, argument is mandatory.
    %
    % For HMMs with MOG outputs: if you want to compute gamma2, you must specify
    % 'obslik2' - obslik(i,j,t) = Pr(Y(t)| Q(t)=i,M(t)=j)  []
    % 'mixmat' - mixmat(i,j) = Pr(M(t) = j | Q(t)=i)  []
    %  or mixmat{t}(m,q) if not stationary
    %
    % For HMMs with discrete inputs:
    % 'act' - act(t) = action performed at step t
    %
    % Optional arguments:
    % 'fwd_only' - if 1, only do a forwards pass and set beta=[], gamma2=[]  [0]
    % 'scaled' - if 1,  normalize alphas and betas to prevent underflow [1]
    % 'maximize' - if 1, use max-product instead of sum-product [0]
    %
    % OUTPUTS:
    % alpha(i,t) = p(Q(t)=i | y(1:t)) (or p(Q(t)=i, y(1:t)) if scaled=0)
    % beta(i,t) = p(y(t+1:T) | Q(t)=i)*p(y(t+1:T)|y(1:t)) (or p(y(t+1:T) | Q(t)=i) if scaled=0)
    % gamma(i,t) = p(Q(t)=i | y(1:T))
    % loglik = log p(y(1:T))
    % xi(i,j,t-1)  = p(Q(t-1)=i, Q(t)=j | y(1:T))  - NO LONGER COMPUTED
    % xi_summed(i,j) = sum_{t=}^{T-1} xi(i,j,t)  - changed made by Herbert Jaeger
    % gamma2(j,k,t) = p(Q(t)=j, M(t)=k | y(1:T)) (only for MOG  outputs)
    %
    % If fwd_only = 1, these become
    % alpha(i,t) = p(Q(t)=i | y(1:t))
    % beta = []
    % gamma(i,t) = p(Q(t)=i | y(1:t))
    % xi(i,j,t-1)  = p(Q(t-1)=i, Q(t)=j | y(1:t))
    % gamma2 = []
    %
    % Note: we only compute xi if it is requested as a return argument, since it can be very large.
    % Similarly, we only compute gamma2 on request (and if using MOG outputs).
    %
    % Examples:
    %
    % [alpha, beta, gamma, loglik] = fwdback(pi, A, multinomial_prob(sequence, B));
    %
    % [B, B2] = mixgauss_prob(data, mu, Sigma, mixmat);
    % [alpha, beta, gamma, loglik, xi, gamma2] = fwdback(pi, A, B, 'obslik2', B2, 'mixmat', mixmat);
    
    '''

    obslik2 = kwargs.pop('obslik2', None)
    mixmat = kwargs.pop('mixmat', None)
    fwd_only = kwargs.pop('fwd_only', False)
    scaled = kwargs.pop('scaled', True)
    act = kwargs.pop('act', None)
    maximize = kwargs.pop('maximize', False)
    compute_xi = kwargs.pop('compute_xi', obslik2!=None)
    compute_gamma2 = kwargs.pop('compute_gamma2', obslik2!=None and mixmat!=None)
    
    init_state_distrib = np.asmatrix(init_state_distrib)
    obslik = np.asmatrix(obslik)
    
    Q, T = obslik.shape;
    
    if act==None:
        act = np.zeros((T,))
        transmat = transmat[np.newaxis,:,:]
    
    scale = np.ones((T,))
    
    # scale(t) = Pr(O(t) | O(1:t-1)) = 1/c(t) as defined by Rabiner (1989).
    # Hence prod_t scale(t) = Pr(O(1)) Pr(O(2)|O(1)) Pr(O(3) | O(1:2)) ... = Pr(O(1), ... ,O(T))
    # or log P = sum_t log scale(t).
    # Rabiner suggests multiplying beta(t) by scale(t), but we can instead
    # normalise beta(t) - the constants will cancel when we compute gamma.
    
    loglik = 0
    
    alpha = np.asmatrix(np.zeros((Q,T)))
    gamma = np.asmatrix(np.zeros((Q,T)))
    if compute_xi:
        xi_summed = np.zeros((Q,Q));
    else:
        xi_summed = None
    
    ######## Forwards ########
    
    t = 0
    alpha[:,t] = np.multiply(init_state_distrib, obslik[:,t].T).T
    if scaled:
        #[alpha(:,t), scale(t)] = normaliseC(alpha(:,t));
        alpha[:,t], scale[t] = normalise(alpha[:,t])
    #assert(approxeq(sum(alpha(:,t)),1))
    for t in range (1, T):
        #trans = transmat(:,:,act(t-1))';
        trans = transmat[act[t-1]]
        if maximize:
            m = max_mult(trans.T, alpha[:,t-1])
            #A = repmat(alpha(:,t-1), [1 Q]);
            #m = max(trans .* A, [], 1);
        else:
            m = np.dot(trans.T,alpha[:,t-1])
        alpha[:,t] = np.multiply(m, obslik[:,t])
        if scaled:
            #[alpha(:,t), scale(t)] = normaliseC(alpha(:,t));
            alpha[:,t], scale[t] = normalise(alpha[:,t])
        if compute_xi and fwd_only:  # useful for online EM
            #xi(:,:,t-1) = normaliseC((alpha(:,t-1) * obslik(:,t)') .* trans);
            xi_summed = xi_summed + normalise(np.multiply(np.dot(alpha[:,t-1], obslik[:,t].T), trans))[0];
        #assert(approxeq(sum(alpha(:,t)),1))

    if scaled:
        if np.any(scale==0):
            loglik = -np.Inf;
        else:
            loglik = np.sum(np.log(scale), 0)
    else:
        loglik = np.log(np.sum(alpha[:,T], 0))
    
    if fwd_only:
        gamma = alpha;
        beta = None;
        gamma2 = None;
        return alpha, beta, gamma, loglik, xi_summed, gamma2
    
    ######## Backwards ########
    
    beta = np.asmatrix(np.zeros((Q,T)))
    if compute_gamma2:
        if isinstance(mixmat, list):
            M = mixmat[0].shape[1]
        else:
            M = mixmat.shape[1]
        gamma2 = np.zeros((Q,M,T))
    else:
        gamma2 = None
    
    beta[:,T-1] = np.ones((Q,1))
    #%gamma(:,T) = normaliseC(alpha(:,T) .* beta(:,T));
    gamma[:,T-1], _ = normalise(np.multiply(alpha[:,T-1], beta[:,T-1]))
    t=T-1
    if compute_gamma2:
        denom = obslik[:,t] + (obslik[:,t]==0) # replace 0s with 1s before dividing
        if isinstance(mixmat, list): #in case mixmax is an anyarray
            gamma2[:,:,t] = np.divide(np.multiply(np.multiply(obslik2[:,:,t], mixmat[t]), np.tile(gamma[:,t], (1, M))), np.tile(denom, (1, M)));
        else:
            gamma2[:,:,t] = np.divide(np.multiply(np.multiply(obslik2[:,:,t], mixmat), np.tile(gamma[:,t], (1, M))), np.tile(denom, (1, M))) #TODO: tiling and asmatrix might be slow. mybe remove
        #gamma2(:,:,t) = normaliseC(obslik2(:,:,t) .* mixmat .* repmat(gamma(:,t), [1 M])); % wrong!

    for t in range(T-2, -1, -1):
        b = np.multiply(beta[:,t+1], obslik[:,t+1])
        #trans = transmat(:,:,act(t));
        trans = transmat[act[t]]
        if maximize:
            B = np.tile(b.T, (Q, 1))
            beta[:,t] = np.max(np.multiply(trans, B), 1)
        else:
            beta[:,t] = np.dot(trans, b)
        if scaled:
            #beta(:,t) = normaliseC(beta(:,t));
            beta[:,t], _ = normalise(beta[:,t])
        #gamma(:,t) = normaliseC(alpha(:,t) .* beta(:,t));
        gamma[:,t], _ = normalise(np.multiply(alpha[:,t], beta[:,t]))
        if compute_xi:
            #xi(:,:,t) = normaliseC((trans .* (alpha(:,t) * b')));
            xi_summed = xi_summed + normalise(np.multiply(trans, np.dot(alpha[:,t],b.T)))[0]
        if compute_gamma2:
            denom = obslik[:,t] + (obslik[:,t]==0) # replace 0s with 1s before dividing
            if isinstance(mixmat, list): #in case mixmax is an anyarray
                gamma2[:,:,t] = np.divide(np.multiply(np.multiply(obslik2[:,:,t], mixmat[t]), np.tile(gamma[:,t], (1, M))), np.tile(denom,  (1, M)))
            else:
                gamma2[:,:,t] = np.divide(np.multiply(np.multiply(obslik2[:,:,t], mixmat), np.tile(gamma[:,t], (1, M))), np.tile(denom,  (1, M)))
            #gamma2(:,:,t) = normaliseC(obslik2(:,:,t) .* mixmat .* repmat(gamma(:,t), [1 M]));
    
    # We now explain the equation for gamma2
    # Let zt=y(1:t-1,t+1:T) be all observations except y(t)
    # gamma2(Q,M,t) = P(Qt,Mt|yt,zt) = P(yt|Qt,Mt,zt) P(Qt,Mt|zt) / P(yt|zt)
    #                = P(yt|Qt,Mt) P(Mt|Qt) P(Qt|zt) / P(yt|zt)
    # Now gamma(Q,t) = P(Qt|yt,zt) = P(yt|Qt) P(Qt|zt) / P(yt|zt)
    # hence
    # P(Qt,Mt|yt,zt) = P(yt|Qt,Mt) P(Mt|Qt) [P(Qt|yt,zt) P(yt|zt) / P(yt|Qt)] / P(yt|zt)
    #                = P(yt|Qt,Mt) P(Mt|Qt) P(Qt|yt,zt) / P(yt|Qt)
    
    return alpha, beta, gamma, loglik, xi_summed, gamma2

def mhmm_logprob(data, prior, transmat, mu, Sigma, mixmat=None):
    '''
    % LOG_LIK_MHMM Compute the log-likelihood of a dataset using a (mixture of) Gaussians HMM
    % [loglik, errors] = log_lik_mhmm(data, prior, transmat, mu, sigma, mixmat)
    %
    % data{m}(:,t) or data(:,t,m) if all cases have same length
    % errors  is a list of the cases which received a loglik of -infinity
    %
    % Set mixmat to ones(Q,1) or omit it if there is only 1 mixture component
    '''

    Q = len(prior);
    if mixmat.shape[0] != Q: # trap old syntax
        raise Exception, 'mixmat should be QxM'
    
    if mixmat==None:
        mixmat = np.ones((Q,1))
    
    if not isinstance(data, list):
        data = [data[:,:,i] for i in range(data.shape[2])]
        
    ncases = len(data);
    
    loglik = 0
    errors = []
    
    for m in range(ncases):
        obslik, _ = mixgauss_prob(data[m], mu, Sigma, mixmat);
        alpha, beta, gamma, ll, _, _ = fwdback(prior, transmat, obslik, fwd_only=True)
        if ll==-np.Inf:
            errors.append(m)
        loglik = loglik + ll
    
    return loglik, errors


class _BaseHMM(object): 
    
    def __init__(self, n_components=1, startprob=None, transmat=None,
                 startprob_prior=None, transmat_prior=None,
                 algorithm="viterbi", random_state=None,
                 n_iter=10, thresh=1e-2, params=string.ascii_letters,
                 init_params=string.ascii_letters):

        self.n_components = n_components
        self.n_iter = n_iter
        self.thresh = thresh
        self.params = params
        self.init_params = init_params
        self.startprob_ = startprob
        self.startprob_prior = startprob_prior
        self.transmat_ = transmat
        self.transmat_prior = transmat_prior
        self._algorithm = algorithm
        self.random_state = random_state

class HMM(_BaseHMM):
    
    def __init__(self, n_components=1, n_mix=1, startprob=None, transmat=None,
                 startprob_prior=None, transmat_prior=None,
                 algorithm="viterbi", gmms=None, covariance_type='diag',
                 covars_prior=1e-2, random_state=None, n_iter=10, thresh=1e-2,
                 params=string.ascii_letters,
                 init_params=string.ascii_letters):
        
        _BaseHMM.__init__(self, n_components, startprob, transmat,
                          startprob_prior=startprob_prior,
                          transmat_prior=transmat_prior,
                          algorithm=algorithm,
                          random_state=random_state,
                          n_iter=n_iter,
                          thresh=thresh,
                          params=params,
                          init_params=init_params)
        
        self.n_mix = n_mix
        self._covariance_type = covariance_type
        self.covars_prior = covars_prior
        self.gmms = gmms
        if self.gmms:
            raise NotImplementedError, 'Providing gmms is not implemented yet'
        
        self.LL = None
        
    def fit(self, obs):
        obs = self._convertObs(obs)

        O = obs[0].shape[0]
        M = self.n_mix
        Q = self.n_components

        self.O = O; self.M = M; self.Q = Q
        
        if 's' in self.init_params:
            self.startprob_, _ = normalise(self.startprob_)
            
        if 't' in self.init_params:
            self.transmat_, _ = mk_stochastic(self.transmat_)
        
        if 'm' in self.init_params or 'c' in self.init_params:
            mu0, Sigma0, weights0 = mixgauss_init(Q*M, obs, cov_type=self._covariance_type)
            
            if 'm' in self.init_params:
                self.means_ = np.transpose(np.reshape(mu0, (O, M, Q)), (0,2,1))
            
            if 'c' in self.init_params:
                self.covars_ = np.transpose(np.reshape(Sigma0, (O, O, M, Q)), (0, 1, 3, 2))
        
        mixmat0, _ = mk_stochastic(np.random.rand(Q,M))
        
        self.LL, prior1, transmat1, mu1, Sigma1, mixmat1 = mhmm_em(data=obs, 
                                                              prior=self.startprob_, 
                                                              transmat=self.transmat_, 
                                                              mu=self.means_, 
                                                              Sigma=self.covars_, 
                                                              mixmat=mixmat0,
                                                              max_iter=self.n_iter,
                                                              thresh=self.thresh,
                                                              cov_type=self._covariance_type,
                                                              adj_trans='t' in self.params,
                                                              adj_mix='w' in self.params,
                                                              adj_mu='m' in self.params,
                                                              adj_Sigma='c' in self.params)
        
        self.startprob_ = prior1
        self.transmat_ = transmat1
        self.means_ = mu1
        self.covars_ = Sigma1
        self.weights_ = mixmat1

    def score(self, obs):
        obs = self._convertObs(obs)
        
        lp = np.empty((len(obs),))
        for i in range(len(obs)):
            lp[i], err = mhmm_logprob(data=[obs[i]], prior=self.startprob_, transmat=self.transmat_, mu=self.means_, Sigma=self.covars_, mixmat=self.weights_)
        return lp
    
    def _convertObs(self, obs):
        assert isinstance(obs[0], np.ndarray)
        assert obs[0].ndim==2
        
        obs = [obs[i].T for i in range(len(obs))]
        
        return obs
    
    def getLL(self):
        return self.LL

    def aic(self):
        p = self.Q**2 + self.Q*(self.O*self.M + self.O**2)
        return -2*self.getLL()[-1] + 2*p
        
