# From https://github.com/nipunbatra/PyHMM
# Seems to be based on Kevin Murphy's Matlab code which is great
# This is the only Python code I've seen that reports the right
# Viterbi path for the dishonest casino example

import matplotlib.pyplot as plt
import numpy as np

def em_converged(loglik, previous_loglik, threshold, check_increased):
    converged = False
    decrease = False
    if check_increased:
      if loglik - previous_loglik < -.000001:
          decrease = True
          converged = False
          return [converged,decrease]
      
    delta_loglik = abs(loglik - previous_loglik)
    avg_loglik = (abs(loglik) + abs(previous_loglik) + np.spacing(1))/2
    if (delta_loglik / avg_loglik) < threshold:
        converged =True
    return [converged,decrease]
    
def normalize(A,dim=None):
    '''normalize: Make the entries of a multidimensional array sum to 1
    Inputs
    ------
    A: Array of type floating point
    dim: Dimension on which to normalize
    
    Outputs
    -------
    M: Normalized Array
    Z: Normalizing Constant
    
    Examples
    --------
    
    Example 1: Normalizing a vector without specifying dimension
    [normalized_array, normalizing_constant]=normalize(np.array([1,2,3]))
    
    Output
    normalized_array=array([ 0.16666667,  0.33333333,  0.5])
    normalizing_constant=6
    
    Example 2: Normalizing a 2-d matrix without specifying dimension
    [normalized_array, normalizing_constant]=normalize(np.array([[1,2,3],[2,3,4]]))
    
    Output
    normalized_array=array([[ 0.06666667,  0.13333333,  0.2       ],
       [ 0.13333333,  0.2       ,  0.26666667]])
    normalizing_constant=15
    
    Example 3: Normalizing a 2-d matrix across the 'row' dimension (2nd dim)
    [normalized_array, normalizing_constant]=normalize(np.array([[1,2,3],[2,3,4]]),dim=2)
    
    Output
    normalized_array=array([[ 0.16666667,  0.33333333,  0.5       ],
       [ 0.22222222,  0.33333333,  0.44444444]])
    normalized_constant=array([[ 6.],[ 9.]])
    
    NB: While using this function make sure that if you do not require the normalizing
    constant you take the first element of the output

    '''
    if dim is None:
        z=sum(A.flatten(1))
        s=1.0*(z+(z==0.0))
        M=A/s
    elif dim==1:
        z=np.sum(A)
        s=1.0*(z+(z==0.0))
        M=A/s
    else:        
        z=np.sum(A,axis=dim-1)
        s = 1.0*(z + (z==0.0))
        s=s[:, np.newaxis]
        M=A/s
    return [M,s]


def mk_stochastic(T):
    '''
    mk_stochastic: Ensure that the argument is a stochastic matrix, sum over last dimension is 1
    Inputs
    ------
    A: Array of type floating point
   
    Outputs
    -------
    M: Normalized Array
        
    Examples
    --------
    
    Example 1: Making a vector stochastic 
    normalized_array=mk_stochastic(np.array([1,2,3]))
    
    Output
    
    normalized_array= array([ 0.16666667,  0.33333333,  0.5])
    
    Example 2: Making a 2d matrix stochastic 
    [normalized_array, normalizing_constant]=mk_stochastic(np.array([[1,2,3],[4,5,6]]))
    
    Output
    normalized_array=array([[ 0.16666667,  0.33333333,  0.5       ],
       [ 0.26666667,  0.33333333,  0.4       ]])

    '''
    if ((np.ndim(T)==2) and (T.shape[0]==1 or T.shape[1]==1)) or np.ndim(T)==1: # isvector
        [out,Z] = normalize(T);
    else:
        out=np.zeros((np.shape(T)))
        number_of_rows=np.shape(T)[0]
        for row in range(0,number_of_rows):            
            sum_row=1.0*np.sum(T[row,:])           
            a=T[row,:]
            b=a/sum_row            
            out[row,:]=b
    return out
        

def forward_backward(prior,transition_matrix,emission_matrix,observation_vector,scaling,obslik):
    number_of_hidden_states=len(prior)
    number_of_observations=len(observation_vector)
    shape_alpha=(number_of_hidden_states,number_of_observations)
    alpha=np.zeros(shape_alpha)
    scale=np.ones(number_of_observations)
    xi = np.zeros((number_of_observations,number_of_hidden_states,number_of_hidden_states)) 
    gamma=np.zeros(shape_alpha)
    gamma2=np.zeros(shape_alpha)

    xi_summed=np.zeros((number_of_hidden_states,number_of_hidden_states))
    
          
    # Forwards
    
    # 1.Initialization
    
    t=0
    for i in range(0,number_of_hidden_states):
        alpha[i][0]=prior[i]*emission_matrix[i][observation_vector[0]]
    
    if scaling:
        [alpha[:,0], n] = normalize(alpha[:,0])
        
        scale[0] = n
    else:
        pass
    
    # 2.Induction

    # Currently Non-vectorized
    for t in range(1,number_of_observations):
        for j in range(0,number_of_hidden_states):
            prob_sum=0            
            for i in range(0,number_of_hidden_states):
                prob_sum+=alpha[i][t-1]*transition_matrix[i][j]                
            alpha[j][t]=prob_sum*emission_matrix[j][observation_vector[t]]
        if scaling:
            [alpha[:,t], n] = normalize(alpha[:,t])
            scale[t] = n
      
    # 3.Termination
    if scaling:
        loglik=sum(np.log(scale))
    else:
        loglik=np.log(sum(alpha[:,number_of_observations-1]))
    
   
    # Backwards
    beta=np.ones(shape_alpha)
    gamma[:,number_of_observations-1] = normalize(alpha[:,number_of_observations-1] * beta[:,number_of_observations-1])[0]
   
    # 1.Initialization
    # We have already set beta as a sequence of 1's.'''
    
    # 2.Induction

    # Currently Non-vectorized
    for t in range(number_of_observations-2,-1,-1):
        b = beta[:,t+1] * obslik[:,t+1]
       
       
        for i in range(0,number_of_hidden_states):
            beta_sum=0            
            for j in range(0,number_of_hidden_states):
                beta_sum+=(beta[j][t+1]*transition_matrix[i][j]*emission_matrix[j][observation_vector[t+1]])         
            beta[i][t]=beta_sum
        if scaling:
            [beta[:,t], n] = normalize(beta[:,t])
            scale[t] = n
        gamma[:,t] = normalize(alpha[:,t] * beta[:,t])[0]
       
        a=alpha[:,t].reshape(number_of_hidden_states,1)
        
        xi_summed  = xi_summed + normalize((transition_matrix * np.dot(a, b.conj().T[np.newaxis])))[0]
    
    # Computing xi
    
    for t in range(number_of_observations-1):
        denom=0.0
        for i in range(0,number_of_hidden_states):
            for j in range(0,number_of_hidden_states):
                denom+=alpha[i][t]*transition_matrix[i][j]*emission_matrix[j][observation_vector[t+1]]*beta[j][t+1]
        for i in range(0,number_of_hidden_states):
            for j in range(0,number_of_hidden_states):
                numer=alpha[i][t]*transition_matrix[i][j]*emission_matrix[j][observation_vector[t+1]]*beta[j][t+1]       
                xi[t][i][j]=numer/denom
 
    # CHECKING
    for t in range(number_of_observations):
        for i in range(0,number_of_hidden_states):
            p=0.0
            for j in range(number_of_hidden_states):
                p+=xi[t][i][j]     
    return [alpha,beta,gamma,xi,xi_summed,loglik]

    
      

def evaluate_pdf_cond_multinomial(data, obsmat):
'''% EVAL_PDF_COND_MULTINOMIAL Evaluate pdf of conditional multinomial 
% function B = eval_pdf_cond_multinomial(data, obsmat)
%
% Notation: Y = observation (O values), Q = conditioning variable (K values)
%
% Inputs:
% data(t) = t'th observation - must be an integer in {1,2,...,K}: cannot be 0!
% obsmat(i,o) = Pr(Y(t)=o | Q(t)=i)
%
% Output:
% B(i,t) = Pr(y(t) | Q(t)=i)

data array([1, 1, 2, 1, 1, 3, 2, 3, 2, 3])
array([[ 0.0284 ,  0.315  ,  0.06565],
       [ 0.3154 ,  0.5503 ,  0.1343 ]])

Output: array([[ 0.0284 ,  0.0284 ,  0.315  ,  0.0284 ,  0.0284 ,  0.06565,
         0.315  ,  0.06565,  0.315  ,  0.06565],
       [ 0.3154 ,  0.3154 ,  0.5503 ,  0.3154 ,  0.3154 ,  0.1343 ,
         0.5503 ,  0.1343 ,  0.5503 ,  0.1343 ]])
         
'''


    (Q,O) = np.shape(obsmat)
    
    T = len(data)
    B = np.zeros((Q,T))

    for t in range(0,T):
        B[:,t] = obsmat[:, data[t]    ]


    return B    

'''Compute Sufficient Statistics
1. Prior
2. Transition Matrix
3. Emission Matrix
4. Observation matrix
'''
def compute_ess_dhmm(observation_vector,prior,transition_matrix,emission_matrix, dirichlet):
    (S,O)=np.shape(emission_matrix)
    exp_num_trans=np.zeros((S,S))
    exp_num_visits1=np.zeros((1,S)).flatten(1)
    exp_num_visitsT=np.zeros((1,S)).flatten(1)
    exp_num_emit = dirichlet*np.ones((S,O))
    loglik = 0
    num_sequences=len(observation_vector)
    for i in range(num_sequences):
        observation_i=observation_vector[i]
        #Number of observations
        T=len(observation_i)
        obslik = evaluate_pdf_cond_multinomial(observation_i, emission_matrix)
        #E
        [alpha, beta, gamma, xi,xi_summed,current_ll] = forward_backward(prior, transition_matrix, emission_matrix,observation_i,True,obslik)
        loglik = loglik + current_ll
 
        exp_num_trans = exp_num_trans + xi_summed
    
        exp_num_visits1 += gamma[:,0]
        exp_num_visitsT = exp_num_visitsT + gamma[:,T-1]
        for t in range(0,T):
            o = observation_i[t]
            exp_num_emit[:,o] = exp_num_emit[:,o] + gamma[:,t]
        
        new_pi=normalize(gamma[:,0])[0]
        new_A=xi_summed/np.sum(gamma,1)
        (number_of_hidden_states,number_of_observation_states)=np.shape(emission_matrix)
        B_new = np.zeros(np.shape(emission_matrix))
        for j in xrange(number_of_hidden_states):
            for k in xrange(number_of_observation_states):
                numer = 0.0
                denom = 0.0
                for t in xrange(T):
                    if observation_i[t] == k:
                        numer += gamma[j][t]
                    denom += gamma[j][t]
                    B_new[j][k] = numer/denom
                    
    return [loglik,exp_num_visits1,exp_num_visitsT,exp_num_trans,exp_num_emit]

    
def dhmm_em(observation_i, prior, transition_matrix, emission_matrix, max_iter, thresh):
    
    previous_loglik = -np.inf
    loglik = 0
    converged = False
    num_iter = 1
    LL = []
    
    while (num_iter <= max_iter) and not converged :
         
         #E step
         [loglik,exp_num_visits1,exp_num_visitsT,exp_num_trans,exp_num_emit]=compute_ess_dhmm(observation_i,prior,transition_matrix,emission_matrix, 0)
         #M Step
         prior=normalize(exp_num_visits1)[0]
         transition_matrix = mk_stochastic(exp_num_trans)         
         emission_matrix=mk_stochastic(exp_num_emit)
                 
         num_iter =  num_iter + 1
         [converged,decrease] = em_converged(loglik, previous_loglik, thresh,False)
         previous_loglik = loglik
         LL.append(loglik)
                  
    return [LL, prior, transition_matrix, emission_matrix, num_iter]

   
def path(prior,transition_matrix,emission_matrix,observation_vector,scaling=True):
    number_of_hidden_states=len(prior)
    number_of_observations=len(observation_vector)
    shape_delta=(number_of_hidden_states,number_of_observations)
    shape_psi=shape_delta
    delta=np.zeros(shape_delta)
    psi=np.zeros(shape_psi,dtype=np.int)
    scale=np.ones(number_of_observations)
    optimum_path=np.zeros(number_of_observations,dtype=np.int)
    
    # 1.Initialization
    first_observation=observation_vector[0]
    delta[:,0]=prior*emission_matrix[first_observation:,0]
    psi[:,0]=0
    if scaling:
        [delta[:,0], n] = normalize(delta[:,0])
        scale[0] = 1/n;

    # 2.Recursion
    # Currently non vectorized
    for t in range(1,number_of_observations):
        for j in range(0,number_of_hidden_states):
            p=0            
            for i in range(0,number_of_hidden_states):
                p=delta[i][t-1]*transition_matrix[i][j]*emission_matrix[j][observation_vector[t]]                
                if p>delta[j][t]:
                    delta[j][t]=p  
                    psi[j][t]=i 
        if scaling:
            [delta[:,t], n] = normalize(delta[:,t]);
            scale[t] = 1/n;
    
    # 3.Termination
    p_star=max(delta[:,number_of_observations-1])
    optimum_path[number_of_observations-1]=np.argmax(delta[:,number_of_observations-1])
    
    # 4.Path Backtracking
    for t in range(number_of_observations-2,-1,-1):
        optimum_path[t]=psi[optimum_path[t+1]][t]
        
    # Log Probability
    if scaling:
        loglik=-sum(np.log(scale))
    else:
        loglik=np.log(p_star)
    return [optimum_path,delta,loglik]
    
  
class HMM:
    def __init__(self,n,m,prior=None,transmat=None,obsmat=None):
        self.n = n
        self.m = m
        self.prior = prior
        self.transmat = transmat
        self.obsmat = obsmat
        if prior == None: self.prior = np.ones(self.n) / self.n
        if transmat == None: self.transmat = mk_stochastic(np.random.rand(self.n,self.n))
        if obsmat == None: self.obsmat = mk_stochastic(np.random.rand(self.n,self.m))

    def train(self, obs,iter=10,threshold=0.0001):
        [LL, prior2, transmat2, obsmat2, nr_iter] = dhmm_em(obs,
                                                            self.prior,
                                                            self.transmat,
                                                            self.obsmat,
                                                            iter,
                                                            threshold );
        self.likelihood = LL

    def viterbi_path(self, obs):
        [optimum_path,delta,loglik]=path(self.prior,self.transmat,self.obsmat,obs)
        return optimum_path
        
    def aic(self):
        p = self.n**2 + self.m*self.n - 1
        return -2*self.likelihood[-1] + 2*p

if __name__ == "__main__": 
    obs=np.array([[ 2,2,2,1,1,0,0,1,0,1]])                
    O = 3
    Q = 2
    prior =np.array([.5,.5])
    transmat = mk_stochastic(np.random.rand(Q,Q))
    obsmat = mk_stochastic(np.random.rand(Q,O))
    [LL, prior2, transmat2, obsmat2,nr_iter] = dhmm_em(obs, prior, transmat, obsmat, 10,.0001 );
    print LL
 
        
