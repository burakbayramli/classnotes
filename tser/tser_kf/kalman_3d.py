from numpy import *

# x_{t+1} = Phi x_t + Sigma_x
# y_t = Hx_t + R    
class Kalman:
    # T is the translation matrix
    # K is the camera matrix calculated by calibration
    def __init__(self, K, mu_init):
        self.ndim = 3
        self.Sigma_x = eye(self.ndim+1)*150
        self.Phi = eye(4)
        self.Phi[2,3] = -0.5
        self.H = append(K, [[0], [0], [0]], axis=1)
        self.mu_hat = mu_init
        self.cov = eye(self.ndim+1)
        self.R = eye(self.ndim)*1.5
        
    def normalize_2d(self, x): 
        return array([x[0]/x[2], x[1]/x[2], 1.0])
    
    def update(self, obs):

        # Make prediction
        #print "self.mu_hat=" + str(self.mu_hat)
        self.mu_hat_est = dot(self.Phi,self.mu_hat) 
        prod = dot(self.Phi, dot(self.cov, transpose(self.Phi)))
        self.cov_est = prod + self.Sigma_x
        #print "self.mu_hat_est=" + str(self.mu_hat_est)
        #print "self.cov_est=" + str(self.cov_est)
                
        # Update estimate
        prod = self.normalize_2d(dot(self.H,self.mu_hat_est))
        self.error_mu = obs - prod
        
        prod = dot(self.cov,transpose(self.H))
        prod = dot(self.H,prod)
        self.error_cov = prod + self.R
        prod = dot(self.cov_est,transpose(self.H))
        self.K = dot(prod,linalg.inv(self.error_cov))
        self.mu_hat = self.mu_hat_est + dot(self.K,self.error_mu)
        
        prod = dot(self.K,self.H)
        left = eye(self.ndim+1) 
        diff = left - prod
        self.cov = dot(diff, self.cov_est)
