import pandas as pd
import numpy as np
import scipy as sp
from scipy.integrate.odepack import odeint

def rhs(u,t,alpha,beta,gamma,nu,r,x_p,y_p,s_p,m_p,x_l,y_l,s_l,m_l):
    Y, w, D, a, N = u
    L=Y/a;                                       #  isgucu
    P=Y-w*L - r*D;                               #  kar
    p=P/(nu*Y);                                  
    #  investment as a function of profit
    I=(y_p-m_p)*np.exp(s_p*(p-x_p)/(y_p-m_p))+m_p;  
    #  istihdam orani
    l=Y/(a*N);                                   
    # isci ucretlerinin istihdam orani fonksiyonu olarak buyume orani 
    H=(y_l-m_l)*np.exp(s_l*(l-x_l)/(y_l-m_l))+m_l;  

    res = [Y*(I/nu - gamma ),\
           H*w, \
           I*Y-P, \
           alpha*a, \
           beta*N]

    return res
    

alpha=0.02;      
beta=0.01;       
gamma=0.01;      
nu=3;
r=0.05;          # banka faiz orani

x_p=0.05;
y_p=0.05;
s_p=1.75;
m_p=0.0;

x_l=0.95; 
y_l=0.0;
s_l=0.5;
m_l=-0.01;
T = 120.0
t=np.linspace(0.0,T,10000.0)
Y0=300;          # baslangic uretimi
w0=0.95;         # baslangic iscu ucreti
D0=0;            # baslangic borc
a0=1;            # baslangic teknoloji (uretkenlik)
N0=300;          # baslangic nufus


arg0 = (alpha,beta,gamma,nu,r,x_p,y_p,s_p,m_p,x_l,y_l,s_l,m_l)

res=odeint(rhs,[Y0,w0,D0,a0,N0],t,args=arg0)
Y1,w1,D1,a1,N1=res[:, 0],res[:, 1],res[:, 2],res[:, 3],res[:, 4]
