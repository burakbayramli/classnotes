\documentclass[12pt,fleqn]{article}\usepackage{../../common}
\begin{document}
Enflasyon

$$ g = \frac{I_{fn}(\pi_r)}{v} - \delta_{Kr} $$

$g$ is the real growth rate, which is determined by the rate of investment
and depreciation.

$$ 
\frac{\dot{\lambda}}{\lambda} =
\left( 
  \underbrace{ g }_{A} - 
  \underbrace{ (\alpha + \beta) }_{B}
\right)
$$

A: Growth Rate

B: Labor productivity and population growth

``Employment rises if the growth rate exceeds population growth and labor productivity.''


$$ 
\frac{\dot{\omega}}{\omega} = 
\left( 
(\underbrace{w_{fn}(\lambda)}_{A} - \underbrace{\alpha}_{B}) + \frac{1}{\tau_p}
\underbrace{\left( 1-\frac{1}{1-s} \cdot \omega \right)}_{C}
\right)
$$

A: Wage share 

B: Labor productivity

C: Inflation

``Wage share rises if money wage demands greater than labor productivity and
the inflation rate''

$$ 
\frac{\dot{d}}{d} = 
\underbrace{\frac{\left( \frac{I_{fn}(\pi_r)}{v} \right) - \pi_s }{d}}_{A} - 
\left[ 
\underbrace{g }_{B}+ 
\underbrace{\frac{1}{\tau_p} \left(1 - \frac{1}{1-s} \omega\right)}_{C}
\right]
$$

A: Debt growth rate

B: Real growth rate

C: Inflation

``Debt ratio will rise if debt growth rate is greater than real growth rate plus inlation''

For price level we look into supply/demand in equi (not the whole econ,
just what is bought is what is sold). Production $Q$ equals physical demand
$D$. $Q = a \cdot L$, productivity times labor. For $L$ we divide wage
money flow with indiv wage. 

$$ 
L = (1-s) \frac{F_D / \tau_s}{W}
$$

$F_D/\tau_s$ is GDP, $F_D$ is company's money, $\tau_s$ is the turnover of
that money, multiply that by $1-s$ to find worker's share. Divide that by
unit wage $W$, we find labor force. Then production is 

$$ Q = a \cdot (1-s) \frac{F_D / \tau_s}{W} $$

Physical demand is money flow towards demand divided by price level,
expenditure divided by $P$, so $(F_D / \tau_s) / P$. There is no $1-s$ now,
labor and corp demand added. 

Equate $D$ and $Q$ and solve for $P$, 

$$  a \cdot (1-s) \frac{F_D / \tau_s}{W} = \frac{(F_D / \tau_s)}{P} $$

$F_D,\tau_s$ cancels out, rearrange,

$$ P = \frac{1}{1-s} \frac{W}{a}$$

This is price in equib, we need a dynamic version, first-order approx
converging to RHS above, 

$$ 
\frac{\ud P}{\ud t} = 
\frac{-1}{\tau_p} \left( P - \frac{1}{1-s} \frac{W}{a} \right)
$$

$\tau_p$ might be familiar from ODE's, ex, 

$$ \frac{\ud x}{\ud t} = k(10-x)$$

is solved by $x = 10 - C e^{-kt}$, $k$ controls convergence. $t=0,x=0$
gives $C=10$, so $x = 10 (1 - e^{-kt})$. In general

$$ x = x_{max} (1-e^{-t/\tau}) $$ 

For $t=\tau,x_{max}=1$ we get  $1-e^{-1}$

```python
print 1-np.exp(-1)
```

```
0.632120558829
```

$\tau$ is approximately how many unit time steps it takes to each 63\% of
something. 

Final equations

$$ 
\frac{\dot{\lambda}}{\lambda} =
  g - (\alpha + \beta)
$$

$$ 
\frac{\dot{\omega}}{\omega} = 
\left( 
(w_{fn}(\lambda) - \alpha) + \frac{1}{\tau_p} \left( 1-\frac{1}{1-s} \cdot \omega\right)
\right)
$$


$$ 
\frac{\dot{d}}{d} = 
\frac{\left( \frac{I_{fn}(\pi_r)}{v} \right) - \pi_s }{d} - 
\left[ 
g + \frac{1}{\tau_p} \left(1 - \frac{1}{1-s} \omega\right)
\right]
$$

$$ g = \frac{I_{fn}(\pi_r)}{v} - \delta_{Kr} $$

```python
import scipy as sp
from scipy.integrate.odepack import odeint

def rhs(u,t,alpha, beta, delta, nu, r_b, s, tau_p, tau_i, x_i, y_i, s_i, m_i, x_w, y_w, s_w, m_w):
    lam, omega, d, i = u
    r=r_b;
    if i>0: r=r+i; 
    p=1.0-omega-r*d;
    f=-(1.0/tau_p)*(1.0-omega/(1.0-s));
    I=(y_i-m_i)*np.exp(s_i*((p/nu)-x_i)/(y_i-m_i))+m_i;
    W=(y_w-m_w)*np.exp(s_w*(lam-x_w)/(y_w-m_w))+m_w;
    return [( ((1.0/nu)*I-delta) -(alpha + beta) )*lam, \
            ( W - (alpha+f) )*omega, \
            ( I-p ) -( (1/nu)*I - delta + f )*d,\
            -(1.0/tau_i)*(i-f)]
    

alpha=0.025;     
beta=0.015;      
delta=0.07;      
nu=3.0;
r_b=0.04;        
s=0.3;
tau_p=1.0;
tau_i=0.5;
x_i=0.03;
y_i=0.03;
s_i=2.25;
m_i=0;
x_w=0.6;
y_w=0.0;
s_w=1.0;
m_w=-0.04;

# baslangic degerleri
lambda0=0.65; # istihdam
omega0=0.82; # maaslarin gsyh'eki orani
d0=0.5; # borc orani
i0=0.1; # enflasyon orani

arg0 = (alpha, beta, delta, nu, r_b, s, tau_p, tau_i, x_i, y_i, s_i, m_i, x_w, y_w, s_w, m_w)

T=80.0
t=np.linspace(0.0,T,1000.0)

res=odeint(rhs,[lambda0, omega0, d0, i0],t,args=arg0)
lambda1,omega1,d1,i1=res[:, 0],res[:, 1],res[:, 2],res[:, 3]
```

```python
plt.plot(t, 100.0*i1)
plt.xlabel(u'Year')
plt.ylabel(u'Annual Inflation')
plt.savefig('inf_01.png')
```

```python
plt.plot(t, 100.0*d1)
plt.xlabel(u'Time')
plt.ylabel(u'Debt to GDP')
plt.savefig('inf_02.png')
```

```python
last=500;
x1=100.0*(1.0-lambda1[:last]);
x2=100.0*i1[:last];
x3=100.0*d1[:last];

from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
fig = plt.figure()
ax = Axes3D(fig)
ax.plot(x1,x2,x3,'.', zs=0,zdir='z', label='zs=0, zdir=z')
ax.set_xlabel(u'Unemployment Rate')
ax.set_ylabel(u'Inflation')
ax.set_zlabel(u'Debt')
ax.view_init(elev=5, azim=250)
plt.savefig('inf_03.png')
```






[x] Keen, {\em A monetary Minsky model of the Great Moderation and the Great Recession}

[x] {\em Greenwich-Kingston PhD students lecture: the logic  maths of modelling Minsky (2)} \url{youtu.be/0Do05hV_Iqo}


\end{document}

