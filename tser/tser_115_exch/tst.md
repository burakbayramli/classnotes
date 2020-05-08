
Model 2


```python
from scipy.integrate import odeint
import pandas as pd

phi1 = 0.03
phi2 = 0.06
gamma = 0.05
xi = 0.1

def dmod2(y, t):
    z, q, d = y
    zdot = -(phi1*z)-(phi2*q)
    return [zdot, \
            xi*(z+d), \
	    -zdot - gamma*d   ]

t = np.linspace(0, 140, 200)
y0 = [1.0, 0.0, -1]
sol = odeint(dmod2, y0, t)
df = pd.DataFrame( sol )
df.columns = ['current account','exchange rate','debt']
df['t'] = t
df = df.set_index('t')
df.plot()
plt.savefig('mod2.png')
```

```text

NameErrorTraceback (most recent call last)
<ipython-input-1-d1037cd6b5b8> in <module>
     18 t = np.linspace(0, 140, 200)
     19 y0 = [1.0, 0.0, 0.0]
---> 20 sol = odeint(dmod2, y0, t)
     21 df = pd.DataFrame( sol )
     22 df.columns = ['current account','exchange rate','debt']

~/Documents/env3/lib/python3.5/site-packages/scipy/integrate/odepack.py in odeint(func, y0, t, args, Dfun, col_deriv, full_output, ml, mu, rtol, atol, tcrit, h0, hmax, hmin, ixpr, mxstep, mxhnil, mxordn, mxords, printmessg, tfirst)
    242                              full_output, rtol, atol, tcrit, h0, hmax, hmin,
    243                              ixpr, mxstep, mxhnil, mxordn, mxords,
--> 244                              int(bool(tfirst)))
    245     if output[-1] < 0:
    246         warning_msg = _msgs[output[-1]] + " Run with full_output = 1 to get quantitative information."

<ipython-input-1-d1037cd6b5b8> in dmod2(y, t)
     14     return [zdot, \
     15             xi*(z+d), \
---> 16 	    -zdot - e-9*d]
     17 
     18 t = np.linspace(0, 140, 200)

NameError: name 'e' is not defined
```




![](mod2.png)

