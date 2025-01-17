from sympy import symbols, pprint, latex
from sympy.matrices import Matrix
import pandas as pd
pd.set_option('display.max_columns', None)

t,t0,t1,t2,t3,p0,p1,p3 = symbols("t,t0,t1,t2,t3,p0,p1,p3")
a = Matrix ([[1,1,1,1],
             [t0,t1,t2,t3],
             [t0**2,t1**2,t2**2,t3**2],
             [t0**3,t1**3,t2**3,t3**3]])

#print (a.inv())

b = Matrix([[-t1*t2*t3/(t0**3 - t0**2*t1 - t0**2*t2 - t0**2*t3 + t0*t1*t2 + t0*t1*t3 + t0*t2*t3 - t1*t2*t3), (t1*t2 + t1*t3 + t2*t3)/(t0**3 - t0**2*t1 - t0**2*t2 - t0**2*t3 + t0*t1*t2 + t0*t1*t3 + t0*t2*t3 - t1*t2*t3), (-t1 - t2 - t3)/(t0**3 - t0**2*t1 - t0**2*t2 - t0**2*t3 + t0*t1*t2 + t0*t1*t3 + t0*t2*t3 - t1*t2*t3), 1/(t0**3 - t0**2*t1 - t0**2*t2 - t0**2*t3 + t0*t1*t2 + t0*t1*t3 + t0*t2*t3 - t1*t2*t3)], [t0*t2*t3/(t0*t1**2 - t0*t1*t2 - t0*t1*t3 + t0*t2*t3 - t1**3 + t1**2*t2 + t1**2*t3 - t1*t2*t3), (-t0*t2 - t0*t3 - t2*t3)/(t0*t1**2 - t0*t1*t2 - t0*t1*t3 + t0*t2*t3 - t1**3 + t1**2*t2 + t1**2*t3 - t1*t2*t3), (t0 + t2 + t3)/(t0*t1**2 - t0*t1*t2 - t0*t1*t3 + t0*t2*t3 - t1**3 + t1**2*t2 + t1**2*t3 - t1*t2*t3), -1/(t0*t1**2 - t0*t1*t2 - t0*t1*t3 + t0*t2*t3 - t1**3 + t1**2*t2 + t1**2*t3 - t1*t2*t3)], [-t0*t1*t3/(t0*t1*t2 - t0*t1*t3 - t0*t2**2 + t0*t2*t3 - t1*t2**2 + t1*t2*t3 + t2**3 - t2**2*t3), (t0*t1 + t0*t3 + t1*t3)/(t0*t1*t2 - t0*t1*t3 - t0*t2**2 + t0*t2*t3 - t1*t2**2 + t1*t2*t3 + t2**3 - t2**2*t3), (-t0 - t1 - t3)/(t0*t1*t2 - t0*t1*t3 - t0*t2**2 + t0*t2*t3 - t1*t2**2 + t1*t2*t3 + t2**3 - t2**2*t3), 1/(t0*t1*t2 - t0*t1*t3 - t0*t2**2 + t0*t2*t3 - t1*t2**2 + t1*t2*t3 + t2**3 - t2**2*t3)], [t0*t1*t2/(t0*t1*t2 - t0*t1*t3 - t0*t2*t3 + t0*t3**2 - t1*t2*t3 + t1*t3**2 + t2*t3**2 - t3**3), (-t0*t1 - t0*t2 - t1*t2)/(t0*t1*t2 - t0*t1*t3 - t0*t2*t3 + t0*t3**2 - t1*t2*t3 + t1*t3**2 + t2*t3**2 - t3**3), (t0 + t1 + t2)/(t0*t1*t2 - t0*t1*t3 - t0*t2*t3 + t0*t3**2 - t1*t2*t3 + t1*t3**2 + t2*t3**2 - t3**3), -1/(t0*t1*t2 - t0*t1*t3 - t0*t2*t3 + t0*t3**2 - t1*t2*t3 + t1*t3**2 + t2*t3**2 - t3**3)]])
print (latex(b))
