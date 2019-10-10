#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

```python
from autograd import numpy as anp, grad

#r = 10.0
r = 1.0

def B(x1,x2):
    return (x1 - 1.0)**2 + (x2 - 2.0)**2 - r*anp.log(x1 + x2 - 4.0)

dx1 = grad(B,0)
dx2 = grad(B,1)

x = anp.array([2.0,3.0])

alpha = 0.1

for i in range(20):   
    nabla = anp.array([dx1(x[0], x[1]), dx2(x[0], x[1])])
    x = x - alpha*nabla

print (x)
```

```text
[1.80901699 2.80901699]
```





















