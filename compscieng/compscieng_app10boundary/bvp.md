#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \DeclareMathOperator{\bdiv}{div}
#+LaTeX_HEADER: \DeclareMathOperator{\curl}{curl}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Bratu Problemi

Bu problem 

$$
y'' + k + \exp(y) = 0
$$

$$
y(0) = y(1) = 0
$$

olarak bilinir. Bu sistemi 1. derece bir sistem olarak değiştirelim,

$$
y_1' = y_2
$$

$$
y_2' = -\exp(y_1)
$$

```python
def fun(x, y):
    # k=1 farz edildi
    return np.vstack((y[1], -np.exp(y[0])))

def bc(ya, yb):
    return np.array([ya[0], yb[0]])

x = np.linspace(0, 1, 5)
```

Bu problemin iki farklı çözümü var. Her iki çözümü de elde etmek için
$y$ için farklı başlangıç noktaları deneyeceğiz, bu seçenekleri $a$ ve
$b$ olarak işaretleyelim,


```python
y_a = np.zeros((2, x.size))
y_b = np.zeros((2, x.size))
y_b[0] = 3

from scipy.integrate import solve_bvp
res_a = solve_bvp(fun, bc, x, y_a)
res_b = solve_bvp(fun, bc, x, y_b)
```


```python
x_plot = np.linspace(0, 1, 100)
y_plot_a = res_a.sol(x_plot)[0]
y_plot_b = res_b.sol(x_plot)[0]
import matplotlib.pyplot as plt
plt.plot(x_plot, y_plot_a, label='y_a')

plt.plot(x_plot, y_plot_b, label='y_b')
plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.savefig('compscieng_app10boundary_03.png')
```




1. dereceden tek bir ODE'yi çözmek istersek, 

$$
\epsilon y' = \sin^2 (x) - \lambda \frac{\sin^4(x)}{y}
$$

ki $\epsilon$ biliniyor, ama $\lambda$ bilinmiyor. Sınır şartları 

$$
y \left( -\frac{\pi}{2} \right) = 1, \quad
y \left( \frac{\pi}{2}  \right) = 1
$$

Sınır şartlarını kullanarak $\lambda$'yi bulmak mümkün. Aslında
üstteki ifadeyi iki denklem olarak görmek mümkün, biri ana ODE için,
diğeri bilinmeyen parametre için (bilinmeyen sayısı denklem sayısına
eşit olmalıdır ya). 



