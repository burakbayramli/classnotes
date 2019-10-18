#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Esitsizlik Sınırlamaları 

Optimizasyon problemimizde 

$$
\min_x f(x), 
\quad 
\textrm{oyle ki}, 
\quad 
c_i(x) \ge 0, 
\quad i=1,2,..,m
$$

$c_i$ ile gösterilen eşitsizlik içeren (üstte büyüklük türünden)
kısıtlamalar olduğunu düşünelim. Bu problemi nasıl çözeriz?

Bir fikir, problemin eşitizliklerini bir gösterge (indicator)
fonksiyonu üzerinden, Lagrange yönteminde olduğu gibi, ana hedef
fonksiyonuna dahil etmek, ve elde edilen yeni hedefi kısıtlanmamış bir
problem gibi çözmek. Yani üstteki yerine, alttaki problemi çözmek,

$$
\min_x f(x) + \sum_{i=1}^{m} I(c_i(x))
$$

ki $I$ pozitif reel fonksiyonlar için göstergeç fonksiyonu,

$$
I(u) = 
\left\{ \begin{array}{ll}
0 & u \le 0 \\
\infty & u > 0
\end{array} \right.
$$

Bu yaklaşımın nasıl işleyeceğini kabaca tahmin edebiliriz. $I$
fonksiyonu 0'dan büyük değerler için müthiş büyük değerler veriyor, bu
sebeple optimizasyon sırasında o değerlerden tabii ki kaçınılacak, ve
arayış istediğimiz noktalara doğru kayacak. Tabii $x_1 > 3$ gibi bir
şart varsa onu $x_1 - 3 > 0$ şartına değiştiriyoruz ki üstteki
göstergeci kullanabilelim.

Fakat bir problem var, göstergeç fonksiyonunu türevini almak, ve
pürüzsüz rahat kullanılabilen bir yeni fonksiyon elde etmek kolay
değil. Acaba $I$ yerine onu yaklaşık temsil edebilen bir başka sürekli
fonksiyon kullanamaz mıyız?

Log fonksiyonunu kullanabiliriz. Altta farklı $\mu$ değerleri için
$-\mu \log(u)$ fonksiyonun değerlerini görüyoruz. Fonksiyon görüldüğü
gibi $I$'ya oldukca yakın.

```python
def I(u): 
   if u<0: return 0.
   else: return 10.0

u = np.linspace(-3,1,100)
Is = np.array([I(x) for x in u])

import pandas as pd
df = pd.DataFrame(index=u)

df['I'] = Is
df['$\mu$=0.5'] = -0.5*np.log(-u)
df['$\mu$=1.0'] = -1.0*np.log(-u)
df['$\mu$=2.0'] = -2.0*np.log(-u)

df.plot()
plt.savefig('func_40_autograd_04.png')
```





















Kaynaklar 

[1] Nocedal, {\em Numerical Optimization}



























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





















