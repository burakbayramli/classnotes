#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}
Rayleigh Problemi

Bir elektrik devresi düşünelim, bu devre düz voltajı salınıma çevirebiliyor, 

\includegraphics[width=20em]{phy_num_01.png}

Devreyi sol taraftan verilen $U_0(t)$ ile kontrol etmek mümkün [1,
sf. 189], [2, sf. 413]. Devreninin denklemi

$$
\ddot{x} = -x(t) + \dot{x}(t) ( 2.0 - p \dot{x}(t)^2 ) +  u(t)
$$

ki biz $p = 0.1$ seçeceğiz. ODE sistemi çıkartmak için $x_1 = x$, $x_2
= \dot{x}$ dersek,

$$
\dot{x_2} = -x_1 + (2.0 - 0.1 x_2^2)x_2 + 4 u(t)
$$

Acaba $x_1(t=0)=-5$ ve $x_2(t=0)=-5$ başlangıç şartları için,
$t_f=2.5$ anına kadar kontrolü ve salınımı az seviyede tutmaya
çalışsak nasıl bir kontrol uygulamamız gerekir?

Hamiltonian'i tanimlarken

$$
\mathcal{H} = V + \lambda^T f
$$

formülü verilmişti. Üstteki tanımda ODE denklem sistemi $f$,

$$
V = x_1^2 + u^2
$$

Şimdi $\mathcal{H}$'yi sembolik olarak bulalım,


```python
import sympy

u, x1, x2, lam1, lam2 = sympy.symbols('u x1 x2 lam1 lam2')
x = sympy.Matrix([[x1],[x2]])
lam = sympy.Matrix([[lam1],[lam2]])
f = sympy.Matrix([[x[1]],[  -x[0]+(2.0 - 0.1*x[1]**2)*x[1] + 4*u ]])
V = x[0]**2 + u**2
H = V + lam.T.dot(f)
print (H)
print (sympy.latex(H))
```

```text
lam1*x2 + lam2*(4*u - x1 + x2*(2.0 - 0.1*x2**2)) + u**2 + x1**2
lam_{1} x_{2} + lam_{2} \left(4 u - x_{1} + x_{2} \left(2.0 - 0.1 x_{2}^{2}\right)\right) + u^{2} + x_{1}^{2}
```

$$
\mathcal{H} = \lambda_{1} x_{2} + \lambda_{2} \left(4 u - x_{1} + x_{2} \left(2.0 - 0.1
x_{2}^{2}\right)\right) + u^{2} + x_{1}^{2}
$$

Eger bu formulu biraz masajlarsak, [2]'deki sonucu elde ederiz,

$$
= x_1^2 + u^2 + \lambda_1 x_2 - \lambda_2
\left( x_1 - 4u + x_2 \left( \frac{x_2^2}{10} - 2 \right)  \right)
$$

```python
lam_dot = -sympy.diff(H, x).T
print (lam_dot)
```

```text
Matrix([[lam2 - 2*x1, -lam1 - lam2*(2.0 - 0.3*x2**2)]])
```





[devam edecek]

