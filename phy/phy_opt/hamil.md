#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}

Ornek [1, sf. 70]

Cift entegre edici (double-integrator) sistemine bakalim. 

$$
\dot{x}_1(t) = x_2(t)
$$

$$
\dot{x}_2(t) = u(t)
$$

Performans olcutu 

$$
J = \frac{1}{2} \int _{t_0}^{t_f} u^2 \ud t
$$

Yani $u(t)$'nin, tüm değerlerinin, ortalama olarak fazla büyük
olmasını istemiyoruz. Sınır şartları $\underbar{x} =
\left[\begin{array}{cc} x_1 & x_2 \end{array}\right]^T$ 
olmak üzere,

$$
\underbar{x}(0) = \left[\begin{array}{cc} 1 & 2 \end{array}\right]^T \quad 
\underbar{x}(2) = \left[\begin{array}{cc} 1 & 0 \end{array}\right]^T 
$$

Yazının geri kalanında $\underbar{x}$, vs. kullanılmayacak, çerçeveden
boyut tahmin edilebilir.

Çözüm

Hamiltonian'ı oluşturalım çünkü tüm sonuç türevleri ona göre alınıyor
artık; o zaman $V,\lambda,f$ gerekiyor. 

$$
V(x,u,t) = V(u) = \frac{1}{2} u^2
$$

$$
f(x,u,t) = \left[\begin{array}{cc} f_1 & f_2 \end{array}\right]^T
$$

oyle ki $f_1 = x_2(t)$, $f_2 = u(t)$. 

Hamiltonian

$$
\mathcal{H} = \mathcal{H}(x_1, x_2, u, \lambda_1, \lambda_2)
$$

$$
= V(u) + \lambda^T f(x,u)
$$

$$
= \frac{1}{2} u^2 + \lambda_1 x_2 + \lambda_2 u 
$$

Optimal $u^*$'yu bulmak için $\frac{\partial \mathcal{H}}{\partial u}$ denklemini kullanıyoruz,

$$
\left( \frac{\partial \mathcal{H}}{\partial u} \right) = 0 \to
u^* + \lambda_2^* = 0
$$

$$
u^* = -\lambda_2^*
$$

Optimal $\mathcal{H}$'yi bulmak icin ustteki degerleri uc ustteki
formule sokuyoruz, 

$$
\mathcal{H}^*(x_1^*, x_2^*,\lambda_1^*,\lambda_2^*) = 
\frac{1}{2} \lambda_2^* + \lambda_1^* x_2^* - \lambda_2^* 
$$

$$
= \lambda_1^* x_2^* - \frac{1}{2} {\lambda_2^*}^2  
$$

Devam edersek, $\dot{x} = \left( \frac{\partial \mathcal{H}}{\partial
\lambda} \right)$  denkleminden hareketle,

$$
\dot{x}^*_1 = \left( \frac{\partial \mathcal{H}}{\partial \lambda_1} \right) =
x_2^*
$$

$$
\dot{x}^*_2 = \left( \frac{\partial \mathcal{H}}{\partial \lambda_2} \right) =
\lambda_2^*
$$

Ve $\dot{\lambda} = - \left( \frac{\partial \mathcal{H}}{\partial x}
\right)$ denkleminden hareketle,

$$
\dot{\lambda}_1^* = - \left( \frac{\partial \mathcal{H}}{\partial x_1} \right) = 0
$$

$$
\dot{\lambda}_2^* = - \left( \frac{\partial \mathcal{H}}{\partial x_2} \right) = 
- \lambda_1^*
$$

```python
from sympy import symbols, Eq, Function, dsolve, latex, simplify

t = symbols('t') 
x1,x2,lam1,lam2 = symbols('x1 x2 lam1 lam2',cls=Function)

system = [Eq(x1(t).diff(t), x2(t)), \
          Eq(x2(t).diff(t), -lam2(t)), \
          Eq(lam1(t).diff(t), 0), \
          Eq(lam2(t).diff(t), -lam1(t)),  \
          ]

sol = dsolve(system, [x1(t),x2(t),lam1(t),lam2(t)])
print (latex(simplify(sol[0])))
print (latex(simplify(sol[1])))
print (latex(sol[2]))
print (latex(sol[3]))
```

```text
x_{1}{\left(t \right)} = C_{1} + C_{2} t + C_{2} + \frac{C_{3} t^{2}}{2} + C_{3} t + C_{3} + \frac{C_{4} t^{3}}{6} + \frac{C_{4} t^{2}}{2} + C_{4} t + C_{4}
x_{2}{\left(t \right)} = C_{2} + C_{3} t + C_{3} + \frac{C_{4} t^{2}}{2} + C_{4} t + C_{4}
lam_{1}{\left(t \right)} = C_{4}
lam_{2}{\left(t \right)} = - C_{3} - C_{4} t - C_{4}
```

$$
x_{1}{\left(t \right)} = C_{1} + C_{2} t + C_{2} + \frac{C_{3} t^{2}}{2} + C_{3} t + C_{3} + \frac{C_{4} t^{3}}{6} + \frac{C_{4} t^{2}}{2} + C_{4} t + C_{4}
$$

$$
x_{2}{\left(t \right)} = C_{2} + C_{3} t + C_{3} + \frac{C_{4} t^{2}}{2} + C_{4} t + C_{4}
$$

$$
lam_{1}{\left(t \right)} = C_{4}
$$

$$
lam_{2}{\left(t \right)} = - C_{3} - C_{4} t - C_{4}
$$


Sınır şartlarını tanımlayarak çözersek, ve sadece $\lambda_2$'ye
bakarsak (çünkü $u(t)$ sonucunu $u(t) = -\lambda_2^*(t)$ olarak bulmuştuk),


```python
ics = { x1(0):1, x2(0):2, x1(2):1, x2(2):0 } 
sol = dsolve(system, [x1(t),x2(t),lam1(t),lam2(t)], ics=ics)
print (latex(sol[3]))
```

```text
lam_{2}{\left(t \right) = 4 - 3 t
```

$$
\lambda_{2}{\left(t \right)} = 4 - 3 t
$$

O zaman, ve 

$$
u = -\lambda_2 = 3t - 4
$$











