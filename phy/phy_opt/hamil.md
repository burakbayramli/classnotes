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
olmasını istemiyoruz. Sınır şartları $\bar{x} =
\left[\begin{array}{cc} x_1 & x_2 \end{array}\right]^T$ olmak uzere,

$$
\underbar{x}(0) = \left[\begin{array}{cc} 1 & 2 \end{array}\right]^T \quad 
\underbar{x}(2) = \left[\begin{array}{cc} 1 & 0 \end{array}\right]^T 
$$












