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

Yazının geri kalanında $\underbar{x},\underbar{\lambda}$, vs.
kullanılmayacak, çerçeveden boyut tahmin edilebilir. 

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

















