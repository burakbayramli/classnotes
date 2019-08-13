#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \DeclareMathOperator{\bdiv}{div}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Div, Grad, Curl

Bir vektor fonksiyonu $f = \vec{f}(x,y,z)$ dusunelim, 

$$
f = \left[\begin{array}{r} f_1(x,y,z) \\ f_2(x,y,z) \\ f_3(x,y,z) \end{array}\right]
$$

Uzaklaşım (divergence) $\bdiv f$, ya da $\nabla \cdot f$ ile gosterilir, 

$$
\bdiv f = \left(
\frac{\partial f_1}{\partial x} + 
\frac{\partial f_2}{\partial y} + 
\frac{\partial f_3}{\partial z} 
\right)
$$

$\nabla \cdot f$ notasyonu aslında şuradan geliyor, üstteki hesabı
$f$'nin gradyanı ve $f$'nin kendisi arasında bir noktasal çarpım
olarak görebiliriz. 

$$
\nabla \cdot f = \left(
\frac{\partial }{\partial x} i + 
\frac{\partial }{\partial y} j + 
\frac{\partial }{\partial z} k 
\right) 
\cdot
\frac{\partial f_1}{\partial x} i + 
\frac{\partial f_2}{\partial y} j + 
\frac{\partial f_3}{\partial z} k
$$

$$
= \left( \frac{\partial }{\partial x} \right)(f_1) + 
\left( \frac{\partial }{\partial y} \right)(f_2) + 
\left( \frac{\partial }{\partial z} \right)(f_3) 
$$

Bu çarpımın, toplamın sonucu $\bdiv$ hesabını verecektir. 






