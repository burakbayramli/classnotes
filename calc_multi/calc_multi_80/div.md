#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \DeclareMathOperator{\bdiv}{div}
#+LaTeX_HEADER: \DeclareMathOperator{\curl}{curl}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Div, Grad, Curl

Bir vektör fonksiyonu $f = \vec{f}(x,y,z)$ düşünelim, 

$$
f = \left[\begin{array}{r} f_1(x,y,z) \\ f_2(x,y,z) \\ f_3(x,y,z) \end{array}\right]
$$

Yonsel Turev (Directional Derivative)

Formel olarak $f \cdot \nabla$ bir operator tanimlar [2, sf. 74],

$$
f \cdot \nabla = 
\left[\begin{array}{ccc} f_1 & f_2 & f_3 \end{array}\right]
\cdot 
\left[\begin{array}{ccc} 
\frac{\partial }{\partial x} & \frac{\partial }{\partial y} & \frac{\partial }{\partial z} 
\end{array}\right]
$$

$$
= 
f_1 \frac{\partial }{\partial x} +
f_2 \frac{\partial }{\partial y} +
f_3 \frac{\partial }{\partial z} 
$$

Ustteki bir operator. Onu mesela bir $g$ uzerinde uygulayabiliriz,

$$
(f_1 \frac{\partial }{\partial x} +
f_2 \frac{\partial }{\partial y} +
f_3 \frac{\partial }{\partial z}) 
g = 
f_1 \frac{\partial g}{\partial x} +
f_2 \frac{\partial g}{\partial y} +
f_3 \frac{\partial g}{\partial z} 
$$


Uzaklaşım (divergence)

Bu hesap $\bdiv f$, ya da $\nabla \cdot f$ ile gosterilir,

$$
\bdiv f = \left(
\frac{\partial f_1}{\partial x} + 
\frac{\partial f_2}{\partial y} + 
\frac{\partial f_3}{\partial z} 
\right)
$$

$\nabla \cdot f$ notasyonu üstteki hesabı $f$'nin gradyanı ve $f$'nin
kendisi arasında bir noktasal çarpım olarak görmek.

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

Curl hesabını $\nabla$ bazında yapabilir miyiz? Evet, 

$$
\curl f = \nabla \times f
$$

Bu nasil oldu [1, sf. 178]? 

$$
= \left(
\frac{\partial }{\partial x}, 
\frac{\partial }{\partial y},
\frac{\partial }{\partial z} 
\right) 
\times 
\left[\begin{array}{r} f_1(x,y,z) \\ f_2(x,y,z) \\ f_3(x,y,z) \end{array}\right]
$$

$$
= \left[\begin{array}{ccc}
i & j & k \\
\frac{\partial }{\partial x} &
\frac{\partial }{\partial y} &
\frac{\partial }{\partial z} \\
f_1 & f_2 & f_3
\end{array}\right]
$$


$$
= \left[\begin{array}{ccc} 
\frac{\partial f_3}{\partial y} - \frac{\partial f_2}{\partial z} & 
\frac{\partial f_1}{\partial z} - \frac{\partial f_3}{\partial x} & 
\frac{\partial f_2}{\partial x} - \frac{\partial f_1}{\partial y} 
\end{array}\right]
$$

Bazi Esitlikler

$a,b$ vektor fonksiyonu olmak uzere, 

$$
\nabla (a \cdot b) = 
(a \cdot \nabla) b + (b \cdot \nabla) a + 
a \times (\nabla \times b) + 
b \times (\nabla \times a)
$$









Kaynaklar 

[1] Corral, {\em Vector Calculus}

[2] Spiegel, {\em Vector Tensor Analysis}
