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











[devam edecek]

