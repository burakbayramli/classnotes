#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}

Hamiltonian Biçimi 

Daha önce Lagrangian biçimini gördük, $x=x(t)$, $u=u(t)$,
$\dot{x}=\dot{x}(t)$, $\lambda=\lambda(t)$ olmak üzere, sistem denklemi

$$
\dot{x} = f(x, u, t)
$$

idi, sınır şartları $x(t_0)$ sabit, $x(t_f)$ serbest
bırakılmış. Performans ölçütü bizim tanımlayabileceğimiz bir $V$
üzerinden basit haliyle şöyleydi,

$$
J(u) = \int _{t_0}^{t_f} V(x, u, t) \ud t
$$

Sınır şartı $g$ sistem denklemi üzerinden,

$$
g(x, \dot{x}, u, t) = f(x, u, t) - \dot{x} = 0
$$

Lagrangian'i oluşturalım ($g$ burada), 

$$
\mathcal{L} = \mathcal{L}( x, \dot{x}, u, \lambda, t) =
V( x, u, t) +  \lambda^T g 
$$

$$
= V(x, u, t) +  \lambda^T \big\{ f(x, u, t) - \dot{x} \big\}
\quad (4)
$$


Performans ölçütü şimdi şöyle oldu,

$$
J_a(u) = \int _{t_0}^{t_f} \mathcal{L}( x, \dot{x}, u, \lambda, t)
$$

Eğer Hamiltonian biçimine geçmek istiyorsak, bir $\mathcal{H}$ tanımlarız,

$$
\mathcal{H}(x, u, \lambda, t) = V( x, u, t) + \lambda^T f(x, u, t)
$$

o zaman Lagrangian $\mathcal{H}$ formunda su hale gelir,

$$
\mathcal{L}( x, \dot{x}, u, \lambda, t) = 
\mathcal{H}(x, u, \lambda, t) - \lambda^T \dot{x}
\mlabel{5}
$$

Bu aslında (4)'ün açılmış hali, ve o ilk bölümün $\mathcal{H}$ olarak tanımlanması,

$$ 
\mathcal{L} = \underbrace{V( x, u, t) + \lambda^T f( x, u, t))}_{\mathcal{H}} - 
\lambda^T \dot{x}(t) 
$$ 

Şimdi Euler-Lagrange işlemini hatırlayalım, eldeki değişkenler
$x,\lambda,u$ üzerinden bu denklemler

$$
\left( \frac{\partial \mathcal{L}}{\partial x} \right) -
\frac{\ud}{dt} \left( \frac{\partial \mathcal{L}}{\partial \dot{x}} \right) 
= 0 
\quad
\textrm{konum (state) denklemi}
$$

$$
\left( \frac{\partial \mathcal{L}}{\partial \lambda} \right) -
\frac{\ud}{dt} \left( \frac{\partial \mathcal{L}}{\partial \dot{\lambda}} \right) 
= 0
\quad
\textrm{eskonum (costate) denklemi}
$$

$$
\left( \frac{\partial \mathcal{L}}{\partial u} \right) -
\frac{\ud}{dt} \left( \frac{\partial \mathcal{L}}{\partial \dot{u}} \right) 
= 0
\quad
\textrm{kontrol (control) denklemi}
$$

Üstte belirtildiği gibi bu denklemlere konum, eşkonum, kontrol
denklemleri ismi veriliyor. Şimdi biz bu türetmeyi içinde
$\mathcal{H}$ olan $\mathcal{L}$ için yapacağız, çünkü bu şekilde
belli daha uygun formlar elde etmek istiyoruz, yani (4) denklemini baz
alarak, üstteki üç formülü uygulayınca,

$$
\frac{\partial \mathcal{L}}{\partial x} = 
\frac{\partial \mathcal{H}}{\partial x} - 
\frac{\ud}{\ud t} \left( -\lambda \right)   = 0
$$

$$
\frac{\partial \mathcal{L}}{\partial \lambda} = 
\frac{\partial \mathcal{H}}{\partial \lambda} - \dot{x} -
\frac{\ud}{\ud t} \left( 0 \right)   = 0
$$

$$
\frac{\partial \mathcal{L}}{\partial u} = 
\frac{\partial \mathcal{H}}{\partial u} - 
\frac{\ud}{\ud t} \left( 0 \right)   = 0
$$

Ve bu turetme uzerinden konum, eşkonum, kontrol denklemlerinin yeniden
duzenlenmis hali soyle olur,

$$
\dot{x} = + \left( \frac{\partial \mathcal{H}}{\partial \lambda} \right)
$$

$$
\dot{\lambda} = - \left( \frac{\partial \mathcal{H}}{\partial x} \right)
$$

$$
0 = + \left( \frac{\partial \mathcal{H}}{\partial u} \right)
$$


Üstteki son denklem Hamiltonian $\mathcal{H}$'nin kontrol $u$'ya göre
nasıl optimize edileceğini gösteriyor. Yani $J$ fonksiyonelin sistem
denklemine göre optimize edilmesi problemi şimdi Hamiltonian
fonksiyonunun $u$ bazında optimize edilmesi problemine
dönüştü. Böylece orijinal fonksiyonel optimizasyonunu normal bir
fonksiyon optimizasyon problemine indirgemiş olduk [1, sf. 92].

Ornek [1, sf. 70]













