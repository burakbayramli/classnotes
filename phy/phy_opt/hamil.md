#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
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
$$

Bu aslında (4)'ün açılmış hali, ve o ilk bölümün $\mathcal{H}$ olarak tanımlanması,

$$
\mathcal{L} = \underbrace{V( x, u, t) +  \lambda^T f( x, u, t ))}_{\mathcal{H}} - \lambda^T \dot{x}(t)
$$



















