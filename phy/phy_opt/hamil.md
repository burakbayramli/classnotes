#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}

Hamiltonian Biçimi 

Daha önce Lagrangian biçimini gördük,

$$
\dot{x} = f( x(t), u(t), t )
$$

Sınır şartları $x(t_0)$ sabit, $x(t_f)$ serbest bırakılmış
haldeydi. Performans ölçütü bizim tanımlayabileceğimiz bir $V$ üzerinden
basit haliyle şöyle idi,

$$
J( u(t) ) = \int _{t_0}^{t_f} V( x(t), u(t), t) \ud t
$$

Sınır şartı $g$ sistem denklemi üzerinden,

$$
g(x(t), \dot{x}(t), u(t), t) = f( x(t), u(t), t) - \dot{x}) = 0
$$

Lagrangian'i oluşturalım ($g$ burada), 

$$
\mathcal{L} = 
\mathcal{L}( x(t), \dot{x}(t), u(t), \lambda(t), t) =
V( x(t), u(t), t) +  \lambda^T g 
$$

$$
= V( x(t), u(t), t) +  \lambda^T \{ f( x(t), u(t), t ) - \dot{x}(t) \}
$$


Performans ölçütü şimdi şöyle oldu,

$$
J_a(u(t)) = \int _{t_0}^{t_f} \mathcal{L}( x(t), \dot{x}(t), u(t), \lambda(t), t)
$$

Eğer Hamiltonian biçimine geçmek istiyorsak, bir $\mathcal{H}$ tanımlarız,

$$
\mathcal{H}(x(t), u(t), \lambda(t), t) = 
V( x(t), u(t), t) + \lambda^T f(x(t), u(t), t)
$$

o zaman Lagrangian $\mathcal{H}$ formunda su hale gelir,

$$
\mathcal
{L}( x(t), \dot{x}(t), u(t), \lambda(t), t) =
\mathcal{H}(x(t), u(t), \lambda(t), t) - \lambda^T \dot{x}(t)
$$
























