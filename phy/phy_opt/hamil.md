#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}

Hamiltonian Biçimi 

Daha önce Lagrangian biçimini gördük,

$$
\dot{x} = f( x(t), u(t), t )
$$

Sınır şartları $x(t_0)$ sabit, $x(t_f)$ serbest bırakılmış
haldeydi. Performans ölçütü basit haliyle şöyle idi,

$$
J( u(t) ) = \int _{t_0}^{t_f} V( x(t), u(t), t) \ud t
$$

Sınır şartı $g$ soyle olsun,

$$
g(x(t), \dot{x}(t), u(t), t) = f( x(t), u(t), t) - \dot{x}) = 0
$$

Lagrangian'i oluşturalım ($g$ burada), 

$$
\mathcal{L} = V( x(t), u(t), t) +  \lambda^T \{ f( x(t), u(t) ) - \dot{x}(t) \}
$$

$$
= \mathcal{L}( x(t), \dot{x}(t), u(t), \lambda(t), t)
$$

Performans ölçütü şimdi şöyle oldu,

$$
J_a(u(t)) = \int _{t_0}^{t_f} \mathcal{L}( x(t), \dot{x}(t), u(t), \lambda(t), t)
$$





