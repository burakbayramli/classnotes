# MCMC, Gibbs Örneklemleri

State $x$:

$x = (x_1, x_2, \ldots, x_i, \ldots, x_n)$

State $x'$:

$x' = (x_1, x_2, \ldots, x'_i, \ldots, x_n)$

Note These two states are identical except for coordinate $i$:

- Position 1: same ($x_1$)
- Position 2: same ($x_2$)
- ...
- Position $i$: **different** ($x_i$ vs $x'_i$)
- ...
- Position $n$: same ($x_n$)

Therefore: $x_{-i} = x'_{-i}$ (they share all the same non-$i$ coordinates)

Theorem

If $\exists$ distribution $\mu$ such that $\mu(x) T(x \to x') =
\mu(x') T(x' \to x)$, then $\mu$ is stationary.

given $T(x \to x') = p(x'_i \mid x_{-i})$

LHS in English

$p(x) \cdot p(x'_i \mid x_{-i})$ 

$p(x)$: "The probability of being in state $x$" (our candidate $\mu(x)$ )

This is the full joint: $\mu(x_1, x_2, \ldots, x_i, \ldots, x_n)$

$p(x'_i \mid x_{-i})$: "The probability of sampling value $x'_i$ for
variable $i$, given all other variables are at $x_{-i}$"

This is the Gibbs transition: we're sampling a new value for
coordinate $i$

Together: The probability we're currently at state $x$, multiplied by
the probability the Gibbs sampler transitions us to state $x'$ (by
sampling $x'_i$ for the $i$-th coordinate)

