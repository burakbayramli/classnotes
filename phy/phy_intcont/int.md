#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}
Çift Entegre Edici ve Hamiltonian Çözüm

Cift-entegre edici (double-integrator) problemine farkli bir sekilde
bakalim [1]. Bu problemde amac tek eksen üzerinde $y$ diyelim, bir
objeyi bir konumdan diğerine hareket ettirmekti.

Ana fizik formülleri $F = ma$'dan hareketle,

$$
m \ddot{y} = f(t)
$$

olabilir, hız $\dot{y}(t)$, ivme $\ddot{y}(t)$, konum $y(t)$. Eğer

$$
x_1(t) = y(t), \quad x_2(t) = \dot{y}(t)
$$

dersek ODE sistemini şu şekilde tanımlayabiliriz,

$$
\dot{x}_1(t) = x_2(t)
$$

$$
\dot{x}_2(t) = u(t)
$$

ki $u(t) = f(t)/m$ olacak. O zaman sistem formulu

$$
\dot{x} = f(x) = \left[\begin{array}{r}
x_2(t) \\ u(t)
\end{array}\right]
$$

Şimdi bir optimal zaman problemi soralım, ve bir kısıtlama
yaratalım.

Kontrol $|u(t)| < 1, \forall t \in [t_0,t_f]$ olmalı. Bu objeyi
herhangi bir $[x_1(0),x_2(0)]$ başlangıcından orijine minimal zamanda
götürmek için kullanılacak optimal kontrol nasıl hesaplanır?

Zaman optimize edildiği için, $V = 1$

$$
J = \int_{t_0}^{t_f} V \ud t  = \int_{t_0}^{t_f} 1 \ud t = t_f - t_0
$$

tanımını kullanmak bize optimal zaman hesabını verecektir. 

Hamiltonian şöyle oluşturulur,

$$
\mathcal{H}(x,\lambda,u) = V + \lambda^T f 
$$

$$
= 1 + \lambda_1 x_2 + \lambda_2 u
$$

Elimizde lineer ve içbükey bedelli sistem var, bu sistemler için optimalliğin
yeterli şartı $0 = \left( \frac{\partial \mathcal{H}}{\partial u} \right)$, optimallik noktasında elde edilen $u^*$ için

$$
\mathcal{H}(x^*,\lambda^*,u^*) \le \mathcal{H}(x^*, \lambda^*, u) 
$$

$$
= \min_{|u|<1} \mathcal{H}(x^*, \lambda^*, u)
$$

doğru anlamına gelir. Problemimiz icin

$$
1 + \lambda_1^* x_2^* + 1 + \lambda_2^* u^* \le
1 + \lambda_1^* x_2^* + 1 + \lambda_2^* u 
$$

ki bu da

$$
\lambda_2^* u^* \le \lambda_2^* u
$$

demektir. Bu şartı tatmin eden optimal kontrol $u^*$ ne olabilir?
Eğer $\lambda_2^*$ pozitif ise $u^*$ mümkün olan en büyük negatif
değere sahip olmalıdır ki üstteki küçüklük şartı her zaman geçerli
olsun, yani $-1$. Eğer $\lambda_2^*$ negatif ise $u^*$ mümkün olan
pozitif değerde kalmalıdır, yani $+1$. Bu değerleri en basit şekilde

$$
u^*(t) = -sgn(\lambda_2^* (t))
$$

ile özetleyebiliriz, ki $sgn$ fonksiyonu işaret (sign) fonksiyonu, bir
sayının sadece işaretini verir, yani -, + anlaminda, ya da -1, +1.
Mesela 3,4 gibi değerler için +1 döndürür, -3,-6,-4 gibi değerler için
-1 döndürür.













[devam edecek]
