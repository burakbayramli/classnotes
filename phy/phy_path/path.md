#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}
Optimal Yol Hesabı

Bir geminin çok kuvvetli dalgaların olduğu bir bölgeden geçmesi
gerekiyor. Bu dalgaların büyüklüğü ve yönü her nokta için o noktaya
etki eden bir vektör olarak gösterilebilir [1, sf. 77]. Bu vektörün
iki öğesi, 

$$
u = u(x,y), \quad v = v(x,y)
$$

Geminin hızı $V$'nin suya göre sabit olduğunu düşünelim. Bu gemiyi
nasıl yönlendirirdik ki $A$ noktasından $B$ noktasına minimal zamanda
gidebilelim?

Problemi çözmek için hareket denklemlerini yazalım, 

$$
\dot{x} = V \cos\theta + u(x,y)
$$

$$
\dot{y} = V \sin\theta + v(x,y)
$$

$\theta$ geminin burnunu hangi yöne doğru tuttuğumuzu kontrol ediyor,
yani geminin hızı $V$ o yönde uygulanmış oluyor. $\theta$ sabitlenmiş
kordinat eksenlerine göredir, ve $x,y$ bu eksende geminin yönünü
gösterir.  Dikkat: $u$ çoğunlukla kontrol girdisi olarak gösterilir,
burada böyle değil.

$$
\mathcal{H}(x, u, \lambda, t) = \mathcal{L}( x, u, t) + \lambda^T f(x, u, t) 
$$

Zaman optimizasyonu icin $\mathcal{L} = 1$. Niye? Mesela

$$
J = \int _{t_0}^{t_f} \mathcal{L}( x, \dot{x}, u, \lambda, t)
$$

performans ölçütünü düşünelim, $\mathcal{L} = 1$ seçmek entegral
çözümünden hareketle $J = t_f - t_0$ anlamına gelir, yani geçen
zamanın hesabı.

Diğer değişken $\lambda =
\left[\begin{array}{cc} \lambda_x & \lambda_y \end{array}\right]^T$
diyelim, sistem denklemi $f$ biraz önce verildi zaten.

Bu sistemin Hamiltonian'ı 

$$
\mathcal{H} = 
\lambda_x (V \cos \theta + u ) + 
\lambda_y (V \sin \theta + v ) + 1
\mlabel{1}
$$

Devam edersek, Euler-Lagrange denklemleri

$$
\dot{\lambda}_x = -\frac{\partial H}{\partial x }  = 
-\lambda_x \frac{\partial u}{\partial x} - 
 \lambda_y \frac{\partial v}{\partial x}
$$

$$
\dot{\lambda}_y = -\frac{\partial H}{\partial y}  = 
-\lambda_x \frac{\partial y}{\partial y} - 
 \lambda_y \frac{\partial u}{\partial y}
$$

$$
0 = \frac{\partial H}{\partial \theta}  = 
V (-\lambda_x \sin \theta + \lambda_y \cos \theta ) \to
\tan\theta = \frac{\lambda_y}{\lambda_x}
$$

Hamiltonian $\mathcal{H}$ zaman $t$'ye direk / belirgin şekilde bağlı
olmadığı durumlarda $\mathcal{H} = \textrm{sabit}$ sistemin
entegrallerinden biridir. İspatlayalım,

$$
\frac{\ud \mathcal{H}(x, u, \lambda, t)}{\ud t} = 
\cancel{\frac{\partial \mathcal{H}}{\partial t}} + 
\frac{\partial \mathcal{H}}{\partial x}\frac{\partial x}{\partial t} + 
\frac{\partial \mathcal{H}}{\partial u}\frac{\partial u}{\partial t} + 
\frac{\partial \mathcal{H}}{\partial \lambda}\frac{\partial \lambda}{\partial t} 
$$

İptalleme mümkün çünkü $\mathcal{H}$ zamandan bağımsız. Devam edelim [1, sf 49],

$$
\dot{H} = \mathcal{H}_x \dot{x} + H_u \dot{u} + \dot{\lambda}^T f
$$

$f$ nereden geldi, Euler-Lagrange denklemlerini hatırlarsak,

$$
\dot{x} = + \left( \frac{\partial \mathcal{H}}{\partial \lambda} \right)
\quad
\dot{\lambda} = - \left( \frac{\partial \mathcal{H}}{\partial x} \right)
\quad
0 = + \left( \frac{\partial \mathcal{H}}{\partial u} \right)
$$

Soldan birinci denklem ile $\dot{x}$ yerine $\frac{\partial
\mathcal{H}}{\partial \lambda}$ kullanabiliyoruz, ayrica $\dot{x} = f$
olduğuna göre bu yerine geçirme mümkün oluyor. 

Aynı şekilde $\dot{x}$ yerine $f$ ve gruplama sonrası,

$$
\dot{H} = \mathcal{H}_u \dot{u} + (\mathcal{H}_x + \dot{\lambda}^T) f
$$

E-L soldan ikinci denklem ile parantez içi sıfır olur, geri
kalanlardan, E-L soldan üçüncü denklem ile,

$$
= \cancelto{0}{\mathcal{H}_u \dot{u}}
$$

Ve sonuç,

$$
\dot{H} = 0
$$

Eğer üstteki doğruysa o zaman optimal gidiş yolu üzerinde $t$
üzerinden entegral $\mathcal{H}$'in sabit olması gerekir. Dikkat
edersek $\mathcal{H}_u = 0$ sıfır şartı optimal gidiş yolu üzerinde
geçerlidir. 

Varabileceğimiz bir diğer sonuç $\mathcal{H}$'nin sabit olmak ötesinde
sıfıra eşit olması mecburiyetidir. Bu nereden geliyor? Eğer zaman
üzerinden optimize ediyorsak ve $\mathcal{H}$ zamana bağlı değilse ve
sabitse, zamanın herhangi bir yerde olabilmesi için sadece sıfır
sabiti bu işi yapabilir.
Çözüme gelelim. (1)'den başlayalım, 

$$
0 = \lambda_x (V \cos\theta + u) + \lambda_y (W \sin\theta + v) + 1
$$

$$
-\frac{1}{\lambda_x} = \cancel{\frac{\lambda_x}{\lambda_x}}
(V  \cos\theta + u) + \frac{\sin\theta}{\cos\theta} (V \sin\theta + v)
$$

$$
\frac{-\cos\theta}{\lambda_x} = 
V \cos^2\theta + u\cos\theta + V \sin^2\theta + V \cos\theta
$$

$$
= V (\cancelto{1}{\cos^2\theta + \sin^2\theta}) + u + v
$$

$$
\lambda_x = \frac{-\cos\theta}{V + u\cos\theta + v\sin\theta}
$$

Benzer işlemler sonrası 

$$
\lambda_y = \frac{-\sin\theta}{V + u\cos\theta + v\sin\theta}
$$

elde edilebilir.








[devam edecek]

Kaynaklar

[1] Bryson, Ho, {\em Applied Optimal Control}

[2] Radhakant Padhi, {\em OPTIMAL CONTROL, GUIDANCE AND ESTIMATION}, 
    \url{https://nptel.ac.in/courses/101108057/downloads/Lecture-34.pdf}


