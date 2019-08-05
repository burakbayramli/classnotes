#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}

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

ki $\theta$ geminin burnunu hangi yöne doğru tuttuğumuzu kontrol
ediyor, yani geminin hızı $V$ o yönde uygulanmış oluyor. $\theta$
sabitlenmiş kordinate eksenlerine göredir, ve $x,y$ bu eksende geminin
yönünü gösterir.  Dikkat: $u$ çoğunlukla kontrol girdisi olarak
gösterilir, burada böyle değil.


Hamiltonian genel olarak şu formdadır (notasyon karışmasın diye $V$
yerine $W$ kullandık),

$$
\mathcal{H}(x, u, \lambda, t) = W( x, u, t) + \lambda^T f(x, u, t)
$$

Optimize edilecek bedel $W(x,y,\theta,t) = 1$. Diğer değişken $\lambda
= \left[\begin{array}{cc} \lambda_x & \lambda_y \end{array}\right]^T$
diyelim, sistem denklemi $f$ biraz önce verildi zaten.

Bu sistemin Hamiltonian'ı 

$$
\mathcal{H} = 
\lambda_x (V \cos \theta + u ) + 
\lambda_y (V \sin \theta + v ) + 1
$$










[1] Bryson, Ho, {\em Applied Optimal Control}
