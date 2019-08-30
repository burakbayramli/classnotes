#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \DeclareMathOperator{\bdiv}{div}
#+LaTeX_HEADER: \DeclareMathOperator{\curl}{curl}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Bilinmeyen Sabit Durumları

Her TPBVP problemi üstte görülen yazılımlara direk, olduğu gibi aktarılacak
durumda olmayabilir. Ama bu problemleri de bazı numaralar kullanarak
çözüm yazılımının beklediği hale çevirebiliriz. 

Mesela çoğu sınır değeri problemi bilinmeyen sabitler içerirler, ki çözüm
içi bu sabitlerin bilinmesi gereklidir. Bu tür problemler üstteki
yöntemlerle direk çözülemez. Ama bilinmeyen sabitleri de birer değişken
olarak kabul edersek, bu engelin etrafından dolaşabiliriz. Bu
``değişkenin'' ilk türevi tabii ki sıfır olacaktır, ve bu türevin sıfıra
eşit olma hali üzerinden sabiti diferansiyel denklem sistemimize dahil
edebiliriz. 

{\em Elastiklik} (elastica) problemi mekanik alanında standart bir problem
[3, sf. 221]. Bir ucu yere bağlı bir esnek çubuğa diğer uçtan uygulanan
kuvvet ile nasıl büküldüğünü gösteriyor. Sabiti dahil etmeden ana sistem
şöyle,

$$
x' = \cos (\phi) 
$$

$$
y' = \sin (\phi) 
$$

$$
\phi' = \kappa
$$

$$
\kappa' = F \cos (\phi)
$$

Bilinmeyen sabit $F$. Sınır değerleri,

$$
x(0) = 0
$$

$$
y(0) = 0
$$

$$
\kappa(0) = 0
$$

$$
y(0.5) = 0
$$

$$
\phi(0.5) = -\pi/2
$$

Görülen beş tane sınır değişkeniyle aslında problemi tamamen tanımlanmış
oluyor. Entegrasyondan ortaya çıkan dört tane sabit olacak, üstte beş tane
değer var. Hatta $F$'yi de bir sınır değeri olarak dahil edince hala çözüm
için yeterli öğe elimizde oluyor. Yeni sınır değeri

$$
F' = 0
$$

Dikkat, yani $F$'nin türevi her yerde sıfır olmalı (çünkü sabit).

[devam edecek]

```python
# x,y,\phi,\kappa, F
from scipy.integrate import solve_bvp

def fun(x, y):
    return np.vstack(( np.cos(y[2]),
                       np.sin(y[2]),
                       y[3],
                       y[4]*np.cos(y[2]),
                       np.zeros(x.shape[0]) ))

def bc(ya, yb):
    return np.array([ ya[0],
                      ya[1],
                      ya[3],
                      yb[1],
                      yb[2]+(np.pi/2) ])

x = np.linspace(0, 0.5, 400)
y = np.zeros((5, x.size))
sol = solve_bvp(fun, bc, x, y)
print (sol.y[4,0])
```

```text
-21.54910449259776
```






















