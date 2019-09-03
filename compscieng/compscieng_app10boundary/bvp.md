#+LaTeX_HEADER: \newcommand{\ud}{\,\mathrm{d}}
#+LaTeX_HEADER: \newcommand{\mlabel}[1]{\quad \text{(#1)}\quad}
#+LaTeX_HEADER: \DeclareMathOperator{\bdiv}{div}
#+LaTeX_HEADER: \DeclareMathOperator{\curl}{curl}
#+LaTeX_HEADER: \usepackage{palatino,eulervm}
#+LaTeX_HEADER: \usepackage{cancel}

Sinir Sistemi Tepkisi (Nerve Impulse)

Tekrar eden sınır şart durumunu bilinmeyen entegral limiti durumu ile
birleştiren bir problemi göreceğiz şimdi [8, sf. 27, 3, sf. 225, 6, 5,
sf. 173]. Bilinmeyen sinir sarti icin bir numara yapabiliriz; Yeni bir
değişken $\tau$ tanımlıyoruz, ve bu değişken sadece $[0,1]$ aralığına
kısıtlanıyor. Bağımsız değişken $x$.  Şimdi $\tau = x/b$, ki $b$
bilinmeyen entegral sınırı,

$$
\frac{\ud y}{\ud x} = f(x,y)
$$

yerine 

$$
\frac{\ud y}{\ud \tau} = b f(x,y)
$$

$$
\frac{\ud b}{\ud \tau} = 0
$$

Problem sorusuna gelelim. Diyelim ki hücre zar potansiyeli $y_1$,
geçirgenlik $y_2$, ve

$$
y_1' = 3 (y_1 + y_2 - 1/3 y_1^3 - 1.3)
$$

$$
y_2' = -(y_1 - 0.7 + 0.8 y_2) / 3
$$

Bu problem $[0,T]$ zaman diliminde tanımlanmıştır. Fakat bir problem
şu, $T$'nin ne olduğunu bilmiiyoruz, ve aynı $T$ bilinmeyeni sinir
şartlarının tanımlanmasında kullanılmış. 

$$
y_1(0) = y_1(T), \quad y_2(0) = y_2(T)
$$

O zaman biraz önceki numarayı kullanabiliriz. 

$$
y_1' = 3 T (y_1 + y_2 - 1/3 y_1^3 - 1.3)
$$

$$
y_2' = -T (y_1 - 0.7 + 0.8 y_2) / 3
$$
 
$$
T' = 0
$$

Böylece problem $[0,1]$ arasında tanımlanmış oldu, ve sinir şartları 

$$
y_1(0) = y_1(1), \quad y_2(0) = y_2(1)
$$












