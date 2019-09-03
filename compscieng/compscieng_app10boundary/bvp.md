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
kısıtlanıyor. Bağımsız değişken $x$.  Şimdi $\tau = t/b$, ki $b$
bilinmeyen entegral sınırı,

$$
\frac{\ud y}{\ud x} = f(t,y)
$$

yerine 

$$
\frac{\ud y}{\ud \tau} = b f(t,y)
$$

$$
\frac{\ud b}{\ud \tau} = 0
$$

Bu nasıl oldu? 

$t = \tau b$ ise $\ud x / \ud \tau = b$. 

$$
\frac{\ud y}{\ud \tau} = \frac{\ud y}{\ud t} \frac{\ud t}{\ud \tau} = 
b f(x,y)
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
şu, $T$'nin ne olduğunu bilmiiyoruz, ve aynı $T$ bilinmeyeni sınır
şartlarının tanımlanmasında kullanılmış. 

$$
y_1(0) = y_1(T), \quad y_2(0) = y_2(T)
$$

O zaman biraz önceki numarayı kullanabiliriz. $y_3 = T$ diyelim,

$$
y_1' = 3 T (y_1 + y_2 - 1/3 y_1^3 - 1.3)
$$

$$
y_2' = -T (y_1 - 0.7 + 0.8 y_2) / 3
$$
 
$$
y_3' = 0
$$

Böylece problem $[0,1]$ arasında tanımlanmış oldu, ve sınır şartları 





```python
from scipy.integrate import solve_bvp

def fun(x, y, p):
    T = p[0]
    return np.vstack((
        3.0 * T *  (y[0] + y[1] - (y[0]**3)/3.0 - 1.3),
        -T / 3.0 * (y[0] - 0.7 + 0.8*y[1]) 
    ))

def bc(ya, yb, p):
    return np.array( [ ya[0], yb[0], ya[1]-yb[1] ]   )
                     
x = np.linspace(0, 1, 5)
y[0] = np.sin(2 * np.pi * x)
y[1] = np.cos(2 * np.pi * x)
sol = solve_bvp(fun, bc, x, y, p=[2*np.pi])
print (sol.p)
```
```text
[10.71071556]
```
