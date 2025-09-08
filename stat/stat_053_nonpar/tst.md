
Tüm toplamın varyansına gerek yok aslında, toplanan her terim
bağımsız, tıpatıp aynı dağılıma sahipse (i.i.d.), o zaman bazı
kolaylıklar elde ediyoruz, diyelim ki

$$
Y_i := \frac{1}{h} K\!\left(\frac{x - X_i}{h}\right), \quad
\hat f(x) = \frac{1}{n} \sum_{i=1}^n Y_i.
$$

ki $Y_1,\dots,Y_n$ i.i.d. ve her dağılımın varyansı aynı,
$\sigma^2$. Tüm toplam üzerinde varyans alırsak,

$$
Var \left[ \frac{1}{n}  \sum_{i=1}^n Y_i \right] =
\frac{1}{n^2}\, n \sigma^2 \;=\; \frac{1}{n} \sigma^2.
$$

$$
Var \left[ \hat f(x) \right] = \frac{1}{n} \, \mathrm{Var}[Y_1].
$$

$Y_1$ kullanıldı, yani herhangi bir $Y_i$ anlamında.. Üstteki diyor ki
toplam içindeki tek bir terimin varyansını hesaplayıp $n$'ye bölersek
istenilen sonuca erisebiliriz. O şekilde devam edelim o zaman,

$$
Var[\hat{f}(x)] = \frac{1}{n} Var \left[
\frac{1}{h} K(\frac{x-x_i}{h})
\right]
$$

Temel İstatistiği hatırlarsak $Var(X) = E(X^2) - E(X)^2$

$$
= \frac{1}{n} \left[
\int \frac{1}{h} K^2(u) (f(x) - h u f'(x) ) \,du - f^2(x) + O(h) 
\right]
$$

$$
= \frac{f(x)}{hn} \int K^2(u) \,du -
f'(x) \int u K^2(u) \,du -
f^2(x) / n +
O(h) / n
\qquad{(2)}
$$

Ilk terimde $\int K^2(u) \,du$ var, bu bir sabite esittir.

$$
K(u)^2 = \left( \frac{1}{\sqrt{2\pi}} e^{-u^2/2} \right)^2
= \frac{1}{2\pi} e^{-u^2}.
$$

$$
\int_{-\infty}^{\infty} K(u)^2 \, du
= \frac{1}{2\pi} \int_{-\infty}^{\infty} e^{-u^2} \, du.
$$

Eşitliğin sağındaki entegralin $\sqrt{\pi}$ olduğu biliniyor, o zaman

$$
\Rightarrow \frac{1}{2\pi} \cdot \sqrt{\pi} \Rightarrow
\int_{-\infty}^{\infty} K(u)^2 \, du = \frac{1}{2\sqrt{\pi}} = R(K)
$$

Üstteki sonuç literatürde $R(K)$ ile anılır.

İkinci terimde $\int u K^2(u) du$ var, bu sıfırdır, $K(u)$ bir çift
fonksiyon (tek fonksiyon, çift fonksiyon kavramlarını hatırlarsak,
$K(u)=K(-u)$), ve $K^2(u)$ yine çift, fakat $u K^2(u)$ tek çünkü $u$
değişkeni, bir fonksiyon olarak düşünülürse tektir. Ve tek
fonksiyonların sıfır etrafındaki simetrik limitleri üzerinden
entegrali sıfırdır.

Formül (2)'den geriye kalanlar

$$
= \frac{1}{n} \left[ \frac{f(x)}{h}R(K) - f(x)^2 + O(h^2)  \right]
$$


$$
= \frac{f(x)}{hn} \int K^2 (u) \,du + O(1/n)
$$
