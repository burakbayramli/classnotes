# Sembolik Matematik - Sympy

Python'da sembolik matematik, cebirsel manipulasyon gibi pek cok
yararli islem icin bir paket: Sympy. Kurmak icin suradan paket
indirilir, ve klasik sekilde kurulur.

Basit bir test icin x**2 (x kare 2) fonksiyonunun turevini alalim:

```
from sympy import *
x = Symbol('x')
f = x**2
pprint (diff(f, x))
```

Sonuc beklenecegi sekilde 2x gelecektir.

Turev almak diff komutu uzerinden, hangi degiskene gore turev
alinacagi bu fonksiyona ikinci parametre ile bildiriliyor. pprint,
"pretty print"'ten geliyor ve "guzel baski" demek, text ortaminda
olabildigince "grafikimsi" bir baski yap anlaminda.

2x gibi ufak bir formulde pek fark gorulmeyebilir, ama daha cetrefil
formullerde guzel baski farkini gosterecektir (altta gorulecegi
gibi).Daha sofistike bir ornek icin Isi Denklemini kullanalim. Bu
denklem bir kismi diferansiyel (partial differential equation)
denklemidir.Bu denklemin cozumlerinden biri altta verilmistir (kismi
diferansiyel denklemlerde cozum bir foksiyon bulmak demektir, ve
birden fazla fonksiyon cozum olabilir)

Peki bu son denklemin Isi Denklemi icin cozum olup olmadigini kontrol
etmek istesek ne yapardik?  Ustteki u(t,x) formulunun bir t'ye gore
kismi turevini (isi denkleminin sol tarafi), sonra x'e gore iki kez
parcali turevini alip (isi denkleminin sag tarafi) sonuclarin
birbiriyle esit olup olmadigina bakabiliriz.

u(t,x) turevlerini manuel olarak ta yapabiliriz, ama biz ornek olmasi
icin Sympy kullanalim.

```
from sympy import *
x = Symbol('x')
t = Symbol('t')
u = (exp((-x**2)/(4*t)) / 2*sqrt(pi*t))
pprint (diff(u,t))
pprint (diff(diff(u, x),x))
```

Son formulun iki kere arka arkaya basildigini gorecegiz, yani formulun
iki tarafi birbirine esit cikiyor. Demek ki u(t,x) hakikaten bir
cozumdur.Entegrasyon

```
from sympy import *
x = Symbol('x')
q = Symbol('q')
I = Symbol('I')
f = 1/(I*x-q)
pprint (integrate(f, x))
```

Bu ornekte q,I sabit degerlerdi, onlari da sembol olarak
tanimladik. Sonuc:

```
log(I x - q)
```

![](heat.png)

![](heat_sol.png)

