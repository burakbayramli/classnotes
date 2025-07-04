# Ders 23

Bu derste birinci dereceden, sabit katsayılı (coefficients) lineer
diferansiyel denklem sistemini çözmeye göreceğiz. Form

$$ \frac{du}{dt} = A u $$

şeklinde olacak. Katsayılar $A$ matrisi içinde. Temel fikir: sabit katsayılı
lineer diferansiyel denklemlerin çözümü üsteldir (exponential). Sonucun
üstel olduğunu bilince bulmamız gereken üstel değerin ne olduğu, yani $e$
üzerine ne geldiği, onu neyin çarptığı, ki bunları bulmak lineer cebirin
işi olacak.

Bir örnekle başlayalım

$$ \frac{du_1}{dt} = -u_1 + 2u_2 $$

$$ \frac{du_2}{dt} = u_1 - 2u_2 $$

Üstteki sistemin katsayılarını dışarı çekersek $A$ matrisi şu olur

$$ A = 
\left[\begin{array}{cc}
-1 & 2 \\
1 & -2
\end{array}\right]
$$

Başlangıç değerleri şöyle olsun

$$ u(0) = 
\left[\begin{array}{c}
1 \\
0
\end{array}\right]
$$

Eğer $u_1$ ve $u_2$'yi bu sistemin temsil ettiği iki kap gibi görsek
(mesela), her şey $u_1$'in "içinde'' olarak başlayacaktı, sonra zaman
geçtikçe oradan çıkacak, $u_2$'ye doğru akacak. Tüm bunları $A$ matrisinin
özdeğer/vektörlerine bakarak anlayabilmemiz lazım. O zaman ilk işimiz
özdeğer/vektörleri bulmak olmalı. 

$A$'ya bakınca ne görüyoruz? Bir kolon diğerinin katı, o zaman matris
eşsiz (singular), bu demektir ki bir özdeğer $\lambda = 0$. Diğer
özdeğer için bir numara kullanalım; özdeğerlerin toplamı matris izine
(trace) yani çaprazdaki sayıların toplamına eşit olduğuna göre, ve toplam
$=-3$, o zaman ikinci özdeğer $-3$ olmalı. Özvektörler ise

$$ 
\left[\begin{array}{c}
2 \\ 1
\end{array}\right] 
,
\left[\begin{array}{c}
1 \\ -1
\end{array}\right]
$$

O zaman çözüm iki çözümün toplamı olacak

$$
u(t) = c_1 e^{\lambda_1 t}x_1 + c_2 e^{\lambda_2 t}x_2
$$

Bu genel çözüm. Genel çözüm $c_1, c_2$ haricindeki birinci ve ikinci terimdeki
"pür üstel'' çözümlerden oluşuyor. Hakikaten mesela $e^{\lambda_1 t}x_1$
diferansiyel denklemi çözüyor değil mi? Not, $x_1$ $A$ matrisinin birinci
kolonu. Kontrol edelim. Denklem

$$ \frac{du}{dt} = Au$$

$u$ için $e^{\lambda_1t}x_1$ koyalım, türev $t$'ye göre alındığına göre

$$ \lambda_1 e^{\lambda_1t}x_1 = A e^{\lambda_1t}x_1$$

İki taraftaki üstel değerler iptal olur, geri kalanlar

$$ \lambda_1 x_1 = A x_1$$

Bu da özdeğer/vektör formülüdür. Artık $u(t)$'nin son formülünü
yazabiliriz. 

$$ u(t) =
c_1 \cdot 1 \cdot 
\left[\begin{array}{c}
2 \\ 1
\end{array}\right]
+
c_2 e^{-3t} \cdot 
\left[\begin{array}{r}
1 \\ -1
\end{array}\right]
$$

$t=0$ anında

$$ u(t) =
c_1 
\left[\begin{array}{c}
2 \\ 1
\end{array}\right]
+
c_2 
\left[\begin{array}{r}
1 \\ -1
\end{array}\right] = 
\left[\begin{array}{c}
1 \\ 0
\end{array}\right] 
$$

İki tane formül, iki tane bilinmeyen var. Sonuç

$$ c_1 = \frac{1}{3},\ c_2 = \frac{1}{3}$$

yani

$$ u(t) =
\frac{1}{3} \left[\begin{array}{c} 2 \\ 1 \end{array}\right]
+
\frac{1}{3}  e^{-3t}  \left[\begin{array}{r} 1 \\ -1 \end{array}\right] 
$$

Denklem kararlı konuma (steady-state) gelince, yani $t \rightarrow \infty$ için,
üstel bölüm yokolacaktır, ve baştaki terim kalacaktır. 

$$ u(\infty) =
\frac{1}{3}
\left[\begin{array}{c}
2 \\ 1
\end{array}\right]
$$

Fakat üstteki durum, yani sabit bir istikrarlı konuma yaklaşmak her zaman
mümkün olmayabilir. Bazen sıfıra, bazen de sonsuzluğa da yaklaşabiliriz. 

1) Stabilite 

Ne zaman $u(t) \rightarrow 0$? Özdeğerler negatif ise, çünkü o zaman eksi
üstel değer olarak küçültücü etki yapacaklar. Eğer $\lambda$ kompleks bir
sayı olsaydı, mesela 

$$ e^{(-3 + 6i)} $$

Bu sayı ne kadar büyüktür? Mutlak değeri (absolute value) nedir? 

$$ | e^{(-3 + 6i)t}| = e^{-3t} $$

çünkü 

$$ |e^{6it}| = 1 $$

Niye 1? Çünkü kesin değer işareti içindeki $e^{6it} = \cos(6t)+i\sin(6t)$, ve
sağdaki $a+ib$ formudur, hatırlarsak kompleks eksenlerde $a$ ve $b$ üçgenin
iki kenarıdır, hipotenüs ise, üstte kesin değer olarak betimlenen şeydir,
uzunluğu $a^2 + b^2$, yani $\cos^2(6t) + \sin^2(6t) = 1$. Aslında $\cos$ ve
$\sin$ içine ne gelse sonuç değişmezdi. 

Yani kompleks kısım konumuz için önemli değil, çünkü nasıl olsa 1 olacak,
esas çözümü patlatabilecek (sonsuza götürecek), ya da küçültecek olan şey
reel kısım. Altta Re ile reel bölüm demek istiyoruz.

2) İstikrarlı konum: $\lambda_1 = 0$ ve öteki $Re \ \lambda < 0$. 

3) Patlama: herhangi bir $Re \ \lambda > 0$ ise. 

$\square$

Şu formülün matris formu nedir?

$$ u(0) =
c_1 
\left[\begin{array}{c}
2 \\ 1
\end{array}\right]
+
c_2 
\left[\begin{array}{c}
1 \\ -1
\end{array}\right] 
=
\left[\begin{array}{c}
1 \\ 0
\end{array}\right] 
$$

Şöyle

$$ 
\left[\begin{array}{cc}
2 & 1 \\ 1 & -1
\end{array}\right]
\left[\begin{array}{c}
c_1 \\ c_2
\end{array}\right] 
=
\left[\begin{array}{c}
1 \\ 0
\end{array}\right] 
$$

Soldaki matris, özvektör matrisi $S$. O zaman $Sc = u_0$ 

$\square$

Probleme bakmanın değişik şekillerinden biri, onu "bağlantısız
(decoupled)'' hale getirmek. Başlangıç formülüne dönersek

$$ \frac{du}{dt} = Au $$

Bu $A$ bağlantılı (coupled) bir halde. $u = Sv$ kullanırsak

$$ S\frac{dv}{dt} = ASv$$

$S$ özvektör matrisi. 

$$ \frac{dv}{dt} = S^{-1}ASv = \Lambda v$$

Bu geçişte iyi bilinen eşitlik $AS=S\Lambda$'nin $S^{-1}AS = \Lambda$
halini kullandık. Bu eşitliğin detayları için [1]'e bakabiliriz.

Böylece denklemler arasındaki bağlantıdan kurtulmuş olduk. Tüm sistem şu
hale geldi:

$$ \frac{dv_1}{dt} = \lambda_1 v_1$$

$$ \frac{dv_2}{dt} = \lambda_2 v_2$$

$$ .. $$

Bu sistemi çözmek daha kolay, her biri için ayrı ayrı çözümü yazalım,
mesela $v_1$

$$ v_1(t) = e^{\lambda_1 t} v_1(0) $$

Tamamı için

$$ v(t) = e^{\Lambda t} v(0) $$

Eğer $u=Sv$ ise o zaman $v=S^{-1}u$, ve $v(0)=S^{-1}u(0)$

$$ S^{-1}u(t) = e^{\Lambda t} S^{-1}u $$

$$ u(t) = Se^{\Lambda t}S^{-1} u(0) $$

Aradığımız formül üstteki şu bölüm

$$ e^{At} = Ae^{\Lambda t} S^{-1} $$

Ama bir matrisin üstel değer haline gelmesi ne demektir? Problemimizde elde
ettiğimiz çözüm bu öncelikle, yani $du/dt = Au$'nun çözümü $e^{At}$. Fakat,
yine soralım, bu demek? 

Güç serilerini (power series) hatırlayalım. $e^{x}$ için güç serisi nedir? 

$$ e^x = 1 + x + \frac{1}{2}x^2 + \frac{1}{6}x^3 + ... $$

1 yerine $I$ (birim matris), ve $x$ yerine $At$ alırsak

$$ e^{At} = I + At + \frac{(At)^2}{2} + \frac{(At)^3}{6} + ... +
 \frac{(At)^n}{n!} + ..
$$

Bu "güzel'' bir Taylor serisi. Aslında matematikte iki tane çok güzel,
temiz Taylor serisi vardır, bir tanesi

$$ e^x = \sum_0^{\infty} \frac{x^n}{n!} $$

Öteki de Geometrik seri

$$ \frac{1}{1-x} = \sum_0^{\infty} x^n$$

Bu da en güzel güç serisidir. Onu da matris formunda kullanabilirdik aslında

$$ (I-At)^{-1} = I + At + (At)^2 + (At)^3 + ... $$

Bu formül, bu arada, eğer $t$ küçük bir değerse bir matrisin tersini
hesaplamanın iyi yaklaşıksal yöntemlerinden biri olabilir. Çünkü üstü
alınan küçük $t$ değerleri daha da küçülüyor demektir; üstteki terimlerin
çoğunu bir noktadan kesip atarız (yaklaşıksallık bu demek), ve sadece $I =
At + (At)^2$ hesabı yaparak matris tersini yaklaşıksal olarak
hesaplayabiliriz.

Zihin egzersizine devam edelim: $e^{At}$ açılımı $(I-At)^{-1}$ açılımından
hangisi daha iyi? İkincisi daha temiz, ama birincisi bir değere yakınsıyor
(converges), çünkü gitgide büyüyen $n$ değerleriyle bölüm yapıyorum. Demek
ki $t$ ne kadar büyürse büyüsün, toplam bir sonlu (finite) sayıya doğru
gidiyor. Fakat ikinci açılım öyle değil. Eğer $A$'nin 1'den büyük özdeğeri
var ise, toplam patlar ($At$ tüm özdeğerleri 1'den küçükse durum değişir
tabii, yani $|\lambda(At)| < 1$ ise).

Neyse, konumuza dönelim. $e^{At}$'nin $Se^{\Lambda t}S^{-1}$ ile bağlantılı
olduğunu nasıl görebilirim? $e^{At}$'yi anlamak için $S$ ve $\Lambda$'yi
anlamaya çalışmak lazım, $S$ zaten özvektör matrisi, $\Lambda$ ise köşegen
(diagonal) bir matris, tüm değerleri çaprazında, bunlar nispeten temiz
formlar, onları anlarsak işimiz kolaylaşacak.

Peki geçişi nasıl yapalım? $e^{At}$ açılımı olan güç serisini kullanarak bu
seriden $S$ ve $\Lambda$ çıkmasını sağlayabilir miyim acaba? Şunu zaten
biliyoruz: $A = S \Lambda S^{-1}$. Peki $A^2$ nedir?

$$ A^2 = (S \Lambda S^{-1})(S \Lambda S^{-1}) $$

$$ = S \Lambda S^{-1}S \Lambda S^{-1} $$

ortadaki $S$ ve $S^{-1}$ birbirini iptal eder. 

$$ = S \Lambda^2 S^{-1} $$

O zaman 

$$ e^{At} = I + S \Lambda S^{-1} t + \frac{S \Lambda^2 S^{-1}}{2}t^2 + ...$$

Dışarı tüm $S$'leri çıkartmak istiyorum. O zaman $I$'yi şu şekilde yazarsam
daha iyi olacak (ki içinden $S$'leri çekip çıkartabilelim), $I = SS^{-1}$

$$ = SS^{-1} + S \Lambda S^{-1} t + \frac{S \Lambda^2 S^{-1}}{2}t^2 + ...$$

ve

$$ = S (I + \Lambda t + \frac{\Lambda^2}{2}t^2 + ...) S^{-1}$$

Ortada kalanlar $e^{\Lambda t}$ değil mi? O zaman

$$ = S e^{\Lambda t}S^{-1} $$

Böylece geçişi yapmış olduk. Soru: bu formül hep işler mi? Hayır. Ne zaman
işler? Eğer $A$ köşegenleştirilebilen bir matris ise işler, yani içinden
$\Lambda$ matrisi çekip çıkartılabilecek matrisler için. Onu yapmanın ön
şartı ise $A$'nin tersine çevirelebilir olması, yani tüm özvektörlerinin
bağımsız olmasıdır.

Peki $e^{\Lambda t}$ nedir? Yani köşegen bir matris üstel değer olarak
karşımıza çıkınca sonuç ne olur? $\Lambda$ nedir?

$$ \Lambda =
\left[\begin{array}{ccc}
\lambda_1 && \\
&&.. \\
&& \lambda_n
\end{array}\right]
$$

Bunu $e$ üzeri olarak hesaplayınca ne elde ederiz? Şunu elde ederiz:

$$ e^{\Lambda t} =
\left[\begin{array}{ccc}
e^{\lambda_1t} && \\
&&.. \\
&& e^{\lambda_n t}
\end{array}\right]
$$

Not: Üstteki açılım sezgisel olarak tahmin edilebilecek bir şey olsa da,
üstel olarak bir matris olunca, beklenen her işlem yapılamayabiliyor. Mesela 
eğer matris

$$ 
\left[\begin{array}{cc}
a & b \\ c & d
\end{array}\right]
$$

olsaydı o zaman her elemanı üstel olarak $e^{a}$, $e^b$ şeklinde kullanmak
ve matris içindeki yerine yazmak ise yaramazdı. Üstel matris dünyasının
kendine has bazı kuralları var.

Örnek

$$ y'' + by' + Ky = 0 $$

2. dereceden bu denklemi 1. dereceden formüllerden oluşan 2x2 bir
"sisteme'' dönüştürebiliriz. Ekstra bir denklem ortaya çıkaracağız, eğer
$y$ yerine bir vektör formundaki $u$'yu şu şekilde kullanırsak

$$ u = 
\left[\begin{array}{c}
y' \\ y
\end{array}\right]
$$

$u$'nun türevi şöyle olur

$$ 
u' = 
\left[\begin{array}{c}
y'' \\ y'
\end{array}\right]
$$

$u'$ formuna göre 2. derece denklemi şu şekilde temsil edebiliriz

$$ 
\left[\begin{array}{cc}
-b & -K \\
1 & 0
\end{array}\right]
\left[\begin{array}{c}
y' \\ y
\end{array}\right]
$$

Genel olarak, mesela 5. dereceden bir denklemi alıp 5x5 boyutlarında 1. 
derece denklem sistemine geçmek te mümkündür. Bu geçiş alttaki matrisin 
üst satırına katsayı değerleri atayacak, ve onun altından başlayarak 1 
değerleri dolduracaktır. 

$$ 
\left[\begin{array}{rrrrr}
- & - & - & - & - \\
1 &&&& \\
& 1 &&& \\
&& 1 && \\
&&& 1 & 
\end{array}\right]
$$

Alternatif Anlatım 1

$y(0)$ başlangıç şartına uygun $y' = Ay$ diferansiyel denklemini çözmek için
$A$'nin özvektör/değerleri kullanılabilir. Çözümler $y = e^{\lambda t} x$ ile
elde edilir ki $\lambda$ ve $x$, $A$'nin bir özdeğer/vektörüdür [2, sf. 349].

Burada temel gözlem bağımsız özvektörlerin bir baz oluşturabildiği ve bu
sebeple başlangıç değeri $y(0)$'i bu baz üzerinden temsil edebileceğimiz,

$$
y(0) = c_1 x_1 + ... + c_n x_n
\qquad (1)
$$

ki $c_1,..,c_n$ bilmediğimiz katsayılar.

$y = e^{\lambda t} x$ bir çözümdür, kontrol edelim,

$$
\frac{\mathrm{d} y}{\mathrm{d} t} = A y
$$

icine $y = e^{\lambda t} x$ sokarsak,

$$
\frac{\mathrm{d} }{\mathrm{d} t} ( e^{\lambda t} x) =
\lambda e^{\lambda t} x =
A (e^{\lambda  t} x)
$$

$e^{\lambda  t}$  iptal olunca,

$$
Ax = \lambda x 
$$

ki üstteki özvektör/değer denklemi. Şimdi çözümü

$$
y(t) = c_1 e^{\lambda_1 t} x_1 + ... + e^{\lambda_n t} x_n
$$

olarak gösteririz, dikkat edersek $t=0$ anında üstteki denklem (1) ile aynı.
Özvektör ve değerleri bulduktan sonra bu başlangıç şartını kullanarak
$c_1,..,c_n$ değerlerini bulmak mümkün olacak.

Ondan önce niye çözümlerin lineer kombinasyonlarının da bir çözüm olduğunu
görelim. Sistemi $x' = Ax$ olarak tanımlayalım, ki $x = [x_1, x_2]$.  İspat
matris çarpımın, ve türevin lineer operatör olmalarıyla alakalı, eğer

$$
y(t) = c_1 x_1(t) + c_2 x_2(t)
$$

ise,

$$
y'(t) = (c_1 x_1(t) + c_2 x_2(t))' = c_1 x_1'(t) + c_2 x_2'(t)
$$

olduğunu biliyoruz. Matris çarpımı da aynı şekilde lineer,

$$
A y(t) = A(c_1 x_1(t) + c_2 x_2(t)) = c_1 A x_1(t) + c_2 A x_2(t)
$$

Daha önceden biliyoruz ki $x_1,x_2$'in çözüm olması $x_1'=Ax_1$ ve
$x_2' = A x_2$ anlamina geliyor. Bu formullerin birincisini $c_1$,
ikincisini $c_2$ ile çarpıp toplarsak,

$$
c_1 x_1'(t) + c_2 x_2'(t) = c_1 A x_1(t) + c_2 A x_2(t)
$$

Yani $y'(t) = A y(t)$ elde etmiş olduk, yani yine $A$ bazlı bir diferansiyel
denklem, o zaman $x_1,x_2$'nin tüm lineer kombinasyonları $x' = Ax$ için de bir
çözümdür [3, sf. 196].

Şimdi bir örnek problemde görelim.

Örnek

$$
y' = \left[\begin{array}{rrr}
-2 & 1 \\ 1 & -2
\end{array}\right]
y, \qquad
y(0) = \left[\begin{array}{c} 6 \\ 2 \end{array}\right]
$$

denkleminin çözümlerini bul, hangi çözüm $y(0)$'a karşılık geliyor?

Çözüm

$A$'nin özdeğerlerini bulalım,

```python
import numpy.linalg as lin
A = np.array([[-2,1],[1,-2]])
evals,evec = lin.eig(A)
print (evals[0], evec[:,0])
print (evals[1], evec[:,1])
```

```
-1.0 [0.70710678 0.70710678]
-3.0 [-0.70710678  0.70710678]
```

Bunlar normalize edilmiş özvektörler bu arada, aslında

```python
print (evec / 0.70710678)
```

```
[[ 1. -1.]
 [ 1.  1.]]
```

ile kitap sonucunu elde edebiliriz. 

$c_1,c_2$ değerlerini bulmak için $c$ vektörü yaratalım, özvektörler $x_1,x_2$'i
bir $V$ matrisinin kolonlarına koyalım, böylece $y(0) = c_1 x_1 + ... + c_n x_n$
yerine $y(0) = Vc$ demek mümkün olacak, ve bilinmeyen $c$ için çözebiliriz,

```python
V = evec / 0.70710678 
print (V.T) # kolonlarda ozvektorler icin devrik aldik
soln = lin.solve(V.T, np.array([[6,2]]).T)
print (soln)
```

```
[[ 1.  1.]
 [-1.  1.]]
[[2.        ]
 [3.99999999]]
```

$y(0)$ icin kontrol edersek,

$$
\left[\begin{array}{r}
6 \\ 2
\end{array}\right] =
2 
\left[\begin{array}{r}
1 \\ -1
\end{array}\right] +
4 
\left[\begin{array}{r}
1 \\ 1
\end{array}\right] 
$$

eşitliği doğru. 

O zaman nihai sonuc

$$
y(t) =
4 e^{-t}
\left[\begin{array}{r}
1 \\ 1
\end{array}\right]  +
2 e^{-3t}
\left[\begin{array}{r}
1 \\ 1
\end{array}\right] =
\left[\begin{array}{r}
4 e^{-t} + 2 e^{-3t} \\
4 e^{-t} + 2 e^{-3t}
\end{array}\right]
$$

Alternatif Anlatım 2

Eğer elimizde bir sabit katsayılı lineer bayağı / normal (ordinary) diferansiyel
denklem (ODE) sistemi var ise, ama bu sistem içinde denklemler birbiri ile bağlı
(coupled) ise, sistemi ufak bir lineer cebir numarası ile bağımsız hale
getirebiliriz, böylece her denklem üzerinde basit $e^x$ bazlı üstel çözümleri
kullanabiliriz [4, sf. 421].

Eğer elimizde sabit katsayılı $n$ tane ODE var ise bu sistemi bir vektör
olarak yazabiliriz,

$$
y' = A y, \quad y(t_0) = y_0
$$

ki $y$ içinde bilinmeyenleri içeren bir vektördür, $A$ ise sabit sayılar
içeren $n \times n$ matrisidir.

Eğer $A$ matrisini köşegenleştirici matris bir $S$ elde edebilseydik, bir
değişken değişimi üzerinden, $y = Su$ deyip sonra $(Su)' = A(Su)$ elde
ederdik ardından her iki tarafı $S^{-1}$ ile çarpıp

$$
u' = S^{-1} A S u
$$

ya erişirdik. Tabii ki $\Lambda = S^{-1} A S$ formülü biliniyor ve $\Lambda$
köşegenleşmiş bir matris. Nihai denklem $u' = \Lambda u$ denklemi ve
$\Lambda$'nin köşegeni hariç tüm diğer hücreleri sıfır, bu bize her $u_1,u_2,..$
için basit, diğer değişkenlerden bağımsız denklemler veriyor, ve onların
çözümünü diferansiyel denklem cebirinden biliyoruz. Bu şekilde $u$ çözümünü
elde ettikten sonra $y = S u$ ile başlangıç problemine geçiş yapabiliyoruz. 

Örnek

Elimizde alttaki gibi bir sistem olsun,

$$
\frac{\mathrm{d} y_1}{\mathrm{d} t} = 9 y_1 + 2 y_2
$$

$$
\frac{\mathrm{d} y_2}{\mathrm{d} t} = y_1 + 8 y_2
$$

Bu sistemde her iki denklem birbiri ile bağlantılı, $\dot{y}_1$ içinde 
$y_2$ ifadesi $\dot{y}_2$ içinde  $y_1$ ifadesi var. Matris kullanarak
yazarsak,

$$
\frac{\mathrm{d}}{\mathrm{d} t} \left[\begin{array}{c}
y_1 \\ y_2 
\end{array}\right] =
\left[\begin{array}{cc}
9 & 2 \\ 1 & 8
\end{array}\right]
\left[\begin{array}{c}
y_1 \\ y_2
\end{array}\right]
$$

Ya da $y' = Ay$ diyebiliriz ki $y = [\begin{array}{cc} y_1 & y_2 \end{array}]^T$
ve $A = \left[\begin{array}{cc} 9 & 2 \\ 1 & 8 \end{array}\right]$.

$A$'nin özdeğerleri $\lambda = 7$ ve $\lambda = 10$ ona tekabül eden
özvektörler $[\begin{array}{cc}1 & -1\end{array}]^T$, $[\begin{array}{cc}2&1\end{array}]^T$.
Köşegenleştirici matrisi artık kurabiliriz, bulduğumuz özvektörleri $S$
kolonlarına koyuyoruz, ve $S^{-1}$'e de ihtiyacımız var, hepsi bir arada,

$$
S = \left[\begin{array}{rr}
1 & 2 \\ -1 & 1
\end{array}\right], \quad
S^{-1} = \left[\begin{array}{rr}
1/3 & -2/3 \\ 1/3 & 1/3
\end{array}\right]
$$

Şimdi $S^{-1} A S$ üzerinden köşegenleştirme yapılabilir, hepsini yeni formüle
koyarsak,

$$
\frac{\mathrm{d}}{\mathrm{d} t} \left[\begin{array}{c}
u_1 \\ u_2 
\end{array}\right] =
\left[\begin{array}{rr}
1/3 & -2/3 \\ 1/3 & 1/3
\end{array}\right]
\left[\begin{array}{cc}
9 & 2 \\ 1 & 8
\end{array}\right]
\left[\begin{array}{rr}
1 & 2 \\ -1 & 1
\end{array}\right]
\left[\begin{array}{c}
u_1 \\ u_2 
\end{array}\right]
$$

$$
= \left[\begin{array}{rr}
7 & 0 \\ 0 & 10
\end{array}\right]
\left[\begin{array}{c}
u_1 \\ u_2 
\end{array}\right]
$$

Görüldüğü gibi çarpım sonucu elde edilen matriste köşegen harici her yer
sıfır. Böylece nihai, transform edilmiş sistemde değişkenleri bağımsız
hale getirmiş olduk, çünkü üstteki sistem şudur aslında,

$$
\frac{\mathrm{d} u_1}{\mathrm{d} t} = 7 u_1, \quad
\frac{\mathrm{d} u_2}{\mathrm{d} t} = 10 u_2, 
$$

Şimdi $S$'i kullanarak ana probleme dönüş te yapabiliriz, 

$$
\left[\begin{array}{c} y_1 \\ y_2  \end{array}\right] =
\left[\begin{array}{rr}
1 & 2 \\ -1 & 1
\end{array}\right]
\left[\begin{array}{c} u_1 \\ u_2  \end{array}\right] =
\left[\begin{array}{rr}
1 & 2 \\ -1 & 1
\end{array}\right]
\left[\begin{array}{c} B e^{7t} \\ C e^{10t}  \end{array}\right] 
$$

O zaman

$$
y_1 = B e^{7t} + 2C e^{10t}
$$

$$
y_2 = -B e^{7t} + C e^{10t} 
$$

Bilinmeyen sabitler $B,C$ problem tanımındaki başlangıç şartı kullanılarak
bulunabilir. Bu şart $t = t_0$'da tanımlanmıştı, o zaman

$$
y_0 =
B \left[\begin{array}{c} 1 \\ -1  \end{array}\right] e^{7 t_0} +
C \left[\begin{array}{c} 2 \\ 1 \end{array}\right] e^{10 t_0}
$$

$$
= \left[\begin{array}{cc}
e^{7 t_0} & 2 e^{10 t_0} \\
-e^{7 t_0} & e^{10 t_0} 
\end{array}\right]
\left[\begin{array}{c}
B \\ C
\end{array}\right]
$$

Mesela $t_0 = 1$ olsa $y_0$ verili bir reel vektör olsa, tek bilinmeyen $B,C$
kalıyor, üstteki matrisin tersi ile basit bir şekilde $B,C$ bulunabilir.

Kaynaklar

[1] Bayramlı, Hesapsal Bilim, *Ders 6*

[2] Strang, *Differential Equations and Linear Algebra*

[3] Boelkins, *Differential Equations with Linear Algebra*

[4] Zwillinger, *Handbook of Differential Equations, 3rd Edition*









