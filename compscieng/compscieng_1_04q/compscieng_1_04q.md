# Ders 1.4, Soru, Cevap


Sorular

Soru 1.2.2

$u''(x) = \delta(x)$, $u(-2) = 0$ ve $u(3) = 0$ problemini çöz. Parçalar
$u = A(x+2)$ ve $u=B(x-3)$ $x=0$ noktasında birleşiyor.
$U = (u(-1), u(0), u(1), u(2))$ vektörünün $KU=F=(0,1,0,0)$ problemini
çözdüğünü göster.

Çözüm

Yukarıda çözümün hangi formda olacağı $A$ ve $B$ üzerinden verilmiş, burada
güzel bir numara var (alternatif çözümde bunu anlattık), fakat biz önce
derste daha gösterilen yöntem üzerinden çözümü kendimiz bulalım.

Özel (particular) çözüm nedir? 

$$ u(x) = -R(x) + C + Dx$$

Bildiğimiz gibi $R(x)$ rampa fonksiyonu şöyle:

$$ 
R(x) = \left\{ \begin{array}{ll}
0 & x \le 0 \\
x & x \ge 0 
\end{array} \right.
$$

Şimdi sınır şartlarını kullanarak $u(x)$ içinde yerine koyalım:

$$ u(-2) = -R(x) + C - 2D = 0 $$

$$ u(-2) = C - 2D = 0 $$

$x=-2$ yani sıfırdan küçük olduğu için $-R(x)=0$ oldu ve onu formülden attık.

$$ u(3) = -3 + 3D + C = 0 $$

Burada $x=3$, o yüzden $-R(3) = -3$ kullanıldı. Sonuç

$$ C = 2D $$

$$ 3 + 3D + 2D = 0 $$

$$ 5D - 3 = 0 $$

$$ D = \frac{3}{5} $$

O zaman

$$ C - 2(\frac{3}{5}) = 0 $$

$$ C = \frac{6}{5} $$

Sıfırdan öncesi ve sonrası için (değişik $R(x)$ durumlarına göre)
fonksiyonu parçalı bir şekilde yazarsak

$$ u(x) =
\left\{ \begin{array}{ll}
& \\
\frac{6}{5} + \frac{3}{5}x & x \le 0 \\
& \\
-x + \frac{6}{5} + \frac{3}{5}x = \frac{6}{5} - \frac{2}{5}x & x \ge 0 \\
& 
\end{array} \right.
$$

Birinci kısmı sadeleştirirsek

$$ \frac{6}{5} + \frac{3}{5}x  = \frac{3}{5}(x + 2)  $$

İkinci kısmı sadeleştirirsek

$$ \frac{6}{5} - \frac{2}{5}x =  -\frac{2}{5}(x - 3)$$

Problemin hazır verdiği forma, ve sonuca eriştik.

```python
import scipy.linalg as lin

def ktbc(n):
    vec = np.zeros((1,n))
    vec[0,0] = 2
    vec[0,1] = -1
    K = lin.toeplitz(vec)
    T = np.copy(K)
    T[0,0] = 1
    B = np.copy(K)
    B[0,0] = 1
    B[n-1,n-1] = 1
    C = np.copy(K)
    C[n-1,n-1] = 1
    
    return K, T, B, C

K,T,B,C = ktbc(4)

print lin.inv(K) 
```

```
[[ 0.8  0.6  0.4  0.2]
 [ 0.6  1.2  0.8  0.4]
 [ 0.4  0.8  1.2  0.6]
 [ 0.2  0.4  0.6  0.8]]
```

Bir sonraki derste göreceğimiz gibi üstteki sonucun 2. kolonu aradığımız
sonuç (çünkü delta ağırlığı 2. hücre üzerinde). Bu kolondaki değerleri
teker teker $x=-1,0,1,2$ değerlerini $u(x)$'i hesaplayarak kontrol edelim.

$$ 6/5 + 3/5(-1) = 3/5 = 0.6 $$

$$ 6/5 + 3/5(0) = 6/5 = 1.2 $$

$$ 6/5 -2/5(1) = 4/5 = 0.8 $$

$$ 6/5 -2/5(2) = 2/5 = 0.4 $$

Sonuçlar birebir uyuyor. 

Alternatif Çözüm

Problemin cebirsel çözümü için bir yöntem daha var, hatta ders notlarındaki
1.2.2 çözümü bu yöntemi kullanıyor. 

$u(x)$'in formunun lineer olacağını bildiğimizden, ve bu formül içinde bir
rampa fonksiyonu olmasından hareketle, çözümün iki lineer parça içerdiğini
ve bu parçaların 0 noktasında birleştiğini farzedebiliriz. Şöyle iki
fonksiyon buluruz: $A(x+2)$ ve $B(x-3)$. Bu her iki fonksiyonun -2 ve +3
noktalarında sıfır olduğuna dikkat, ki bu diferansiyel denklemin sınır
şartları ile uyumlu.

Şimdi alttaki numaralara bakalım, tek bir integral, ve tek bir türev alarak
çok daha basit cebirsel ifadelerle çalışma imkanı var. İki tarafın
entegrali:

$$ -\int u''(x) = \int \delta(x) $$

$$ -[u'(x)]_L^R = 1 $$

R ve L sağ (right) ve sol (left) ibareleri, delta fonksiyonunun yoğunluk
yarattığı noktanın sağındaki ve solundaki herhangi birer nokta için
kullanılıyor, delta fonksiyonunun entegralini alırken bu noktanın
"üzerinden geçersek'' sonuç her zaman 1 verecektir. O noktaların tam
olarak ne olduğu önemli değil, çünkü $x=0$ solunda ve solunda eğim her
noktada aynı.

$$ u_R'(x) - u_L'(x) = -1 $$

Üstteki türevleri formlara uygularız

$$ B - A = -1 $$

İki parça $x=0$ noktasında birleşiyor, o zaman

$$ A(0+2) = B(0-3) $$

$$ A = -\frac{3}{2}B $$

Birleştirince

$$ B - (-3/2 B ) = -1 $$

$$ B = -0.4 $$

$$ A = 0.6 $$

Soru 1.2.4

$$
T_n = (geri)(-ileri) =
\left[\begin{array}{rrrr}
1  &   0        &    &   \\
-1 &   1        & 0  &   \\
   & \ddots & \ddots & 0 \\
   &        &    -1  & 1 
\end{array}\right]
\left[\begin{array}{rrrr}
1  &  -1        &    &   \\
0  &   1        & -1 &   \\
   & \ddots & \ddots & -1 \\
   &        &    0  & 1 
\end{array}\right]
\qquad (1)
$$

$$
T_n^{-1} =
\left[\begin{array}{rrrr}
1  &  1     & \ddots   & 1       \\
   &  1     &  1       & \ddots  \\
   &        & 1        & 1       \\
   &        &          & 1 
\end{array}\right]
\left[\begin{array}{rrrr}
1        &        &         &   \\
1        &  1     &         &   \\
\ddots   &  1     & 1       &   \\
1        &  \ddots & 1       & 1 
\end{array}\right]
\left[\begin{array}{r}
1 \\ 1 \\ \vdots \\ 1
\end{array}\right]
\qquad (2)
$$

(1)'deki geriye doğru farklar matrisi $\Delta_{-}$ tersinin (2)'deki
toplamlar matrisi olduğunu kontrol edin. Dikkat: $\Delta_0 = (\Delta_{+} +
\Delta_{-}) / 2$ ifadesinin tersi olmayabilir! $\Delta_0 u = 0$ denklemini
$n=3$ ve $n=5$ için çözün.

```python
import scipy.linalg as lin

DB = lin.toeplitz([1, -1, 0], [1, 0, 0])
print DB; print lin.inv(DB)

DF = lin.toeplitz([-1, 0, 0], [-1, 1, 0])

D_0 = (DF + DB) / 2
print D_0
```

```
[[ 1  0  0]
 [-1  1  0]
 [ 0 -1  1]]
[[ 1.  0.  0.]
 [ 1.  1.  0.]
 [ 1.  1.  1.]]
[[ 0  0  0]
 [-1  0  0]
 [ 0 -1  0]]
```

`D_0` matrisini soruda istendiği şekilde yarattık. Bu matrisin sıfır
uzayı, yani `D_0 u = 0` denklemindeki $u$ sıfır olmadığı için, bu
matris tersine çevirilemez demektir, yani matris eşsiz (singular)
demektir. 

Soru 1.2.10

27. denklemden bahsediliyor, bu yanlış. Sorunun istediğini kodlamak daha
iyi: $\Delta_+$ için `DF` ve $\Delta_-$ yerine `DB` kullanıp,
çarpımını alırsak,

```python
import scipy.linalg as lin

DB = lin.toeplitz([1, -1, 0], [1, 0, 0])
print "backward"
print DB

DF = lin.toeplitz([-1, 0, 0], [-1, 1, 0])
print "forward"
print DF
print 'product'
print np.dot(DF, DB)
```

```
backward
[[ 1  0  0]
 [-1  1  0]
 [ 0 -1  1]]
forward
[[-1  1  0]
 [ 0 -1  1]
 [ 0  0 -1]]
product
[[-2  1  0]
 [ 1 -2  1]
 [ 0  1 -1]]
```

Bu matriste $u = 0$ sınır şartının hangi satır ile temsil edildiği
soruluyor, yani $u(..) = 0$ şartında '..' neresi? Bu şart için sol taraftaki
kolonun atıldığını hayal edelim, geriye kalanlar üst 1. satırı [-2 1]
üzerinden $u(0)=0$ şartını zorlar. Doğru cevap 1. satır.

Peki $u'(..) = 0$ şartı hangi satırla, yani hangi '..' değeriyle zorlanır?
En alt satır gibi duruyor, kontrol edelim, 

$$ \frac{u_4-u_3}{h} = 0$$

o zaman

$$ u_4 = u_3 $$

Matrisin en son satırını cebirsel şekilde yazalım

$$ \frac{u_4 - 2u_3 + u_2}{h} $$

$u_4 = u_3$ olduğu için

$$ = \frac{u_3 - 2u_3 + u_2}{h} $$

$$ = \frac{-u_3 + u_2}{h} $$

Son ifade matrisin sonuncu satırını aynen tarif ediyor.

Soru 1.2.19

Bir merkezi farksal yaklaşıksallama (difference approximation) kur, bunu
yaparken $K/h^2$ ve $\Delta_0 / 2h$ kullan, tüm bunları 

$$ -u'' + u' = 0. $$

çözmek için kullan, ki $u(0) = 0$ ve $u(1) = 0$. Ayrı olarak $du/dx$ için
öne doğru farksal (forward difference) $\Delta_{+}U/h$ kullan. Dikkat
edelim $\Delta_0 = (\Delta_{+} + \Delta_{-}) / 2$. Ortalanmış $u$ ve
ortalanmamış $U$'yu çözelim, ki $h = 1/5$ olsun. Gerçek $u(x)$ $u=x$ özel
çözümüdür ve bu çözüme $A+Be^x$ eklenir. Hangi $A$ ve $B$ sınır şartlarını
tatmin eder? $u$ ve $U$ ne kadar $u(x)$'e yakındır? 

Cebirsel olarak bu denklemi çözmek için onun sabit katsayılı, 2. seviye
(homojen olmayan -sıfıra eşit değil-) denklem olduğunu görmek yeterli. Önce
ana denklemle bağlantılı homojen denklemi (sıfıra eşitlenmiş halini yani)
çözeriz.

$$ -u'' + u' = 0. $$

Bu denklemi çözmek için karakteristik denklemini buluruz, bkz [2]. Bu
denklem $-r^2 + r = 0$ olacaktır, kökleri $0$ ve $1$, o zaman homojen
denklemin çözüm yelpazesini $e^{0x}=1$ ve $e^{x}$ tanımlar. Genel çözüm
demek ki

$$ \mathbf{s} + A + Be^x $$

olur, ki $A$ ve $B$ rasgele sabitlerdir, ve $\mathbf{s}$, $-u''+u'=1$ denkleminin
özel (particular) bir çözümüdür. $u(x)=x$'in bu özel çözüm olduğunu bulmak
zor değildir, o zaman çözümün tamamı

$$ u(x) = x + A + Be^x $$

olacaktır. 

$$ u(0) = A + B = 0 $$

$$ A = -B $$

$$ u(1) = 1 + -B + Be^1 = 0$$

$$ B = \frac{1}{1-e} $$

$$ A = \frac{-1}{1-e} $$

Denklemin tam çözümü

$$ u(x) = x - \frac{1}{1-e} + \frac{1}{1-e}e^x $$

```python
import scipy.linalg as lin

K,T,B,C = ktbc(4); print K

C = lin.toeplitz([0, -1, 0, 0], [0, 1, 0, 0]); print C

print "ortalanmis",  lin.solve((25*K + 2.5*C), [1.,1.,1.,1.])

F = lin.toeplitz([-1, 0, 0, 0], [-1, 1, 0, 0]); print F

print "ileri farklilik", lin.solve((25*K + 2.5*F), [1.,1.,1.,1.])

def ux(x): return x - 1/(1-np.e) + np.e**x/(1-np.e)

print ux(0.2), ux(0.4), ux(0.6), ux(0.8)
```

```
[[ 2. -1.  0.  0.]
 [-1.  2. -1.  0.]
 [ 0. -1.  2. -1.]
 [ 0.  0. -1.  2.]]
[[ 0  1  0  0]
 [-1  0  1  0]
 [ 0 -1  0  1]
 [ 0  0 -1  0]]
ortalanmis [ 0.07135546  0.11412325  0.12195055  0.0870728 ]
[[-1  1  0  0]
 [ 0 -1  1  0]
 [ 0  0 -1  1]
 [ 0  0  0 -1]]
ileri farklilik [ 0.07956826  0.123533    0.12793827  0.08838856]
0.0711487519142 0.11376948211 0.121546007893 0.0867637263024
```

Bu çözümlerden ortalanmış olanın daha iyi olduğunu görebiliriz. 

Soru 1.2.21

$u(h) = u(0) + hu'(0) + \frac{1}{2}h^2u''(0)+..$ açılımını ve "sıfır eğim
koşulu'' yani $u'(0) = 0$ olarak belirtilen sınır şartını ve $-u'' = f(x)$
ifadesini kullanarak $u_0 - u_1 = \frac{1}{2}h^2f(0)$ şeklinde üst sınır
şartını türet. $\frac{1}{2}$ faktörü $O(h)$ hatasından kurtulmamıza
yarayacak. 

Öncelikle türetmemiz istenen şeyin 2. Ders Problem 1.2 A'da kullanılan
ifade ile aynı olduğunu görelim. O problemde $u_0 - u_1 =  \frac{1}{2}h^2f(0)$ 
ifadesine farklı bir yönden erişmiştik, orada ortalama farklılık tekniğini 
kullanmıştık. Burada Taylor açılımını kullanıyoruz, ve aynı noktaya geliyoruz! 

$u(0)$ noktasındayız, ve ileri doğru $h$ adımı atıyoruz, bu adımı Taylor
açılımı ile nasıl gösteririz?

$$ u(h) = u(0) + h \cdot u'(0) + \frac{1}{2}h^2u''(0) + ... $$

Değil mi? Şimdi, elimizde diferansiyel denklemin tanımından gelen bazı
tanımları kullanarak üstteki denklemi değiştirelim. $-u''(x) = f(x)$ ise,
$u''(0) = -f(0)$ demektir. Ayrıca $u'(0) = 0$ ise $h \cdot u'(0)$
denklemden atılabilir. Noktadan sonrasını biz atıyoruz, yaklaşıksal olarak
temsil ettiğimiz için, o zaman

$$ u(h) = u(0) - \frac{1}{2}h^2f(0) $$

$$ u_1 - u_0 = -\frac{1}{2}h^2f(0)$$

$$ u_0 - u_1 = \frac{1}{2}h^2f(0) $$

Ve Çözülmüş Problem 1.2 A'daki tanımın aynısına eriştik. 

Problem 1.4.5

$-u''= \delta(x-a)$ denkleminin serbest-serbest şartları, yani $u'(0) = 0$
ve $u'(1) = 0$ üzerinden çözümü *olamayacağını* göster, bu durumda $C$
ve $D$ sabitleri bulunamayacak. 

Çözüm

Tam çözüm neydi? 

$$ u(x) = R(x-a) + Cx + D $$

Eldeki şartlar sadece $u'(x)$ için olduğuna göre üstteki denklemin türevini
alalım, ve 0 ve 1 değerlerini yerine koyarak ele geçen sonuca bakalım. 

$$ u'(0) = 0 + C = 0 $$

Rampa fonksiyonunun türevi basamak fonksiyonu, fakat o noktada daha basamak
başlamamış (yani sıfır seviyesinde). Aslında soruda $a > 0$ bilgisini
verseler iyi olurdu, her neyse, bu sebeple ilk terim 0. $Cx$'den geriye $C$
kalır, $D$ yokolur.

$$ C = 0 $$

Diğer koşulla

$$ u'(1) = -1 + C + 0 = 0 $$

Bu noktada basamak başlamış, çünkü $a$ noktası ilerisindeyiz, 
basamak fonksiyonu 1 değerinde, negatifi alındığı için sonuç -1. Devam
edersek: 

$$ C = 1 $$

Bu bir absürtluk ortaya çıkartı, $C$'nin hem 0 hem 1 olması mümkün
değildir. Demek ki serbest-serbest probleminin çözümü yoktur. 

Teori

$$ \int_{-\infty}^{\infty} \delta(x)g(x) \mathrm{d} x = g(0)$$

İspat

$\delta(x)=0$ $x \ne 0$ için sıfır olduğuna göre, entegrasyon operasyonunun
tek kullanabileceği değer $g(0)$ değeridir (çünkü diğer her yerde iç çarpım
sıfır), ki bu değer de bir sabit olarak addedilebilir ve entegralin dışına
çıkartılır [1, sf. 416]. Yani

$$
\int_{-\infty}^{\infty} \delta(x)g(x) \mathrm{d} x = 
\int_{-\infty}^{\infty} \delta(x)g(a) \mathrm{d} x = 
g(a) \int_{-\infty}^{\infty} \delta(x) \mathrm{d} x = 
g(a) \cdot 1 = g(a)
$$


Alternatif İspat

Parçalı entegral yöntemini uygularsak,

$$ \int u \mathrm{d} v = uv - \int v \mathrm{d} u $$

$$ u = g(x), \quad \mathrm{d} v = \delta(x) \mathrm{d} x $$

$$
\int_{-A}^{A} g(x)\delta(x) \mathrm{d} x = g(x)u(x) \bigg]_{-A}^{A} -
\int_{-A}^{A} u(x) \frac{\mathrm{d} g(x)}{\mathrm{d} x}\mathrm{d} x
$$

$-A$ ve $A$ entegral sınırları sıfırı ortalayacak şekilde seçilmiş iki
değerdir, $A$ herhangi bir sayı olabilir. $u(x)$ $\delta(x)$ fonksiyonunun
entegrali olduğuna göre $x=0$ öncesi sıfır, sonrası 1 olacak. O zaman
birinci kısım

$$ g(x)u(x) \bigg]_{-A}^{A} = g(x)u(x) \bigg]_{0}^{A} = g(A)\cdot 1 = g(A)$$

$x=0$ öncesi önemli değil çünkü orada $u(x) = 0$. 

İkinci kısım

$$ \int_0^A 1\cdot \frac{\mathrm{d} g(x)}{\mathrm{d} x}\mathrm{d} x = g(A) - g(0) $$

Biraraya koyarsak

$$  g(A) - (g(A) - g(0)) = g(A) - g(A) + g(0) = g(0) $$

İspat böylece tamamlanıyor.

Kaynaklar 

[2] Bayramlı, Diferansiyel Denklemler, *Ders 9*





