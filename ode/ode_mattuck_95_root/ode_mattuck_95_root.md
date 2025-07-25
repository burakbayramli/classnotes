# Kök Bulmak, Karesel Formül (Root Finding, Quadratic Formula)

$$ ax^2 + bx + c = 0$$

Yukarıdaki gibi bir formülü çözmek için, lise matematiğinden
hatırlayabileceğimiz aşağıdaki formül kullanılır.

$$ x = \frac{-b \pm \sqrt{b^2-4ac}}{2a} $$

Böylece denklemin kökleri bulunur, karesel durumda $(x-r_1)(x-r_2)=0$,
$r_1,r_2$ iki tane köktür.

Çözümü türetmek için kullanacağımız yöntem, "kareyi tamamlama (completing
the square)" yöntemi. Bu yönteme göre, c değerini denklemin solundan,
sağına atıyoruz, ve öyle bir yeni c değeri buluyoruz ki, karesel denklemin
tek bir kökü oluyor. Tek kökü olan karesel denklemleri biliyoruz, $x^2 +
6x + 9 = 0$ gibi bir denklem, tek kökü olan bir denklemdir.

Gelelim çözüm türetilmesine.. iki üstteki förmül ile başlayalım, iki tarafı
a ile bölelim. Elimize aşağıdaki sonuç gelecek.

$$ x^2 + \frac{b}{a}x + \frac{c}{a} $$

$c/a$ değerini sağ tarafa taşıyalım. 

$$ x^2 + \frac{b}{a}x = -\frac{c}{a} $$

Şimdi, kareyi tamamlama yöntemi ile, iki tarafa da aşağıdaki değeri ekleyelim.

$$ \bigg( \frac{b}{2a} \bigg)^2 $$

Böylece, aşağıdaki işlem serisini başlatmış olacağız.

$$ x^2 + \frac{b}{a}x + \bigg( \frac{b}{2a} \bigg)^2 = 
-\frac{c}{a} + \bigg( \frac{b}{2a} \bigg)^2 $$

$$ \bigg( x + \frac{b}{2a} \bigg)^2 = 
-\frac{c}{a} + \frac{b^2}{4a^2}
$$

$$ \bigg( x+ \frac{b}{2a} \bigg)^2 = \frac{b^2 - 4ac}{4a^2} $$

İki tarafın karekökünü alırsak: 

$$ x + \frac{b}{2a} = \pm \frac{\sqrt{b^2 - 4ac}}{2a}  $$

ya da

$$ x = -\frac{b}{2a} \pm \frac{\sqrt{b^2 - 4ac}}{2a} $$

İşte bu formüle karesel formül denir, ve normalde şöyle yazılır. 

$$ x = \frac{-b \pm \sqrt{b^2-4ac}}{2a} $$

+ ve - işareti, elimizde 'iki' sonuç var demektir, yani elimizdeki
sayılardan biri

$$ x = \frac{-b + \sqrt{b^2-4ac}}{2a} $$

Öteki de 

$$ x = \frac{-b - \sqrt{b^2-4ac}}{2a} $$

olacak.

Örnek

$a=3,b=3,c=5$, 

```python
a=1.0;b=3.0;c=1.0
tmp=np.sqrt((b**2)-(4.0*a*c))
print ((-b-tmp) / (2.0 * a))
print ((-b+tmp) / (2.0 * a))
```

```
-2.61803398875
-0.38196601125
```

Kütüphane çağrısı ile

```python
print (np.roots([a, b, c]))
```

```
[-2.61803399 -0.38196601]
```

Sayısal Yöntemler

Eğer analitik bir şekilde $f(x)=0$'da kök bulmak mümkün değilse, sayısal
yöntemler kullanılabilir. Newton'un yöntemi (Newton's method) bunlardan
biri, bu yöntem $f(x)$ karesel, küpsel, ne kadar çetrefil olursa olsun
kullanılabilir. Diyelim ki $f(x)$'in köklerinden birini $x_0$ olarak
tahmin ediyoruz. Bu tahmin etrafında fonksiyonun Taylor açılımı [1,
sf. 175],

$$ f(x) = f(x_0) + f'(x_0)(x-x_0) + ...$$

Kök aradığımız için $f(x)=0$ yaparız, ve $x$'i bir tarafa alacak şekilde
tekrar düzenleriz (ayrıca noktalı kısmı atarız çünkü belli bir yaklaşık
temsil ile yetinmeye karar verdik), 

$$ 0 = \frac{f(x_0)}{f'(x_0)} + x-x_0$$

$$ x = x_0 - \frac{f(x_0)}{f'(x_0)} $$

Bu denklemi $x_0$'i baz alarak bir sonraki $x$'i hesaplayacak bir formül
gibi görebiliriz, 

$$ x_1 = x_0 - \frac{f(x_0)}{f'(x_0)} $$

Genel olarak

$$ x_{i+1} = x_i - \frac{f(x_i)}{f'(x_i)} $$

Yani bir $x_0$ tahmini ile başlıyoruz, oradan $x_1$ elde ediyoruz, onu geri
verip $x_2$ elde ediyoruz, böyle devam ediyor.

Not: yöntemin işlemesi için $f(x)$'in türevi $f'(x)$ gerekiyor, fakat çoğu
zaman türev elde olmaz, o zaman türevi de yaklaşık olarak hesaplayabiliriz, 

$$ f'(x) \approx \frac{f(x + \mathrm{d} x) - f(x)}{\mathrm{d} x}$$

Türev yaklaşık olarak hesaplandığında Newton'un Yöntemi ismi değiştirilip
Sekant Yöntemi (Secant Method) ismi kullanılıyor.

Örnek

$$ x^2 + 3 x + 1 = 0$$ formülünün köklerini bulalım. 

```python
def newton(f, x, dfdx=None, eps=1e-6):
    if dfdx is None: 
        delta = eps**0.5
    while True:
        fx = f(x)
        # turev bilinmiyorsa onu da sayisal hesapla
        if dfdx is None:
            dx = delta*x
            if abs(dx) < delta: dx = delta
            df = (f(x+dx) - fx)/dx
        else:
            df = dfdx(x)
        dx = -fx/df
        x += dx
        if abs(dx) < eps: return x

a=1.0;b=3.0;c=1.0

def f1(x): return a*(x**2.0) + b*x + c
print (newton(f1,0.0)) # baslangic 0'da
print (newton(f1,-3.0)) # farkli noktadan baslatalim

# bilinen turev 
def df1(x): return 2*a*x + b
print ('\n',newton(f1,0.0,df1))
print (newton(f1,-3.0,df1) )

```

```
-0.381966010827
-2.61803398875

-0.38196601125
-2.61803398875
```

Karekök Hesaplamak

Bir $a$ sayisinin karekokunu bulmak demek soyle ifade edilebilir; oyle bir $x$
bul ki $x^2 = a$ olsun. O zaman bir bakima $f(x) = x^2 - a$ fonksiyonunun kokunu
bulmus oluyoruz. Demek ki karekok problemini kok bulma problemine cevirebiliriz.
$f'(x) = 2x$ olduguna gore, $a = 612$ icin mesela,

```python
number = 612
a = float(number)
iter = 10
for i in range(iter): # iteration number
    number = 0.5 * (number + a / number)
print (number)    
```

```
24.73863375370596
```

```python
print (np.sqrt(612))
```

```
24.73863375370596
```

Tıpatıp aynı sonuca eriştik. Galiba arka planda aynı metot kullanılıyor!

Çok Boyutlu Newton'un Yöntemi

Çok boyutta Taylor açılımı,

$$ 
0 = f(x_i + \Delta x_i) = f(x_i) + J(x_i) \cdot \Delta x_i + ... 
$$

$x_i$ bir $N$ boyutlu vektör, ve $J$ Jacobian matrisi

$$ 
J = \left[\begin{array}{rrr}
\frac{\partial f_1}{\partial x_1}  & \dots & \frac{\partial f_1}{\partial x_n} \\
 \vdots & \ddots & \vdots \\
\frac{\partial f_N}{\partial x_1}  & \dots & \frac{\partial f_n}{\partial x_N} 
\end{array}\right]
$$

$\Delta x_i$ adımlarını hesaplamak için,

$$  -f(x_i) =  J(x_i) \cdot \Delta x_i  $$

$$  -f(x_i)J(x_i)^{-1} = \Delta x_i  $$

Üstte ters alma işlemi yapıldı fakat çoğunlukla Gaussian eliminasyon
kullanılarak bu pahalı ters alma işleminden kaçınılmaya uğraşılır.

O zaman güncellemeyi

$$ x_{i+1} = x_i + \Delta x_i $$

olarak özyineli bir şekilde yapabiliriz.

İkiye Bölme Yöntemi (Bisection Method)

Bu yöntemle kök araması belli $x$ aralıkları içinde yapılır, her aralığın
orta noktasının $f(x)=0$'a ne kadar yaklaştığı kontrol edilir, eğer
yaklaşma yoksa aralık ikiye bölünerek sıfıra daha yaklaştıracak parça
içinde devam edilir. Parçalar bölündüğü için $O(\log)$ hızında sonuca
ulaşmak mümkündür.

```python
def bisect(f, a, b, eps=1e-6):
     if f(a) > 0: # swap a and b
          (a, b) = (b, a)
     xmid = None
     while np.abs(a - b) > eps:
         xmid = (a+b)/2.0
         if f(xmid) < 0:
             a = xmid
         else:
             b = xmid
     return xmid

print (bisect(f1,-5.0,0.0) )
print (bisect(f1,-2.0,0.0) )
```

```
-2.618034482
-0.381966590881
```

Dikkat, hem Newton hem de ikiye bölme yönteminde bazı patalojik durumlar
ortaya çıkabiliyor, bunlara karşı tetikte olunmalı, detaylar için [2, sf. 71]

Kaynaklar 

[1] Creighton, *Numerical Methods*

[2] Kincaid, *Numerical Analysis*

[3] *Newton's Method, Square Root*,
    [https://en.wikipedia.org/wiki/Newton's_method#Square_root](https://en.wikipedia.org/wiki/Newton's_method#Square_root)


