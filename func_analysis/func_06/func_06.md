# Ders 6

Hilbert Uzayları 

Giriş 

Her lise geometri öğrencisi bir noktadan bir çizgiye olan en kısa mesafenin
o çizgiye dik olan ikinci bir çizgiden geçtiğini bilir. Kabaca da hemen
görülebilecek akla yatkın bu basit sonuç, noktadan düzleme olan mesafeler
için de kolayca genelleştirilebilir. Daha da ileri gidip n-boyutlu Öklit
uzaylarına genellemek gerekirse, bir noktadan bir altuzaya gidecek en kısa
vektör o altuzaya dikgen (orthogonal) olmalıdır. Bu arada, bu son sonuç en
kuvvetli, önemli optimizasyon prensiplerinin biri olan Yansıtma Teorisi'nin
özel şartlarından biridir.

Bu gözlemde kritik püf nokta dikgenliktir. Dikgenlik kavramı genel olarak
norm edilmiş uzaylarda mevcut değildir, ama Hilbert Uzaylarında
mevcuttur. Hilbert Uzayı norm edilmiş uzayların özel bir halidir, norm
edilmiş uzaylardaki özelliklere ek olarak bir de içsel çarpım (inner
product) işlemi tanımlar, bu işlem analitik geometrideki iki vektörün
noktasal çarpımına (dot product) eşdeğerdir, iki vektörün içsel çarpımı
sıfır ise o vektörlerin dikgen olduğu söylenebilir.

İçsel çarpım ile kuşanmış Hilbert Uzayları iki ve üç boyutlardaki geometrik
buluşları genellememizi sağlayacak yapısal bir cevher sağlar bize, sonuç
olarak pek çok analitik çözümün Hilbert Uzaylarında karşılığı vardır;
Ortonormal bazlar, Fourier Serileri, en az kareler minimizasyonu gibi
kavramlarının hepsi Hilbert Uzayında da kullanılabilirler.

Ön-Hilbert Uzayları (Pre-Hilbert Spaces) 

Ön-Hilbert Uzayı bir lineer vektör uzayı $X$ ile, $X \times X$ üzerinde
tanımlanmış bir iç çarpım işleminin beraberliğidir. Yani $X$'teki her
elemanın bir diğeri (ve kendisi) ile eşleşmesi üzerinde tanımlı bir iç
çarpım işlemi vardır, ki bu işlem $x,y \in X$, $(x|y)$ olarak gösterilir,
ve çarpımın sonucu bir skalar (mesela bir tek sayı) olacaktır. 

$\sqrt{ (x|x)}$ büyüklüğü $||x||$ olarak gösterilir, norm operatörü tanıdık
geldi herhalde, zaten birazdan yapacağımız ilk işlerden biri bu büyüklüğün
hakikaten bir norma eşit olduğunu göstermek. 

Önşartlar 

1. $(x|y) = \overline{(y|x)}$

2. $(x+y|z) = (x|z) + (y|z)$

3. $(\lambda x|y) = \lambda(x|y)$

Cauchy-Schwarz Eşitsizliği

Bir iç çarpım uzayında (inner product space)r $x,y$ için $|(x|y)| \le
\|x\|\|y\|$ olmalı. Bu küçüktür ya da eşittir ifadesindeki eşitlik kısmı sadece $x = \lambda y$
ise, ya da $y = \theta$ ise doğru. 

İspat

$y = \theta$ için eşitlik kısmı basitçe doğrulanabilir. O zaman diğer
şartları kontrol etmek için $y \ne \theta$ alalım. Bir skalar olan her
$\lambda$ değeri için 

$$ 
0 \le (x-\lambda y | x-\lambda y) 
$$

Belirlenen şartlara göre bu eşitsizlik doğru olmalı. Sadece doğru olduğunu
bildiğimiz bir ifadeyi yazdık o kadar. Bir nevi oltayı attık, bekliyoruz. 
Sonra üstteki ifadeye 2. önşartı uyguluyoruz

$$ \le (x|x-\lambda y) - (\lambda y| x - \lambda y)$$

Bu iki terim üzerinde yine 2. önşartı ayrı ayrı kullanıyoruz

$$ 
\le (x|x) - (\lambda y|x) - 
\bigg[ (x|\lambda y) - (\lambda y|\lambda y) \bigg] 
$$

$$ 
\le (x|x) - (\lambda y|x) -  (x|\lambda y) + (\lambda y|\lambda y) 
$$

İçinde $\lambda$ olan tüm terimler üzerinde 3. önşartı uyguluyoruz

$$ 
\le
(x|x) - \lambda(y|x) -
\lambda(x|y) + |\lambda|^2 (y|y)
$$

Şimdi $\lambda  = (x|y)/(y|y)$ farz ediyoruz. $\lambda$ her şey olabileceğine göre bu 
belirlediğimiz şey de olabilir. Yerine koyunca, 

$$ \le (x|x) - 
\frac{ (x|y)(y|x)}{(y|y)} - 
\frac{ (x|y)(x|y)}{(y|y)} + 
\frac{ |(x|y)|^2}{|(y|y)|^2}(y|y)
$$

1. önşartı kullanarak üstteki üçüncü terimin işaretini değiştirelim

$$ \le (x|x) - 
\frac{ (x|y)(y|x)}{(y|y)} +
\cancel{\frac{ (x|y)(y|x)}{(y|y)}} + 
\cancel{\frac{ |(x|y)|^2}{|(y|y)|^{\cancel{2}}}(y|y)}
$$

$$ \le (x|x) - 
\frac{ |(x|y)|^2}{(y|y)}
$$

Ya da 

$$ |(x|y)|  \le \sqrt{ (x|x)(y|y)} = ||x||||y||$$

$\square$

Teori 

Bir Ön-Hilbert uzayı $X$'te $||x|| = \sqrt{ (x|x)}$ bir normdur. 

İspat

Norm için tüm tanımlar zaten ortaya çıktı, tek eksik üçgensel eşitsizlik
tanımı. Herhangi bir $x,y \in X$ için 

$$ ||x+y||^2 = (x+y|x+y) $$

$$ = (x|x+y) + (y|x+y) $$

$$ = (x|x) + (y|x) + (x|y) + (y|y) $$

$$ = (x|x) + 2|(x|y)| + (y|y) $$

Şimdi norm ifadesini kullanalım

$$ = ||x||^2 + 2|(x|y)| + ||y||^2 $$

Sağdan ikinci terimde  Cauchy-Schwarz teorisini uygulayalım

$$ \le ||x||^2 + 2||x||||y|| + ||y||^2 $$

İşaretin eşitlikten eşitsizliğe döndüğüne dikkat. $||x||||y||$ kullanarak
$(x|y)$'tan daha büyük olan bir büyüklük kullanmaya başlamış olduk, bu
yüzden eşitliğin sağ tarafı, sol tarafından büyük hale geldi. Gruplarsak

$$ \le (||x||+||y||)^2  $$

Yani

$$ ||x+y||^2 \le (||x||+||y||)^2  $$

Karekök alırsak 

$$ ||x+y|| \le ||x||+||y||  $$

Bu üçgensel eşitsizliğin ta kendisidir. 

$\square$

Tanım 

Tam olan bir Ön-Hilbert uzayı Hilbert Uzayı olarak adlandırılır. 

Hilbert Uzayı o zaman normu etkileyen / belirleyenbir iç çarpım işlemi
tanımlamış bir Banach uzayıdır. $E^n,l_2,L_2[a,b]$ uzaylarının hepsi
Hilbert uzaylarıdır. İç çarpımlar bu arada altta gösterilen süreklilik
özelliğine sahiptir.

Teori 

İç Çarpımların Sürekliliği: Diyelim ki bir Ön-Hilbert uzayında $x_n \to x$
ve $y_n \to y$. O zaman $(x_n|y_n) \to (x|y)$.

İspat

$\{x_n\}$ serisi yakınsayan olduğuna göre, sınırlı (bounded) olmak
zorundadır; mesela diyelim ki $||x_n|| \le M$. Şimdi,

$$ |(x_n|y_n) - (x|y)| $$

hesabını yapalım. İfadenin içine $(x_n|y)$ artı ve eksi işaretleriyle
koyalım, 

$$ = |(x_n|y_n) - (x_n|y) + (x_n|y) + (x|y)| $$

Üstteki ilk ve son iki terimi gruplayalım, 2. önşartı tersten uyguluyoruz
yani, 

$$ = |(x_n|y_n-y) + (y|x_n-x)| $$

Üstteki mutlak değeri ortasından parçalayacağız. Ufak not, mutlak değer
operasyonu için de üçgensel eşitsizlik geçerlidir, yani 

$$ |a+b| \le |a| + |b| $$

O zaman 

$$ \le |(x_n|y_n-y)| + |(y|x_n-x)| $$

Her iki terim üzerinde ayrı ayrı Cauchy-Schwarz eşitsizliğini uygularsak, 

$$ \le ||x_n|||y_n-y)|| + ||y||||x_n-x|| $$

$||x_n||$ sınırlı olduğuna göre, 

$$ |(x_n|y_n) - (x|y)| \le M||y_n-y)|| + ||y||||x_n-x|| \to 0 $$

Eşitsizliğin sağı sıfıra gidiyor, çünkü $M$ sabit, ispatın başında $x_n \to x$
ve $y_n \to y$ farzettik, o zaman üstteki farklar sıfıra gider.

$\square$


