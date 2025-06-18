# Ders 2.15

Konumuz çok, çok büyük ve seyrek matrisler üzerinden $Ax = b$ çözümü. Çok
büyük boyutlarda $A$'nin tersini almak pahalı bir işlem olacaktır. Standart
teknik Gauss Eliminiasyon tekniği de yüksek boyutlarda pahalı bir işlem
olur. Pahalı olmayan işlem nedir? $A$'yi bir vektör ile çarpmaktır
mesela. Bu işlemin nasıl devreye gireceğini göreceğiz.

Genel ismiyle daha hızlı olacak genel kategori özyineli (iterative)
metotlardır. Bu yöntemlerde en iyi cevaba erişmeyiz, ama yeterince
yaklaşırız, ve daha önemlisi bu işi çok hızlı bir şekilde
yapabiliriz. Bu metotlarda iyi bir önkoşullandırıcı (preconditioner)
matris $P$'yi seçmek önemlidir. $P$, $A$'yi temel alan ve bazı işlemleri
kolaylaştıran bir yapı olacaktır. 

Özyineli tekniklerden en iyi bilinenlerden biri eşlenik gradyan
tekniğidir. Bu yöntem için $A$'nin simetrik, pozitif kesin olması gerekir.

Özyineli metotlarda bir başlangıç $x_0$ değeri vardır, ve oradan $x_{k+1}$
elde edilir. Lineer metotlar için başlangıcın nerede olduğu önemli
değildir, sıfırda bile başlanabilir. Gayrılineer (nonlinear), "Newton''
metotlarında sonuca yakın bir yerde olmak önemlidir, bunun için uğraşılır.

Çözmek istediğimiz 

$$ Ax = b $$

Bunu şöyle de yazabilirim 

$$ x = x - Ax + b $$

$$ x = (I - A)x + b $$

Şimdi bu denklemi alıp sağ tarafı "eski'' sol tarafı "yeni'' olarak
temsil edersek,

$$ x_{k+1} = (I - A)x_k + b $$

elde ederiz. Bu önkoşulsuz, basit bir özyinelemedir. Önkoşul $P$ istersek,

$$ Ax = b $$

$$ 0 = -Ax + b $$

$$ Px = Px - Ax + b $$

$$ Px = (P -A)x + b $$

$$ Px_{k+1} =  (P - A)x_k + b $$

Eğer $P = A$ olsaydı, o zaman direk eski denklemi çözüyor olurduk.  
Biz $P\approx A$ dedik, "yakın ama aynı olmayan bir $P$'' istiyoruz,
özellikle. Bu $P$'nin işlerimizi kolaylaştıracağını umuyoruz çünkü.

Bazı $P$ örnekleri şunlardır: Jacobi $A$'nin sadece çaprazındaki değerleri
alıp $P$'ye koyar. Gauss-Seidel yaklaşımı [1], hem çaprazı, hem alt üçgensel
(lower triangular) kısmı alıp $P$'ye koyar.

Not: İlginç bir tarihi anektod, Gauss Eliminasyon yöntemini keşfeden bizzat
Gauss'un kendisi bile bu yöntemi kullanmak istememişti, büyük matrislerde
eliminasyon işinin özellikle hesabın elle yapıldığı eski yılllarda çok
külfet getiriyordu. Özyineli ilk metotlardan Gauss-Seidel tekniği Gauss'u
çok memnun etti, ve kendi hesaplarında bu tekniği kullandı.

Diğer yaklaşımlar fazla rahatlatma (overrelaxation), ve tamamlanmamış
(incomplete) LU gibi yaklaşımlar. Ben üstlisans yaparken bu son iki yöntem
Jacobi, Gauss-Seidel'den bir adım ileri gitme yönündeki denemelerin
başlangıcıydı. 

Peki $x$'lerin doğru cevaba erişip erişmediğini nereden anlarız? Hata
hesabı için bir formüle ihtiyacım var. Alttaki formüllerde 2. formülü
1. formülden çıkartırsam, ve $e_k = x - x_k$ ise

$$ x_{k+1} = (I - A)x_k + b $$

$$ x_k = (I - A)x + b $$

Şunu elde ederim,

$$ Pe_{k+1} = (P-A)e_k $$

İki tarafı $P^{-1}$ ile çarparsam,

$$ e_{k+1} = (I-P^{-1}A)e_k = Me_k$$

O zaman hata hesabı için her özyineleme adımında üstteki hesabı
yaparım. Parantez içindeki büyük ifadeye $M$ ismi verdim, buna özyineleme
matrisi de diyebiliriz. 

Değerlere yakında bakarsak, $P$'nin $A$'ya yakın olmasını istiyoruz
demiştik, o zaman $P^{-1}A$, $I$'ya yakın olacaktır, ve bu $I$'ya yakın
olan şey $I$'dan çıkartılınca sonuç sıfıra yakın olacaktır. Hatanın ufak
olmasını istediğimize göre bu mantıklı. 

Her adımda $M$ ile çarptığımıza göre, 

$$ e_k = M^k e_0 $$

Üstteki sıfıra gider mi? Giderse ne kadar hızlı gider? Bunun olması için
$M$'nin hangi öğesine bakmak gerekir? En büyük özdeğerine bakmak
gerekir. Genel olarak şunu söyleyebiliriz, her $|\lambda(M)| < 1$ olması
gerekir. Notasyonel olarak en büyük özdeğer $\rho(M)$'dir, $|\rho(M)|$ ise
spektral yarıçapı (spectral radius) olarak adlandırılır.

Bazı örnekler

$$ K = A = 
\left[\begin{array}{rrrr}
2 & -1 && \\
& 2 & -1 & \\
&&& \\
&& -1 & 2 \\
\end{array}\right]
 $$

Özdeğerler $\lambda_j(A) = 2 - 2 \cos\theta_j$

$P_{Jacobi} = 2I$

$$ M = I-P^{-1}A  $$

Sonuç

$$ 
\left[\begin{array}{rrrr}
0 & \frac{ 1}{2} & & \\
\frac{ 1}{2} & 0 & \frac{ 1}{2}& \\
 & \frac{ 1}{2} & \ddots & \ddots \\
 && \ddots & 0
\end{array}\right]
 $$

Boş olan yerlerde sıfır değerleri var. 

Yani $P^{-1} = 1/2$

$$ M = I-\frac{ 1}{2}A  $$

$$ \lambda_j(M) = 1 - \frac{ 1}{2}\lambda_j(A) = \cos \frac{ j\pi}{N+1}$$

O zaman yaklaşıksallama olacak. En büyük özdeğer

$$ \rho = \cos \frac{ \pi}{N+1} $$

Eğer her döngüde bir şeyleri grafiklemek istesem, neyi seçerdim? Her
döngüdeki hatayı, "artığı (residual)'' grafikleyebilirdim. Tam denklem

$$ Ax = b $$

$Ax_k$ gerçeğe "yakın'', o zaman artık değer $r$ bu ikisi arasındaki fark
olabilir, 

$$ r = Ax - Ax_k $$

$$ r = Ae_k $$

[hata grafikleme atlandı]

Örnek Jacobi kodları

Kod \#1

```python
import scipy.linalg as lin

A = np.array([[6.,1.,1.],
              [1.,7.,1.],
              [1.,1.,8.]])
b = [1.,1.,1.]

xreal = lin.solve(A, b); print "solution", xreal

P = np.diag(np.diag(A)); print "P",P
x = np.zeros(A.shape[0]); print x
T = P - A
for i in range(10):
    x =  lin.solve(P, b+np.dot(T,x))
    print x
```

```
solution [ 0.13249211  0.11041009  0.09463722]
P [[ 6.  0.  0.]
 [ 0.  7.  0.]
 [ 0.  0.  8.]]
[ 0.  0.  0.]
[ 0.16666667  0.14285714  0.125     ]
[ 0.12202381  0.10119048  0.08630952]
[ 0.13541667  0.11309524  0.09709821]
[ 0.13163442  0.10964073  0.09393601]
[ 0.13273721  0.11063279  0.09484061]
[ 0.1324211   0.11034603  0.09457875]
[ 0.13251254  0.11042859  0.09465411]
[ 0.13248622  0.11040476  0.09463236]
[ 0.13249381  0.11041163  0.09463863]
[ 0.13249162  0.11040965  0.09463682]
```

Kod \#2

```python
A = np.array([[6.,1.,1.],
              [1.,7.,1.],
              [1.,1.,8.]])

b = [1.,1.,1.]

xreal = lin.solve(A, b); print "solution", xreal

P = np.diag(np.diag(A)); print "P",P
x = np.zeros(A.shape[0]); print x
J = lin.solve(P,P-A)
c = lin.solve(P,b)
for i in range(10):
    x = np.dot(J,x) + c
    print x
```

```
solution [ 0.13249211  0.11041009  0.09463722]
P [[ 6.  0.  0.]
 [ 0.  7.  0.]
 [ 0.  0.  8.]]
[ 0.  0.  0.]
[ 0.16666667  0.14285714  0.125     ]
[ 0.12202381  0.10119048  0.08630952]
[ 0.13541667  0.11309524  0.09709821]
[ 0.13163442  0.10964073  0.09393601]
[ 0.13273721  0.11063279  0.09484061]
[ 0.1324211   0.11034603  0.09457875]
[ 0.13251254  0.11042859  0.09465411]
[ 0.13248622  0.11040476  0.09463236]
[ 0.13249381  0.11041163  0.09463863]
[ 0.13249162  0.11040965  0.09463682]
```

Bu kodların ikisi de özyineli Jacobi hesabı yapıyor. Birincisi her döngüde 
`solve` işlemi yapıyor. Fakat daha önce belirttiğimiz gibi, her
döngüde çarpım işlemi yapmak çok daha optimal olur. İkinci kod [1]

$$ Px_{k+1} =  (P - A)x_k + b $$ 

işlemini iki parçaya ayırmış, $P,P-A$ ve $P,b$ sistemlerini ayrı ayrı
çözerek, döngü içinde $Jx + c$ ile sadece çarpma ve toplama kullanmayı
başarmış. Bu parçalamanın yapılabilmesinin sebebi tabii ki bir lineer
sistemle çalışıyor olmamız. Çok akıllıca bir teknik. 

Kaynaklar

[1] Olver, *A Basic Introduction to Matlab*, [http://www.math.umn.edu/~olver/matlab.html](http://www.math.umn.edu/~olver/matlab.html)




