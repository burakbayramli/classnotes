# Ders 1.3

Konumuz $Au = b$ sistemini çözmek. Bu çözüm için Python'da
`linalg.solve` çağrısı var. Mesela

```python
import scipy.linalg
A = [[2,3,4],[5,5,6],[7,7,7]]
b = [1,2,3]
u = scipy.linalg.solve(A, b)
print (u)
```

```
[ 0.14285714  0.42857143 -0.14285714]
```

`linalg.solve` çağrısı Matlab'de `` çağrısının karşılığı,
oradaki kullanım u = A `` b şeklinde. 

Eğer elimizde ikinci bir `c` vektörü var ise, ve eşitliğin sağ
tarafında `b` sonrası onu kullanmak istiyorsak ayrı ayrı `solve`
komutlarına gerek yoktur. Her iki vektörü birbirine ekleyerek,
`solve`'u toplu halde çağırabiliriz, bu performans açısından daha iyi
olur. 

```python
c = [2,3,8]
bc = np.vstack((b,c)).T
u = scipy.linalg.solve(A,  bc)
print (u)
```

```
[[ 0.14285714 -1.28571429]
 [ 0.42857143  5.14285714]
 [-0.14285714 -2.71428571]]
```

Python `vstack` komutu iki matrisi üst üste koymak için kullanılır.

Her iki çözüm beraber olarak geri gelecektir. Bu niye daha hızlı? Çünkü
Python'un çözücüsü daha eşitliğin sağ tarafına bile gelmeden sadece
`A`'ya bakarak bir sürü işlem gerçekleştiriyor, eliminasyon yaparak
`A`'yı üçgensel hale getirmek gibi. Bu tür işlemleri gereksiz kere iki
kere yapmak pahalı olurdu.

Eğer $A$ karesel değilse, ama biz her iki durumda da işleyen bir komut
istiyorsak, `np.linalg.lstsq(A,b)` kullanabiliriz. 

Soru: matematiksel olarak $u$'yu bulmak 

$$ Au = b $$

$$ u = A^{-1}b $$

demektir. Peki Python bu hesap için gerçekten $A^{-1}$'i hesaplar mı?

Hayır. Çünkü büyük problemler için matris tersini hesaplamak oldukça
pahalıdır. Ayrıca $A$ matrisi zaten üçlü köşegen (tridiagonal) bir halde
olabilir, ve cevap zaten hazır haldedir, bu noktada ters alma işlemi
gereksiz olurdu. 

Biraz zihin egzersizi yapalım. Eğer şöyle bir komut kullansam ne elde
ederim (ki `I` matrisi birim matrisi) ?

```
solve(A, I)
```

Cevap, tabii ki $A$'nin tersini elde ederim, yani $A^{-1}$ çünkü $AA^{-1} =I$,
sağ tarafta birim matrisi var ise çözüm sadece $A^{-1}$ olabilir.

$$ 
A 
\left[\begin{array}{rrr}
u_1 & u_2 & u_3
\end{array}\right]
=
\left[\begin{array}{rrr}
1 & 0 & 0\\
0 & 1 & 0\\
0 & 0 & 1
\end{array}\right]
$$

Bu probleme bakmanın değişik bir yolu: sağ taraftaki birim matrisi içindeki
[1 0 0] gibi değerler içindeki 1 değerlerini birer zıplama (impulse) anı
gibi görmek, sanki elimizde bir düz [0 0 .. 0] bir veri var, içinde tek
zıplama olan yer orası, ve bu [1 0 0], [0 1 0], .. içinde tek zıplama olan
veriler "işlenerek'' bize $u_1$, $u_2$, .. gibi sonuçları veriyorlar. 

Elle $A$'nin tersini bulmak için ne yapardık? Bir blok matrisi yaratırdık,
`[A İ]`, yani 3x3 ve 3x3 iki matrisi yanyana koyup 3x6 boyutunda yeni
bir matris elde ederdik, ve bu matriste $A$ üzerinde eliminasyon, pivotları
sıfırlama gibi numaraları kullanarak onu birim matrise çevirirdik, bu arada
aynı operasyonları tabii ki $I$ üzerinde uygulardık. En sonunda $A$ birim
olunca $I$ $A^{-1}$'e dönüşmüş olurdu!

Şimdi biraz büyük resme bakalım. 

Lineer cebirin 4 büyük problemi Python komutları ile beraber şunlardır:

1) Eliminasyon, 
`scipy.linalg.lu(A)` $A = LU$

2) Dikgenleştirme (orthogonalization), 
`scipy.linalg.qr(A)`, $A = QR$

3) Özdeğerler (eigenvalues), 
`scipy.linalg.eig(A)` $A = SAS^{-1}$

4) Eşsiz değerler (singular values), 
`scipy.linalg.svd(A)` $A = U \Sigma V^{T}$

Eliminasyon ne yapar? Dikkat edersek aslında bu işlemin bir alt üçgensel
(lower triangular) matris ($L$) ve bir tane de üst üçgensel matris ($U$)
ortaya çıkardığını görürüz. Şimdi alttaki matris üzerinde eliminasyon
yapalım ve bu arada tersini de bulmuş olalım. 

$$
\left[\begin{array}{rrr}
1 & -1 & 0 \\
-1 & 2 & -1 \\
0 & -1 & 2 
\end{array}\right]
$$

Eliminasyon işlemlerini yapalım (pivotlar öğeleri parantez içinde)

$$
\left[\begin{array}{rrr}
(1) & -1 & 0 \\
-1 & 2 & -1 \\
0 & -1 & 2
\end{array}\right]
\to
\left[\begin{array}{rrr}
(1) & -1 & 0 \\
0 & (1) & -1 \\
0 & 0 & (1)
\end{array}\right] = U
$$

$$ l_{21} = -1 \quad l_{31} = 0 \quad l_{32} = -1 $$

$$
L = \left[\begin{array}{rrr}
1 & 0 & 0 \\ -1 & 1 & 0 \\ 0 & -1 & 1
\end{array}\right]
$$

$l_{21} = -1$ yapılan ilk işlemi kodluyor, 1. satırı -1 ile çarp ve
2. satırdan çıkart anlamına geliyor. Diğerlerini de sırasıyla görüyoruz ve
bu işlemlerin sonucunda üst üçgensel matris $U$'yu elde ediyoruz. Tüm $l$
değerlerini bir araya koyup $L$'yi elde edebiliriz. Bir tane daha yapalım:

$$
\left[\begin{array}{rrr}
2 & -1 & \\ -1 & 2 & -1 \\ & -1 & 2
\end{array}\right]
\to
\left[\begin{array}{rrr}
2 & -1 & 0 \\ 0 & 3/2 & -1 \\ 0 & 0 & 4/3
\end{array}\right] = U
$$

$$ l_{21}=-\frac{1}{2}, \quad
l_{31} = 0, \quad
l_{32} = -\frac{2}{3}
$$

$$ L =
\left[\begin{array}{ccc}
1 &  &  \\ -1/2 & 1 &  \\ 0 & -2/3 & 1
\end{array}\right] 
$$


Eğer eşsiz (singular) bir matris üzerinde eliminasyon yapsak, bu işlemi
nasıl etkilerdi? 

$$
\left[\begin{array}{rrr}
(1) & -1 & 0 \\ -1 & 2 & -1 \\ 0 & -1 & 1
\end{array}\right]
\to
\left[\begin{array}{rrr}
(1) & -1 & 0 \\ 0 & (1) & -1 \\ 0 & 0 & (0)
\end{array}\right] 
$$

Yani bu durumda 3 tane pivot elde edemezdik, sağ alt köşedeki değer
eliminasyon sırasında 0 olurdu, ve sağ matris, aynen sol matris gibi, eşsiz
olurdu. Bu işimize yaramazdı. 

İki üstteki probleme dönelim: Burada ilk matris simetrik idi, ama $L$ ve $U$
matrisi artık simetrik değil. Simetriyi geri getirebilir miyiz? $U$ içinden
sadece çaprazları çekip çıkartalım

$$
\left[\begin{array}{rrr}
(2) & -1 & 0 \\
0 & (3/2) & -1 \\
0 & 0 & (4/3)
\end{array}\right] = U =
\left[\begin{array}{rrr}
2 & & \\ & 3/2 & \\ & & 4/3
\end{array}\right] 
\left[\begin{array}{rrr}
1 & -1/2 & 0 \\ 0 & 1 & -2/3 \\ 0 & 0 & 1
\end{array}\right]
$$


Böylece çaprazında
$\left[\begin{array}{ccc} 2 & 3/2 & 4/3 \end{array}\right]$ olan bir matris
elde ettik. Peki bu matrisin çarptığı (onun hemen sağında) içinden
çaprazları çekip çıkardığımız matristen geri kalanlar tanıdık geliyor mu?
Evet, bu matris te $L^T$'e eşit. Demek ki $LU = LDL^T$ gibi bir ifade
mümkün.

Biliyoruz ki 

$K = LDL^T$

ifadesinde $K$ her zaman simetriktir. Ters yönden söylersek, herhangi bir
simetrik $K$ matrisini alıp eliminasyon yaparsam ve $L$ ve $D$ elde edince,
$L^T$ ile çarpabilirim. 

Peki şunu ispat edebilir miyiz? Herhangi bir $L$ ve çapraz $D$ var ise,
$LDL^T$ her zaman simetrik midir? Bir matrisin simetrik olması demek
kendi devriğine (transpose) eşit olması demektir. Yani 

$$ K = LDL^T $$

$$ K^T = (LDL^T)^T $$

Devriği alınca parantez içindeki çarpımların sırası değişir.

$$ = (L^T)^TD^TL^T $$

$D^T = D$ çünkü $D$ zaten köşegen bir matris, önemli tüm değerleri
çaprazında ve devrik işlemi bu durumu değiştirmiyor. O zaman

$$ = LDL^T $$ 

Tekrar başladığımız noktaya döndük. Demek ki başladığımız matris
simetriktir. İspat tamamlandı. 

Genele dönelim: $A^TA$'nin mesela karesel olduğunu biliyorduk ($n \times m$ ile
$m \times n$ çarpılınca $n \times n$ boyutu elde edilir). Şimdi bunun üzerine
simetrik olduğunu da artık biliyoruz, üstte ispatladık.

Kural: Simetrik matrislerin tersi (inverse) de simetriktir. O zaman
$K^{-1}$ de simetriktir. 









