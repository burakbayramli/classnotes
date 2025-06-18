# Özyineli En Az Kareler (Recursive Least Squares)

$Ax = b$ denklem sistemini çözmek için 

$$ 
x = (A^TA)^{-1}A^Tb  
\qquad (1) 
$$ 

formülü en az kareler çözümü olarak biliniyor, bkz [2]. Bu çözüm iyi işler,
fakat bazı durumlarda negatif bir tarafı var, çözüm toptan (batch) olarak
yapılıyor. $A$ içinde 100 tane satır olabilir, ona göre bir çözüm bulunur,
ardından 1 tane ek veri satırı gelirse olsa 101 tane satır için tüm
işlemlerin tekrar baştan yapılması gerekir. Acaba sadece o yeni verilen
satır için önceki $x$ tahminini bir şekilde güncellemek mümkün mü?

Özyineli en az kareler ile bunu yapabiliriz. Diyelim ki 

$$ c_1 t + c_2 = b $$

lineer sistemini çözmek istiyoruz, yani bu bir çizgi uydurma (line fitting)
olacak, kesi $c_2$, eğim $c_1$. Notasyon altta, altsimge $k$ kaç tane veri
satırı olduğunu gösterecek,

$$ A_kx_k \approx b_k, \quad 
A_k = \left[\begin{array}{cc}
t_1 & 1 \\
t_2 & 1 \\
\vdots & \vdots \\
t_k & 1 
\end{array}\right], \quad 
x_k = \left[\begin{array}{r}
c_{1,k} \\ c_{2,k}
\end{array}\right], \quad
b_k = \left[\begin{array}{r}
B_1 \\ B_2 \\ \vdots \\ B_k
\end{array}\right]
$$

Eğer tek istediğimiz tek boyutlu bir zaman serisi için çizgi uydurma yapmak
ise $t_1,..,t_k$ 1 ve $k$ arası tam sayılar olurdu, bu durumda $A_k$ iyice
basitleşir. Devam edelim, eğer (1)'i üstteki format için uyarlarsak,

$$ 
x_k = (A_k^TA_k)^{-1}A_k^T b_k 
\qquad (5) 
$$

Yani elde $k$ tane veri var, üstteki formülü uyguladık ve bir çözüm
bulduk. Şimdi diyelim ki yeni ölçümler $(t_{k+1}, B_{k+1})$ aldık, ve 

$$ 
x_{k+1} = (A_{k+1}^TA_{k+1})^{-1}A_{k+1}^T b_{k+1} 
\qquad (3) 
$$

hesabını yapmamız lazım. Ek notasyon;

$$ 
A_{k+1} = \left[\begin{array}{c}
A_k \\ a_{k+1}^T
\end{array}\right], \quad
a_{k+1}^T = \left[\begin{array}{c}
t_{k+1} \\ 1
\end{array}\right], \quad 
b_{k+1} = \left[\begin{array}{c}
b_k \\ B_{k+1}
\end{array}\right], \quad 
P_k = (A_k^TA_k)^{-1}
\qquad (4)
 $$

Matris tersi $P_k$'nin yeni veri noktası gelince nasıl güncellendiğini
görelim, 

$$ 
P_{k+1} = (A_{k+1}^TA_{k+1})^{-1} = 
\bigg[
\left[\begin{array}{cc}A_k & a_{k+1} \end{array}\right]
\left[\begin{array}{c}A_k \\ a_{k+1}^T \end{array}\right]
\bigg]^{-1}
$$

Eşitliğin sağındaki matris çarpımını yaparsak, ve $P_k$'yi yerine koyarsak,

$$ = [ A_k^TA_k + a_{k+1}a_{k+1}^T ]^{-1} 
= [ P_k + a_{k+1}a_{k+1}^T ]^{-1} 
\qquad (2)
$$

Üstte yine sağdaki formül $(A+BCD)^{-1}$ formunda bir ters alma işlemi gibi
gözüküyor; Matris Tersi Yardımcı Teorisi (Matrix Inversion Lemma) diyor ki
[1, sf. 469], herhangi bir $A,B,C,D$ için,

$$ [A + BCD]^{-1} = A^{-1} - A^{-1}B[C^{-1} + DA^{-1} B]^{-1} DA^{-1} $$

(2)'deki ifadenin üstteki forma göre paylaştırmasını şöyle yapalım, 
$A = P_k$, $B = a_{k+1}$, $C=I$, $D=a_{k+1}^T$. Buna göre (2) üstteki 
açılım üzerinden ve paylaştırılan sembollere göre şu hale gelir,

$$ P_{k+1} = P_k - P_k a_{k+1}(I + a_{k+1}^T P_k a_{k+1})^{-1} a_{k+1}^TP_k  $$

Parantez içindeki büyük çarpım bir tek sayı olduğu için $I$ değeri 1
yapılabilir,

$$ P_{k+1} = P_k - P_k a_{k+1}(1 + a_{k+1}^T P_k a_{k+1})^{-1} a_{k+1}^TP_k  
\qquad (6)
$$

Bu durumda tersi alınan parantez içindeki tüm ifade de tek sayı demektir,
ve bu tek sayının tersini almak çok basittir ($x$ için $1/x$). 

Nihai güncelleme formülü için devam edelim; (3) formülüne (4)'teki
eşitlikleri koyalım,

$$ x_{t+1} = 
P_{k+1} 
\left[\begin{array}{cc} A_k^T & a_{k+1} \end{array}\right]  
\left[\begin{array}{c} b_k \\ B_{k+1} \end{array}\right]  
$$

$$ = P_{k+1} [A_k^Tb_k + a_{k+1}B_{k+1} ] $$

(5) formülünü değiştirerek şu hale getirebiliriz,

$$ (A_k^TA_k) x_k = A_k^T b_k $$

Bu sonucu iki üstteki formüle sokarsak, 

$$ = P_{k+1} [A_k^TA_kx_k + a_{k+1}B_{k+1} ] $$

(4)'teki formlar üzerinden 

$$ A_{k+1}^TA_{k+1} =  A_k^TA_k + a_{k+1}a_{k+1}^T  $$

diyebileceğimizi görmüştük, o zaman 

$$ A_{k+1}^TA_{k+1}x_k =  (A_k^TA_k + a_{k+1}a_{k+1}^T)x_k  $$

Üç üstteki formülde yerine koyalım,

$$ = P_{k+1} [(A_k^TA_k + a_{k+1}a_{k+1}^T)x_k + a_{k+1}B_{k+1} ] $$

$$ = P_{k+1} [P_{k+1}^{-1}x_k + a_{k+1}a_{k+1}^Tx_k + a_{k+1}B_{k+1} ] $$

$$ x_{k+1} = x_k + P_{k+1}a_{k+1}a_{k+1}^Tx_k  + P_{k+1}a_{k+1}B_{k+1}  $$

$$ 
x_{k+1} = x_k + P_{k+1}a_{k+1}(a_{k+1}^Tx_k  + B_{k+1})  
\qquad (7)
$$

Şimdi $P_{k+1}$'yi özyineli olarak temsil etmek şunları yapalım. $K_{k+1} =
P_{k+1}a_{k+1}$  sistemin kazanç matrisi (gain matrix) olsun, ve (6)'daki 
$P_{k+1}$ eşitliği kullanarak formülü genişletelim,

$$ K_{k+1} = P_{k+1}a_{k+1} = 
[ P_k - P_k a_{k+1} [ 1 + a_{k+1}^T P_k a_{k+1} ]^{-1} a_{k+1}^TP_k ] a_{k+1}
$$

$$ = P_ka_{k+1} - P_k a_{k+1}[a_{k+1}^T P_k a_{k+1} + 1]^{-1} a_{k+1}^TP_ka_{k+1} $$

$$ = P_ka_{k+1} 
\big[ I - [ a_{k+1}^T P_k a_{k+1} + 1 ]^{-1} a_{k+1}^TP_ka_{k+1} \big] $$

Eğer bu formülü aynı anda hem $(a_{k+1}^TP_ka_{k+1})$ hem de $(a_{k+1}^TP_ka_{k+1})^{-1}$ 
ile çarparsak (hiçbir etkisi olmayan bir işlem, birbirini iptal ediyor
çünkü) bazı temizleme işlemlerini yapmak mümkün olur,

$$ 
= P_ka_{k+1} 
\big[ (a_{k+1}^T P_k a_{k+1} + 1) -  a_{k+1}^TP_ka_{k+1} \big] (a_{k+1}^T P_k a_{k+1} + 1)^{-1}
$$

Büyük parantez içinde sadece +1 sağ kalır, geri kalanlar iptal olur,

$$ 
K_{k+1} = P_ka_{k+1} (a_{k+1}^T P_k a_{k+1} + 1)^{-1}
$$

Bu formülü (7) içine geri $K_{k+1}$ olarak koyarsak, 

$$ x_{k+1} = x_k + K_{k+1}(a_{k+1}^Tx_k  + B_{k+1})  
$$

Aynı şekilde (6) içine koyarsak,

$$ 
P_{k+1} = P_k - 
\underbrace{P_k a_{k+1}(1 + a_{k+1}^T P_k a_{k+1})^{-1}}_{K_{k+1}}
a_{k+1}^TP_k 
$$

$$ 
P_{k+1} = P_k - K_{k+1}a_{k+1}^TP_k 
$$

Böylece $K_{k+1},P_{k+1},x_{k+1}$ özyineli güncelleme formüllerini elde
etmiş oluyoruz. 

Kodlar

Güncelleme kodları alttadır,

```python
import numpy as np

def rlse_online(aT_k1,b_k1,x,P): 
    K = np.dot(P,aT_k1.T)/(np.dot(np.dot(aT_k1,P),aT_k1.T)+1)
    x = x +K*(b_k1-np.dot(aT_k1,x))
    P = P-np.dot(K,np.dot(aT_k1,P))
    return x,K,P

```

Örnek olarak alttaki veriyi kullanalım. 

```python
import numpy.linalg as lin
b = np.array([[3.0,4.0,6.0,3.0,8.0,7.0,5.0]]).T
A= np.ones((len(b),2)); A[:,1] = range(len(b))
```

Özyineli olarak problemi çözelim; her veri noktasını teker teker güncelleme 
rutinine geçelim. 

```python
import rls
n = 2
P = np.eye(n,n)*100.
x = np.zeros((n,1))
for k in range(len(b)):
   x,K,P = rls.rlse_online(np.array([[k,1]]),b[k,:],x,P)
print x
```

```
[[ 0.5037057 ]
 [ 3.62655923]]
```

Üstteki sonuç bulundu. Şimdi aynı verileri en az kareler ile toptan şekilde
çözelim,

```python
import statsmodels.api as sm

y = b; x = A
f = sm.OLS(y,x).fit()
print f.params
```

```
[ 3.64285714  0.5       ]
```

Önce Toptan, Sonra Özyineli

Eğer verinin bir kısmı için toptan başlayıp sonra özyineli gitmek istersek
ne yaparız? O zaman elde bir $(A_k^TA_k)^{-1}$, yani $P_{k}$ olurdu, toptan
şekilde hesaplanmış olacaktı, ve bu değerin sonraki hali için güncelleme
formülünü biliyoruz, böyle devam ederdik. Tabii bu durumda
$(A_k^TA_k)^{-1}$'yi toptan hızlı hesaplamak için bir teknikten bahsetmek
lazım, en az kareler rutinleri genelde bu değeri geri döndürmezler, {\em
  Lineer Cebir Ders 16}'dan hatırlarsak bu hesabı direk yapmak oldukça
pahalı, o yüzden QR bazlı bir yaklaşım lazım (aynen $x$'in kendisinin QR
bazlı hesaplandığı gibi). Her $A_k$ matrisinin bir $A_k = QR$ açılımı
olacağından hareketle, 

$$ A_k^TA_k = (QR)^TQR = R^TQ^TQR = R^TR $$

O zaman 

$$ (A_k^TA_k)^{-1} = (R^TR)^{-1} = R^{-1}R^{-T} $$

Şimdi verinin en son satırı hariç ilk kısmı üzerinde bu değeri hesaplayalım,

```python
A_k = A[:-1,:]
b_k = b[:-1,:]
print A.shape, A_k.shape
q,r = lin.qr(A_k)
Pk_r = np.dot(lin.inv(r), lin.inv(r.T))
print Pk_r
Pk = lin.inv(np.dot(A_k.T,A_k))
print Pk
```

```
(7, 2) (6, 2)
[[ 0.52380952 -0.14285714]
 [-0.14285714  0.05714286]]
[[ 0.52380952 -0.14285714]
 [-0.14285714  0.05714286]]
```

Direk usül ve QR bazlı ters işleminin aynı sonuçlara erişildiğini
görüyoruz. Toptan $x_k$

```python
x_batch = np.dot(np.dot(lin.inv(r), q.T), b_k)
print x_batch.T[0]
```

```
[ 3.0952381   0.82857143]
```

Şimdi yeni veri noktası ile güncelleyelim,

```python
A_new = A[-1,:]
b_new = b[-1,:]
x_new,K_new,P_new = rls.rlse_online(A_new,b_new,x_batch.T[0],Pk_r)
print x_new
```

```
[ 3.64285714  0.5       ]
```

Aynı sonuca eriştik. 
 
Kaynaklar

[1] Yang, *Applied Numerical Methods using Matlab*

[2] Bayramlı, Lineer Cebir, *Ders 16*




