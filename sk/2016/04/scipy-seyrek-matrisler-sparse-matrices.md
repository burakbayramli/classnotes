# Scipy Seyrek Matrisleri (Sparse Matrices)

Scipy kütüphanesinde seyrek matrisler üzerinde işlemi sağlayan pek çok
class, metot var. Seyrek matrislerde bilindiği gibi sıfır değeri
depolanmaz, kıyasla Numpy "yoğun (dense)" matrislerde herşey
depolanır, seyrek durumda bu sebeple sadece sıfır harici değerler için
yer ayrılacaktır. Tabii bu seyrek matrisler hala çarpım, toplama, vs,
gibi işlemlerin yapılmasına izin verirler, mesela toplama durumunda
iki seyrek matris A,B üzerinden A+B arka planda şöyle kodlanabilir,
B'de olmayan öğe  ile A'da olan bir değer toplanıyorsa sonuç direk
A'daki değer olur, çünkü olmayan değer otomatik olarak sıfırdır, bu
şekilde pek çok hesap daha hızlı şekilde yapılabilir.

Örnek: diyelim ki Netflix, kullanıcı filmi seyretmişse  müşterileri
satırda, filmler kolonda olacak şekilde bir matriste ona verdiği
beğeni değerini 1-5 arası bir değerle kaydetmiş olabilir. Bir
kullanıcı çoğunlukla filmlerin hepsini seyretmiş olamaz, o zaman bu
matriste satır başı en fazla 100-200 civarı  öğe / kolon değeri "dolu"
olacak, gerisi boş olacaktır. Eğer bu matrisi scipy.sparse üzerinden
işlersek, bellek ve işlem hızında ilerleme sağlarız.

Seyrek matris yaratmak icin onceden boyut tanımlanabilir, fakat o
boyut kadar yer bellekte onceden ayrilmaz.  Mesela

```
import scipy.sparse as sps
A = sps.lil_matrix((N,D)) 
```

N,D ne olursa olsun A bellekte hiç / çok az yer tutar.

Scipy kütüphanesinde pek çok farklı seyrek matris çeşitleri var,
mesela coo_matrix, csr_matrix, vs.. Bu matrislerin hepsinin duruma
göre avantajları / dezavantajları var. İyi haber birinden diğerine
geçiş çok kolay, mesela A bir lil_matris'ten coo_matrix'e geçiş için
A.tocoo() yeterli. Avantajlara örnek, mesela lil ile dilimleme
(slicing) yapılabilir, i.e. "bana sadece 3. kolonu ver" demek gibi,
ama coo ile bu yapılamaz.

Bazı Numaralar

```
X = sps.lil_matrix((2,5))
X[0,2] = 2.0  
X[1,3] = 5.0
```

olsun. Yogun hali

```
print X.todense()
```

```
[[ 0.  0.  2.  0.  0.]
 [ 0.  0.  0.  5.  0.]]
```

Sıfır Olmayan Öğeleri Gezmek 

Direk `lil_matrix` ile

```
rows,cols = X.nonzero()

for row,col in zip(rows,cols):
    print row,col, ' = ', X[row,col]
```

Eger coo_matrisi olsaydi

```
cx = X.tocoo()

for i,j,v in zip(cx.row, cx.col, cx.data):
   print i,j,' = ',v
```

Bir satırı bir matrisin her satırıyla ögesel olarak çarpmak, 

```
X2 = sps.lil_matrix((1,5))
X2[0,3] = 9
print X2.todense()
```

```
[[ 0.  0.  0.  9.  0.]]
```

Bu matrisi alıp önceki X'in her satırı ile öğesel çarptırmak için 

```
print X.multiply(X2).todense()
```

```
[[  0.   0.   0.   0.   0.]
 [  0.   0.   0.  45.   0.]]
```

Sadece sıfır olmayan ogelerinin log'unu almak, 

```
X=X.tocoo()
X2=X2.tocoo()
X2.data = np.log(X2.data)
print X2.todense()
```

```
[[ 0.          0.          0.          2.19722458  0.        ]]
```

Ortalama Çıkartmak

Scipy seyrek matrislerde ortalamayı almak külfetli olabiliyor, Scipy
ortalamayı çıkartmayı izin vermez (olmayan değerler ortalama alırken
sıfır mı kabul edilecektir? bu tam bilinmediği için izin
verilmemiş). Fakat bu özellik gerekiyorsa, şöyle yapılır,

```
import scipy.sparse as sps

def center(mat):
    mat = mat.T
    vec = sps.csc_matrix(mat.mean(axis=1))
    mat_row = mat.tocsr()
    vec_row = vec.T
    mat_row.data -= np.repeat(vec_row.toarray()[0],np.diff(mat_row.indptr))
    return mat_row.T

mat = sps.csc_matrix([[1, 2, 3, 5.],
                      [2, 3, 4, 5.],
                      [3, 4, 5, 5.]])

print center(mat).todense()

[[-1. -1. -1.  0.]
 [ 0.  0.  0.  0.]
 [ 1.  1.  1.  0.]]
```

Normalize Etmek 

Standardize etmek hem ortalamayı çıkartmak (demean), sonra normalize
etmek demektir. Bu iki işlem birbirinden bağımsız yapılabilir, bazen
biri bazen diğeri kullanılabilir. Normalize etmek, mesela satir L2
normalizasyonu diyelim, oyle bir islemdir ki o islem ardindan her
satirdaki ogelerin karesini toplayip karekoku alinca 1 sonucunu elde
edebilmek mumkun olacaktir, Normalize etmek için scikit-learn
paketinin fonksiyonları vardır, 

```
from sklearn.preprocessing import normalize
print normalize(mat, norm='l1', axis=0).todense()
```

```
[[-0.5 -0.5 -0.5  0. ]
 [ 0.   0.   0.   0. ]
 [ 0.5  0.5  0.5  0. ]]
```

Üstteki çağrı matrisin kolonlarını (çünkü axis=0 seçildi, bu kolon
demek) L1 normu kullanarak normalize etti; yani her kolonun L1
büyüklüğü hesaplandı ve o kolonun her hücresi bu büyüklük ile
bölündü. L2 norm kullabilirdik,

```
print normalize(mat, norm='l2', axis=0).todense()
```

```
[[-0.70710678 -0.70710678 -0.70710678  0.        ]
 [ 0.          0.          0.          0.        ]
 [ 0.70710678  0.70710678  0.70710678  0.        ]]
```

Satırları normalize edebilirdik, 

```
[[-0.33333333 -0.33333333 -0.33333333  0.        ]
 [ 0.          0.          0.          0.        ]
 [ 0.33333333  0.33333333  0.33333333  0.        ]]
```

Kosegenlik

Eğer bir seyrek matrisi köşegeninde belli değerler ile yaratmak
istiyorsak, köşegene gelecek değerleri bir vektör olarak tanımlayıp
matrisi yaratabiliriz. Hatta köşegenin bir, iki, vs. üstü ya da altına
gelecek değerleri yine vektör şeklinde tanımlamak ta mümkün.

```python
data = np.array([[-1, -1, -1, -1], [1, 1, 1, 1]])
diags = np.array([0, 1])
spdiags(data, diags, 4, 4).toarray()
```

```text
Out[1]: 
array([[-1,  1,  0,  0],
       [ 0, -1,  1,  0],
       [ 0,  0, -1,  1],
       [ 0,  0,  0, -1]])
```

Diske Yazmak, Okumak

En kullanışlı yaklaşım `scipy.iö` içindeki `mmread` ve `mmwrite`
kullanımı. Matrisi diske yazmak için

```python
import scipy.sparse as sps, scipy.io as io

A = sps.lil_matrix((4,4))
A[2,3] = 10
io.mmwrite("/tmp/A",A)
```

Yazılan dosya metin bazlı, düz okunabilen bir dosyadır,

```python
! cat /tmp/A.mtx
```

```text
%%MatrixMarket matrix coordinate real general
%
4 4 1
3 4 1.000000000000000e+01
```

Okumak için

```python
X = io.mmread("/tmp/A").tolil()
print (X.shape, X[2,3])
```

```text
(4, 4) 10.0
```

