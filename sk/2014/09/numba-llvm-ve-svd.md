# Numba, LLVM, ve SVD

Numerik hesaplarda, ya da diger konularda Python nasil daha hizli
isletilir? Numpy durumunda tum islemleri matrisler, ve kutuphane
cagrilari uzerinden olacak sekilde yapmak bu hizli isletimin bir
yolu. Bu durumda mesela pur Python dongulerinden kacinilir. Fakat illa
dongu kullanilacaksa ve pur Python tipleri kullanilacaksa, Cython
hizlandirma yapmanin yollarindan biri. Ya da Continuum tarafindan
gelistirilen Numba.

http://numba.pydata.org

Kurmak icin

```
apt-get install llvm-devpip install llvmpy
```

Numba ile dekorator kullanarak Python kodumuzun belli kisimlarini LLVM
uzerinden isletilebilmesini sagliyoruz. LLVM'i isledik, son zamanlarda
cok populer bir yaklasim, bir tur sanal kod isleticisi + derleyici
alet cantasi + araclari birlesimi.

Ilk ornek

Ornekte goruldugu gibi bir metot @jit ile isaretlenirse, hizli olarak
isletilebilir. Eger @jit(nopython=True) secilirse bu hizli yerel mod
demektir, bu durumda Python API cagrilari yapilamaz, nopython=False
ile obje moduna gireriz.

Ogrendigimiz bazi dersler (Numba 0.13.2 uzerinden edinilen bilgiler):  

Projemizde ogrendik ki obje modu digerine nazaran cok daha yavastir,
yerel mod tavsiye edilir. Yerel modda sadece temel tipler ve Numpy
matrisleri  / vektorleri kullanilabilir.

Bazen bir dongude kullandiginiz bir degiskeni tekrar mesela baska bir
dongude kullanamiyorsunuz (bir acaiplik aslinda, bir hata dogal
olarak). Mesela for i in range(..) sonrasi tekrar for i in range(...)
yapamamak.. Eger problem cikarsa sac yolmak yerine nereden
kaynaklandigini bilmek faydali olabilir :) Bu yuzden emin olmak icin
bir fonksiyon icinde hep degisik indis ismi kullanmak en iyisi.

Yerel modda fonksiyona disaridan gecilen ya da fonsiyon icinde
yaratilan vektorlerin icerigini vektorsel / toptan erisim ile
degistiremiyorsunuz (yani tum bir satirin icerigini degistirebilmek
gibi mesela). Sadece tekil oge (element) degisimi yapabiliyorsunuz. Bu
demektir ki indisler yaratip, for donguleri uzerinde vs. ogelere
erisip onlari degistirebiliyorsunuz. Simdi bu bir yetenegi kazanip
digerini kaybetmek demek oluyor, ne guzel tam np.dot(...)
yapabiliyorken birdenbire onu kaybediyoruz! Bu dogru.. Tabii diger
yandan for donguleri artik dehset hizli isleyecek (cunku Numba
icindeyiz), fakat bir kolayligi kaybetmek te ufak bir dezavantaj,
evet. Not: Ustteki durum obje modunda problem degil, ama obje modu
daha once soyledigimiz gibi cok daha yavas.

Seyrek matrisler nasil islenir? Dikkat, jit ile isaretlenmis
fonksiyonlar seyrek matris objeleri alamazlar, sadece Numpy matrisleri
ve vektorleri gonderebiliriz. O zaman seyrek matris icinden "sifir
olmayan" tum degerlerini surekli bir vektor olarak almaliyiz, mesela
altta bir seyrek matrisin tum degerlerini toplayan bir fonksiyon,

```
from numba import jit
import numpy as np
from scipy.sparse import coo_matrix, lil_matrix

@jit
def sum(A_rows, A_cols, A_data):
    res = 0.
    for j in range(len(A_data)):
        u = A_rows[j]
        i = A_cols[j]
        res += A_data[j]
    return resA = lil_matrix([[3,4,5,5],
                [5,4,5,0],
                [0,0,0,0]])

print AA = coo_matrix(A)
print sum(A.row, A.col, A.data.astype(np.float32))
```

Indisler u ve i icinde baktigimiz degerin satir ve kolon indislerini
elde edebiliyoruz. Ustteki ornekte bu lazim degildi ama
alinabilecegini gostermek icin ekledik. Not: ufak bir ornek oldugu
icin hiz farki gorulmeyebilir, ama alttaki gibi cagrilirsa fark
kesinlikle gorulecektir,

```
k = 3000
A = coo_matrix(np.random.randint(2, size=(k,k)))
print sum(A.row, A.col, A.data.astype(np.float32))
```

Bir ornek daha, SVD kodlamasinin Numba ile yapilmis halini
gorelim. Not: Alttaki gibi bir SVD kodlamasinin, bu arada, piyasadaki
neredeyse tum diger SVD kodlamalarindan farkli oldugunu belirtelim,
olmayan degerleri olmayan deger olarak kabul ediyor yani onlari
atliyor. Bu bir "matris tamamlama" problemidir, ki tavsiye
algoritmalari icin ozellikle bu yaklasim gerekir. Paket SVD'lerin cogu
(ki buna scipy svds, sparsesvd paketleri dahil) olmayan degerleri
"sifir" kabul ediyor, bu biraz farkli bir problem kabul edilir. Konu
hakkinda D. Gleich adli profosorun bir blog yazisi surada.

```
import numpy as np
from scipy.sparse import coo_matrix
from numba import jit

@jit(nopython=True)
def pred(u, i, P, Q):
    dot = 0.
    for f in range(P.shape[1]):
        dot += P[u, f] * Q[i, f]
    return dot

@jit
def sgd_mult(rows, cols, vals, P, Q, gam, rlam):
    q = np.empty(Q.shape[1]).astype(np.float32)
    p = np.empty(P.shape[1]).astype(np.float32)
    for j in range(len(vals)):
        u = rows[j]
        i = cols[j]
        value = vals[j]
        dev = value - pred(u, i, P, Q)
        for r in range(Q.shape[1]):
            q[r] = gam * (P[u, r]* dev - rlam*Q[i, r])
            p[r] = gam * (Q[i, r]* dev - rlam*P[u, r])
        for l in range(Q.shape[1]):
            Q[i, l] += q[l]
            P[u, l] += p[l]def svds(A, k, it=20, rlam=0.1, gam=0.1):
    P = 1./k * np.random.randn(A.shape[0], k).astype(np.float32)
    Q = 1./k * np.random.randn(A.shape[1], k).astype(np.float32)
    for it in range(it):
        sgd_mult(A.row, A.col, A.data.astype(np.float32), P, Q, gam, rlam)
    return P, Q.T@jit(nopython=True)def rmse(rows, cols, vals, P, Q):
    dev = 0.
    for j in range(len(vals)):
        u = rows[j]
        i = cols[j]
        val = vals[j]
        dev += (val - pred(u, i, P, Q))**2
    return np.sqrt(dev/len(rows))
```





