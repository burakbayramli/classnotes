# Üstdüşümlü Matris Sistemini Çözmek

[1] yazisinda anlatilan sistemi seyrek matrisler ve eslenik gradyan
tekniklerini kullanarak cozmenin iki yolu alttadir.

Once [1]'deki uc matrisi tekrar olusturualim,

```python
from sympy import symbols, latex, simplify
from sympy.matrices import Matrix
from scipy.sparse import csr_matrix
import pandas as pd, numpy as np
import pickle, json

C,S,C1,C2,L,A,E,I = symbols("C,S,C1,C2,L,A,E,I")
kprime = Matrix([ [C1, 0, 0, -C1, 0, 0],
                  [0, 12*C2, 6*C2*L, 0, -12*C2, 6*C2*L],
                  [0, 6*C2*L, 4*C2*L**2, 0, -6*C2*L, 2*C2*L**2],
                  [-C1, 0, 0, C1, 0, 0],
                  [0, -12*C2, -6*C2*L, 0, 12*C2, -6*C2*L],
                  [0, 6*C2*L, 2*C2*L**2, 0, -6*C2*L, 4*C2*L**2]])

T = Matrix([[C,S,0,0,0,0],[-S,C,0,0,0,0],[0,0,1,0,0,0],
            [0,0,0,C,S,0],[0,0,0,-S,C,0],[0,0,0,0,0,1]])

res = T.transpose()*kprime*T
res = res.subs(C1,A*E/L) 
frame = res.subs(C2,E*I/L**3)

res = res / (E/L) 

from sympy import symbols, latex, simplify
from sympy.matrices import Matrix
import pickle, pandas as pd
pd.set_option('display.max_columns', None)
pd.set_option("display.precision", 4)
C,S,L,A,E,I = symbols("C,S,L,A,E,I")

d = {L:3000.0, C:0.0, S:1.0, E:200.0*1e3, A:6500.0, I:80.0*1e6}
res = frame.subs(d) / (1e3*d[E]/d[L])
df1 = pd.DataFrame(np.array(res).astype(np.float64))
vars1 = ['u1','v1','phi1','u2','v2','phi2']
df1.columns = vars1; df1.index = vars1 

d = {L:3000.0, C:1.0, S:0.0, E:200.0*1e3, A:6500.0, I:40.0*1e6}
res = frame.subs(d) / (1e3*d[E]/d[L])
vars2 = ['u2','v2','phi2','u3','v3','phi3']
df2 = pd.DataFrame(np.array(res).astype(np.float64))
df2.columns = vars2; df2.index = vars2

d = {L:3000.0, C:0.0, S:-1, E:200.0*1e3, A:6500.0, I:80.0*1e6}
res = frame.subs(d) / (1e3*d[E]/d[L])
vars3 = ['u3','v3','phi3','u4','v4','phi4']
df3 = pd.DataFrame(np.array(res).astype(np.float64))
df3.columns = vars2; df3.index = vars2
```

Bu matrisleri ustdutum ile birlestirmek istiyoruz. Bunun icin her
matriste diger matristekilere uyan degiskenleri toplamak
gerekiyor. [1] yazisinda gosterim amacli her matrisi nihai boyutlara
buyutmustuk, ve ayni boyutta olan uc matrisi ustdusum icin
toplamistik.

Fakat bu islem bellekte tutulan yer, performans icin ideal
olmayabilir.  Bir yogun matrisi buyutunce sifir olan degerlerin bile
bellekte depolanmasi gerekiyor. Bu durum ayni sekilde ustdusum matrisi
sonucu icin de gecerli.

Alttaki ilk yontem matrisleri Python sozlugu olarak muhafaza ediyor.
Her alt matrisi sozluk olarak gezilir, ve nihai matris bir sozluk
olarak olusturulur. Matrisler bir sozluk icinde sozluk olarak temsil
edilir, her satir ayri bir sozluktur, ilk anahtar u1, ikinci v1, boyle
gidiyor. Ilk satirin anahtari `u1`, ilk kolonun anahtari ayni sekilde.

Sozluk yapisini kullanmanin bir faydasi bildik bir yapi olmasi ve
aynen diger seyrek depolama yaklasimlarinda oldugu gibi kodun geri
kalani olmayan degerin sifir kabul edebilir.

```python
import pandas as pd, pickle

pd.set_option("display.precision", 4)
pd.set_option('display.max_columns', None)

def mult_A_b(A,b):
    return dict((k, sum(b[key]*row.get(key, 0) for key in b)) for k,row in A.items()  )

def dot(a,b):
    return sum(b[key]*a.get(key, 0) for key in b)

def vec_sum(a,b):
    return {k: a.get(k, 0) + b.get(k, 0) for k in set(a) | set(b)}

def vec_subt(a,b):
    return {k: a.get(k, 0) - b.get(k, 0) for k in set(a) | set(b)}

def vec_scalar_times (vec,a):
    return dict((k,v*a) for k,v in vec.items())

def vec_scalar_sum (vec,a):
    return dict((k,v+a) for k,v in vec.items())

def vec_scalar_subt (vec,a):
    return dict((k,v-a) for k,v in vec.items())

df_super = {}

for v in df1.index: df_super[v] = {}
for v in df2.index: df_super[v] = {}
for v in df3.index: df_super[v] = {}

drop_vars = ['u1','v1','phi1','u4','v4','phi4']

def add_to_super(df):

    for rowid,row in df.to_dict().items():
        if rowid in drop_vars: continue
        for k,v in row.items():
            if v != 0 and k not in drop_vars:
                #print (rowid,k)
                df_super[rowid][k] = df_super[rowid].get(k,0) + v
 
add_to_super(df1)
add_to_super(df2)
add_to_super(df3)
```

Cozmek icin [3]'te anlatilan eslenik gradyan (conjugate gradient)
yontemik kodlandi. CG kodu altta A matrisini sozluk icinde sozluk
olarak aliyor. 

```python
A = df_super.copy()

for id,row in A.items():
    row.update( (key, value * 66.67*1e3) for key, value in row.items() )

b = {'u2': 4*1e4, 'phi3': 5*1e5}

p = b; r = b; x = {}

r2 = dot(r,r)

for i in range(40):

    Ap = mult_A_b(A,p)

    alpha = r2 / dot(Ap, p)

    x = vec_sum(x,vec_scalar_times(p,alpha))

    r = vec_subt(r,vec_scalar_times(Ap,alpha))

    r2old = r2

    r2 = dot(r,r)

    beta = r2 / r2old

    p = vec_sum(r,vec_scalar_times(p,beta))

print (x)
```

```text
{'v1': 0.0, 'u1': 0.0, 'u3': 8.689191625966664, 'phi2':
-0.0020595226448240213, 'v2': -3.3229233662294724e-17, 'v3':
-0.01250651341977623, 'phi3': 0.001035030753854022, 'u2':
8.714002731297926, 'phi1': 0.0}
```


```python
from scipy.sparse.linalg import cg
import numpy as np, scipy.sparse as sps, pickle

pd.set_option("display.precision", 4)
pd.set_option('display.max_columns', None)


all_vars = ['u1','v1','phi1','u2','v2','phi2','u3','v3','phi3','u4','v4','phi4']

drop_vars = ['u1','v1','phi1','u4','v4','phi4']

A = sps.lil_matrix((12,12))

def add_to_super2(df):

    for id,row in df.iterrows():
        for c in df.columns:
            A[all_vars.index(id),all_vars.index(c)] += row[c]
 
add_to_super2(df1)
add_to_super2(df2)
add_to_super2(df3)

index_to_drop = [all_vars.index(x) for x in drop_vars]

to_keep = list(set(range(A.shape[1]))-set(index_to_drop))    
A2 = A[:,to_keep]
to_keep = list(set(range(A.shape[0]))-set(index_to_drop))    
A3 = A2[to_keep,:]

b = np.array([4*1e4, 0, 0, 0, 0, 5*1e5])

x, exit_code = cg(66.67*1e3*A3, b)

print (x)
```

```text
[ 8.71400273e+00  2.09901541e-16 -2.05952264e-03  8.68919163e+00
 -1.25065134e-02  1.03503075e-03]
```

Kaynaklar

[1] <a href="../../../phy/phy_020_strs_06/materyel_mekanigi__6.html">Materyel Mekaniği - 6</a>

[2] https://docs.scipy.org/doc/scipy/reference/generated/scipy.sparse.linalg.cg.html

[3] <a href="../../..//compscieng/compscieng_2_19/ders_2.19.html">Ders 2.19</a>

