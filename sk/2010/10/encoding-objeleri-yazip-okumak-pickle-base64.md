# Kodlama (Encoding), Objeleri Yazip Okumak - Pickle, zlib, base64

Pickle

Python pickle servisi herhangi bir nesneyi alıp dosyaya yazabilmek
yeteneğine sahip. Bir dictionary objemiz olduğunu düşünelim:

```python
dict = {}
dict['a'] = 33
dict['b'] = 55
```

Bu nesneyi şöyle dosyaya yazarız

```python
import pickle
pickle.dump(dict, open('dict.pkl', 'wb'))
```

Geri okumak için

```python
dict= pickle.load(open("dict.pkl","rb"))
f.close()
```

Dikkat: pickle objeleri kendine özel ikisel bir formatta yazar ve
versiyonlar arası uyumsuzluk problemi çıkabilir.  Paketlerarası
versiyon, ya da Python 2 ve 3 arası uyumsuzluklar görülebilir. Bu
sebeple eğer mümkün ise pickle'ları çok kullanmamak en iyisi.

Pickle kütüphanesi diske yazmak yerine hafızada string bazlı çıktı da
verebilir.

```python
import pickle
a1 = list(range(10))
a1b = pickle.dumps(a1)
print (a1b)
```

```text
b'\x80\x03]q\x00(K\x00K\x01K\x02K\x03K\x04K\x05K\x06K\x07K\x08K\te.'
```

```python
a2 = pickle.loads(a1b)
print (a2)
```

```text
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

Sıkıştırarak  ve String Olarak Yazmak

Bir Numpy matrisi çetrefil bir objedir aslında. Onu sıkıştırarak bir
metin haline döndürebilir miyiz? Bu lazım olabilir çünkü ikisel
(binary) değil bir dosyada, veri tabanında rahat depolamak için metin
formatı daha kullanışlı olabilir ama hala sıkıştırma gerekmektedir. Bu
durumlarda zlib ve base64 kodlaması kullanılabilir.

```python
import numpy as np, zlib

a1 = np.eye(3) 
a1[0,1] = 3.4423423423423
# liste yap, onu string yap, sonra kodla
a2 = list(a1.reshape(9))
a3 = str(a2).encode('zlib').encode('base64')

print a1
print a2
print 'kodlama', a3

# geriye cevir
d1 = a3.decode('base64').decode('zlib')
d2 = eval(d1)
d3 = np.array(d2).reshape(3,3)

print d1
print d2
print d3
print d3.shape
```

```
[[1.         3.44234234 0.        ]

 [0.         1.         0.        ]

 [0.         0.         1.        ]]

[1.0, 3.4423423423423, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]

kodlama eJyLNtQz0FEw1jMxMTKGIx0FA5AomDCEs1DEYgE2uQqF

[1.0, 3.4423423423423, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]

[1.0, 3.4423423423423, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]

[[1.         3.44234234 0.        ]

 [0.         1.         0.        ]

 [0.         0.         1.        ]]

(3, 3)
```

String tipinden kodlanmış bir metne, oradan geriye nasıl gidilir görelim,

```python
import base64
s = 'bubirkelime'
print (s)
```

```text
bubirkelime
```

```python
b = bytes(s,'utf-8')
print (type(b))
print (b)
```

```text
<class 'bytes'>
b'bubirkelime'
```

```python
e = base64.encodestring(b)
print (e)
```

```text
b'YnViaXJrZWxpbWU=\n'
```

```python
d = base64.decodestring(e)
print (d)
```

```text
b'bubirkelime'
```

Bir örnek daha,

```python
import pickle, base64

a = list(range(10))
a = base64.encodestring(pickle.dumps(a)).decode()
print (a)
b = pickle.loads(base64.decodestring(a.encode('utf-8')))
print (b)
```

```text
b'gANdcQAoSwBLAUsCSwNLBEsFSwZLB0sISwllLg==\n'
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

Sonda hep bir `\n` olduğunu farkedebiliriz.. Bu ek aslında gerekli
değil, onu çıkartsak ta geriye kodlama bize aynı sonucu verecektir.

