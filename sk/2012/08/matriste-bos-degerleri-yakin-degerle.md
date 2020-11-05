# Matriste Bos Degerleri Yakin Degerle Doldurmak (Interpolation)

Eger icinde nan yani "tanimsiz" ve "bos" degerler olan bir vektorumuz
var ise, bu tanimsiz degerlerin yerine, yine ayni vektorde, ve o bos
degerin iki yanindaki degere yakin olan bir degerle doldurmak
isteyebiliriz.

Mesela vektor

```
1, nan, nan, 2, 2, nan, 0

olsun, ve nan diyen yerlerde 1 ve 2 arasi, sonraki nan yerine 2 ve 0
arasi degerler olmali.

```
data = np.array([1, nan, nan, 2, 2, nan, 0])
print databad_indexes = np.isnan(data)
good_indexes = np.logical_not(bad_indexes)
good_data = data[good_indexes]
interpolated = np.interp(bad_indexes.nonzero()[0], good_indexes.nonzero()[0], good_data)
data[bad_indexes] = interpolated
print data
```

Sonuc

```
[ 1.
          1.33333333  1.66666667  2.
     2.
     1.
        0.
    ]
```

Peki islemi bir matris uzerinde, ve her kolon icin ayri ayri yapmak
istersek?

```
def pad(data):
    bad_indexes = np.isnan(data)
    good_indexes = np.logical_not(bad_indexes)
    good_data = data[good_indexes]
    interpolated = np.interp(bad_indexes.nonzero()[0], good_indexes.nonzero()[0], good_data)
    data[bad_indexes] = interpolated
    return dataA = np.array([[1, 20, 300],
              [nan, nan, nan],
              [3, 40, 500]])
A = np.apply_along_axis(pad, 0, A)
```

Sonuc

```
[[   1.   20.  300.]
 [   2.   30.  400.]
 [   3.   40.  500.]]
```

