# Cython

Python icinden C cagirmanin, Python kodunu C'ye cevirmenin yani daha
hizli isler hale getirmenin yollarindan olan Cython'un populerligi
artiyor. Bu arac ile Python icinde, Python sozdizimine benzer bir
sekilde yazdiginiz kodlardan C kodu uretilmesi mumkun. Bir fark, bu
kodu derlemek icin "python" cagrisi yerine "cython" cagrisi kullanmak
gerekli. Bu cagri sonrasi .c dosyasi uretilecek, bu dosya gcc ile .o
sonra .so dosyasina derlenecek. Elde edilen .so dosyasi artik pur
Python tarafindan "import" edilebilir bir modul olacaktir.

Kurmak icin

http://cython.org/#download

Bildigimiz unzip, ve python setup.py install.

Bizim ornek kodlar bu yazinin altinda. Dosya p1.py icindeki pur
Python, hemen python p1.py ile isletilebilir. c1.pyx icinde cython ile
C'ye cevirilebilecek kodlar var. Bunlar "sh make.sh" ile hemen .o ve
.so yapilabilir. Ardindan "python c1-main.py" ile hizlandirilmis
kodlari isletmek mumkun. Zaman her iki koddan rapor edilecektir.

Alttaki baglantida konu hakkinda daha fazla ornek bulunabilir.

Dikkat: Python'a benzer kodlarin bir test.pyx icinde yazildigini
varsayalim, derlenince bu kodun test.c dosyasina, ve test.so ile ayni
isimdeki so kutuphanesine derlenmesi gereklidir.

c1.pyx

```
import math

def great_circle(float lon1,float lat1,float lon2,float lat2):
    cdef float radius = 3956.0
    cdef float pi = 3.14159265
    cdef float x = pi/180.0
    cdef float a,b,theta,c

    a = (90.0-lat1)*(x)
    b = (90.0-lat2)*(x)
    theta = (lon2-lon1)*(x)
    c = math.acos((math.cos(a)*math.cos(b)) + (math.sin(a)*math.sin(b)*math.cos(theta)))
    return radius*c
```

```
import timeit  

lon1, lat1, lon2, lat2 = -72.345, 34.323, -61.823, 54.826
num = 500000

t = timeit.Timer("p1.great_circle(%f,%f,%f,%f)" % (lon1,lat1,lon2,lat2),
                       "import p1")
print "Pure python function", t.timeit(num), "sec"
```

c1-main.py

```
import timeit

lon1, lat1, lon2, lat2 = -72.345, 34.323, -61.823, 54.826
num = 500000

t = timeit.Timer("c1.great_circle(%f,%f,%f,%f)" % (lon1,lat1,lon2,lat2),
                  "import c1")
print "Cython function (still using python math)", t.timeit(num), "sec"
```


p1.py


```
import math

def great_circle(lon1,lat1,lon2,lat2):
    radius = 3956 #miles
    x = math.pi/180.0
    a = (90.0-lat1)*(x)
    b = (90.0-lat2)*(x)
    theta = (lon2-lon1)*(x)
    c = math.acos((math.cos(a)*math.cos(b)) +
                  (math.sin(a)*math.sin(b)*math.cos(theta)))

    return radius*c
```


```
import timeit  

lon1, lat1, lon2, lat2 = -72.345, 34.323, -61.823, 54.826
num = 500000

t = timeit.Timer("p1.great_circle(%f,%f,%f,%f)" % (lon1,lat1,lon2,lat2),

                       "import p1")

print "Pure python function", t.timeit(num), "sec"
```


make.sh

```
cython c1.pyx

gcc -Wall  -I/usr/include/python2.7 -lpython2.7 -c c1.c

gcc -shared c1.o -o c1.so
```








