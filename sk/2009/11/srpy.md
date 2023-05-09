# SrPy

Java JVM icinde Python kodu isletebilen Jython, C kodlari kullanan
Python kutuphaneleri ne yazik ki isletemedigi icin Scipy, Numpy gibi
icinde C kodu iceren kutuphanelere erismek icin, ayri sureclerde
(process) isleyen Python kodlari lazim. Bu baglanti Python -> Python
iletisimi olacak; Client tarafindaki Python, Jython ile isleyecek,
server tarafi ise bildigimiz komut satirindan "python" komutu ile
baslattigimiz kodlar olacak. Bu kodlar her turlu C bazli kutuphaneye
erisebilecek, sonucu baglanan tarafa dondurecek.

SrPy, Simple Remote Python paketi iki surec (process) arasinda uzak
cagri (remote procedure call) yapilabilmesini sagliyor. Kurmak
icinhttp://code.google.com/p/srpy/adresinden kodu indirin, actiktan
sonra python ez_install.py ile kodlari kurabilirsiniz.Deneme icin
server.py adli bir dosyadafrom pylab import *def doit(): return
normal(0,sqrt(10))Simdi srpy kurulan dizine girerek: python
srpy/srpyapp.py isletelim. Bu bir server sureci baslatacak. Soyle bir
ekran geliyor:

```
SRPy Server - Simple Remote Python, Network Server

http://code.google.com/p/srpy/ - updates, documentation, examples and
support

Starting Basic Server...
URI info:PYRO://127.0.1.1:7766/[BIR SAYI]
```

Client tarafi ise suna benzer:

```
import syssys.path.append('[SRPY DIZINI]')

import srpy

pyeng=srpy.PythonEngine("[PYRO IBARESINI ICEREN URL TAMAMEN]")

pyeng.imp("sys")

pyeng.eval("sys.path.append('[SERVER.PY KODUNUN DIZINI]')")

pyeng.exe("from server import doit");

pyeng.exe("x = doit()")

print pyeng.get('x')pyeng.exe("x = doit()")
print pyeng.get('x')pyeng.exe("x = doit()")

print pyeng.get('x')
```

Ustteki kodu Jython ile islettigimizde Numpy uzerinden uretilmis bir
rasgele sayinin ekrana basildigini gorecegiz.





