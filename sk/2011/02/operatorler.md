# Operatorler

Bir class tıpı üzerinde işlemesini istediğimiz operatörler için o
tipin class'ı içindeki "özel" fonksiyon isimlerini kullanmamız
gerekir.

Mesela X adlı bir tip için küçüktür işaretini tanımlamak isteseydik,
`__lt__` fonksiyonunu tanımlamamız gerekirdi.

Örnek:

```python
class X:
   def __init__(self, x):
       self.x = x
   def __lt__(self, other):
      if (other < self.x): return True
      return False

xx = X(10)
yy = X(20)
if (xx < yy): print "kucuk"
if (xx > yy): print "degil"
```




