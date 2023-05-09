# Cookie, ab ve Yuk Testleri

Apache ab ile bir kullaniciyi taklit etmek icin URL uzerinden bilgi
gonderilebilecegi gibi cookie uzerinden de uygulamaya bilgi
gonderebilir.ab -kc 1 -n 1 -C [COOKIE ISMI]=[COOKIE DEGERI]
http://www.site.com/sayfa.seamCogu Web uygulamasi kullanici kimligini
bir cookie uzerinde tutarak, bu cookie servis tarafina gelince
kullaniciyi otomatik olarak sisteme giris yaptirir. Bu kavrami test
amacli olarak kullanamaz miyiz? Hatta yuk testleri icin, pek cok
kullaniciyi taklit etmek icin diyelim ki bir girdi text dosyasi icinde
kimlik degerlerini tutuyoruz (kimlikleri uygulamanin veri tabanindan
cikarttik) ve bir Python script bu idleri oradan okuyup, id sayisi
kadar Thread baslatip (cunku kullanicilarin sisteme eszamanli
girmesini simule ediyoruz) kullanici basina bir sayga istegini N kadar
arka arkaya isletebilir.Script cookied_page_load.py suna benzer:


```
import threading, re, os, sys

times = int(sys.argv[1])

class Caller(threading.Thread):
def __init__(self, userId):
   threading.Thread.__init__(self)
   self.userId = userId
   def run(self): url = "http://www.site.com/sayfa.seam"
   cmd = "ab -kc 1 -n " + str(times) + " -C userId=" + self.userId + " " + url
   print cmd
   os.system(cmd)
   ts = []
   f = open ("[ID DOSYASI]")
   for id in f.readlines():
   id = id.replace("\n","")
   a = Caller(id)a.start()ts.append(a)
   for t in ts:t.join()
```

Bu script

python cookied_page_load.py 10

ile [ID DOSYASI] icinde buldugu kimlikleri userId adli Cookie'ye koyup
sistemin belirttigimiz bir sayfasina kullanici basina 10 kere girmeye
calisacaktir.




