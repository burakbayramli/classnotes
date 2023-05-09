# LAN Icindeki Diger Makinalari Bulmak

Diyelim ki monitoru olmayan (ya da islemeyen) bir makinayi bir cafe'ye
getirdiniz, Wifi baglantisini yapti (hatirladi cunku), simdi ikinci
bir makinadan bu alete baglanmak lazim (ssh ile). Ama ilk makina hangi
IP adresinde acaba?

Cafe gibi yerlerde Wifi hizmeti var ise, Wifi dagitim noktasi arka
planda DHCP kullanir, ve 192.168.2.x gibi adresler her yeni
kullaniciya dagilitir, bir kisiye 192.168.2.100 verilebilir, digerine
192.168.2.5 vs. Diger makinalari (kendi makinamizi en azindan)
"bulmak" icin teker teker ping ile bakabilirdik, ama uzun
surer. Script edelim,

```
import os
for i in range(255):
    cmd = "ping -c 1 192.168.2.%d > /tmp/out" % i
        os.system(cmd)
    if " 0% packet loss" in open("/tmp/out").read():
        print i, "Found"
```

Secenek -c 1 kullanildi, sadece bir kere ping edilmesi icin (yoksa 3
kere denenir). Sonucta tek ilgilendigimiz "0% packet loss" gormek, o
adreste bir makina varsa bu sonucu verir cunku, diger durumlarda "100%
packet loss" ibaresi cevapta oluyor.





