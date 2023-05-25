# Baz İstasyonu ile Yer Bulma, Android

Bu bölüme bazı ekler:

Altta baz istasyonları hakkında veri tabanından bahsedildi. Telefonun
o anda bağlı olduğu baz istasyonun verisini Android'den almak ve
kaydetmek için şurada bahsettiğimiz ölçüm toplayıcı kodlara ekler
yaptık. Artık cell.txt adlı bir text dosyasında o anda bağlı olunan
(servis veren) baz istasyonunun bilgileri yazılıyor. Mesela biraz önce
bir örnek topladık, bir satırda

262:02:15:13227508:

Bunlar sirasiyla mcc, mnc, lac, cid kodlari. Bu kodlari kullanarak
opencellid verisine bakiyoruz, ve

UMTS,262,2,15,13227508,167,13.321302,52.502313,185,9,1,1458941010,1478909517,-101

satirini buluyoruz, yani enlem/boylam 52.502313,13.321302 imis
(hakikaten de oradaydik). Bu en yakin baz istasyonunun yeri.

GPS ile algilayici fuzyonu (sensor fusion) yapilarak bu veri oldugumuz
yeri daha iyi hesaplamak icin kullanilabilir. GPS bilindigi gibi
duvarlardan yansima sonucu bazi sinyallerin yolunun uzamasi sebebiyle
(multipath problem) bazen yanlis sonuclar verebiliyor.  GPS baglanti
da kuramayabilir bazen, ya da  baglanma uzun surebilir. Tum bu
sebeplerden dolayi baz istasyonlari yer bulmak icin takviye amacli
kullanilabilir.

Tum dunyadaki baz istasyon kodlari ve yerlerini kayitli tutan bir acik
yazilim projesi,

opencellid.org

Baz istasyonu kodu uzerinden bulunur, ve tabanda istasyonun enlem  /
boylami da vardir, 3 veya daha fazla istasyon ile ucgenleme
yapilabilir. Istasyon tabanini almak icin, once Database | request for
API key secilir, email / sifre ile kayit olduktan sonra ile bir arayuz
anahtari alinir; Sonra Database | Download Database ile ve API
anahtari girilerek taban indirilir.

Tum taban oldukca buyuk, sıkıştırılmış hali bile 600 MB civari - eh
normal, tum dunyadaki istasyonlar kayitlanmaya ugrasiliyor. Bu dosyayi
kucultmek icin icinde oldugumuz sehir disindakileri
filtreleyebiliriz. Mesela sadece istenen enlem / boylam yakininda
olanlar icin alttaki kod, diyelim icinde 13 enlem 52 ile baslayan baz
istasyonlarini alip ekrana basiyor (komut satirinda > ile cikti
dosyarina yonlendirilir)

```
import re
with open(...) as f:
    for line in f:
        if re.search("13\.\d+,52\.\d+",line): print line.strip()
```

Not: Kaynaklarda telefonun etrafindaki birden fazla olabilecek baz
istasyonunun bilgisini almak icin
TelephonyManager.getNeighboringCellInfo cagrisindan bahsediliyor. Bu
cagri "kizaga alinmis (deprecated)" bir cagri, artik onun
yerine TelephonyManager.getAllCellInfo tavsiye ediliyor, API 17'den
itibaren bu cagri Android API kod bazinda var. Fakat tum telefonlar bu
cagriyi halen desteklemiyor, biz Android 6.0 (API 23) destekli olan
bir telefonda denedik (ucuz bir marka) ve getAllCellInfo bos bir liste
geri getirdi. 

