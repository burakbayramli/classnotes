# Flask ile Web Üzerinden Dizin Gezebilmek - webfilebrowser

Bir makinadan diğerinin dosya sistemini gezebilmek, dosyalarını
kopyalama, yer değiştirme, görüntüleme gibi işlemleri yapmaya izin
veren pür Web bazlı çalışan bir kod `webfilebrowser`. 

<img width='340' src='https://github.com/burakbayramli/webfilebrowser/raw/main/webfilebrowser.png'/> 

[https://github.com/burakbayramli/webfilebrowser](https://github.com/burakbayramli/webfilebrowser)

Bize gereken bir özellik, dizüstü makinasında çalışıyoruz, cep
telefonu ya da tablet üzerinden ana makinadaki dosyaları görmek, temel
dosya işlemleri yapmak gerekiyor, VNC gibi uzaktan tüm masaüstü
görüntüsünü taşıyan programlara girmeden Web üzerinden
`webfilebrowser` ile bunları yapabiliyoruz.

İlk hali ile kod yerel kullanım için hazır. Ağdaki diğer makinaların
erişimini sağlamak için `app.py` daki kod, mesela 192.168.1.2 için

```
app.run(host="192.168.1.2",port=8080)
```

olarak değişmeli. Artık üstteki adres ve port üzerinden dosya işlemleri
yapılabilir.

`webfilebrowser` ile yaygın kullanılan dosya tipleri görüntülenebilir,
`.txt`, `.md`, belge olarak `.pdf`, imaj formatları `.png` ve `.jpg`,
hatta video dosyaları da mesela `.mp4`.

Ağdan bahsettik, bu konuyu biraz açalım, ilgilenenlerin bilebileceği
gibi eğer İnternet bağlantımızı cep telefonu üzerinden 'Wifi hotspot'
ile paylaşıyorsak bu özellik aslında ufak bir DHCP servisi işletir,
yani onun üzerinden İnternet'e bağlananlara sadece yerel ağda geçerli
olan bir İnternet IP adresi atanır. Çoğunlukla bu adres `192.168.` ile
başlar, mesela telefon `192.168.1.1`, ona bağlanan dizüstü
`192.168.1.2` olabilir. Normal kullanımda bu adreslerin bilinmesine
gerek yok, fakat ana bilgisayarda bir Web servisi başlatırsam, mesela
Flask ile, üstteki port ile diyelim, cep telefonu tarayıcısı bu
servise yerel erişim üzerinden (İnternet'e çıkmadan)
`https://192.168.1.2:8080` ile bağlanabilir. Biz `webfilebrowser`
uygulamasını bu şekilde servis ediyoruz.

DHCP adresi dinamik olarak atanır (DHCP açılımı *Dynamic* Host
Configuration Protocol) , teorik olarak her sefer farklı bir IP adresi
gelebilir, fakat çoğunlukla DHCP aynı makinaya aynı adresi veriyor,
herhalde kullanışlılık açısından böyle bir seçim yapmışlar. Adresler
geliştirme makinasında Unix'te `ifconfig -a`, Windows'da `ipconfig`
ile alınabilir.

`192.168.` adresleri dışarından görülmezler, bu sebeple yerel
kullanımları güvenlidir.  Hatta dış Internet bağlantımızı kessek bile
yerel bağlantılar, DHCP hala yerel ortamda işlemeye devam edecektir.



