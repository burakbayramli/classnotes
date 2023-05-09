# Python Radyo

Internet'ten bedava radyo dinleyebilmek icin bazi sayfalar var, fakat
hepsine HTML bazli girip, belli yerlere tiklamak, beklemek, vs. gibi
isler cok ugrastirici olabiliyor. Python bazli bir program yazilmis,
kodlar acik, programi komut satirindan baslatiyorsunuz, bir suru kanal
icinden istediginiz seciyorsunuz, alet arka planda o kanaldaki muzigi
calan HTML sayfalarina baglanip oradan gelen muzigi canli olarak
aktariyor (streaming).

En iyi bilinen program `pyradio`. Normal `pip` ile kurulur, `pyradı`
ile başlatınca bir radya listesi görülür.

Güzel bir özellik kendi bir .pls adresi vererek yeni kanallar
ekleyebilmek. Mesela

https://somafm.com/listen/

adresinde programın ilk listesinde gösterilmeyen bazı seçenekler
var. Eğer daha düşük hızda dinlemek istiyorsam 32 Kbit/s için bir ÜRL var,

https://somafm.com/secretagent32.pls

Bu adresi radyoya ekleyebilirim. `pyradio -a` deriz, ve kanal ismi
(herhangi bir isim) ve üstteki adres girilirse `pyradio` ya bir dahaki
girişte bu kanal listede gözükecektir.

Belki daha da kolay bir yaklaşım `mpv` komutu pls ÜRL'e bağlanacaktır.



