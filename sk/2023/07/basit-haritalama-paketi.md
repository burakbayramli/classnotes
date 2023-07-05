# Yeni Haritalama Paketi

Corbada bizim de tuzumuz olsun; İnternet bağlantısı gerektirmeyen,
gerekli verisini paket kurulum dosyalarında taşıyan haritalama paketi
bulamadık, kendimiz yazdık - `simplegeomap`.  Açık yazılım olarak
paylaşılıyor [1], ve PyPi üzerinde kurulmaya hazır whl dosyası var,
`pip install simplegeomap` ile kurulabilir.

Simplegeomap temel ihtiyaçları basit, hızlı bir şekilde cevaplamasi
icin yazilmistir, bu ihtiyaçlar en azından bizim için istenen bir
bölge içine düşen kıta, ülke sınırlarını çizebilmek, sınırlar
dışındaki denizleri belli bir renkte vermek, çok detaylı olmasa da
yükseklik (dağlar) ve şu alanları (nehir, gol gibi) haritalamanın,
raporlamanın mümkün olması.

Smgm yuvarlık olan yerkürenin farklı şekildeki iki boyuta yansıtma
tekniklerini kullanmıyor, en temel yaklaşım olan boylamı x, enlemi y
kordinatı kabul edip grafiklemeyi bu şekilde yapmayı seçiyor. Bu
yaklaşım her çok uzun mesafelerde kesin olmayabilir, fakat yakın
mesafeler ve objelerin genel yerlerini göstermesi açısından
yeterlidir.








Kaynaklar

[1] https://github.com/burakbayramli/simplegeomap

[2] https://pypi.org/project/simplegeomap/