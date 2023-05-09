# Web ve JSON ile Servis Arayuzu, UWSGI

Flask islemleri su yazida.

Simdi buyuk olcegi dusunelim. Komut satirindan basit sekilde
baslatilan Flask olceklenemez. Fakat Flask onunde yuk dagitici olarak
gorev yapabilecek ve Flask'e rahat entegre olabilen bazi secenekler
var. Bunlardan biri UWSGI.

http://projects.unbit.it/downloads/

2.0.3 indirin. python setup.py build ve sudo python setup.py install
ile kurun.

Kullanmak icin main.py olan dizinden,

```
nohup sudo uwsgi --processes 4 --http :8080 -s /tmp/mysock.sock \ --module main --callable app  --py-autoreload 1 \
--logto /tmp/main.log  --pidfile /tmp/main.pid \
> /tmp/cour.log 2> /tmp/cour.err < /dev/null &
```

Ustteki cagri birden fazla surec icinde kodunuzu isletime
sunar. Erisim noktasi 8080 port'u uzerinden her istek baska bir surece
gonderilir boylece olceklenme halledilir. Secenek --py-autoreload 1
ile sureclerden biri cokerse otomatik olarak tekrar baslatilmasini
soyluyoruz, boylece dayaniklilik acisindan onemli bir avantaj elde
ediyoruz.

Ayrica --pythonpath  ile ek paket gerekiyorsa onlarin nereden
yuklenmesi gerektigini set edebiliriz. Bu paketler setup.py build
install ile kurulabiliyorsa tabii bu secenege gerek yok, ama bazen
gerekebilir.

Durdurmak icin

```
uwsgi --stop=/tmp/main.pid 

uwsgitop
```

Unlu Unix top gibi sadece uwwsgi sureclerini izlemek icin bir arac yazilmis,

https://github.com/unbit/uwsgitop

Yanliz uwsgi sureclerini izleyebilmek icin ozel bir sekilde baslatmak
lazim, secenek  --stats /tmp/stats.socket eklenmeli. Sonra

```
sudo uwsgitop /tmp/stats.socket
```

ile arac baslatilir.

Not: Bu arada literature gore artik LAMP paketi degismis (ki bu
harfler Linux, Apache, MySQL, PHP'yi temsil ediyordu), artik LEMP
olmus. E harfi nginx'i temsil icin kullaniliyor, bilindigi gibi nginx
"engine x" olarak okunuyor, ve onun E'si A yerine, yani Apache yerini
almis. Ilginc bir gelisme. Bizim icin de PHP yerine Python var tabii -
hala P!

