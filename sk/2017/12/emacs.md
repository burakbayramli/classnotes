# Emacs

Ubuntu'da kurmak icin

sudo apt-get install emacs24

Benim tüm ayarlarım

https://github.com/burakbayramli/kod/tree/master/site-lisp

Türkçe yazmak isteyenler ama benim gibi US klavyeye çok alışıklar
için turkish-deasciifier projesini Emacs'e entegre ettik. Bu ekler
ustteki ayarlarla otomatik olarak geliyor, bir paragrafta iken
M-x deascify-convert tüm paragrafı Türkçe karakterlerine değiştirir
(bazı hatalar oluyor çünkü bazen "ise" kelimesi mesela ise kalmalı,
bazen "işe" olmalı).

Postfix

Diğer ihtiyaçlar için postfix kullanımı var, bu kullanımda alternatifi
olabilecek harfler girildikten sonra, mesela s,c,u,i gibi, M-; ile ek
bir tuşlama yapınca o alternatife geçilir, ş,ç,ü,ı gibi. Bu moda gidip
gelmek mümkün, her an aktive halde olmasına gerek yok,
M-x toggle-input-method bu gelip gidişi yapıyor ya da CTRL-'

Python Kodu İşletmek

Github projelerimizden bir digeri emacs-ipython -  Emacs'te LaTeX
doküman içinde iken Python kodlarını  direk belge içinde işletmek
mümkün (arka planda ipython'a bağlanıyor, yani aynı temel yapı
kullanılıyor), sonuc hemen alta yaziliyor, verbatim ciktisi, ya da
grafik resim dahil komutu olarak.






