# Unicode, Ascii, Deascii, Python, Emacs

Her dilden farkli karakterleri ascii haline cevirmek icin, unidecode paketi var,

```
# -*- coding: utf-8 -*-
from unidecode import unidecode
print (unidecode(u"işlerken çagrı"))
```

islerken cagri

olacak, yani TR karakterleri Ingilizce harflerden en yakin olanlarina
tercume edilecek. Bu niye yapilir? Belki metin arama yaparken lazim
olabilir, okuma icin, vs. 

Ters yone gitmek istersek ne yapariz? Yani elde bir ascii halde metin
var, onu Turkcelestirmek istiyoruz, yani s yoksa gereken yere ş koymak
gibi mesela. Burada deasciifier projesi var, bizim baglantisi surada, 

```
from turkish.deasciifier import Deasciifier
dea = Deasciifier("ise giderken isten olmak")
print (dea.convert_to_turkish())
```

Benim Emacs [ayarlarım](../../2004/10/emacs.md) içinde üsttekilerin
editörden nasıl çağrıldığını bulabiliriz 









