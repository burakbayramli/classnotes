# Pymacs

Lisp sürekli kullandığım dillerden biri değil; sadece uzun aralarla
Emacs editörümü kışiselleştirmek için kullanıyorum ve uzun süre
geçtikten bazı Lisp numaralarını tekrar öğrenmem gerekiyor. Bir araçı
birden yerde fazla kullanabilme meraklısı olduğumüz için Lisp'ten
mümkün olduğu kadar uzak durmaya karar verdik. Bu kararda önemli bir
faktör, bir editör uzantısı için okkalı kodlama yapmamız gerektiği
idi... Peki bu iş hangi dilde yapılacaktı?

Bilimsel hesaplama icin zaten Python kullanmaya karar vermistik. Acaba
Python'u Emacs'i uzatmak/kisisellestirmek icin kullanamaz miyiz?

Editör uzantısı kodlamanın ağır sıklet kısmı için de Perl kullanmayı
düşünüyorduk, çünkü bir sürü dosya/dizin gezme/işleme gerekecekti,
fakat bu kullanımda Perl yerine de Python kullanabilirdik. Bir taşla
iki kuş.

Biraz arama sonucunda Emacs'i Python ile uzatmami saglayacak bir araci
buldum: Pymacs. Pymacs, tum Emacs lisp fonksiyonlarina bir Python
script'i icinden ve tum Python modullerine de Emacs icinden
erisebilmenize izin veriyor. Kurmak icin ya apt-get ile ya da kaynagi
indirip python setup.py install ile kurabilirsiniz.

https://github.com/pinard/Pymacs

Hemen bir Emacs uzantısı yazalım, mesela pymaçstest.py adında bir
program yazalım; bu programı /usr/share/emacs/22.2/site-lisp/ altında
diğer EL dosyaları ile birlikte tuttuğunuzu varsayalım. O zaman .emacs
ıcinden

```
(pymacs-load "/usr/share/emacs/22.2/site-lisp/pymacstest")
```

çağrısı yüklemek için yeterli. Bu kod içinde neler olsun?

```
from Pymacs import lispdef test():    lisp.message("i am here")
```

Gördüğünüz üzere lisp objesi üzerinde Emacs metotları var;
lisp.message çağrısı elisp içindeki (message "..") çağrısını
yapıyor. pymaçstest.test çağrısını yapmak artık çocuk oyuncağı, Pymacs
ışimlendirme kurallarına göre, dosya ismi + "-" + metot ismi
kullanarak bu çağrı Lisp tarafından yapılabiliyor. Metot ismi demek ki
pymaçstest-test olacak.

Hızlı geliştirme için bazı numaralar; Python kodu değiştiği zaman
modülü tekrar yüklettirmek için *Pymacs* ışımlı buffer'i oldurmek,
sonra modülü tekrar yüklemek. Bizim .emacs dosyamız şu anda şöyle
(başka modül ve metot ışimleriyle):

```
(defun reload-pyjde()
  (interactive)
  (if (buffer-live-p (get-buffer "*Pymacs*" ))
  (kill-buffer (get-buffer         "*Pymacs*")))
  (pymacs-load "/usr/share/emacs/22.2/site-lisp/pyjde"))
  (defun test-me()(interactive)(pyjde-test))
  (global-set-key [f5] 'test-me)
  (global-set-key [f11] 'reload-pyjde)
  (reload-pyjde) 
```

Böylece geliştirme sırasında pyjde.py dosyası değiştikçe F11 tuşu ile
kodu tekrar yükletip F5 ile çağrıyı yapabiliyoruz. Metot test-me()
daha sonra başka yerlere map edecek, bu tuşa gerek kalmayacak fakat
durumu anladınız sanıyorum.

Böylece alet çantamızdaki Lisp, Perl'den aynı anda kurtulduk, ayrıca
hesapsal işler için Python kullanacağız - iki tool gitti, yerine üç iş
yapan tek tool geldi.

Bazı komutlar:

Üzerinde olduğumüz nokta bir blok içinde, bu bloğa tekabül eden tüm
metni istiyoruz,

```
b = lisp.search_forward("[BITIS]")e = lisp.search_backward("[BASLA]")content = lisp.buffer_substring(b, e)
```

Burada Emacs LİSP fonksiyonlarından ikisini kullanmış olduk.

Herhangi bir noktada *Messages* içinde ve Emacs alt boşluğunda
gözükebilecek bir metin basmak için

```
lisp.message("mesaj")
```

İçinde olduğumüz Emacs buffer'in ismi

```
lisp.buffer_name()
```

Icinde oldugumuz dosyanin ismi

```
lisp.buffer_file_name()
```

Diyelim ki olduğumuz noktaya bir sürü işlemden sonra dönmek istiyoruz

```
remember_where = lisp.point()...
lisp.goto_char(remember_where)
```





